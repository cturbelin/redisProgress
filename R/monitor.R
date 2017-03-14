#' Monitor Progress of a tasks queue
#'
#' Basic monitoring of task queue progress created using
#'
#' @param name name of the queue (used unless \code{key} is provided)
#' @param key key name where the current queue name is stored
#' @param use.bar use progress bar
#' @param bar.size size of the progress bar (number of chars)
#' @param steps number of steps of all the tasks (if always the same and not provided by task)
#' @param ncol number of columns
#' @param sleep number of seconds to sleep between update
#' @param redis redis client definintion object, returned by \code{redis_client()}
#' @param log.size number of log messages to show (only lasts)
#' @export
redis_progress_monitor = function(name=NULL, key=NULL, use.bar = TRUE, bar.size = 20, steps = NULL, ncol = 2, sleep=1, log.size=10, redis=NULL) {

    if( is.null(redis) ) {
        redis = redis_client()
    }

    if(is.null(redis$handle("hashGetAll"))) {
        stop("This redis client doesnt implement hashGetAll, it is not useable to monitor tasks")
    }

    redis$connect()

    if( is.null(name) && is.null(key)) {
        stop("either name or key should be provided")
    }

    if(is.null(name) && !is.null(key)) {
        name = redis$get(key)
        if(is.null(name)) {
            stop(paste("Unable to find queue name using key", key))
        }
    } else {
        name = redis_queue_name(name)
    }

    # Compute elapsed string
    elapsed_time = function(elapsed) {
        times = c("d"=86400,"h"=3600, "m"=60)
        i = 1
        n = length(times)
        while(i <= n) {
            m = times[i]
            if(elapsed > m) {
                elapsed = floor(elapsed / m)
                return(paste0(elapsed, names(m)))
            }
            i = i + 1
        }
        paste0(elapsed, "s")
    }

    log.name = paste0(name, ":logs")
    log.index = 0

    while(TRUE) {
        # Get all data
        h = redis$hashGetAll(name)

        if(is.null(h)) {
            cat("Unable to get hash", name," waiting...\n")
            Sys.sleep(5)
            next()
        }

        n = names(h)
        n = n[!grepl("^_", n)]

        # Task names (should not have ":")
        tasks = n[grepl("^[^:]+$", n)]

        values = h[tasks]
        started = h[ paste0(tasks,":started") ]
        steps = h[ paste0(tasks,":steps") ]

        # Maximum
        width = max(nchar(tasks))

        # Format each task cell
        tt = Map(function(name, value, steps, started) {
            prop = NULL # Completion proportion
            if(steps > 0) {
                format = "% 2.1f%%"
                prop = value / steps
                value = round(prop * 100, 1)
            } else {
                format = "%5d"
            }
            elapsed = floor(as.numeric(Sys.time()) - started)

            if(!is.null(prop) && use.bar) {
                nbars = floor(prop * bar.size)
                b = paste0(" |", str_rep("=", nbars), str_rep(" ", bar.size - nbars),"| ")
            } else {
                b = ""
            }

            sprintf(paste0("%-",width,"s (%5s) ",b, format), name, elapsed_time(elapsed), value)
        }, names(values), values, steps, started)


        # Clear console
        cat("\014")

        # Format console
        cat("\nJobs ", name, " started at ", as.POSIXct(h$"_created_", origin="1970-01-01"),"\n\n")
        g = floor(1:length(tt) / ncol) + 1
        tt = split(tt, g)
        tt = unlist(lapply(tt, paste, collapse="    "))
        cat(paste(tt, collapse="\n"))
        Sys.sleep(sleep)
    }

}
