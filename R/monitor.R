#' Monitor Progress of a tasks queue
#'
#' Basic monitoring of task queue progress created using
#'
#' @param from source of the queue to monitor, simplest is the queue name (without the queue prefix)
#' @param redis redis client definintion object, returned by \code{redis_client()}
#' @param options list (see section options)
#' @param debug show verbose information message if TRUE (for dev)
#' @export
#'
#' @section Options:
#' \describe{
#' \item{use.bar}{use progress bar}
#' \item{bar.size}{size of the progress bar (number of chars)}
#' \item{steps}{number of steps of all the tasks (if always the same and not provided by each task)}
#' \item{ncol}{number of columns to use to show the tasks progress}
#' \item{sleep}{number of seconds to sleep between update}
#' \item{log.size}{number of log messages to show (only lasts)}
#' }
#'
#'
#' @section from Parameter:
#' from parameter can be either a single charater value,
#' \describe{
#'   \item{key}{redis key where the current queue name is stored in case of publish strategy}
#' }
redis_progress_monitor = function(from, redis=NULL, options=list(), debug=TRUE) {

    if( is.null(redis) ) {
        redis = redis_client()
    }

    if(is.null(redis$handle("hashGetAll"))) {
        stop("This redis client doesnt implement hashGetAll, it is not useable to monitor tasks")
    }

    if(debug) {
        cat("Connecting")
    }
    redis$connect()
    if( !is.null(redis$cnx) ) {
        if(debug) {
            cat(" Connected\n")
        }
    }

    if(is.character(from)) {
        queues = redis_queue_name(from)
    } else {
        if( !is.list(from) ) {
            stop("from should be either a character vector")
        }
        if( !is.null(from$name) ) {
            queues = redis_queue_name(from$name)
        }
        if( !is.null(from$key) ) {
            queues = redis$get(from$key)
            if(is.null(queues)) {
                stop(paste("Unable to find queue name using key", from$key))
            }
        }

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
                return(paste0(sprintf("%3d", elapsed), names(m)))
            }
            i = i + 1
        }
        paste0(sprintf("%3d",elapsed), "s")
    }

    format_task = function(name, value, steps, started) {
        prop = NULL # Completion proportion
        if(steps > 0) {
            format = "% 2.1f%%"
            prop = value / steps
            value = round(prop * 100, 1)
        } else {
            format = "%5d"
        }

        elapsed = floor(as.numeric(Sys.time()) - started)

        if(!is.null(prop) && options$use.bar) {
            nbars = floor(prop * options$bar.size)
            b = paste0(" |", str_rep("=", nbars), str_rep(" ", options$bar.size - nbars),"| ")
        } else {
            b = ""
        }

        sprintf(paste0("%-",width,"s (%5s) ",b, format), name, elapsed_time(elapsed), value)
    }

    log.names = paste0(queues, ":logs")
    log.indexes = rep(0, length(log.names))

    defs = list(use.bar = TRUE, bar.size = 20, steps = NULL, ncol = 2, sleep=1, log.size=10)

    options = modifyList(defs, options)

    cls = clear_console()

    if(debug) {
        cat("Monitoring queues ", queues, "\n")
        cls = function() {}
    } else {
        cls = clear_console()
    }

    while(TRUE) {
        # Clear console
        cls()
        for(queue.index in seq_along(queues)) {

            name = queues[queue.index]
            if(debug) {
                cat("Queue : ", name,"\n")
            }

            # Get all data
            h = redis$hashGetAll(name)

            if(is.null(h)) {
                cat("Unable to get hash", name," waiting...\n")
                Sys.sleep(5)
                next()
            } else {
                if(debug) {
                    cat("Hash ", length(h), "fields\n")
                }
            }

            n = names(h)
            n = n[!grepl("^_", n)]

            # Task names (should not have ":")
            tasks = n[grepl("^[^:]+$", n)]

            values = h[tasks]

            # For some client type counter are stored are raw string, convert
            values = lapply(values, as.integer)

            started = h[ paste0(tasks,":started") ]
            steps = h[ paste0(tasks,":steps") ]

            # Maximum
            width = max(nchar(tasks))

            # Format each task cell
            tt = Map(format_task, names(values), values, steps, started)

            log.name = log.names[queue.index]
            logs = try(redis$getTail(log.name, options$log.size))
            if( is(logs, "try-error") ) {
                logs = c()
            }

            # Format console
            cat("\nJobs ", name, " started at ", format(format ="%Y-%m-%d %T", as.POSIXct(h$"_created_", origin="1970-01-01")),"\n\n")
            g = floor(1:length(tt) / options$ncol) + 1
            tt = split(tt, g)
            tt = unlist(lapply(tt, paste, collapse="    "))
            cat(paste(tt, collapse="\n"))
            if(length(logs) > 0) {
                cat("\n  ")
               cat(paste(logs, collapse = "\n  "))
            }
        }
        Sys.sleep(options$sleep)
    }

}

#' @noRd
clear_console = function() {
    default_cls = function() { cat("\014") }

    if(Sys.getenv("RSTUDIO") == "1") {
        return(default_cls)
    }
    os = R.version$os
    if(any(grepl("^darwin", os), grepl("linux",os))) {
        term = Sys.getenv("TERM")
        if( term != "") {
            clear.sequence = system("tput clear", intern=TRUE)
            if(!is.null(clear.sequence) && clear.sequence != "") {
                return(function() { cat(clear.sequence) })
            }
        }
    }
    # default clear function
}
