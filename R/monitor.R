#' Monitor Progress of a tasks queue
#'
#' This function provides a minimal console based interface to follow the progression of tasks in a queue
#' One 'queue' can follows an arbitrary number of tasks run in parallel.
#'
#' @param from source of the queue to monitor, simplest is the queue name (without the queue prefix)
#' @param redis redis client definition object, returned by \code{redis_client()}
#' @param options list (see section options)
#' @param debug show verbose information message if TRUE (for development)
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
#' @section from Parameter:
#' from parameter can be either a single character value,
#' \describe{
#'   \item{key}{redis key where the current queue name is stored in case of publish strategy}
#' }
#'
#' @examples
#' \dontrun{
#' # Follow the progress of the queue named "queue1"
#' redis_progress_monitor("queue1")
#'
#' # Following tasks published in a list
#' redis_progress_monitor(list(key="myqueues"))
#'
#' # Using progress bar and show only the 10 last log messages
#' redis_progress_monitor(from=queue1, options=list(use.bar=TRUE, log.size=10))
#' }
#'
redis_progress_monitor = function(from, redis=NULL, options=list(), debug=TRUE) {

    if( is.null(redis) ) {
        redis = redis_client()
    }

    if(is.null(redis$handle("hashGetAll"))) {
        stop("This redis client doesnt implement hashGetAll, it is not useable to monitor tasks")
    }

    if(debug) {
        message("Connecting")
    }
    redis$connect()
    if( !is.null(redis$cnx) ) {
        if(debug) {
            message("Connected")
        }
    }

    queues = get_redis_queue(from, redis)

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
            b = str_bar(prop, options$bar.size)
        } else {
            b = ""
        }

        sprintf(paste0("%-", width, "s (%5s) ",b, format), name, elapsed_time(elapsed), value)
    }

    log.names = queue_log_name(queues)
    log.indexes = rep(0, length(log.names))

    defs = list(use.bar = TRUE, bar.size = 20, steps = NULL, ncol = 2, sleep=1, log.size=10)

    options = modifyList(defs, options)

    cls = clear_console()

    if(debug) {
        message("Monitoring queues ", queues)
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
                message("Queue : ", name)
            }

            # Get all data
            h = redis$hashGetAll(name)

            if(is.null(h)) {
                warning("Unable to get hash", name," waiting...")
                Sys.sleep(5)
                next()
            } else {
                if(debug) {
                    message("Hash ", length(h), "fields")
                }
            }

            n = names(h)
            n = n[!grepl("^_", n)]

            # Task names (should not have ":")
            tasks = n[grepl("^[^:]+$", n)]

            values = h[tasks]

            # For some client type counter are stored are raw string, convert
            values = lapply(values, as.integer)

            started = h[ task_started(tasks) ]
            steps = h[ task_steps(tasks) ]

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

#' Internal function to clear the console
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

#' Get Redis queue
#' Internal function
#' @param from from parameter
#' @param redis redis_client
#' @noRd
get_redis_queue = function(from, redis) {
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
            queues = redis_queue_name(redis$get(from$key))
            if(is.null(queues)) {
                stop(paste("Unable to find queue name using key", from$key))
            }
        }
    }
    queues
}

