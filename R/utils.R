# utils.R

#' Get the full name of the queue including current queue's prefix
#' @param name name of the queue (without prefix)
#' @export
redis_queue_name = function(name) {
    prefix = get_option("queue_prefix")
    paste0(prefix, name)
}

#' Cleanup all job queues with the given name
#' @export
#' @param name name of the queue
#' @param redis redis client object \code{redis_client}
redis_cleanup_progress = function(name, redis=NULL) {
    queue = redis_queue_name(name)
    if(is.null(redis)) {
        redis = redis_client()
    }
    if( is.null(redis$keys) ) {
        stop("keys() command not implemented for this client")
    }
    redis$connect()
    keys = redis$keys(pattern = paste0(queue,"*"))
    invisible(sapply(keys, redis$delete))
}

# From https://github.com/hadley/dplyr/blob/master/R/progress.R
# dplyr package, Hadley Wickham
str_rep <- function(x, i) {
    paste0(rep.int(x, i), collapse = "")
}
