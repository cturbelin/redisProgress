# utils.R

#' Get the real name of the queue including current queue's prefix in the `Redis` server
#'
#' Queue are stored using a common prefix in the `redis` key. The prefix is defined in the options
#' under the `queue_prefix` entry.
#'
#' @param name name of the queue (without prefix)
#' @return queue name with the configured prefix
#'
#' @examples
#' redis_queue_name
#'
#' @export
redis_queue_name = function(name) {
    prefix = get_option("queue_prefix")
    paste0(prefix, name)
}

#' Cleanup all job queues with the given name
#' @export
#' @param name name of the queue
#' @param redis redis client object \code{\link{redis_client}}
#' @return vector of deleted keys as name and deletion result as value
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