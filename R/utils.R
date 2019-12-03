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
#' redis_queue_name("queue-01")
#'
#' @export
redis_queue_name = function(name) {
    prefix = get_option("queue_prefix")
    paste0(prefix, name)
}

#' Defined a way to publish a queue name
#'
#' In some context you need to create a random queue name to make it unique. By doing this you don't know
#' the real queue name before to run the script.
#' The publish feature allows to store this queue name once it's created under a predefined key name.
#'
#' This function should be used to set the `publish` parameter of \code{\link{create_redis_progress}}
#'
#' @param name key name to use to publish last generated queue name
#' @param type type of storage to use (see details)
#' @param prefix use queue prefix to the key (by default false)
#' @details
#' Using 'key' as type will only store the last created, 'list' will store it in a list as the last element
#'
#' @export
#'
#' @examples
#'
#' publish_queue('my-jobs', 'list') # Store task queue names in a list under the 'my-jobs' names
#'
publish_queue = function(name, type=c('key','list'), prefix=FALSE) {
    type = match.arg(type)
    attr(name, "type") <- type
    if(prefix) {
        name = redis_queue_name(name)
    }
    name
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