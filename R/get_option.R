
#' Get package options
#' From 'redis.progress' options() entry
#' @param name option name to get, if null returns all options
get_option = function(name=NULL) {
    o = base::getOption("redisProgress")
    if(is.null(name)) {
        o
    } else {
        o[[name]]
    }
}