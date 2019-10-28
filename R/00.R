
#' Get package options
#' From 'redis.progress' options() entry
#' @param name option name to get, if null returns all options
#' @return option value
#' Internal function
#' @noRd
get_option = function(name=NULL) {
    o = base::getOption("redisProgress")
    if(is.null(name)) {
        o
    } else {
        o[[name]]
    }
}

.onLoad <- function(libname, pkgname) {

    o = get_option()
    defaults = list(
        redis_type = "rredis",
        queue_prefix = "R:progress:"
    )

    if( is.null(o) ) {
        o = defaults
    } else {
        for(n in names(defaults)) {
            if(is.null(o[[n]])) {
                o[[n]] = defaults[[n]]
            }
        }
    }
    base::options("redisProgress"=o)
    invisible()
}

