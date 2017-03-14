

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

