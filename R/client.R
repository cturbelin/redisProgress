#
# Redis client layer
#
# Provides a an abstraction to the redis client library
# Use a very simple implementation to reduce dependencies

#' Client interface for rredis library
RedisClientRRedis = setRefClass("RedisClientRRedis",
    fields = list(
        "args"="list",
        "database"="integer",
        "cnx"="ANY"
    ),
    methods = list(
        initialize = function(host="localhost", port=6379, database=NA, ...) {
            a = list(...)
            a$host = host
            a$port = port
            args <<- a
            database <<- as.integer(database)
        },

        connect = function() {
            require(rredis)
            cnx <<- do.call(rredis::redisConnect, args)
            if( !is.na(database) ) {
                rredis::redisSelect(database)
            }
            cnx
        },

        hashSet = function(key, field, value) {
            rredis::redisHSet(key = key, field=field, value=value, NX=FALSE)
            # rredis HSet returns "0" whatever success or not
            return(TRUE)
        },

        hashGet = function(key, field) {
            rredis::redisHGet(key, field)
        },

        hashGetAll = function(key) {
            rredis::redisHGetAll(key)
        },

        delete = function(key) {
           as.numeric(rredis::redisDelete(key)) > 0
        },

        exists = function(key) {
            rredis::redisExists(key)
        },

        pushTail = function(key, value) {
            as.numeric(rredis::redisLPush(key, value))
        },

        getTail = function(key, size, start=NULL) {
           len = as.integer(rredis::redisLLen(key))
           if(len == 0) {
               return(c())
           }
           if(size > len) {
               size = len
           }
           if( is.null(start) ) {
               start = max(0, len - size)
           }
           unlist(rredis::redisLRange(key, start=start, end=len - 1))
        },

        set = function(key, value) {
            rredis::redisSet(key, value) == "OK"
        },

        get = function(key) {
            rredis::redisGet(key)
        },

        keys = function(pattern="*") {
            rredis::redisKeys(pattern)
        },
        handle = function(name=NULL) {
            cap = c("keys"=TRUE, "hashGetAll"=TRUE)
            if( is.null(name) ) {
                cap
            } else {
                cap[name]
            }
        }
    )
)


#' Client interface for RcppRedis library
RedisClientRcpp = setRefClass("RedisClientRcpp",
    fields = list(
        args="list",
        cnx="ANY",
        database="integer"
    ),
    methods = list(
        initialize = function(host="localhost", port=6379, database=NA, ...) {
            a = list(...)
            a$host = host
            a$port = port
            args <<- a
            database <<- as.integer(database)
        },
        connect = function() {
            "Connect to the Redis database"
            require(RcppRedis)
            p = list(RcppRedis::Redis)
            if( !is.null(args$host) ) {
                p$host = args$host
            }
            if( !is.null(args$port) ) {
                p$port = args$port
            }
            if(!is.null(args$password)) {
                p$password = args$password
            }
            if(!is.null(args$timeout)) {
                p$timeout = args$timeout
            }
            cnx <<- do.call(new, p)
            if( !is.na(database) ) {
                cnx$exec(paste("SELECT", database))
            }
            cnx
        },

        hashSet = function(key, field, value) {
            "Set [field] of hash named by [key]"
            cnx$hset(key, field, value) > 0
        },

        hashGet = function(key, field) {
            "Get field value in hash named by key"
                cnx$hget(key, field)
        },

        delete = function(key) {
            "Delete key"
            cnx$exec(paste("DEL", key)) > 0
        },

        exists = function(key) {
            "Key exists
            @returns boolean
            "
            cnx$exists(key) != 0
        },

        pushTail = function(key, value) {
            "Push value on tail of the list named by key"
            cnx$listLPush(key, value) > 0
        },

        getTail = function(key, size, start=NULL) {
            len = as.integer(cnx$llen(key))
            if(len == 0L) {
                return(c())
            }
            if(size > len) {
                size = len
            }
            if( is.null(start) ) {
                start = max(0L, len - size)
            }
            unlist(cnx$listRange(key, start=start, end=len - 1))
        },

        set = function(key, value) {
            "Set key value"
            cnx$set(key, value) > 0
        },

        get = function(key) {
            "Get key value"
            cnx$get(key)
        },

        keys = function(pattern="*") {
            unlist(client$cnx$exec(paste0("KEYS ", pattern)))
        },

        handle = function(name=NULL) {
            cap = c("keys"=TRUE, "hashGetAll"=FALSE)
            if( is.null(name) ) {
                cap
            } else {
                cap[name]
            }
        }


    )
)

#' Client interface for redux & rrlite library
RedisClientRedux = setRefClass("RedisClientRedux",
  fields = list(
      args="list",
      cnx="ANY",
      database="integer",
      type="character"
  ),
  methods = list(
          initialize = function(host="localhost", port=6379, database=NA, ..., .type="redux") {
              a = list(...)
              a$host = host
              a$port = port
              args <<- a
              database <<- as.integer(database)
              type <<- .type
          },
        connect = function() {
            require(type)
            api_func = switch(type,
                "redux"=redux::hiredis,
                "rrlite"=rrlite::hirlite
            )
            p = list()
            p$host = args$host
            p$port = args$port
            p$password = args$password
            p$db = database
            cnx <<- do.call(api_func, p)
            cnx
        },
        hashSet = function(key, field, value) {
            cnx$HSET(key, field, value) == "OK"
        },

        hashGet = function(key, field=NULL) {
            cnx$HGET(key, field)
        },

        hashGetAll = function(key) {
            cnx$HGETALL(key)
        },

        delete = function(key) {
            cnx$DEL(key) > 0
        },

        exists = function(key) {
            cnx$EXISTS(key) != 0
        },

        pushTail = function(key, value) {
            cnx$LPUSH(key, value) > 0
        },

        getTail = function(key, size, start=NULL) {
            len = as.integer(cnx$LLEN(key))
            if(len == 0L) {
                return(c())
            }
            if(size > len) {
                size = len
            }
            if( is.null(start) ) {
                start = max(0, len - size)
            }
            unlist(cnx$LRANGE(key, start=start, end=len - 1))
        },
        set = function(key, value) {
            cnx$SET(key, value) > 0
        },

        get = function(key) {
            cnx$GET(key)
        },

        keys = function(pattern="*") {
            cnx$KEYS(pattern)
        },
        handle = function(name=NULL) {
            cap = c("keys"=TRUE, "hashGetAll"=TRUE)
            if( is.null(name) ) {
                cap
            } else {
                cap[name]
            }
        }
  )
)

#' Create a redis client
#'
#' Returns an instance of RedisClient* class with methods allowing to manipulate redis database using a unified interface
#' Only needed commands by this package are implemented. It's a very simple implementation to avoid
#' extra dependency, limited to this package needs
#'
#' The returned "client" holds connexion configuration and is transmissible as a variable in distributed execution
#' (like in foreach). So the client connexion parameters are only to be defined once
#'
#' @details
#'
#' The returned instance exposes the following functions :
#' \describe{
#' \item{connect()}{connect to preconfigured redis db}
#' \item{get(key)}{get key value}
#' \item{hashGet(key, field)}{get field value in HashSet named by key}
#' \item{hashSet(key, field, value)}{set field value in HashSet named by key}
#' \item{delete(key)}{remove the key key value}
#' \item{exists(key)}{returns TRUE the key exists}
#' \item{pushTail(key, value)}{add a value on the tail of the list named by key}
#' \item{hashGetAll(key)}{returns all fields in the HashSet named by key. If not implemented client will not be useable for monitoring}
#' }
#'
#' Accepted parameters are
#' \describe{
#'  \item{host}{name of host}
#'  \item{port}{get key value}
#'  \item{database}{Redis database number}
#'  \item{...}{Some other parameters, not yet normalized accross interfaces}
#' }
#'
#' @param type type of client to use (rredis, rcpp for RcppRedis)
#' @param ... paramerters to transmit to the client implementation (host, port, ...)
#' @return environment
#' @export
redis_client = function(type=NULL, ...) {
    if( is.null(type) ) {
        type = get_option("redis_type")
    }
    engine = switch(type,
        "rredis"= RedisClientRRedis,
        "rcpp"=RedisClientRcpp,
        "redux"=RedisClientRedux,
        stop(paste("Unknown client type '",type,"'"))
    )
    client = engine$new(...)
    client
}

