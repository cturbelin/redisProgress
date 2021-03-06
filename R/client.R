#
# Redis client layer
#
# Provides a an abstraction to the redis client library
# Use a very simple implementation to reduce dependencies

#' Client interface for rredis library
#' Internal class.
#' @keywords internal
#' @family backend
#' @seealso \code{\link{redis_client}()}
RedisClientRRedis = setRefClass("RedisClientRRedis",
    fields = list(
        "args"="list",
        "database"="integer",
        "cnx"="ANY"
    ),
    methods = list(
        initialize = function(host="localhost", port=6379, database=NA, ...) {
            "initialize the instance with connection parameters"
            a = list(...)
            a$host = host
            a$port = port
            args <<- a
            cnx <<- NULL
            database <<- as.integer(database)
        },

        name = function() {
            "get the instance name"
          n = paste0(args$host,":",args$port)
          if( !is.na(database) ) {
              n = paste0(n, "/", database)
          }
          n
        },

        connect = function() {
            "connect to the redis database"
            require(rredis)
            aa = args
            aa$returnRef = TRUE
            cnx <<- do.call(rredis::redisConnect, aa)
            if( !is.na(database) ) {
                rredis::redisSelect(database)
            }
            cnx
        },

        hashSet = function(key, field, value) {
            "set the field in a 'HSet' with a value"
            rredis::redisHSet(key = key, field=field, value=value, NX=FALSE)
            # rredis HSet returns "0" whatever success or not
            return(TRUE)
        },

        # Set counter value, to be usable by incr()
        hashSetCounter = function(key, field, value) {
            value = charToRaw(as.character(value))
            rredis::redisHSet(key = key, field=field, value=value, NX=FALSE)
        },

        hashGet = function(key, field) {
            "Get the value of a field in a 'HSet'"
            rredis::redisHGet(key, field)
        },

        hashGetAll = function(key) {
          "Get all fields of an 'HSet'"
            rredis::redisHGetAll(key)
        },

        hashIncrBy = function(key, field, value) {
            rredis::redisHIncrBy(key = key, field=field, value=value)
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

        type = function(name) {
          rredis::redisType(name)
        },

        handle = function(name=NULL) {
            cap = c("keys"=TRUE, "hashGetAll"=TRUE, "stringCounter"=TRUE)
            if( is.null(name) ) {
                cap
            } else {
                cap[name]
            }
        }
    )
)


#' Client interface for RcppRedis library
#' Internal class
#' @field args arguments to use to connect to the redis database
#' @field cnx connection instance return by the backend
#' @field database database number to connect to
#' @keywords internal
#' @family backend
#' @seealso \link{redis_client}
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
        name = function() {
            n = paste0(args$host,":",args$port)
            if( !is.na(database) ) {
                n = paste0(n, "/", database)
            }
            n
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

        hashSetCounter = function(key, field, value) {
            "Set [field] of hash named by [key]"
            cnx$hset(key, field, as.integer(value)) > 0
        },

        hashIncrBy = function(key, field, value) {
          "increment [field] in [key] hash"
          old = hashGet(key, field)
          if(is.null(old)) {
            old = 0
          }
          old = old + value
          cnx$hset(key, field, old)
          old
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
            @return boolean
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

        type = function(name) {
          cnx$exec(paste0("TYPE ",name))
        },
        keys = function(pattern="*") {
            unlist(cnx$exec(paste0("KEYS ", pattern)))
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
#' @field type type of library to use  redux or rrlite (default is redux)
#' @keywords internal
#' @family backend
#' @seealso \link{redis_client}
RedisClientRedux = setRefClass("RedisClientRedux",
  fields = list(
      args="list",
      cnx="ANY",
      database="integer",
      backend="character"
  ),
  methods = list(
        initialize = function(host="localhost", port=6379, database=NA, ..., .type="redux") {
              a = list(...)
              a$host = host
              a$port = port
              args <<- a
              database <<- as.integer(database)
              backend <<- .type
        },

        name = function() {
            n = paste0(args$host,":",args$port)
            if( !is.na(database) ) {
                n = paste0(n, "/", database)
            }
            n
        },
        connect = function() {
            require(backend, character.only=TRUE)
            api_func = switch(backend,
                "redux"=redux::hiredis,
                "rrlite"=rrlite::hirlite
            )
            p = list()
            p$host = args$host
            p$port = args$port
            p$password = args$password
            if( !is.na(database) ) {
             p$db = database
            }
            cnx <<- do.call(api_func, p)
            cnx
        },
        hashSet = function(key, field, value) {
          cnx$HSET(key, field, redux::object_to_bin(value))
          TRUE # Returns number of modified field. error if something wrong
        },

        hashSetCounter = function(key, field, value) {
            "Set counter [field] of hash named by [key]"
            cnx$HSET(key, field, value)
            TRUE
        },

        hashGet = function(key, field) {
          v = cnx$HGET(key, field)
          if(is.raw(v)) {
            v =redux::bin_to_object(v)
          }
          v
        },

        hashIncrBy = function(key, field, value) {
            "increment [field] in [key] hash  "
            cnx$HINCRBY(key, field, value)
        },

        hashGetAll = function(key) {
            ff = cnx$HGETALL(key)
            if(length(ff) == 0) {
              return(list())
            }
            nn = ff[seq(1, length(ff), by=2)]
            values = ff[seq(2, length(ff), by=2)]
            values = lapply(values, function(x) {
              if(is.raw(x)) {
                x = redux::bin_to_object(x)
              }
              x
            })
            names(values) <- nn
            values
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
            unlist(cnx$LRANGE(key, start=start, stop=len - 1))
        },
        set = function(key, value) {
            cnx$SET(key, redux::object_to_bin(value)) > 0
        },

        get = function(key) {
          v = cnx$GET(key)
          if(is.raw(v)) {
            v = redux::bin_to_object(v)
          }
          v
        },
        type = function(name) {
          cnx$TYPE(name)
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

#' Client interface for mocking Redis Database
#' @field data environment where data are stored
#' @keywords internal
#' @family backend
#' @seealso \link{redis_client}
RedisClientMock = setRefClass("RedisClientMock",
 fields = list(
   data="ANY"
 ),
 methods = list(
   initialize = function(...) {
      data <<- new.env(parent = emptyenv())
   },
   name = function() {
     "mock"
   },
   connect = function() {
     TRUE
   },
   hashSet = function(key, field, value) {
     r = base::get0(key, envir=data, ifnotfound=list())
     if(!is.list(r)) {
       stop(paste0("Entry", sQuote(key)," is not a hash"))
     }
     r[[field]] <- value
     base::assign(key, r, envir=data)
     return(TRUE)
   },

   hashSetCounter = function(key, field, value) {
     "Set counter [field] of hash named by [key]"
     hashSet(key, field, value)
   },

   hashGet = function(key, field) {
     r = base::get0(key, envir=data, ifnotfound=NULL)
     if(is.null(r)) {
       return(NULL)
     }
     return(r[[field]])
   },

   hashIncrBy = function(key, field, value) {
     "increment [field] in [key] hash  "
     v = hashGet(key, field)
     if(is.null(v)) {
       v = 0
     }
     v = v + value
     hashSet(key, field, v)
     v
   },

   hashGetAll = function(key) {
     base::get0(key, envir=data, ifnotfound=list())
   },

   delete = function(key) {
     if(base::exists(key, envir = data)) {
        rm(list=key, envir=data)
     }
     TRUE # Returns number of delete key
   },

   exists = function(key) {
     base::exists(key, envir=data)
   },

   pushTail = function(key, value) {
     r = base::get0(key, envir=data, ifnotfound = list())
     r[[length(r) + 1]] = value
     attr(r, "redis_type") <- "list"
     base::assign(key, r, envir=data)
   },

   getTail = function(key, size, start=NULL) {
     r = base::get0(key, envir=data, ifnotfound = list())
     len = length(r)
     if(len == 0L) {
       return(c())
     }
     if(size > len) {
       size = len
     }
     if( is.null(start) ) {
       start = max(0, len - size)
     }
     unlist(r[start:(len)])
   },
   set = function(key, value) {
     base::assign(key, value, envir=data)
     TRUE
   },

   get = function(key) {
    base::get0(key, envir=data, ifnotfound = NULL)
   },

   type = function(name) {
     r = get(name)
     if(is.null(r)) {
       return("none")
     }
     rtype = attr(r, "redis_type")
     if(!is.null(rtype)) {
       return(rtype)
     }
     if(is.list(r)) {
       return("hash")
     }
     return("string")
   },

   keys = function(pattern="*") {
     p = glob2rx(pattern)
     n = ls(envir=data)
     n = n[grepl(p, n)]
     n
   },
   handle = function(name=NULL) {
     c("keys"=TRUE, "hashGetAll"=TRUE)
   }
 )
)


#' Create a redis client
#'
#' This functions return an object embedding Redis connection parameters. It can be propagated across all workers to share the same
#' connect to the same redis database.
#'
#' @details RedisClient:
#' Returns an instance of \code{RedisClient} class with methods allowing to manipulate 'Redis' database using a unified interface
#'
#' As this package only needs a subset of 'Redis' commands are implemented. It's a very simple implementation to avoid
#' extra dependency, limited to this package needs.
#'
#' The returned "client" embeds connection configuration and can be propagated in distributed workers
#' (like with 'foreach' package). So the client connection parameters are only to be defined once.
#'
#' Clients:
#'
#' \describe{
#'  \item{rredis}{rredis package, discontinued}
#'  \item{rcpp}{for RcppRedis}
#'  \item{rredux}{for redux and rrlite}
#'  \item{mock}{Mock type, in memory R-only client}
#' }
#' The returned instance exposes the following functions :
#' \describe{
#' \item{connect()}{connect to redis db}
#' \item{get(key)}{get key value}
#' \item{hashGet(key, field)}{get field value in \code{HashSet} named by key}
#' \item{hashSet(key, field, value)}{set field value in \code{HashSet} named by key}
#' \item{hashSetCounter(key, field, value)}{set field value for a counter in HashSet named by \code{key}}
#' \item{hashIncrBy(key, field, by)}{Increment a field by the amount provided in \code{by}. Caution the field must have been initialized by \code{hashSetCounter} not by \code{hashSet} }
#' \item{delete(key)}{remove the key key value}
#' \item{exists(key)}{returns TRUE the key exists}
#' \item{pushTail(key, value)}{add a value on the tail of the list named by key}
#' \item{hashGetAll(key)}{returns all fields in the HashSet named by key. If not implemented client will not be usable for monitoring}
#' }
#'
#' Accepted parameters are
#' \describe{
#'  \item{host}{name of host}
#'  \item{port}{get key value}
#'  \item{database}{Redis database number}
#'  \item{...}{Some other parameters, not yet normalized across interfaces}
#' }
#'
#' @param type type of client, see details
#' @param ... parameters to transmit to the client implementation (host, port, ...)
#' @return instance of a RedisClient class (described in details section)
#'
#' @examples
#' # Create a simple client using  rredis as backend
#' client = redis_client(host="127.0.0.1", type="rredis")
#'
#' # Using another backend and database number 2
#' client = redis_client(host="127.0.0.1", type="redux", database=2)
#' \dontrun{
#' client$connect() # Initiate connection
#' }
#'
#' client = redis_client("mock") # Mock client (in-memory only client)
#'
#' client$connect()
#'
#' client$exists("my-key") # Test if 'my-key' exists
#' client$set("my-key", 12) # Set a value
#' client$get("my-key")
#'
#' client$hashSetCounter("a-key", "counter", 1) # Set a counter value in a hash field
#' client$hashIncrBy("a-key", "counter", 1) # Increment by 1
#'
#' @family backend
#'
#' @export
redis_client = function(type=NULL, ...) {
    if( is.null(type) ) {
        type = get_option("redis_type")
    }
    engine = switch(type,
        "rredis"= RedisClientRRedis,
        "rcpp"=RedisClientRcpp,
        "redux"=RedisClientRedux,
        "mock"=RedisClientMock,
        stop(paste("Unknown client type '",type,"'"))
    )
    client = engine$new(...)
    client
}

