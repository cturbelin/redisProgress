# Test client rredis

# Need a local redis instance
# @todo configure test setting (redis db)

random_id = function(prefix="") {
    paste0(prefix, as.hexmode(as.integer(runif(1, 1, .Machine$integer.max))))
}

clients = list(
    'rredis'=list(pkg="rredis"),
    'rcpp'=list(pkg="RcppRedis"),
    'redux'=list(pkg="redux"),
    'mock'=list()
)

get_client = function(client_type) {
    def = clients[[client_type]]
    if(!is.null(def$pkg)) {
        testthat::skip_if_not_installed(def$pkg)
    }
    client = redis_client(client_type)
    client$connect()
    client
}

ensure_key = function(client, key) {
    if(client$exists(key)) {
        client$delete(key)
    }
    expect_equal(client$exists(key), FALSE)
}


key_prefix = "R:package:redisProgress:tests:keys"


for(client_type in names(clients)) {

    testthat::context(paste(client_type))

    testthat::test_that("test key", {

        client = get_client(client_type)

        key = random_id(key_prefix)
        ensure_key(client, key)

        value = random_id("value")
        expect_equal(client$exists(key), FALSE) # Be sure keys doesnt exists
        expect_equal(client$set(key, value), TRUE)
        expect_equal(client$exists(key), TRUE)
        expect_equal(client$get(key), value)
        expect_equal(client$delete(key), TRUE)
        expect_equal(client$exists(key), FALSE)

    })

    testthat::test_that("test hash", {

        client = get_client(client_type)

        key = random_id(key_prefix)
        ensure_key(client, key)

        values = list(
            x=runif(1),
            y=random_id(),
            z="toto"
        )

        expect_equal(client$exists(key), FALSE) # Be sure keys doesnt exists

        for(i in seq_along(values)) {
            field = names(values[i])
            value = values[[i]]
            expect_equal(client$hashSet(key, field, value), TRUE)
            expect_identical(client$hashGet(key, field), value)
        }

        if(client$handle("hashGetAll")) {
            r = client$hashGetAll(key)
            expect_equal(values, r)
        }

    })

    testthat::test_that("test counter", {

        client = get_client(client_type)

        key = random_id(key_prefix)
        ensure_key(client, key)

        client$hashSetCounter(key, "counter", 23L)
        client$hashIncrBy(key, "counter", 1)

        v = client$hashGet(key, "counter")
        if(client_type == "redux") {
            v = as.integer(v) # Redux cast to string
        }

        expect_equal(v, 24L)

    })

}