# Test client rredis

# Need a local redis instance
# @todo configure test setting (redis db)

random_id = function(prefix="") {
    paste0(prefix, as.hexmode(as.integer(runif(1, 1, .Machine$integer.max))))
}

context("rredis client tests")

clients = list(
    'rredis'=list(pkg="rredis"),
    'rcpp'=list(pkg="RcppRedis"),
    'redux'=list(pkg="redux")
)

pkgs = rownames(installed.packages())

for(client_type in names(clients)) {

    context(paste("Testing ", client_type))

    def = clients[[client_type]]

    if(!def$pkg %in% pkgs) {
        warning(paste(client_type," not installed, tests are not complete"))
        next()
    }

    client = redis_client(client_type)
    client$connect()

    test_that("test key", {

        key = random_id("test:redisProgress:key")
        value = random_id("value")
        expect_equal(client$exists(key), FALSE) # Be sure keys doesnt exists
        expect_equal(client$set(key, value), TRUE)
        expect_equal(client$exists(key), TRUE)
        expect_equal(client$get(key), value)
        expect_equal(client$delete(key), TRUE)
        expect_equal(client$exists(key), FALSE)

    })

    test_that("test hash", {

        key = random_id("test:redisProgress:key")

        value = list(x=runif(1), y=random_id(), z="toto")

        expect_equal(client$exists(key), FALSE) # Be sure keys doesnt exists

        Map(function(field, value) {
            expect_equal(client$hashSet(key, field, value), TRUE)
            expect_equal(client$hashGet(key, field), value)
        }, names(value), value)

        if(client$handle("hashGetAll")) {
            r = client$hashGetAll(key)
            expect_equal(value, r)
        }

    })
}