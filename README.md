redisProgress Package
================
Clément Turbelin

Overview
--------

The goal of redisProgress is to create a progress bar system to follow tasks distributed accross several R instances (using foreach package for example). It uses Redis instance to follow tasks progress. Tasks are grouped in a common named "queue" and can be monitored together on the same screen.

Installation
------------

``` r
# Install the released version from CRAN:
install.packages("redisProgress")

# Install the cutting edge development version from GitHub:
# install.packages("devtools")
devtools::install_github("cturbelin/redisProgress")
```

Usage
-----

See introduction [vignettes/introduction.Rmd](vignettes/introduction.Rmd)

Redis Client implementations
----------------------------

redisProgress can handle several packages implementing redis client API

-   rredis
-   RCppRedis
-   redux
-   rrlite (Redis without Redis)

`redis_client()` creates a client object embedding the connection context, using a unified interface, regardless the redis package used. The connection parameters (host, port,...) will be propagated to the R instances
