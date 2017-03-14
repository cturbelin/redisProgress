library(redisProgress)
library(parallel)
library(foreach)
library(doParallel)

registerDoParallel(3)

redis = redis_client()
progress = create_redis_progress("toto", redis, publish = "jobs:clement")

data = foreach(job=1:20, .verbose = TRUE, .combine = c, .export = "progress") %dopar% {

    N = 100
    name = paste0("task",job)
    progress$start(name, steps=N)
    # Long running job
    r = 0
    for(i in 1:N) {
        time = as.integer(runif(1, min=5, max=30))
        Sys.sleep(time)
        r = r + time
        progress$incr()
        progress$message(paste(name, "took", time, "secs"))
    }
    time
}

stopImplicitCluster()