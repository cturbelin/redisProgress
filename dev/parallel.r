library(redisProgress)
library(parallel)
library(foreach)
library(doParallel)

registerDoParallel(3)

redis = redis_client(type = "redux")

redis$connect()

progress = create_redis_progress("progress-parallel-demo", redis, publish = "jobs:clement", unique.name = TRUE)

data = foreach(job=1:20, .verbose = TRUE, .combine = c, .export = "progress") %dopar% {

    N = 100
    name = paste0("task", job)
    progress$start(name, steps=N)
    # Long running job
    r = 0
    for(i in 1:N) {
        time = as.integer(runif(1, min=5, max=30))
        progress$message(paste(name, "waiting", time, "secs"))
        Sys.sleep(time)
        progress$incr()
        r = r + time
        progress$message(paste(name, "took", time, "secs"))
    }
    time
}

stopImplicitCluster()