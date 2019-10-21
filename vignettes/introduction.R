## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE----------------------------------------------------------
#  library(redisProgress)
#  
#  redis = redis_client("rredis") # creating redis client, using rredis with default parameters
#  
#  # Creating the queue
#  progress = create_redis_progress("my-queue-name", redis=redis)
#  
#  progress$start("job1", steps=100) # Starting job1 in the queue
#  progress$step(10)
#  progress$step(100) # Another step value
#  
#  # Starting another task
#  progress$start("job2", steps=5) # Starting job1 in the queue
#  progress$incr() # Simply incrementing task step
#  progress$message("job2 is running")
#  progress$incr() # Simply incrementing task step
#  

## ----eval=FALSE----------------------------------------------------------
#  library(redisProgress)
#  library(parallel)
#  library(foreach)
#  library(doParallel)
#  
#  registerDoParallel(3)
#  
#  redis = redis_client("rredis", host="localhost")
#  progress = create_redis_progress("my-jobs", redis, unique.name=TRUE, publish = "jobs:clement")
#  
#  data = foreach(job=1:20, .verbose = TRUE, .combine = c, .export = "progress") %dopar% {
#      progress$start(paste0("task",job), steps=100)
#      for(i in 1:100) {
#          # Doing a step of my long running task
#          # Sys.sleep(runif(1, min=5, max=30))
#          progress$incr() #
#      }
#  }
#  
#  stopImplicitCluster()

## ----eval=FALSE----------------------------------------------------------
#  library(redisProgress)
#  
#  redis = redis_client("rredis")
#  
#  # Known and fixed queue name
#  redis_progress_monitor(from="my-queue-name", redis=redis)
#  
#  # Queue name has been generated and published in a list, use the "key" parameter in a list to get the real queue name from the list
#  redis_progress_monitor(list(key="jobs:clement"), redis=redis)
#  

