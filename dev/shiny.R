#shiny

library(redisProgress)

source("R/app.R")

get_redis_queue = redisProgress:::get_redis_queue
elapsed_time = redisProgress:::elapsed_time
queue_log_name = redisProgress:::queue_log_name
task_started = redisProgress:::task_started
task_steps = redisProgress:::task_steps
str_bar = redisProgress:::str_bar


redis = redis_client(type="redux")

redis_progress_shiny(list(key="jobs:clement"), redis = redis)

