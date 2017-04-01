library(redisProgress)

redis = redis_client("rredis")

redis_progress_monitor(list(key="jobs:clement"), redis = redis)