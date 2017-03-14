library(redisProgress)

redis = redis_client("rredis")

redis_progress_monitor(key="jobs:clement", redis = redis)