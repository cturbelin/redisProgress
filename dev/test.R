library(redisProgress)

redis = redis_client("rredis")
progress = create_redis_progress("toto", redis, "jobs:clement")

progress$start("job1")

progress$step(10)
