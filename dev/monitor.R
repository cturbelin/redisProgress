library(redisProgress)

redis = redis_client(nodelay=TRUE, type="redux")

redis_progress_monitor(list(key="jobs:clement"), redis = redis, debug=F, options = list(use.bar=TRUE))
