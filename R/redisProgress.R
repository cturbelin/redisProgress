#' redisProgress Package
#'
#' The goal of redisProgress is to create a progress bar system to follow tasks distributed across several R instances
#' (using foreach package for example). It uses `Redis` instance, a key-value store server <https://redis.io/>,  to follow progress of all the
#' tasks in a queue.
#'
#' @author Clement Turbelin
#' @title Progress bar for distributed tasks with redis backend
#' @name redisProgress
#' @importFrom stats runif
#' @importFrom methods new is
#' @importFrom utils modifyList
#'
"_PACKAGE"



