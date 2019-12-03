
#' Progress bar generator function
#'
#' Holds the environment containing progress bar data
#' For one task instance
#'
#' @param name progress name prefix
#' @param redis redis_client() results
#' @param debug debug mode
#'
#' @return progress bar structure (list with function)
#'
#' @section progress-bar:
#' The progress bar instance is currently a very simple object-mimic list exposing several functions
#'
#' \describe{
#'  \item{start}{Start a task}
#'  \item{incr}{Increment the step value}
#'  \item{step}{Set the current step value}
#'  \item{message}{Send a message}
#'  \item{task}{get the task name}
#'  \item{queue}{get the queue name}
#'  \item{value}{get the current step value}
#' }
#' Parameters of these functions (if any), are described in the following sections
#'
#' @section \code{start(taskname, steps=0, init = 0, modulo=NULL)}:
#'
#' Declare the starting of a task
#' \describe{
#'  \item{taskname}{Name of the starting task (cannot contain colon character) }
#'  \item{steps}{Total number of steps of the task}
#'  \item{init}{Initial step value}
#' }
#'
#' @section \code{step_task(value)}:
#' Define the current task step to be value
#'
#' @section \code{message(msg)}:
#' Send a message
#'
redis_progress_bar = function(name, redis, debug=FALSE) {
    # Task name
    task = NULL
    value = NULL
    steps = NULL
    cnx = NULL
    log.name = paste0(name, ":logs")

    start_task = function(taskname, steps=0, init = 0) {

        if( grepl(":", taskname, fixed = TRUE) ) {
            warning("Taskname should not contain colon character, fixing to underscore")
            taskname =  gsub(":","_", taskname, fixed = TRUE)
        }

        task <<- taskname
        value <<- init
        cnx <<- redis$connect()

        if(debug) {
            message(paste("Starting task", sQuote(task)," into job-queue ", sQuote(name)))
        }
        redis$hashSet(name, paste0(task,":started"), as.numeric(Sys.time()))
        redis$hashSet(name, paste0(task,":steps"), as.numeric(steps))
        redis$hashSetCounter(name, task, value)
        update()
    }

    step_task = function(newvalue) {
        redis$hashSetCounter(name, task, value)
        value <<- newvalue
        update()
    }

    incr_task = function(by=1) {
        value <<- redis$hashIncrBy(name, task, by)
        update()
    }

    update = function() {
        if(debug) {
            message(paste("Updating task", task,"to value ", value))
        }
        # Update time of update
        redis$hashSet(name, paste0(task,":updated"), as.numeric(Sys.time()))
    }

    message = function(msg) {
        redis$pushTail(log.name, paste0(format(fmt="%Y-%m-%d %T", Sys.time()), " [", task,"] ", msg))
    }

    # Public interface
    structure(list(

        # Start a new task
        start = start_task,

        # Set the value
        step = step_task,

        # Increment the value by a given amount
        incr = incr_task,

        # log a message
        message = message,

        # Get the task name
        task = function() {
            task
        },

        #' get the queue name
        queue = function() {
            name
        },

        #' get the value
        value = function() {
            value
        },
        log_name = function() {
            log.name
        },
        debug = function() {
            as.list(environment(step_task))
        }

    ), class="redis_progress")

}

#' Create redis progress bar
#'
#' @export
#'
#' @section publish:
#' Publish argument allows to set the current queue name into another redis entry. Use \code{\link{publish_queue}()}
#' to configure it.
#'
#' @section queue name:
#' The queue name will be used to create an Hash set structure in Redis. To avoid collision between progress bars, 2 mechanisms
#' are used :
#' \itemize{
#'  \item{passing unique.name=TRUE will add a unique suffix to the queue name, ensuring uniqueness of the queue name.
#'    Previous progress bar with the same name will be deleted unless \code{append} parameter is TRUE
#'  }
#'  \item{Provided name is prefixed using redis_queue_name function, ensuring a dedicated keyspace for progress bar. the prefix string can be configured
#'  in R options. It is defined once in the session of the creator of the progress bar instance. So it has only to be defined in the R instance
#'  creating the jobs. If R instances are created independently and the progress object is not propagated you have to be sure the same
#'  prefix is used.
#'  }
#' }
#'
#' @param name character string used to create the queue name (with predefined prefix).
#' @param redis redis_client object used to hold connection parameters
#' @param publish how to publish the generated key name, name of key or value returned by \code{\link{publish_queue}()}
#' @param debug print debug information
#' @param unique.name ensure queue has unique name, add a random generated string (useful with publish)
#' @param append queue name (fully qualified) already exists, it will be reused
#' @param verbose show information message
#'
#' @examples
#'
#' # create a redis client (using mock to make this example runnable without a redis server)
#' client = redis_client("mock")
#' # Create the progress bar
#' progress = create_redis_progress("queue01", redis=client)
#'
#' progress$start('first-task', steps=5) # Start my first task, declaring 10 steps to finish
#' # Do something
#' progress$incr() # first step complete
#' # Do something else
#' progress$incr()
#' # Log Message can also be sent
#' progress$message("My task is running well")
#' progress$incr(3) # Increase the progress counter by a value
#'
#' # Create a progress with a random name and publish under in "myjobs" key
#' publish_to = publish_queue("myjobs", type="key")
#' progress = create_redis_progress("queue01", redis=client, publish=publish_to, unique.name=TRUE)
create_redis_progress = function(name, redis=NULL, publish=NULL, debug=FALSE, unique.name=FALSE, append=TRUE, verbose=TRUE) {

    if( is.null(redis) ) {
        redis = redis_client()
    }

    redis$connect()

    if(unique.name) {
        unique = paste0(":", as.hexmode(as.integer(Sys.time())),"-", as.hexmode(as.integer(runif(1, min=1, max=.Machine$integer.max))))
    } else {
        unique = ""
    }

    if(debug) {
        verbose = TRUE
    }

    # create queue name
    # with current prefix and uniqueness suffix if needed
    queue_name = paste0(name, unique)

    redis_key = redis_queue_name(queue_name)

    if(verbose) {
        message(paste("Creating queue", sQuote(name)))
    }

    progress = redis_progress_bar(redis_key, redis, debug=debug)
    reg.finalizer(environment(progress$start), function(env) {
        message("Closing ", env$redis$name())
    }, onexit=TRUE)

    if( !is.null(publish) ) {
        type = attr(publish, "type")
        publish_key = publish
        if( is.null(type) ) {
            type = "key"
        }
        if(type == "key") {
            redis$set(publish_key, queue_name)
        }
        if(type == "list") {
            if(redis$exists(publish_key)) {
                found_type = redis$type(publish_key)
                if(found_type != 'list') {
                    stop(paste0("Publish key",sQuote(publish_key)," must be a list when exists, found ", found_type))
                }
            }
            redis$pushTail(publish_key, queue_name)
        }
        if(debug) {
            message(paste("Setting queue ", sQuote(queue_name), " into ", publish_key, " key"))
        }
    }

    # If queue name is not build to be unique, check and remove previous instance of the queue if needed
    if( !unique.name ) {
        if(!append && redis$exists(redis_key)) {
            redis$delete(redis_key)
            log.name = progress$log_name()
            if(redis$exists(log.name) ) {
                redis$delete(log.name)
            }
        }
    }

    # Initialize Hash
    redis$hashSet(redis_key, "_created_", as.numeric(Sys.time()))

    progress
}



