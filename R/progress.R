
#' Progress bar generator function
#'
#' Holds the environment containing progress bar data
#' For one task instance
#'
#' @param name progress name prefix
#' @param redis redis_client() results
#' @param debug debug mode
#'
#' @return progress bar instance
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
#'  \item{value}{get the queue name}
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
#'  \item{module}{if defined, progress state will be updated to redis only when step value is a multiple of modulo}
#' }
#'
#' @section \code{step_task(newvalue)}:
#' Define the current task step to be newvalue
#'
#' @section \code{message(msg)}:
#' Send a message
redis_progress_bar = function(name, redis, debug=FALSE) {
    # Task name
    task = NULL
    value = NULL
    steps = NULL
    cnx = NULL
    modulo = NULL
    log.name = paste0(name, ":logs")

    start_task = function(taskname, steps=0, init = 0, modulo=NULL) {

        if( grepl(":", taskname, fixed = TRUE) ) {
            warning("Taskname should not contain colon character, fixing to underscore")
            taskname =  gsub(":","_", taskname, fixed = TRUE)
        }

        task <<- taskname
        value <<- init
        modulo <<- modulo
        cnx <<- redis$connect()

        if(debug) {
            cat("Starting task ", task," into job-queue ", name,"\n")
        }
        redis$hashSet(name, paste0(task,":started"), as.numeric(Sys.time()))
        redis$hashSet(name, paste0(task,":steps"), as.numeric(steps))
        update()
    }

    step_task = function(newvalue) {
        value <<- newvalue
        update()
    }

    incr_task = function(by=1) {
        value <<- value + by
        update()
    }

    update = function() {
        if( !is.null(modulo) ) {
            # Only update each time value is multiple of modulo value
            if( ! (value %% modulo == 0)) {
                return()
            }
        }
        if(debug) {
            cat("Updating task ", task," to value ", value, "\n")
        }
        # If steps is defined, the def
        redis$hashSet(name, task, value)
        # Update time of update
        redis$hashSet(name, paste0(task,":updated"), as.numeric(Sys.time()))
    }

    message = function(msg) {
        redis$pushTail(log.name, paste0("[", task,"] ", msg))
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
        }

    ), class="redis_progress")

}

#' Create redis progress bar
#' @export
#'
#' @section publish:
#' Publish argument allows to set the current queue name into another redis entry.
#' publish should be the key name to use.
#' If the passed value has an attribute "type" it is used to determine the type of operation to do with the key
#' \describe{
#'  \item{key}{uses SET operation}
#'  \item{list}{uses LPUSH so the queue name will be pushed to the tail of the list named by the provided key}
#' }
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
#' @param redis redis_client object used to hold connexion parameters
#' @param publish name of a redis key to use to publish the generated queue name (caution, no namespace)
#' @param debug print debug information
#' @param unique.name ensure queue has unique name, add a random generated string (useful with publish)
#' @param append queue name (fully qualified) already exists, it will be reused
#' @param verbose show information message
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
    name = paste0(redis_queue_name(name), unique)

    if(verbose) {
        cat("Creating queue '", name,"'\n")
    }

    progress = redis_progress_bar(name, redis, debug=debug)
    reg.finalizer(environment(progress$start), function(env) {
        cat("Closing ", env$redis,"\n")
    }, onexit=TRUE)

    if( !is.null(publish) ) {
        type = attr(publish, "type")
        if( is.null(type) ) {
            type = "key"
        }
        if(type == "key") {
            redis$set(publish, name)
        }
        if(type == "list") {
            redis$pushTail(publish, name)
        }
        if(debug) {
            cat("Setting queue ", name, " into ", publish, " key")
        }
    }

    # If queue name is not build to be unique, check and remove previous instance of the queue if needed
    if( !unique.name ) {
        if(!append && redis$exists(name)) {
            redis$delete(name)
            if(redis$exists(progress$log.name) ) {
                redis$delete(progress$log.name)
            }
        }
    }

    # Initialize Hash
    redis$hashSet(name, "_created_", as.numeric(Sys.time()))

    progress
}
