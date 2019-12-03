# Internals

#' Queue logs key
#' @noRd
queue_log_name = function(queue) {
    paste0(queue, ":logs")
}

#' task started field
#' @noRd
task_started = function(task) {
    paste0(task, ":started")
}

#' task steps field
#' @noRd
task_steps = function(task) {
    paste0(task,":steps")
}

#' Simple progress bar render
#' @noRd
str_bar = function(prop, size) {
    nbars = floor(prop * size)
    b = paste0(" |", paste0(rep.int("=", nbars), collapse = ""), paste0(rep.int(" ",  size - nbars), collapse = ""),"| ")
    b
}


# Compute elapsed string
#' @noRd
elapsed_time = function(elapsed) {
    times = c("d"=86400,"h"=3600, "m"=60)
    i = 1
    n = length(times)
    while(i <= n) {
        m = times[i]
        if(elapsed > m) {
            elapsed = as.integer(floor(elapsed / m))
            return(paste0(sprintf("%3d", elapsed), names(m)))
        }
        i = i + 1
    }
    paste0(sprintf("%3d",as.integer(elapsed)), "s")
}


as_time = function(value) {
    if(is.null(value)) {
        return(NA)
    }
    if(is.na(value)) {
        return(NA)
    }
    if(is.numeric(value)) {
        as.POSIXct.numeric(value, origin="1970-01-01")
    } else {
        as.POSIXct(value, origin="1970-01-01" )
    }
}