
#' Monitor tasks using a shiny application
#' @inherit redis_progress_monitor
redis_progress_shiny = function(from, redis, debug=FALSE, options=list()) {

  if( is.null(redis) ) {
    redis = redis_client()
  }

  if(is.null(redis$handle("hashGetAll"))) {
    stop("This redis client doesnt implement hashGetAll, it is not useable to monitor tasks")
  }

  if(debug) {
    message("Connecting")
  }
  redis$connect()
  if( !is.null(redis$cnx) ) {
    if(debug) {
      message(" Connected")
    }
  }

  defs = list(refresh=2000, log.size=10, bar.size=10)

  options = modifyList(defs, options)

  queues = get_redis_queue(from, redis)

  bar.size = as.integer(options$bar.size)

  requireNamespace("shiny")

  # Define UI for application that draws a histogram
  ui <- shiny::fluidPage(
     # Application title
    shiny::titlePanel(paste("Redis Monitor", queues)),
    shiny::fluidRow(
      shiny::column(6,
        shiny::textOutput("text"),
        shiny::tableOutput("tasks"),
        shiny::textOutput("debug")
      ),
      shiny::column(6,
        shiny::verbatimTextOutput("logs")
      )
     )
  )

  # Define server logic required to draw a histogram
  server <- function(input, output) {

    autoInvalidate <- shiny::reactiveTimer(options$refresh)

    output$tasks <- shiny::renderTable({

       autoInvalidate()
       queue = redis$hashGetAll(queues)
       n = names(queue)
       n = n[!grepl("^_", n)]

       created = as.POSIXct(queue$"_created_")

       # Task names (should not have ":")
       tasks = unique(gsub(":(.*)$","", n))

       values = as.integer(as.vector(queue[tasks]))
       started = as_time(as.vector(unlist(queue[ task_started(tasks) ])))
       steps = as.integer(unlist(queue[ task_steps(tasks)]))

       elapsed = as.numeric(Sys.time()) - as.numeric(started)

       prop = unlist(Map(function(value, step) {
         if(step > 0) {
           prop = value / step
           value = round(prop * 100, 1)
           if(!is.na(bar.size)) {
             b = str_bar(prop, bar.size)
           } else {
             b = ""
           }
           paste(sprintf(as.integer(value), fmt = "% 3-d%%"), b)
         } else {
           value
         }
       }, values, steps))

       ee = sapply(elapsed, elapsed_time )

       r = data.frame(
         task=as.vector(tasks),
         started=format(format ="%Y-%m-%d %T",started),
         elasped=ee,
         progress=prop
        )

       r = r[order(started, decreasing = T),]

     })


     output$logs = shiny::renderText({
       log.name = queue_log_name(queues)
       logs = try(redis$getTail(log.name, options$log.size))
       if( is(logs, "try-error") ) {
         logs = c()
       }
       if(length(logs) > 0) {
         paste(logs, collapse = "\n")
       }

     })
  }

  # Run the application
  shiny::runApp(shiny::shinyApp(ui = ui, server = server))

}
