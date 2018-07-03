#' Launch bdclean Shiny Application
#'
#'@import shiny shinydashboard leaflet shinyjs
#'
#'@export
run_bdclean <- function(){
    app_path <- system.file("shiny/bdclean", package = "bdclean")
    return(shiny::runApp(app_path, launch.browser = TRUE))
}