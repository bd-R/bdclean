#' Launch bdclean Shiny Application
#'
#'@import shiny shinydashboard shinyjs leaflet DT data.table rgbif spocc finch bdDwC bdchecks
#'
#'@export
run_bdclean <- function() {
    app_path <- system.file("shiny/bdclean", package = "bdclean")
    return(shiny::runApp(app_path, launch.browser = TRUE))
}