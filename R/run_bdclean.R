#' Launch bdclean Shiny Application
#'
#' @import shinydashboard shinyjs leaflet data.table rgbif spocc finch bdDwC bdchecks tools
#' @importFrom shiny runApp
#' 
#' @examples
#' 
#' if(interactive()){
#' 
#' run_bdclean()
#' 
#' } 
#'
#' @export
run_bdclean <- function() {
    app_path <- system.file("shiny/bdclean", package = "bdclean")
    return(shiny::runApp(app_path, launch.browser = TRUE))
}
