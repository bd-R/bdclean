#' Launch bdclean Shiny Application
#'
#' @import shinydashboard shinyjs bdDwC bdchecks tools
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
bdclean_app <- function() {
    app_path <- system.file("shiny/bdclean", package = "bdclean")
    return(shiny::runApp(app_path, launch.browser = TRUE))
}
