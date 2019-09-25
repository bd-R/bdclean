# UI Function
CitationsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    radioButtons(
    ns("citation_level"),
    label = h3("Citation Level"),
    choices = list(
      "Core - Citation for just base R and bdclean" = 1,
      "Dependency - Citations for packages used by bdclean directly" = 2,
      "Full - Citations for every single package bdclean depends on directly and indirectly" = 3
    ),
    selected = 1
  ),
  
  downloadButton(ns("download_bib"), label = "Download Bibtext file for current citation"),
  
  uiOutput(ns("citationsUI"))
  )
}


# Server Function
Citations <- function(input, output, session, data_store) {
  ns <- session$ns
  
  output$citationsUI <- renderUI({
    components <- list()
    components[[1]] <- tagList(h3("R"),
                               suppressWarnings(format(citation(), style = "text")))
    components[[2]] <- tagList(h3("bdclean"),
                               suppressWarnings(format(citation("bdclean"), style = "text")))
    
    if (input$citation_level == 1) {
      
    } else if (input$citation_level == 2) {
      dep <- gtools::getDependencies("bdclean")
      dep <- rev(dep)
      
      for (ind in 1:15) {
        components[[ind + 2]] <- tagList(h3(dep[ind]),
                                         suppressWarnings(format(citation(dep[ind]), style = "text")))
      }
    } else {
      dep <- gtools::getDependencies("bdclean")
      dep <- rev(dep)
      
      for (ind in 1:length(dep)) {
        components[[ind + 2]] <- tagList(h3(dep[ind]),
                                         suppressWarnings(format(citation(dep[ind]), style = "text")))
      }
    }
    return(components)
  })
  
  output$download_bib <- downloadHandler(
    filename = function() {
      paste("citation-", Sys.Date(), ".bib", sep = "")
    },
    content = function(con) {
      if (input$citation_level == 1) {
      cont <- c("base", "bdclean")
      } else if (input$citation_level == 2) {
        cont <- c("base", "bdclean", rev(gtools::getDependencies("bdclean"))[1:15])
      } else {
        cont <- c("base", "bdclean", rev(gtools::getDependencies("bdclean")))
      }
      
      write_bib(cont, con)
    }
  )
}