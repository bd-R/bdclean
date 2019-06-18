# UI Function
CitationsUI <- function(id) {
    ns <- NS(id)
    
    tagList(
        radioButtons(ns("citation_level"), label = h3("Citation Level"),
                     choices = list("Core" = 1, "Dependency" = 2, "Full" = 3),
                     selected = 1),
        
        uiOutput(ns("citationsUI"))
        
    )
}


# Server Function
Citations <- function(input, output, session, data_store) {
    ns <- session$ns
    
    output$citationsUI <- renderUI({
        components <- list()
        
        components[[1]] <- tagList(
            h3("R"),
            suppressWarnings(format(citation(), style = "text"))
        )
        
        components[[2]] <- tagList(
            h3("bdclean"),
            suppressWarnings( format(citation("bdclean"), style = "text"))
        )
        
        
      
        
        if (input$citation_level == 1){
            
        } else if (input$citation_level == 2) {
            dep <- gtools::getDependencies("bdclean")
            dep <- rev(dep)
            
            for (ind in 1 : 12) {
                print(ind)
                components[[ind + 2]] <- tagList(
                    h3(dep[ind]),
                    suppressWarnings( format(citation(dep[ind]), style = "text"))
                )
            }
        } else {
            dep <- gtools::getDependencies("bdclean")
            dep <- rev(dep)
            
            for (ind in 1 : length(dep)) {
                components[[ind + 2]] <- tagList(
                    h3(dep[ind]),
                    suppressWarnings( format(citation(dep[ind]), style = "text"))
                )
            }
        }
        
        
        return(components)
        
    })
}