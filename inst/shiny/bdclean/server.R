#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    dataset <- data.frame()
    
    observeEvent(input$dataToConfigure, {
        message("in")
        updateTabItems(session, "sideBar", "configure")
    })
    
    map <- leafletProxy("mymap")
    
    observeEvent(input$queryDatabase, {
        withProgress(message = paste("Querying", input$queryDB, "..."), {
            dataset <- spocc::occ(input$scientificName, input$queryDB, limit=input$recordSize)
        })
        
        leafletProxy("mymap", data = dataset$gbif$data$Puma_concolor) %>%
            clearShapes() %>%
            addCircles(~longitude, ~latitude)
        
        output$inputDataTable <- DT::renderDataTable(DT::datatable({
            dataset$gbif$data$Puma_concolor
        }))
        
        shinyjs::addClass(id='dataToConfigure', 'done')
        showNotification("Recieved Data")
    })
    
    observeEvent(input$inputFile, {
    
        
        withProgress(message = paste("Reading", input$inputFile, "..."), {
            if (is.null(input$inputFile))
                return("No data to view")
            
            dataset <- read.csv(input$inputFile$datapath)
        })
        
        leafletProxy("mymap", data = dataset) %>%
            clearShapes() %>%
            addCircles(~longitude, ~latitude)
        
        output$inputDataTable <- DT::renderDataTable(DT::datatable({
            dataset
        }, options = list(scrollX = TRUE)))
        
        shinyjs::addClass(id='dataToConfigure', selector = 'dataToConfigure', class = 'done')
        showNotification("Read Data")
    })
    
    
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles("Stamen.Watercolor") %>%
            addProviderTiles("Stamen.TonerHybrid") %>%
            setView(0, 0, zoom = 2)
            

    })
    
    
})
