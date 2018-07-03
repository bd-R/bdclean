shinyServer(function(input, output, session) {
    dataset <- data.frame()
    cleanedData <- data.frame()
    questionnaire <- create_default_questionnaire()
    
    # ------------- Next Buttons Navigation Control -------------------
    observeEvent(input$dataToConfigure, {
        updateTabItems(session, "sideBar", "configure")
    })
    
    
    observeEvent(input$configureToFlag, {
        getResponse <- function(bdQuestion) {
            # set response
            bdQuestion$setResponse(input[[bdQuestion$question.id]])
            
            if (bdQuestion$question.type == "Router") {
                if (bdQuestion$users.answer %in% bdQuestion$router.condition) {
                    for (question in bdQuestion$child.questions) {
                        getResponse(question)
                    }
                }
            }
        }
        
        for (question in questionnaire$BdQuestions) {
            if (question$question.type != "Child") {
                getResponse(question)
            }
        }
    })
    
    # ------------- End of Side Bar Tab Navigation Control -------------------
    
    # ------------- Add Data Module -------------------
    
    map <- leafletProxy("mymap")
    
    observeEvent(input$queryDatabase, {
        withProgress(message = paste("Querying", input$queryDB, "..."), {
            dataset <-
                spocc::occ(input$scientificName,
                           input$queryDB,
                           limit = input$recordSize)
        })
        
        leafletProxy("mymap", data = dataset$gbif$data$Puma_concolor) %>%
            clearShapes() %>%
            addCircles(~ longitude, ~ latitude)
        
        output$inputDataTable <- DT::renderDataTable(DT::datatable({
            dataset$gbif$data$Puma_concolor
        }))
        
        shinyjs::addClass(id = 'dataToConfigure', 'done')
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
            addCircles(~ longitude, ~ latitude)
        
        output$inputDataTable <- DT::renderDataTable(DT::datatable({
            dataset
        }, options = list(scrollX = TRUE)))
        
        shinyjs::addClass(id = 'dataToConfigure',
                          selector = 'dataToConfigure',
                          class = 'done')
        showNotification("Read Data")
    })
    
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles("Stamen.Watercolor") %>%
            addProviderTiles("Stamen.TonerHybrid") %>%
            setView(0, 0, zoom = 2)
    })
    
    # ------------- End of Add Data Module -------------------
    
    # ------------- Questionnaire Module -------------------
    
    output$questionnaire <- renderUI({
        components <- list()
        val <- 1
        
        createQuestionsUI = function(question, index) {
            switch(
                question$ui.type,
                "single-checkbox" = tagList(
                    h4(paste(
                        index, question$question, sep = ") "
                    )),
                    checkboxInput(
                        question$question.id,
                        label = "Yes",
                        value = FALSE
                    ),
                    br()
                ),
                
                "select" = tagList(
                    h4(paste(
                        index, question$question, sep = ") "
                    )),
                    selectInput(
                        question$question.id,
                        label = "",
                        choices = setNames(
                            as.character(question$possible.responses),
                            question$possible.responses
                        )
                    ),
                    br()
                ),
                "radio" = tagList(
                    h4(paste(
                        index, question$question, sep = ") "
                    )),
                    radioButtons(
                        question$question.id,
                        label = "",
                        choices = setNames(
                            as.character(question$possible.responses),
                            question$possible.responses
                        )
                    ),
                    br()
                ),
                "numericInput" = tagList(
                    h4(paste(
                        index, question$question, sep = ") "
                    )),
                    numericInput(
                        question$question.id,
                        label = "",
                        value = 1
                    ),
                    br()
                ),
                
                "date-range" = tagList(
                    h4(paste(
                        index, question$question, sep = ") "
                    )),
                    dateRangeInput(question$question.id,
                                   label = ""),
                    br()
                )
            )
        }
        
        createUIContainer <- function(bdQuestion) {
            components[[val]] <<- createQuestionsUI(bdQuestion, val)
            val <<- val + 1
            
            for (question in bdQuestion$child.questions) {
                components[[val]] <<- conditionalPanel(
                    condition = paste(
                        "input.",
                        bdQuestion$question.id,
                        " == true",
                        sep = ""
                    ),
                    createQuestionsUI(question, val)
                )
                val <<- val + 1
            }
        }
        
        for (question in questionnaire$BdQuestions) {
            if (question$question.type != "Child") {
                createUIContainer(question)
            }
        }
        
        return(components)
    })
    
    # ------------- End of Questionnaire Module -------------------
    
    # ------------- Flagging Module -------------------
    
    observeEvent(input$flagButton, {
        cleanedData <- dataset
        message(length(cleanedData))
        for (question in responses$BdQuestions) {
            if (question$question.type != "Router" &&
                length(question$users.answer) > 0) {
                cleanedData <- question$cleanData(cleanedData)
            }
        }
        message(length(cleanedData))
    })
    
    flaggedDataTable <-  DT::renderDT(
        cleanedData
    )
    
})
