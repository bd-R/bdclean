shinyServer(function(input, output, session) {
    dataset <- data.frame()
    cleanedData <- data.frame()
    questionnaire <- create_default_questionnaire()
    
    shinyjs::hide("flaggedContent")
    
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
        
        updateTabItems(session, "sideBar", "flag")
    })
    
    # ------------- End of Side Bar Tab Navigation Control -------------------
    
    # ------------- Add Data Module -------------------
    
    map <- leafletProxy("mymap")
    
    observeEvent(input$queryDatabase, {
        withProgress(message = paste("Querying", input$queryDB, "..."), {
            data <-
                spocc::occ(input$scientificName,
                           input$queryDB,
                           limit = input$recordSize)
            
            dataset <<- data$gbif$data$Puma_concolor
            
        })
        
        dataLoadedTask(dataset)
    })
    
    observeEvent(input$inputFile, {
        withProgress(message = paste("Reading", input$inputFile, "..."), {
            if (is.null(input$inputFile))
                return("No data to view")
            
            dataset <<- read.csv(input$inputFile$datapath)
        })
        
        dataLoadedTask(dataset)
    })
    
    dataLoadedTask <- function(data) {
        leafletProxy("mymap", data = data) %>%
            clearShapes() %>%
            addCircles( ~ longitude, ~ latitude)
        
        output$inputDataTable <- DT::renderDataTable(DT::datatable({
            data
        }, options = list(scrollX = TRUE)))
        
        shinyjs::addClass(id = 'dataToConfigure',
                          selector = 'dataToConfigure',
                          class = 'done')
        showNotification("Read Data")
        
        
        output$inputDataRows <- renderText(nrow(data))
        output$inputDataColumns <- renderText(length(data))
        output$inputDataSpecies <-
            renderText(length(unique(data$scientificName)))
    }
    
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
        shinyjs::show("flaggedContent")
        
        cleanedData <- dataset
        for (question in questionnaire$BdQuestions) {
            if (question$question.type != "Router" &&
                length(question$users.answer) > 0) {
                cleanedData <- question$cleanData(cleanedData)
            }
        }
        
        output$flaggedContentUI <- renderUI({
            tagList(
                h3("Flagged Data"),
                
                sliderInput(
                    "cleanControl",
                    label = h4("Cleanliness Treshold:"),
                    min = 0,
                    max = 10,
                    value = 7
                ),
                
                helpText(
                    "Note: Cleanliness Score determines how clean your data has to be.",
                    "Score of 10 will return only the perfect records, while scores less than 3 will also return somewhat okay records.",
                    "Tweak the score value and check the remaining records in statistics boxes to determine the score you require."
                ),
                br(),
                
                fluidRow(
                    infoBox(
                        "Clean Data",
                        "56%",
                        icon = icon("flag"),
                        color = "red"
                    ),
                    infoBox(
                        "# of Clean Records",
                        nrow(cleanedData),
                        icon = icon("list-ol")
                    ),
                    infoBox(
                        "# of Newly Added Columns",
                        length(cleanedData) - length(dataset),
                        icon = icon("th-list"),
                        color = "purple"
                    ),
                    infoBox(
                        "# of Unique Scientific Names Remaining",
                        length(unique(cleanedData$scientificName)),
                        icon = icon("paw"),
                        color = "yellow"
                    )
                    
                ),
                
                
                
                actionButton("action", label = "Next: Remove Flagged Data"),
                
                actionButton("action", label = "Next: Continue with Just Flagging"),
                
                taskItem(value = 45, color = "red",
                         "Step 4 of 6"),
                
                h3("Flagged Data:"),
                
                br(),
                
                downloadButton("downloadData", "Download Flagged Data"),
                br(),
                
                DT::renderDataTable(cleanedData, width = 300)
            )
        })
    })
    
    output$flaggedDataTable <-  reactive(DT::renderDT(cleanedData))
    
})
