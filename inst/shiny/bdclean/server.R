shinyServer(function(input, output, session) {
    inputData <- data.frame()
    flaggedData <- data.frame()
    cleanedData <- data.frame()
    questionnaire <- create_default_questionnaire()
    
    cleaningThresholdControl <- 7
    
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
            if (length(input$queryDatabase == 1) && input$queryDatabase == "gbif"){
                print("in")
                data <- occ_search(input$scientificName, limit = input$recordSize)
                inputData <<- data$data
                
            } else {
                data <-
                    spocc::occ(input$scientificName,
                               input$queryDB,
                               limit = input$recordSize)
                inputData <<- data$gbif$data$Puma_concolor
            }
        })
        
        dataLoadedTask(inputData)
    })
    
    observeEvent(input$inputFile, {
        withProgress(message = paste("Reading", input$inputFile, "..."), {
            if (is.null(input$inputFile))
                return("No data to view")
            
            inputData <<- read.csv(input$inputFile$datapath)
        })
        
        dataLoadedTask(inputData)
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
        tempData <- inputData
        withProgress(message = "Flagging Data...", {
            for (question in questionnaire$BdQuestions) {
                if (question$question.type != "Router" &&
                    length(question$users.answer) > 0) {
                    tempData <- question$flagData(tempData)
                }
            }
            
            flaggedData <<- tempData
        })
    })
    
    output$flaggedContentUI <- renderUI({
        input$flagButton
        input$cleanControl
        
        flaggedCount <-
            get_flagging_statistics(flaggedData, cleaningThresholdControl)
        
        conditionalPanel(
            "input.flagButton > 0",
            tagList(
                h3("Flagged Data"),
                
                sliderInput(
                    "cleanControl",
                    label = h4("Cleanliness Treshold:"),
                    min = 0,
                    max = 10,
                    value = cleaningThresholdControl
                ),
                
                helpText(
                    "Note: Cleanliness Score determines how clean your data has to be.",
                    "Score of 10 will return only the perfect records, while scores less
                    than 3 will also return somewhat okay records.",
                    "Tweak the score value and check the remaining records in statistics 
                    boxes below to determine the score you require."
                ),
                br(),
                
                tabsetPanel(
                    type = "tabs",
                    tabPanel(
                        "Statistics View",
                        div(class = "secondaryHeaders", h3("View 01: Statistics Boxes")),
                        fluidRow(
                            infoBox(
                                "Clean Data",
                                paste(((
                                    flaggedCount / nrow(inputData)
                                ) * 100), "%", sep = ""),
                                icon = icon("flag"),
                                color = "red"
                            ),
                            infoBox("# of Clean Records",
                                    flaggedCount,
                                    icon = icon("list-ol")),
                            infoBox(
                                "# of Newly Added Columns",
                                length(flaggedData) - length(inputData),
                                icon = icon("th-list"),
                                color = "purple"
                            ),
                            infoBox(
                                "# of Unique Scientific Names Remaining",
                                length(unique(flaggedData$scientificName)),
                                icon = icon("paw"),
                                color = "yellow"
                            )
                            
                        )
                        
                    ),tabPanel(
                        "Table View",
                        div(class = "secondaryHeaders", h3("View 02: Summarized Table")),
                        DT::renderDataTable(flaggedData, width = 300)
                        
                    )
                    ),
                
                actionButton("flagToClean", label = "Next: Perform Cleaning"),
                actionButton("flagToDocument", label = "Next: Continue with Just Flagging"),
                
                taskItem(value = 60, color = "red",
                         "Step 4 of 6")
               
            )
        )
    })
    
    output$cleanedResultsUI <- renderUI({
        conditionalPanel(
            "input.flagToClean > 0",
            tagList(
                p(paste("Wow! Cleaning is succesfully done.")),
                
                actionButton("cleanToDocument", label = "Next: Manage Artifacts and Reports"),
                
                taskItem(value = 80, color = "red",
                         "Step 5 of 6")
            )
        )
    })
    
    observeEvent(input$flagToClean, {
        withProgress(message = "Cleaning Data...", {
            cleanedData <<- perform_Cleaning(flaggedData, cleaningThreshold = cleaningThresholdControl)
        })
        
    })
    
    observeEvent(input$flagToDocument, {
        cleanedData <<- flaggedData
        updateTabItems(session, "sideBar", "document")
        
    })
    
    observeEvent(input$cleanToDocument, {
        updateTabItems(session, "sideBar", "document")
    })
    
    observeEvent(input$cleanControl, {
        cleaningThresholdControl <<- input$cleanControl
    })
    
    output$flaggedDataTable <-  reactive(DT::renderDT(flaggedData))
    
})
