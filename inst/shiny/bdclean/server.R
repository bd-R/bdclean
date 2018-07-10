source("functions/decision_making.R")
source("functions/generate_report.R")

shinyServer(function(input, output, session) {
    inputData <- data.frame()
    flaggedData <- data.frame()
    cleanedData <- data.frame()
    questionnaire <- create_default_questionnaire()
    qualityChecks <- get_checks_list()
    
    cleaningThresholdControl <- 7
    cleaningControl <- FALSE
    
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
            if (length(input$queryDatabase == 1) &&
                input$queryDatabase == "gbif") {
                print("in")
                data <-
                    occ_search(input$scientificName, limit = input$recordSize)
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
    
    observeEvent(input$mapTexture, {
        if (length(inputData) == 0) {
            return(NULL)
        }
        leafletProxy("mymap", data = inputData) %>%
            clearShapes() %>%
            addCircles(~ longitude, ~ latitude, color = input$mapColor)
    })
    
    observeEvent(input$mapColor, {
        if (length(inputData) == 0) {
            return(NULL)
        }
        leafletProxy("mymap", data = inputData) %>%
            clearShapes() %>%
            addCircles(~ longitude, ~ latitude, color = input$mapColor)
    })
    
    dataLoadedTask <- function(data) {
        leafletProxy("mymap", data = data) %>%
            clearShapes() %>%
            addCircles(~ longitude, ~ latitude, color = input$mapColor)
        
        output$inputDataTable <- DT::renderDataTable(DT::datatable({
            data
        }, options = list(scrollX = TRUE)))
        
        shinyjs::addClass(id = 'queryDatabaseDiv',
                          class = 'readyButton')
        shinyjs::removeClass(id = 'queryDatabaseDiv',
                             class = 'activeButton')
        
        shinyjs::addClass(id = 'inputFileDiv',
                          class = 'readyButton')
        shinyjs::removeClass(id = 'inputFileDiv',
                             class = 'activeButton')
        
        shinyjs::addClass(id = 'dataToConfigureDiv',
                          class = 'completedButton')
        shinyjs::removeClass(id = 'queryDatabaseDiv',
                             class = 'readyButton')
        
        showNotification("Read Data Succesfully", duration = 2)
        
        
        output$inputDataRows <- renderText(nrow(data))
        output$inputDataColumns <- renderText(length(data))
        output$inputDataSpecies <-
            renderText(length(unique(data$scientificName)))
    }
    
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(input$mapTexture) %>%
            setView(0, 0, zoom = 2)
    })
    
    output$qualityChecks <- renderUI({
        components <- list()
        
        for (i in 1:length(qualityChecks)) {
            components[[i]] <- tagList(
                HTML(
                    paste(
                        "<input type=checkbox
                        name=typeInput value=",
                        qualityChecks[[i]]$nameOfQualityCheck,
                        ">"
                    )
                ),
                div(
                    class = "checksListContent",
                    h4(qualityChecks[[i]]$nameOfQualityCheck),
                    
                    div(class = "checksListTopic col-sm-3", p("Description: ")),
                    div(class = "checksListTitle", p(qualityChecks[[i]]$description)),
                    
                    div(class = "checksListTopic col-sm-3", p("Sample Passing Data: ")),
                    div(class = "checksListTitle", p(
                        qualityChecks[[i]]$samplePassData
                    )),
                    
                    div(class = "checksListTopic col-sm-3", p("Sample Failing Data: ")),
                    div(class = "checksListTitle", p(
                        qualityChecks[[i]]$sampleFailData
                    )),
                    
                    div(class = "checksListTopic col-sm-3", p("Category of Quality Check: ")),
                    div(class = "checksListTitle", p(
                        qualityChecks[[i]]$checkCategory
                    )),
                    
                    div(class = "checksListTopic col-sm-3", p(
                        "DWC Field Targetted by Check: "
                    )),
                    div(class = "checksListTitle", p(
                        qualityChecks[[i]]$targetDWCField
                    ))
                ),
                br(),
                br()
            )
        }
        
        return(
            div(
                id = 'typeInput',
                class = "form-group shiny-input-checkboxgroup shiny-input-container shiny-bound-input",
                tags$br(),
                tags$br(),
                column(width = 12,
                       components)
            )
        )
    })
    
    output$domainCleaning <- renderUI({
        components <- list()
       
            components[[1]] <- tagList(
                HTML(
                    paste(
                        "<input type=radio
                        name=domainInput value=",
                        "as",
                        ">"
                    )
                ),
                
                div(
                    class = "checksListContent",
                    h4("Marine Research"),
                    
                    div(class = "checksListTopic col-sm-3", p("Description: ")),
                    div(class = "checksListTitle", p("Researches focused on marine species and marine occarance distribution")),
                    
                    div(class = "checksListTopic col-sm-3", p("Quality checks performed: ")),
                    div(class = "checksListTitle", p(
                        "depth_out_of_range_flag, country_coordinate_mismatch_flag, precision_uncertainty_mismatch_flag
, center_of_the_country_coordinates_flag
, coordinate_negated_flag"
                    )),
                    
                    div(class = "checksListTopic col-sm-3", p(
                        "DWC Fields Targetted by Checks: "
                    )),
                    div(class = "checksListTitle", p(
                        "coordinates"
                    ))
                ),
                
                br(),
                br()
            )
            
            components[[2]] <- tagList(
                HTML(
                    paste(
                        "<input type=radio
                        name=domainInput value=",
                        "as",
                        ">"
                    )
                ),
                
                div(
                    class = "checksListContent",
                    h4("Climate Research"),
                    
                    div(class = "checksListTopic col-sm-3", p("Description: ")),
                    div(class = "checksListTitle", p("Researches focused on climate changes and affects on species")),
                    
                    div(class = "checksListTopic col-sm-3", p("Quality checks performed: ")),
                    div(class = "checksListTitle", p(
                        "depth_out_of_range_flag, country_coordinate_mismatch_flag, precision_uncertainty_mismatch_flag
                        , center_of_the_country_coordinates_flag
                        , coordinate_negated_flag"
                    )),
                    
                    div(class = "checksListTopic col-sm-3", p(
                        "DWC Fields Targetted by Checks: "
                    )),
                    div(class = "checksListTitle", p(
                        "coordinates"
                    ))
                    ),
                
                br(),
                br()
                    )
            
            components[[3]] <- tagList(
                HTML(
                    paste(
                        "<input type=radio
                        name=domainInput value=",
                        "as",
                        ">"
                    )
                ),
                
                div(
                    class = "checksListContent",
                    h4("Genetics Research"),
                    
                    div(class = "checksListTopic col-sm-3", p("Description: ")),
                    div(class = "checksListTitle", p("Researches focused on genetics and bioinformnatics of species")),
                    
                    div(class = "checksListTopic col-sm-3", p("Quality checks performed: ")),
                    div(class = "checksListTitle", p(
                        "depth_out_of_range_flag, country_coordinate_mismatch_flag, precision_uncertainty_mismatch_flag
                        , center_of_the_country_coordinates_flag
                        , coordinate_negated_flag"
                    )),
                    
                    div(class = "checksListTopic col-sm-3", p(
                        "DWC Fields Targetted by Checks: "
                    )),
                    div(class = "checksListTitle", p(
                        "coordinates"
                    ))
                ),
                
                br(),
                br()
            )
        
        return(
            div(
                id = 'domainInput',
                class = "form-group shiny-input-radiogroup shiny-input-container shiny-bound-input",
                tags$br(),
                tags$br(),
                column(width = 12,
                       components)
            )
        )
        
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
                        
                    ),
                    tabPanel(
                        "Table View",
                        div(class = "secondaryHeaders", h3("View 02: Summarized Table")),
                        DT::renderDataTable(flaggedData, width = 300)
                        
                    )
                ),
                
                actionButton("flagToClean", label = "Next: Perform Cleaning"),
                actionButton("flagToDocument", label = "Next: Continue with Just Flagging"),
                
                div(class = "progressStep",  taskItem(
                    value = 60, color = "red",
                    "Step 4 of 6"
                ))
                
                
                
                )
        )
    })
    
    output$cleanedResultsUI <- renderUI({
        conditionalPanel("input.flagToClean > 0",
                         tagList(
                             div(id = "completedIcon", img(
                                 src = 'completed.png', align = "center"
                             )),
                             p(paste("Cleaning is succesfully done.")),
                             
                             actionButton("cleanToDocument", label = "Next: Manage Artifacts and Reports"),
                             
                             div(class = "progressStep",  taskItem(
                                 value = 80, color = "red",
                                 "Step 5 of 6"
                             ))
                             
                             
                         ))
    })
    
    output$documentContentUI <- renderUI({
        print(getwd())
        
        withProgress(message = "Generating Artifacts...", {
            # Report
            for (question in questionnaire$BdQuestions) {
                if (question$question.type != "Router" &&
                    length(question$users.answer) > 0) {
                    question$addToReport(flaggedData,
                                         cleaningThresholdControl,
                                         cleaningControl)
                }
            }
            create_report_data(inputData,
                               cleanedData,
                               questionnaire,
                               FALSE,
                               c("md_document"))
        })
        
        conditionalPanel(
            "input.flagToDocument > 0 || input.cleanToDocument > 0",
            tagList(
                tabsetPanel(
                    type = "tabs",
                    tabPanel(
                        "Input Data",
                        div(class = "secondaryHeaders", h3("Artifact 01: Input RAW Data")),
                        downloadButton("downloadInput", "Download Input Data"),
                        br(),
                        br(),
                        DT::renderDataTable(inputData, width = 300)
                        
                    ),
                    tabPanel(
                        "Flagged Data",
                        div(class = "secondaryHeaders", h3(
                            "Artifact 02: Complete Flagged Data"
                        )),
                        downloadButton("downloadFlagged", "Download Flagged Data"),
                        br(),
                        br(),
                        DT::renderDataTable(flaggedData, width = 300)
                        
                    ),
                    tabPanel(
                        "Cleaned Data",
                        div(class = "secondaryHeaders", h3("Artifact 03: Cleaned Data")),
                        downloadButton("downloadCleaned", "Download Cleaned Data"),
                        br(),
                        br(),
                        DT::renderDataTable(cleanedData, width = 300)
                        
                    ),
                    tabPanel(
                        "Cleaning Report",
                        div(class = "secondaryHeaders", h3(
                            "Report 01: Short Cleaning Summary"
                        )),
                        downloadButton("downloadShortReport", "Download Cleaning Summary"),
                        br(),
                        br(),
                        includeMarkdown("CleaningReports/generateShortReport.md")
                    ),
                    tabPanel(
                        "Detailed Quality Check Report",
                        div(class = "secondaryHeaders", h3(
                            "Report 02: Detailed Quality Check Report"
                        )),
                        downloadButton("downloadDetailedReport", "Download Detailed Report"),
                        br(),
                        br(),
                        includeMarkdown("CleaningReports/generateDetailedReport.md")
                        
                    ),
                    tabPanel(
                        "Source Code",
                        div(class = "secondaryHeaders", h3(
                            "Environment 01: Workflow Source Code"
                        )),
                        downloadButton("downloadCode", "Download Detailed Report"),
                        br()
                        
                    ),
                    tabPanel(
                        "R Environment",
                        div(class = "secondaryHeaders", h3("Environment 02: R Environment")),
                        downloadButton("downloadDetailedReport", "Download Detailed Report"),
                        br()
                        # DT::renderDataTable(inputData, width = 300)
                        
                    )
                ),
                
                div(
                    class = "progressStep",
                    taskItem(value = 100, color = "green",
                             "Step 6 of 6")
                )
                
            )
        )
    })
    
    output$downloadInput <- downloadHandler(
        filename = function() {
            paste('inputData-', Sys.Date(), '.csv')
        },
        content = function(con) {
            write.csv(inputData, con)
        }
    )
    
    output$downloadFlagged <- downloadHandler(
        filename = function() {
            paste('flaggedData-', Sys.Date(), '.csv')
        },
        content = function(con) {
            write.csv(flaggedData, con)
        }
    )
    
    output$downloadCleaned <- downloadHandler(
        filename = function() {
            paste('cleanedData-', Sys.Date(), '.csv')
        },
        content = function(con) {
            write.csv(cleanedData, con)
        }
    )
    
    observeEvent(input$flagToClean, {
        cleaningControl <<- TRUE
        withProgress(message = "Cleaning Data...", {
            cleanedData <<-
                perform_Cleaning(flaggedData, cleaningThreshold = cleaningThresholdControl)
        })
        
    })
    
    observeEvent(input$flagToDocument, {
        cleaningControl <- FALSE
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
