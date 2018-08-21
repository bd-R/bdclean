options(shiny.maxRequestSize = 50 * 1024 ^ 2)

shinyServer(function(input, output, session) {
    # End session with browser close
    # session$onSessionEnded(function() {
    #     stopApp()
    # })
    
    # jscode <- "shinyjs.closeWindow = function() { window.close(); }"
    
    # ------------- Local Data store ------------------------
    dataStore <-
        list(
            inputData = data.frame(),
            inputReceived = FALSE,
            
            configuredCleaning = FALSE,
            customizedChecks = c(),
            customizedCheck = FALSE,
            
            flaggedData = data.frame(),
            flaggingDone = FALSE,
            
            cleanedData = data.frame(),
            cleaningDone = FALSE,
            
            questionnaire = bdclean::create_default_questionnaire(),
            # bdclean::
            qualityChecks = bdclean::get_checks_list(),
            # bdclean::
            
            warningData =
                data.frame(
                    from = c("Startup"),
                    message = c("bdclean Started"),
                    time = format(Sys.time(), "%I:%M %p"),
                    icon = "rocket"
                ),
            
            cleaningThresholdControl = 7
        )
    
    # ------------- End of Local Data store ------------------------
    
    # ------------- Warning Menu Notifiation ------------------------
    options(warn = 1)
    
    addWarnings <- function(from, warnings, icon = "avatar") {
        if (length(warnings) == 0) {
            return()
        }
        temp <-
            data.frame(
                from = from,
                message = warnings,
                time = format(Sys.time(), "%I:%M %p"),
                icon = icon
            )
        dataStore$warningData <<- rbind(temp, dataStore$warningData)
    }
    
    # ------------- End of Warning Menu Notifiation ------------------------
    
    # ------------- Information Modal ------------------------
    
    showModal(modalDialog(
        title = h3("Welcome to bdclean!"),
        p(
            "Clean your Biodiversity data with this tool with greater control."
        ),
        p(
            "Click the tabs in the left and follow the instructions to customize cleaning."
        ),
        img(src = 'bdverse.png', align = "center"),
        helpText(
            "MIT License ©Tomer Gueta, Vijay Barve, Thiloshon Nagarajah, Ashwin Agrawal and Carmel Yohay (2018).
            bdclean: Biodiversity Data Cleaning Workflow. R package version 1.0.0."
        ),
        helpText(
            "Contribute: ",
            a("https://github.com/bd-R/bdclean", href = "https://github.com/bd-R/bdclean"),
            " Join: ",
            a("https://bd-r-group.slack.com",     href = "https://bd-r-group.slack.com")
        )
        
        ))
    
    # ------------- End of Information Modal ------------------------
    
    
    # ------------- Next Buttons Navigation Control -------------------
    
    observeEvent(input$dataToConfigure, {
        if (dataStore$inputReceived) {
            updateTabItems(session, "sideBar", "configure")
        } else {
            showNotification("Please add data first!", duration = 2)
        }
    })
    
    observeEvent(input$configureToFlag, {
        if (length(input$typeInput) > 0) {
            showNotification("Response to customized cleaning detected",
                             duration = 2)
            
            dummyQuestion <-
                bdclean::BdQuestion(
                    question = "Customized Quality Checks",
                    possible.responses = c("Yes" , "No"),
                    question.type = "ChildRouter",
                    router.condition = c("Yes"),
                    quality.checks = input$typeInput,
                    question.id = "dummy",
                    ui.type = "single-checkbox"
                    
                )
            dummyQuestion$users.answer <- "Yes"
            
            dataStore$customizedChecks <<-
                BdQuestionContainer(c(dummyQuestion))
            dataStore$customizedCheck <<- TRUE
            
        } else {
            getResponse <- function(bdQuestion) {
                showNotification("Response to questionnaire detected",
                                 duration = 2)
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
            
            for (question in dataStore$questionnaire$BdQuestions) {
                if (question$question.type != "Child") {
                    getResponse(question)
                }
            }
        }
        
        dataStore$configuredCleaning <<- TRUE
        updateTabItems(session, "sideBar", "flag")
    })
    
    observeEvent(input$flagToClean, {
        if (!dataStore$flaggingDone) {
            showNotification("Please click Flag first!", duration = 2)
            return()
        }
        
        withProgress(message = "Cleaning Data...", {
            # For threshold uncomment this
            # dataStore$cleanedData <<-
            #     perform_Cleaning(dataStore$flaggedData,
            #                      cleaningThreshold = dataStore$cleaningThresholdControl)
            
            
            warnings <- capture.output(
                dataStore$cleanedData <<-
                    bdclean::cleaning_function(dataStore$flaggedData) # bdclean::
                ,
                type = "message"
            )
            
            addWarnings("Warning while Cleaning", warnings, "trash")
        })
        
        dataStore$cleaningDone <<- TRUE
    })
    
    observeEvent(input$flagToDocument, {
        if (!dataStore$flaggingDone) {
            showNotification("Please click Flag first!", duration = 2)
            return()
        }
        
        updateTabItems(session, "sideBar", "document")
        
        checks <-
            ifelse(dataStore$customizedCheck,
                   "customizedChecks",
                   "questionnaire")
        
        withProgress(message = "Generating Artifacts...", {
            warnings <- capture.output(
                bdclean::create_report_data(
                    # bdclean::
                    dataStore$inputData,
                    dataStore$flaggedData,
                    dataStore$cleanedData,
                    dataStore[[checks]],
                    dataStore$cleaningDone,
                    c("md_document")
                ),
                type = "message"
            )
            
            addWarnings("Warning in Report Generation", warnings, "file")
        })
        
        dataStore$cleaningDone <- FALSE
        dataStore$cleanedData <-   dataStore$flaggedData
    })
    
    observeEvent(input$cleanToDocument, {
        updateTabItems(session, "sideBar", "document")
        
        withProgress(message = "Generating Artifacts...", {
            checks <-
                ifelse(dataStore$customizedCheck,
                       "customizedChecks",
                       "questionnaire")
            
            warnings <- capture.output(
                bdclean::create_report_data(
                    # bdclean::
                    dataStore$inputData,
                    dataStore$flaggedData,
                    dataStore$cleanedData,
                    dataStore[[checks]],
                    dataStore$cleaningDone,
                    c("md_document")
                ),
                type = "message"
            )
            
            addWarnings("Warning in Report Generation", warnings, "file")
        })
    })
    
    # ------------- End of Next Buttons Navigation Control -------------------
    
    
    # ------------- Add Data Module -------------------
    
    map <- leafletProxy("mymap")
    
    observeEvent(input$queryDatabase, {
        withProgress(message = paste("Querying", input$queryDB, "..."), {
            if (input$queryDB == "gbif") {
                data <-
                    rgbif::occ_search(
                        scientificName = input$scientificName,
                        limit = input$recordSize
                    )
                dataStore$inputData <<- data$data
                
            } else {
                data <-
                    spocc::occ(input$scientificName,
                               input$queryDB,
                               limit = input$recordSize)
                
                tempData <- data[[input$queryDatabase]]$data[[1]]
                dataStore$inputData <<- tempData
            }
        })
        
        dataLoadedTask(dataStore$inputData)
    })
    
    observeEvent(input$inputFile, {
        withProgress(message = paste("Reading", input$inputFile$name, "..."), {
            if (is.null(input$inputFile))
                return("No data to view")
            
            if (grepl("zip", tolower(input$inputFile$type))) {
                message("Reading DWCA ZIP...")
                finchRead <-
                    finch::dwca_read(input$inputFile$datapath, read = T)
                dataStore$inputData <<- finchRead$data[[1]]
                
            } else {
                dataStore$inputData <<-
                    data.table::fread(input$inputFile$datapath)
            }
        })
        
        dataLoadedTask(dataStore$inputData)
    })
    
    observeEvent(input$mapTexture, {
        if (length(dataStore$inputData) == 0) {
            return(NULL)
        }
        leafletProxy("mymap", data = dataStore$inputData) %>%
            clearShapes() %>%
            addCircles( ~ decimalLongitude, ~ decimalLatitude, color = input$mapColor)
    })
    
    observeEvent(input$mapColor, {
        if (length(dataStore$inputData) == 0) {
            return(NULL)
        }
        leafletProxy("mymap", data = dataStore$inputData) %>%
            clearShapes() %>%
            addCircles( ~ decimalLongitude, ~ decimalLtitude, color = input$mapColor)
    })
    
    dataLoadedTask <- function(data) {
        if (length(data) == 0) {
            showNotification("Empty data returned! Try different setting.",
                             duration = 3)
            return()
        }
        
        # ------------ Darwinizing Data -------------
        
        if (input$darwinizerControl) {
            showNotification("Cleaning Headers", duration = 2)
            dictionaryPath <-
                system.file("txts/customDwCdictionary.txt", package = "bdclean")
            customDictionary <-
                data.table::fread(file = dictionaryPath)
            
            darwinizer <-
                bdDwC::darwinizeNames(myData, customDictionary)
            
            fixed <-
                darwinizer[darwinizer$matchType == "Darwinized", ]
            
            if (nrow(fixed) > 0) {
                tidyData <- bdDwC::renameUserData(myData, darwinizer)
                dataStore$inputData <<- data
                
                showNotification(paste("Converted Columns:", paste(
                    paste(fixed[, 1], collapse = ", "),
                    paste(fixed[, 2], collapse = ", "),
                    sep = " -> "
                )), duration = 7)
            }
        }
        
        
        # ------------ End of Darwinizing Data -------------
        
        try(leafletProxy("mymap", data = data) %>%
                clearShapes() %>%
                addCircles( ~ decimalLongitude, ~ decimalLatitude, color = input$mapColor))
        
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
        
        dataStore$inputReceived <<- TRUE
        
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
                    div(class="subSpan", createQuestionsUI(question, val))
                    
                )
                val <<- val + 1
            }
        }
        
        for (question in dataStore$questionnaire$BdQuestions) {
            if (question$question.type != "Child" &&
                question$question.type != "ChildRouter") {
                createUIContainer(question)
            }
        }
        
        return(components)
    })
    
    # ------------- End of Questionnaire Module -------------------
    
    
    # ------------- Quality Checks Module -------------------
    
    output$qualityChecks <- renderUI({
        components <- list()
        
        for (i in 1:length(dataStore$qualityChecks)) {
            components[[i]] <- tagList(
                HTML(
                    paste(
                        "<input type=checkbox
                        name=typeInput value=",
                        dataStore$qualityChecks[[i]]$nameOfQualityCheck,
                        ">"
                    )
                ),
                div(
                    class = "checksListContent",
                    h4(dataStore$qualityChecks[[i]]$nameOfQualityCheck),
                    
                    div(class = "checksListTopic col-sm-3", p("Description: ")),
                    div(class = "checksListTitle", p(
                        dataStore$qualityChecks[[i]]$description
                    )),
                    
                    div(class = "checksListTopic col-sm-3", p("Sample Passing Data: ")),
                    div(
                        class = "checksListTitle",
                        p(dataStore$qualityChecks[[i]]$samplePassData)
                    ),
                    
                    div(class = "checksListTopic col-sm-3", p("Sample Failing Data: ")),
                    div(
                        class = "checksListTitle",
                        p(dataStore$qualityChecks[[i]]$sampleFailData)
                    ),
                    
                    div(class = "checksListTopic col-sm-3", p("Category of Quality Check: ")),
                    div(
                        class = "checksListTitle",
                        p(dataStore$qualityChecks[[i]]$checkCategory)
                    ),
                    
                    div(class = "checksListTopic col-sm-3", p(
                        "DWC Field Targetted by Check: "
                    )),
                    div(
                        class = "checksListTitle",
                        p(dataStore$qualityChecks[[i]]$targetDWCField)
                    )
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
    
    # ------------- End of Quality Checks Module -------------------
    
    
    # ------------- Domain Cleaning Module -------------------
    
    output$domainCleaning <- renderUI({
        components <- list()
        
        components[[1]] <- tagList(
            HTML(
                paste("<input type=radio
                      name=domainInput value=",
                      "as",
                      ">")
            ),
            div(
                class = "checksListContent",
                h4("Marine Research"),
                
                div(class = "checksListTopic col-sm-3", p("Description: ")),
                div(
                    class = "checksListTitle",
                    p(
                        "Researches focused on marine species and marine occarance distribution"
                    )
                ),
                
                div(class = "checksListTopic col-sm-3", p("Quality checks performed: ")),
                div(
                    class = "checksListTitle",
                    p(
                        "depth_out_of_range_flag, country_coordinate_mismatch_flag, precision_uncertainty_mismatch_flag
                        , center_of_the_country_coordinates_flag
                        , coordinate_negated_flag"
                    )
                    ),
                
                div(class = "checksListTopic col-sm-3", p("DWC Fields Targetted by Checks: ")),
                div(class = "checksListTitle", p("coordinates"))
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
    
    # ------------- End of Domain Cleaning Module -------------------
    
    
    # ------------- Flagging Module -------------------
    
    observeEvent(input$flagButton, {
        tempData <- dataStore$inputData
        dataStore$flaggedData <<- data.frame()
        dataStore$cleanedData <<- data.frame()
        
        withProgress(message = "Flagging Data...", {
            checks <-
                ifelse(dataStore$customizedCheck,
                       "customizedChecks",
                       "questionnaire")
            
            warnings <- capture.output(
                dataStore$flaggedData <<-
                    dataStore[[checks]]$flagData(dataStore$inputData, missing =
                                                     input$missingCase),
                type = "message"
            )
            dataStore$flaggingDone <<- TRUE
            
            addWarnings("Warning while Flagging", warnings, "flag")
        })
    })
    
    output$messageMenu <- renderMenu({
        msgs <-
            apply(as.data.frame(dataStore$warningData), 1, function(row) {
                messageItem(
                    from = row[["from"]],
                    message = row[["message"]],
                    time = row[["time"]],
                    icon = icon(row[["icon"]])
                )
            })
        
        input$flagToClean
        input$flagButton
        input$flagToDocument
        input$cleanToDocument
        
        dropdownMenu(type = "messages", .list = msgs)
    })
    
    
    output$flaggedContentUI <- renderUI({
        input$flagButton
        #input$cleanControl
        
        get_flagging_statistics <-
            function(flaggedData) {
                flaggedData <- as.data.frame(flaggedData)
                
                if (nrow(flaggedData) == 0) {
                    return(0)
                }
                
                checkColumns <-
                    which(grepl("bdclean", names(flaggedData)))
                
                if (length(checkColumns) == 0) {
                    warning("Dataset has no flag columns!")
                    return(nrow(flaggedData))
                }
                
                checkData <- flaggedData[, checkColumns]
                
                return(nrow(flaggedData) - sum(rowSums(checkData != TRUE, na.rm = TRUE) >= 1))
            }
        
        #Uncomment if threshold
        # flaggedCount <-
        #     get_flagging_statistics(dataStore$flaggedData,
        #                             dataStore$cleaningThresholdControl)
        
        warnings <- capture.output(flaggedCount <-
                                       get_flagging_statistics(dataStore$flaggedData) ,
                                   type = "message")
        addWarnings("Message while Flagging", warnings, "question")
        
        
        conditionalPanel(
            "input.flagButton > 0",
            tagList(
                h3("Flagged Data"),
                
                # Uncomment if threshold needed
                # sliderInput(
                #     "cleanControl",
                #     label = h4("Cleanliness Treshold:"),
                #     min = 0,
                #     max = 10,
                #     value = dataStore$cleaningThresholdControl
                # ),
                #
                # helpText(
                #     "Note: Cleanliness Score determines how clean your data has to be.",
                #     "Score of 10 will return only the perfect records, while scores less
                #     than 3 will also return somewhat okay records.",
                #     "Tweak the score value and check the remaining records in statistics
                #     boxes below to determine the score you require."
                # ),
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
                                    flaggedCount / nrow(dataStore$inputData)
                                ) * 100), "%", sep = ""),
                                icon = icon("flag"),
                                color = "red"
                            ),
                            infoBox("# of Clean Records",
                                    flaggedCount,
                                    icon = icon("list-ol")),
                            infoBox(
                                "# of Newly Added Columns",
                                length(dataStore$flaggedData) - length(dataStore$inputData),
                                icon = icon("th-list"),
                                color = "purple"
                            ),
                            infoBox(
                                "# of Unique Scientific Names Remaining",
                                length(unique(
                                    dataStore$flaggedData$scientificName
                                )),
                                icon = icon("paw"),
                                color = "yellow"
                            )
                        )
                        
                    ),
                    tabPanel(
                        "Table View",
                        div(class = "secondaryHeaders", h3("View 02: Summarized Table")),
                        DT::renderDataTable(dataStore$flaggedData, width = 300)
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
    
    output$flaggedDataTable <-
        reactive(DT::renderDT(dataStore$flaggedData))
    
    # ------------- End of Flagging Module -------------------
    
    
    # ------------- Cleaning Module ------------------------
    
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
    
    # observeEvent(input$cleanControl, {
    #     dataStore$cleaningThresholdControl <<- input$cleanControl
    # })
    
    # ------------- End of Cleaning Module ------------------------
    
    
    # ------------- Documentation Module ------------------------
    
    output$documentContentUI <- renderUI({
        input$flagButton
        tagList(
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
                            DT::renderDataTable(dataStore$inputData, width = 300)
                        ),
                        tabPanel(
                            "Flagged Data",
                            div(class = "secondaryHeaders", h3(
                                "Artifact 02: Complete Flagged Data"
                            )),
                            downloadButton("downloadFlagged", "Download Flagged Data"),
                            br(),
                            br(),
                            DT::renderDataTable(dataStore$flaggedData, width = 300)
                        ),
                        tabPanel(
                            "Cleaned Data",
                            div(class = "secondaryHeaders", h3("Artifact 03: Cleaned Data")),
                            downloadButton("downloadCleaned", "Download Cleaned Data"),
                            br(),
                            br(),
                            DT::renderDataTable(dataStore$cleanedData, width = 300)
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
                        )
                    ),
                    div(
                        class = "progressStep",
                        taskItem(value = 100, color = "green",
                                 "Step 6 of 6")
                    )
                )
            )
        )
    })
    
    output$downloadShortReport <- downloadHandler(
        filename = function() {
            paste('shortReport-', Sys.Date(), switch(
                input$reportFormat,
                "pdf_document" = ".pdf",
                "html_document" = ".html",
                "word_document" = ".docx",
                "md_document" = ".md"
            ), sep = "")
        },
        content = function(file) {
            withProgress(message = "Preparing download...", {
                checks <-
                    ifelse(dataStore$customizedCheck,
                           "customizedChecks",
                           "questionnaire")
                
                bdclean::create_report_data(
                    # bdclean::
                    dataStore$inputData,
                    dataStore$cleanedData,
                    dataStore$flaggedData,
                    dataStore[[checks]],
                    dataStore$cleaningDone,
                    input$reportFormat
                )
            })
            
            file.copy(file.path(
                getwd(),
                "CleaningReports",
                paste("generateShortReport", switch(
                    input$reportFormat,
                    "pdf_document" = ".pdf",
                    "html_document" = ".html",
                    "word_document" = ".docx",
                    "md_document" = ".md"
                ), sep = "")
            ),
            file)
        }
    )
    
    output$downloadDetailedReport <- downloadHandler(
        filename = function() {
            paste('detailedReport-', Sys.Date(), switch(
                input$reportFormat,
                "pdf_document" = ".pdf",
                "html_document" = ".html",
                "word_document" = ".word",
                "md_document" = ".md"
            ), sep = "")
        },
        content = function(file) {
            withProgress(message = "Preparing download...", {
                checks <-
                    ifelse(dataStore$customizedCheck,
                           "customizedChecks",
                           "questionnaire")
                bdclean::create_report_data(
                    # bdclean::
                    dataStore$inputData,
                    dataStore$cleanedData,
                    dataStore$flaggedData,
                    dataStore[[checks]],
                    dataStore$cleaningDone,
                    input$reportFormat
                )
            })
            file.copy(file.path(
                getwd(),
                "CleaningReports",
                paste("generateDetailedReport", switch(
                    input$reportFormat,
                    "pdf_document" = ".pdf",
                    "html_document" = ".html",
                    "word_document" = ".word",
                    "md_document" = ".md"
                ), sep = "")
            ),
            file)
        }
    )
    
    
    output$downloadInput <- downloadHandler(
        filename = function() {
            paste('inputData-', Sys.Date(), '.csv')
        },
        content = function(con) {
            write.csv(dataStore$inputData, con)
        }
    )
    output$downloadFlagged <- downloadHandler(
        filename = function() {
            paste('flaggedData-', Sys.Date(), '.csv')
        },
        content = function(con) {
            write.csv(dataStore$flaggedData, con)
        }
    )
    
    output$downloadCleaned <- downloadHandler(
        filename = function() {
            paste('cleanedData-', Sys.Date(), '.csv')
        },
        content = function(con) {
            write.csv(dataStore$cleanedData, con)
        }
    )
    
    # ------------- End of Documentation Module ------------------------
})
