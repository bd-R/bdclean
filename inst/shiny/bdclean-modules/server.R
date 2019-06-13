options(shiny.maxRequestSize = 50 * 1024 ^ 2)
library(bdchecks)

shinyServer(function(input, output, session) {
    # ------------- Local Data store ------------------------
    data_store <-
            list(
                inputData = data.frame(),
                configuredCleaning = FALSE,
                customizedChecks = c(),
                customizedCheck = FALSE,
                flaggedData = data.frame(),
                flaggingDone = FALSE,
                cleanedData = data.frame(),
                cleaningDone = FALSE,
                questionnaire = bdclean::create_default_questionnaire(),
                
                warningData =
                    data.frame(
                        from = c("Startup"),
                        message = c("bdclean Started"),
                        time = "Now",
                        icon = "rocket"
                    ),
                
                cleaningThresholdControl = 7
            )
        
    
    # ------------- End of Local Data store ------------------------
    
    
    # ------------- Information Modal ------------------------
    
    showModal(modalDialog(
        title = h3("Welcome to bdclean!"),
        p(
            "Clean your Biodiversity data with this tool with greater control."
        ),
        p(
            "Click the tabs in the left and follow the instructions to customize cleaning."
        ),
        img(src = "bdverse.png", align = "center"),
        helpText(
            "GPL-3 ©Tomer Gueta, Vijay Barve, Thiloshon Nagarajah, Ashwin Agrawal and Carmel Yohay (2018).
            bdclean: Biodiversity Data Cleaning Workflow. R package version 0.1.900"
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
        # Converting reactive element to dataframe
        data_store$inputData <<- data_store$inputData()
        
        
        output$inputDataRows <- renderText(nrow(data_store$inputData))
        output$inputDataColumns <- renderText(length(data_store$inputData))
        output$inputDataSpecies <-
            renderText(length(unique(data_store$inputData$scientificName)))
        
        if (nrow(data_store$inputData) > 0) {
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
                    # bdclean::
                    question = "Customized Quality Checks",
                    possible.responses = c("Yes", "No"),
                    question.type = "ChildRouter",
                    router.condition = c("Yes"),
                    quality.checks = input$typeInput,
                    question.id = "dummy",
                    ui.type = "single-checkbox"
                    
                )
            dummyQuestion$users.answer <- "Yes"
            
            data_store$customizedChecks <<-
                BdQuestionContainer(c(dummyQuestion))
            data_store$customizedCheck <<- TRUE
            
        } else {
            getResponse <- function(bdQuestion) {
                showNotification("Response to questionnaire detected",
                                 duration = 2)
                # set response
                bdQuestion$set_response(input[[bdQuestion$question.id]])
                
                if (bdQuestion$question.type == "Router") {
                    if (bdQuestion$users.answer %in% bdQuestion$router.condition) {
                        for (question in bdQuestion$child.questions) {
                            getResponse(question)
                        }
                    }
                }
            }
            
            for (question in data_store$questionnaire$bdquestions) {
                if (question$question.type != "Child") {
                    getResponse(question)
                }
            }
        }
        
        data_store$configuredCleaning <<- TRUE
        updateTabItems(session, "sideBar", "flag")
    })
    
    observeEvent(input$flagToClean, {
        data_store$flaggedData <<- data_store$flaggedData()
        data_store$flaggingDone <<- TRUE 
        
        if (!data_store$flaggingDone) {
            showNotification("Please click Flag first!", duration = 2)
            return()
        }
        
        withProgress(message = "Cleaning Data...", {
            data_store$cleanedData <<-
                bdclean::cleaning_function(data_store$flaggedData) # bdclean::
        })
        
        shinyjs::addClass(id = "flagToCleanDiv",
                          class = "readyButton")
        shinyjs::removeClass(id = "flagToCleanDiv",
                             class = "completedButton")
        
        data_store$cleaningDone <<- TRUE
    })
    
    observeEvent(input$flagToDocument, {
        data_store$flaggedData <<- data_store$flaggedData()
        data_store$flaggingDone <<- TRUE 
        
        updateTabItems(session, "sideBar", "document")
        
        checks <-
            ifelse(data_store$customizedCheck,
                   "customizedChecks",
                   "questionnaire")
        
        withProgress(message = "Generating Artifacts...", {
            bdclean::create_report_data(
                # bdclean::
                data_store$inputData,
                data_store$flaggedData,
                data_store$cleanedData,
                data_store[[checks]],
                data_store$cleaningDone,
                c("md_document")
            )
        })
        
        data_store$cleaningDone <- FALSE
        data_store$cleanedData <-   data_store$flaggedData
    })
    
    observeEvent(input$cleanToDocument, {
        updateTabItems(session, "sideBar", "document")
        
        withProgress(message = "Generating Artifacts...", {
            checks <-
                ifelse(data_store$customizedCheck,
                       "customizedChecks",
                       "questionnaire")
            
            bdclean::create_report_data(
                # bdclean::
                data_store$inputData,
                data_store$flaggedData,
                data_store$cleanedData,
                data_store[[checks]],
                data_store$cleaningDone,
                c("md_document")
            )

        })
    })
    
    # ------------- End of Next Buttons Navigation Control -------------------
    
    
    # ------------- Add Data Module -------------------

    data_store$inputData <- callModule(bdFile, "bdFileInput")
    
    # ------------- End of Add Data Module -------------------
    
    
    # ------------- Questionnaire Module -------------------
    
    callModule(questionnaire, "questionnaireMod", bdquestions = data_store$questionnaire$bdquestions)
    
    # ------------- End of Questionnaire Module -------------------
    
    
    # ------------- Quality Checks Module -------------------
    
    callModule(customizedCheck, "customCheckMod")
    
    # ------------- End of Quality Checks Module -------------------
    
    
    # ------------- Flagging Module -------------------
    
    
    data_store$flaggedData <- callModule(Flagging, "flaggingMod", reactive({data_store}))
    
    # ------------- End of Flagging Module -------------------
    
    
    # ------------- Cleaning Module ------------------------
    
    output$cleanedResultsUI <- renderUI({
        conditionalPanel("input.flagToClean > 0",
                         tagList(
                             div(id = "completedIcon", img(
                                 src = "completed.png", align = "center"
                             )),
                             p(paste("Cleaning is succesfully done.")),
                             
                             div(
                                 id = "cleanToDocumentDiv",
                                 class = "completedButton",
                                 actionButton("cleanToDocument", label = "Next: Manage Artifacts and Reports")
                             ),
                             
                             div(class = "progressStep",  taskItem(
                                 value = 80, color = "red",
                                 "Step 5 of 6"
                             ))
                         ))
    })
    
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
                            DT::renderDataTable(summarizeDataframe(data_store$inputData), width = 300)
                        ),
                        tabPanel(
                            "Flagged Data",
                            div(class = "secondaryHeaders", h3(
                                "Artifact 02: Complete Flagged Data"
                            )),
                            downloadButton("downloadFlagged", "Download Flagged Data"),
                            br(),
                            br(),
                            DT::renderDataTable(summarizeDataframe(data_store$flaggedData), width = 300)
                        ),
                        tabPanel(
                            "Cleaned Data",
                            div(class = "secondaryHeaders", h3("Artifact 03: Cleaned Data")),
                            downloadButton("downloadCleaned", "Download Cleaned Data"),
                            br(),
                            br(),
                            DT::renderDataTable(summarizeDataframe(data_store$cleanedData), width = 300)
                        ),
                        tabPanel(
                            "Cleaning Report",
                            div(class = "secondaryHeaders", h3(
                                "Report 01: Short Cleaning Summary"
                            )),
                            
                            downloadButton("downloadShortReport", "Download Cleaning Summary"),
                            br(),
                            br(),
                            includeMarkdown(paste(tempdir(), "/generateShortReport.md", sep = ""))
                        ),
                        tabPanel(
                            "Detailed Quality Check Report",
                            div(class = "secondaryHeaders", h3(
                                "Report 02: Detailed Quality Check Report"
                            )),
                            downloadButton("downloadDetailedReport", "Download Detailed Report"),
                            br(),
                            br(),
                            includeMarkdown(paste(tempdir(), "/generateDetailedReport.md", sep = ""))
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
            paste("shortReport-", Sys.Date(), switch(
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
                    ifelse(data_store$customizedCheck,
                           "customizedChecks",
                           "questionnaire")
                
                bdclean::create_report_data(
                    # bdclean::
                    data_store$inputData,
                    data_store$cleanedData,
                    data_store$flaggedData,
                    data_store[[checks]],
                    data_store$cleaningDone,
                    input$reportFormat
                )
            })
            
            file.copy(file.path(
                tempdir(),
                paste("/generateShortReport", switch(
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
            paste("detailedReport-", Sys.Date(), switch(
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
                    ifelse(data_store$customizedCheck,
                           "customizedChecks",
                           "questionnaire")
                bdclean::create_report_data(
                    # bdclean::
                    data_store$inputData,
                    data_store$cleanedData,
                    data_store$flaggedData,
                    data_store[[checks]],
                    data_store$cleaningDone,
                    input$reportFormat
                )
            })
            file.copy(file.path(
                tempdir(),
                paste("/generateDetailedReport", switch(
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
            paste("inputData-", Sys.Date(), ".csv")
        },
        content = function(con) {
            write.csv(data_store$inputData, con)
        }
    )
    output$downloadFlagged <- downloadHandler(
        filename = function() {
            paste("flaggedData-", Sys.Date(), ".csv")
        },
        content = function(con) {
            write.csv(data_store$flaggedData, con)
        }
    )
    
    output$downloadCleaned <- downloadHandler(
        filename = function() {
            paste("cleanedData-", Sys.Date(), ".csv")
        },
        content = function(con) {
            write.csv(data_store$cleanedData, con)
        }
    )
    
    # ------------- End of Documentation Module ------------------------
})
