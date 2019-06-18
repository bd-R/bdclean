# UI Function
FlaggingUI <- function(id) {
    ns <- NS(id)
    
    tagList(
        h1("Flag Data"),
        br(),
        h4("Input Data"),
        div(
            class = "center",
            fluidRow(
                infoBox("# of Records", textOutput("inputDataRows"), icon = icon("list-ol")),
                infoBox(
                    "# of Fields",
                    textOutput("inputDataColumns"),
                    icon = icon("th-list"),
                    color = "purple"
                ),
                infoBox(
                    "# of Unique Scientific Names",
                    textOutput("inputDataSpecies"),
                    icon = icon("paw"),
                    color = "yellow"
                )
            ),
            
            h4("Flag Settings"),
            checkboxInput(ns("missingCase"), label = "Mark missing values as Fail", value = FALSE),
            helpText(
                "Quality checks in bdclean check the validity of each records of the column it targets. If ticked, records with missing values will be considered as invalid record and will be removed. If not ticked, missing records will not be considered in the quality check, so, will remain in the cleaned data.
                                    "
            ),
            fluidRow(
                div(
                    id = ns("flagButtonDiv"),
                    class = "completedButton",
                    actionButton(ns("flagButton"), label = "Flag Data")
                )
                
            )
        ),
        br(),
        
        # -------------------------------
        
        uiOutput(ns("flaggedContentUI"))
        
     
        
        # -------------------------------
    )
}

# Server Function
Flagging <- function(input, output, session, data_store) {
    ns <- session$ns
    returnState <- data.frame()
    
    observeEvent(input$flagButton, {
        tempData <- data_store()$inputData
        
        withProgress(message = "Flagging Data...", {
            checks <-
                ifelse(data_store()$customizedCheck,
                       "customizedChecks",
                       "questionnaire")
            
            warnings <- capture.output(
                returnState <<-
                    data_store()[[checks]]$flag_data(data_store()$inputData, missing =
                                                       input$missingCase),
                type = "message"
            )
        })
        
        shinyjs::addClass(id = "flagButtonDiv",
                          class = "readyButton")
        
        shinyjs::removeClass(id = "flagButtonDiv",
                             class = "completedButton")
        
        shinyjs::addClass(id = "flagToCleanDiv",
                          class = "completedButton")
        shinyjs::removeClass(id = "flagToCleanDiv",
                             class = "activeButton")

    })
    
    
    output$flaggedContentUI <- renderUI({
        input$flagButton
        
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
                
                
                if (class(checkData) == "logical") {
                    return(nrow(flaggedData) - length(checkData[checkData != TRUE]))
                }
                
                return(nrow(flaggedData) - sum(rowSums(checkData != TRUE, na.rm = TRUE) >= 1))
            }
        
        warnings <- capture.output(flaggedCount <-
                                       get_flagging_statistics(returnState),
                                   type = "message")
        #addWarnings("Message while Flagging", warnings, "question")
        
        
        conditionalPanel(
            "input['flaggingMod-flagButton'] > 0",
            tagList(
                h3("Flagged Data"),

                br(),
                
                tabsetPanel(
                    type = "tabs",
                    tabPanel(
                        "Statistics View",
                        div(class = "secondaryHeaders", h3("View 01: Statistics Boxes")),
                        fluidRow(
                            infoBox("# of Clean Records",
                                    flaggedCount,
                                    icon = icon("list-ol")),
                            infoBox(
                                "# of Newly Added Columns",
                                length(returnState) - length(data_store()$inputData),
                                icon = icon("th-list"),
                                color = "purple"
                            ),
                            infoBox(
                                "# of Unique Scientific Names Remaining",
                                length(unique(
                                    returnState$scientificName
                                )),
                                icon = icon("paw"),
                                color = "yellow"
                            ),
                            infoBox(
                                "Clean Data",
                                paste(((
                                    flaggedCount / nrow(data_store()$inputData)
                                ) * 100), "%", sep = ""),
                                icon = icon("flag"),
                                color = "red"
                            )
                        )
                    ),
                    tabPanel(
                        "Table View",
                        div(class = "secondaryHeaders", h3("View 02: Summarized Table")),
                        DT::renderDataTable(summarizeDataframe(returnState), width = 300)
                    )
                ),
                
                div(
                    id = "flagToCleanDiv",
                    class = "completedButton",
                    actionButton("flagToClean", label = "Next: Perform Cleaning")
                ),
                
                actionButton("flagToDocument", label = "Next: Continue with Just Flagging")
            )
        )
        
       
    })
    
    output$flaggedDataTable <-
        reactive(DT::renderDT(summarizeDataframe(data_store$flaggedData)))
    
    returnDataReact <- reactive({
        # Input actions that need to trigger new dataframe return 
        input$flagButton
        
        returnState
    })
    
    
    return(returnDataReact)
}