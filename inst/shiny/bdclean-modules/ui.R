suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(bdchecks))
suppressPackageStartupMessages(library(finch))
suppressPackageStartupMessages(library(leaflet))

source("functions.R")

shinyUI(dashboardPage(
    #Header Title
    dashboardHeader(title = "bdclean", dropdownMenuOutput("messageMenu")),
    
    # ------------- Sidebar  -------------------
    dashboardSidebar(
        sidebarMenu(
            id = "sideBar",
            menuItem(
                "Add Data",
                tabName = "add",
                icon = icon("plus-circle")
            ),
            menuItem(
                "Configure Cleaning",
                tabName = "configure",
                icon = icon("wrench")
            ),
            menuItem("Flag & Clean", tabName = "flag", icon = icon("flag")),
            menuItem(
                "Artifacts & Documentation",
                tabName = "document",
                icon = icon("file")
            ),
            menuItem("Citations", tabName = "citTab", icon = icon("bookmark"))
        )
    ),
    
    # ------------- End of Sidebar  -------------------
    
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
            tags$link(rel = "stylesheet", type = "text/css", href = "checkbox.css")
        ),
        useShinyjs(),
        tabItems(
            # ------------- Add Data Module -------------------
            tabItem("add",
                    fluidRow(
                        div(
                            
                            # -------------------------------
                            
                            bdFileInput("bdFileInput", "User data (.csv format)"),
                        
                            # -------------------------------
                            
                            column(12,
                                   div(
                                       id = "dataToConfigureDiv",
                                       actionButton("dataToConfigure", "Next: Configure Cleaning")
                                   ),
                                   
                                   div(class = "progressStep", taskItem(
                                       value = 15, color = "orange",
                                       "Step 1 of 6"
                                   ))
                            )
                        )
                        
                    )),
            
            # -------------  End of Add Data Module -------------------
            
            # ------------- Cleaning Configuration Module -------------------
            
            tabItem("configure",
                    fluidRow(column(
                        12,
                        h1("Configure Cleaning"),
                        column(
                            12,
                            tabsetPanel(
                                type = "tabs",
                                tabPanel(
                                    "Option 01: Questionnaire ",
                                    div(class = "secondaryHeaders", h3("Option 01: Questionnaire")),
                                    helpText(
                                        "Note: If you have limited knowledge in Biodiversity data,
                                        this option is preferred.",
                                        "Answer a few questions and let bdclean take care of the cleaning."
                                    ),
                                    
                                    
                                    # -------------------------------
                                    
                                    uiOutput("questionnaire")
                                    
                                    # -------------------------------
                                ),
                                tabPanel(
                                    "Option 02: Customized Checks",
                                    div(class = "secondaryHeaders", h3("Option 02: Customized Checks")),
                                    helpText(
                                        "Note: Select the quality checks you prefer and
                                        continue cleaning with just those checks"
                                    ),
                                    
                                    # -------------------------------
                                    
                                    uiOutput("qualityChecks")
                                    
                                    # -------------------------------
                                    
                                ),
                                
                                div(class = "progressStep", taskItem(
                                    value = 30, color = "green",
                                    "Step 2 of 6"
                                ))
                            ),
                            div(class = "completedButton", actionButton("configureToFlag", "Next: Flagging"))
                        )
                    ))),
            
            # ------------- End of Cleaning Configuration Module -------------------
            
            
            # ------------- Flagging Module -------------------
            
            tabItem("flag",
                    fluidRow(column(
                        12,
                        column(
                            12,
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
                                checkboxInput("missingCase", label = "Mark missing values as Fail", value = FALSE),
                                helpText(
                                    "Quality checks in bdclean check the validity of each records of the column it targets. If ticked, records with missing values will be considered as invalid record and will be removed. If not ticked, missing records will not be considered in the quality check, so, will remain in the cleaned data.
                                    "
                                ),
                                fluidRow(
                                    div(
                                        id = "flagButtonDiv",
                                        class = "completedButton",
                                        actionButton("flagButton", label = "Flag Data")
                                    )
                                    
                                ),
                                
                                div(class = "progressStep", taskItem(
                                    value = 45, color = "yellow",
                                    "Step 3 of 6"
                                ))
                            ),
                            br(),
                            
                            # -------------------------------
                            
                            uiOutput("flaggedContentUI"),
                            
                            uiOutput("cleanedResultsUI")
                            
                            # -------------------------------
                        )
                    ))),
            
            # ------------- End of Flagging Module -------------------
            
            # ------------- Documentation Module -------------------
            tabItem("document",
                    fluidRow(column(
                        12,
                        column(
                            12,
                            h1("Artifacts and Reports"),
                            br(),
                            selectInput(
                                "reportFormat",
                                "Report Type",
                                choices = list(
                                    "PDF" = "pdf_document",
                                    "HTML" = "html_document",
                                    "Word" = "word_document",
                                    "Markdown" = "md_document"
                                ),
                                selected = "pdf_document"
                            ),
                            
                            # -------------------------------
                            
                            uiOutput("documentContentUI")
                        )
                    )))
        )
        
        # ------------- End of Documentation Module -------------------
        
    )
))
