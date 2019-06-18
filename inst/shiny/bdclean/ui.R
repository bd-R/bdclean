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
                                    
                                    # -------------------------------
                                    
                                    questionnaireUI("questionnaireMod")
                                    
                                    # -------------------------------
                                ),
                                tabPanel(
                                    "Option 02: Customized Checks",
                                    
                                    # -------------------------------
                                    
                                    customizedCheckUI("customCheckMod")
                                    
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
                            # -------------------------------
                            
                            FlaggingUI("flaggingMod"),
                        
                            
                            # -------------------------------
                            
                            
                            
                            div(class = "progressStep", taskItem(
                                value = 45, color = "yellow",
                                "Step 3 of 6"
                            )),
                            
                            uiOutput("cleanedResultsUI")
                            
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
                         ))),

                                    tabItem("citTab",
                                            fluidRow(column(
                                                12,
                                                column(
                                                    12,
                                                    h1("Package Citations"),
                                                    uiOutput("citationsUI")
                                                )
                    )))
        )
        
        # ------------- End of Documentation Module -------------------
        
    )
))
