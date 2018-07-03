library(shiny)
library(shinydashboard)
library(leaflet)
library(shinyjs)

shinyUI(dashboardPage(
    #Header Title
    dashboardHeader(title = "bdclean"),
    
    #Sidebar
    dashboardSidebar(
        sidebarMenu(
            id = "sideBar",
            menuItem("Add Data", tabName = "add", icon = icon("plus-circle")),
            menuItem(
                "Configure Cleaning",
                tabName = "configure",
                icon = icon("wrench")
            ),
            menuItem("Flag Records", tabName = "flag", icon = icon("flag")),
            menuItem("Clean", tabName = "clean", icon = icon("trash")),
            menuItem("Document", tabName = "document", icon = icon("file")),
            menuItem("Citations", tabName = "citTab", icon = icon("bookmark"))
        )
    ),
    
    #Dashboard Tabs
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
        ),
        useShinyjs(),
        tabItems(
            tabItem("add",
                    fluidRow(column(
                        12,
                        h1("Add Occurrence Data"),
                        column(
                            3,
                            tabsetPanel(
                                type = "tabs",
                                tabPanel(
                                    "Option 01",
                                    div(class = "secondaryHeaders", h3("Option 01: From Online Database")),
                                    textInput(
                                        "scientificName",
                                        label = h3("Scientific Name:"),
                                        value = "Puma concolor"
                                    ),
                                    
                                    sliderInput(
                                        "recordSize",
                                        label = h3("Record Size:"),
                                        min = 0,
                                        max = 50000,
                                        value = 500
                                    ),
                                    checkboxGroupInput(
                                        "queryDB",
                                        label = h3("Online Database:"),
                                        choices = list(
                                            "GBIF" = 'gbif',
                                            "Vertnet" = 'vertnet',
                                            "Bison" = 3,
                                            "Inat" = 4,
                                            "eBird" = 5,
                                            "Ecoengine" = 6,
                                            "Vertnet" = 7
                                        ),
                                        selected = 'gbif'
                                    ),
                                    br(),
                                    div(class = "primaryButton", actionButton("queryDatabase", "Query Database", icon("download")))
                                    
                                    
                                ),
                                tabPanel(
                                    "Option 02",
                                    div(class = "secondaryHeaders", h3("Option 02: From Local disk")),
                                    div(class = "primaryButton", fileInput(
                                        "inputFile",
                                        label = h3("CSV file input"),
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")
                                    ))
                                ),
                                div(class = "progressStep", taskItem(
                                    value = 15, color = "green",
                                    "Step 1 of 6"
                                ))
                                
                            ),
                            actionButton("dataToConfigure", "Next: Configure Cleaning")
                            
                        ),
                        column(9,
                               tabsetPanel(
                                   type = "tabs",
                                   tabPanel("Map View",
                                            leafletOutput("mymap", height = "700")),
                                   tabPanel("Table View",
                                            DT::dataTableOutput("inputDataTable"))
                               ))
                        
                    ))),
            tabItem("configure",
                    fluidRow(column(
                        12,
                        h1("Configure Cleaning"),
                        column(
                            12,
                            tabsetPanel(
                                type = "tabs",
                                tabPanel(
                                    "Option 01",
                                    div(class = "secondaryHeaders", h3("Option 01: Questionnaire")),
                                    br(),
                                    
                                    # -------------------------------
                                    
                                    uiOutput("questionnaire")
                                    
                                    
                                    # -------------------------------
                                ),
                                tabPanel(
                                    "Option 02",
                                    div(class = "secondaryHeaders", h3("Option 02: Cleaning Templates")),
                                    p("Under Development")
                                ),
                                tabPanel(
                                    "Option 03",
                                    div(class = "secondaryHeaders", h3("Option 03: Customized Checks")),
                                    p("Yet to be developed")
                                ),
                                div(class = "progressStep", taskItem(
                                    value = 30, color = "orange",
                                    "Step 2 of 6"
                                ))
                            ),
                            actionButton("configureToFlag", "Next: Begin Flagging")
                
                        )
                        
                    ))),
            
            tabItem("flag",
                    fluidRow(column(
                        12,
                        column(
                            12,
                            h1("Flagged Data"),
                            br(),
                            
                            fluidRow(
                                selectInput(
                                    "select",
                                    label = "Cleaning Intensity",
                                    choices = list(
                                        "High" = 1,
                                        "Medium" = 2,
                                        "Low" = 3
                                    ),
                                    selected = 1
                                ),
                                actionButton("flagButton", label = "Flag Data")
                            ),
                            
                            br(),
                            
                            p("Input Data"),
                            fluidRow(
                                infoBox("Rows", 5000, icon = icon("flag"), width = 2),
                                infoBox(
                                    "Columns",
                                    92,
                                    icon = icon("flag"),
                                    width = 2,
                                    color = "purple"
                                ),
                                infoBox(
                                    "Unique Species",
                                    235,
                                    icon = icon("flag"),
                                    width = 2,
                                    color = "yellow"
                                )
                            ),
                            
                            p("Flagged Data"),
                            fluidRow(
                                infoBox(
                                    "Clean Data",
                                    "56%",
                                    icon = icon("flag") ,
                                    width = 2,
                                    color = "red"
                                ),
                                infoBox("Rows", 3268, icon = icon("flag"), width = 2),
                                infoBox(
                                    "Columns",
                                    96,
                                    icon = icon("flag"),
                                    width = 2,
                                    color = "purple"
                                ),
                                infoBox(
                                    "Unique Species",
                                    186,
                                    icon = icon("flag"),
                                    width = 2,
                                    color = "yellow"
                                )
                                
                            ),
                            
                            taskItem(value = 45, color = "red",
                                     "Step 3 of 6"),
                            actionButton("action", label = "Next: Continue with Cleaning"),
                            
                            actionButton("action", label = "Next: Continue with Just Flagging"),
                            
                            br(),
                            br(),
                            
                            
                            h4("Flagged Data:"),
                            DT::dataTableOutput("flaggedDataTable", width = 300)
                            
                            
                            
                        )
                    ))),
            tabItem("document",
                    fluidRow(column(
                        12,
                        column(
                            12,
                            h1("Cleaning Report"),
                            br(),
                            downloadButton("downloadData", "Download Report"),
                            br()
                        )
                    )))
            
        )
    )
))
