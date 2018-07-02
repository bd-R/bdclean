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
            menuItem("Add Data", tabName = "add", icon = icon("database")),
            menuItem(
                "Configure Cleaning",
                tabName = "configure",
                icon = icon("sliders")
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
                                    actionButton("queryDatabase", "Query Database", icon("download"))
                                    
                                    
                                ),
                                tabPanel(
                                    "Option 02",
                                    div(class = "secondaryHeaders", h3("Option 02: From Local disk")),
                                    fileInput(
                                        "inputFile",
                                        label = h3("CSV file input"),
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")
                                    )
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
                                   tabPanel("Tabel View",
                                            DT::dataTableOutput("inputDataTable"))
                               ))
                        
                    ))),
            tabItem("configure",
                    fluidRow(column(
                        12,
                        column(
                            12,
                            h1("Configure Cleaning"),
                            br(),
                            h4("1) What is the lowest taxonomic level you require in your data?"),
                            selectInput(
                                "select",
                                label = "",
                                choices = list(
                                    "Sub Species" = 1,
                                    "Species" = 2,
                                    "Family" = 3,
                                    "Phylum" = 4,
                                    "Kingdom" = 5
                                ),
                                selected = 1
                            ),
                            br(),
                            
                            h4(
                                "2) What is the spatial resolution required for your data? (in meteres)"
                            ),
                            sliderInput(
                                "slider1",
                                label = "",
                                min = 0,
                                max = 50000,
                                value = 500
                            ),
                            br(),
                            
                            h4(
                                "3) What is the range of date of your observations in this data set? In format (YYYY-mm-dd)"
                            ),
                            dateRangeInput("dates",
                                           label = ""),
                            br(),
                            
                            h4("4) What temporal resolution are you interested in?"),
                            selectInput(
                                "select",
                                label = "",
                                choices = list(
                                    "Day" = 1,
                                    "Month" = 2,
                                    "Year" = 3
                                ),
                                selected = 1
                            ),
                            br(),
                            
                            h4("5) What cleaning procedure do you want?"),
                            selectInput(
                                "select",
                                label = "",
                                choices = list("Just Flagging" = 1,
                                               "Cleaning" = 2),
                                selected = 1
                            ),
                            
                            br(),
                            
                            h4("6) What cleaning intensity do you want?"),
                            selectInput(
                                "select",
                                label = "",
                                choices = list(
                                    "Max" = 1,
                                    "Medium" = 2,
                                    "Low" = 3
                                ),
                                selected = 1
                            ),
                            
                            br(),
                            br(),
                            taskItem(value = 30, color = "orange",
                                     "Step 2 of 6"),
                            actionButton("action", label = "Next: Flag Data")
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
                                actionButton("action", label = "Flag Data Again")
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
                            DT::dataTableOutput("tableTwo", width = 300)
                            
                            
                            
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
