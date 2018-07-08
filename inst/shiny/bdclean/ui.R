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
            menuItem("Artifacts & Documentation", tabName = "document", icon = icon("file")),
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
                                    div(class = "primaryButton", actionButton(
                                        "queryDatabase", "Query Database", icon("download")
                                    ))
                                    
                                    
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
                                    value = 15, color = "orange",
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
                                    value = 30, color = "green",
                                    "Step 2 of 6"
                                ))
                            ),
                            actionButton("configureToFlag", "Next: Flagging")
                            
                        )
                        
                    ))),
            
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
                                
                                taskItem(value = 45, color = "yellow",
                                         "Step 3 of 6"),
                                
                                fluidRow(actionButton("flagButton", label = "Flag Data"))
                            ),
                            
                            br(),
                            
                            uiOutput("flaggedContentUI"),
                            
                            uiOutput("cleanedResultsUI")
                        )
                    ))),
            tabItem("document",
                    fluidRow(column(
                        12,
                        column(
                            12,
                            h1("Cleaning Report"),
                            br(),
                            downloadButton("downloadDoc", "Download Report"),
                            br()
                        )
                    )))
            
        )
    )
))
