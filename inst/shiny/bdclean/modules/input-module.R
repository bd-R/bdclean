# Input Module UI function
bdFileInput <- function(id, label = "Add Occurrence Data") {
    # Create a namespace function using the provided id
    ns <- NS(id)
    
    tagList(
        column(
            12,
            h1("Add Occurrence Data"),
            column(
                3,
                class = "upload_side",
                # ------------- DB Module -------------------
                tabsetPanel(
                    type = "tabs",
                    tabPanel(
                        "Download Data",
                        div(class = "secondaryHeaders", h3("Option 01: From Online Database")),
                        textInput(
                            ns("scientificName"),
                            label = h3("Scientific Name:"),
                            value = "Puma concolor"
                        ),
                        
                        numericInput(
                            ns("recordSize"),
                            label = h3("Record Size:"),
                            value = 500
                        ),
                        
                        selectInput(
                            ns("hasCoords"),
                            label = h3("Records Filter:"),
                            choices = list(
                                "With Coordinates" = "1",
                                "Without Coordinates" = "2",
                                "No Filter" = "3"
                            ),
                            selected = 3
                        ),
                        
                        radioButtons(
                            ns("queryDB"),
                            label = h3("Online Database:"),
                            choices = list(
                                "GBIF (Global Biodiversity Information Facility)" = "gbif",
                                "iDigBio (Integrated Digitized Biocollections)" = "idigbio",
                                "EcoEngine (Berkeley Ecoinformatics Engine)" = "ecoengine",
                                "Vertnet (Vertebrate Network)" = "vertnet",
                                "BISON (Biodiversity Information Serving Our Nation)" = "bison",
                                "iNaturalist" = "inat",
                                "ALA (Atlas of Living Australia)" = "ala"
                                # "OBIS (Ocean Biogeographic Information System)" = "obis",
                                # "AntWeb" = "antweb"
                            ),
                            selected = "gbif"
                        ),
                        
                  
                        div(
                            id = ns("queryDatabaseDiv"),
                            class = "activeButton",
                            actionButton(ns("queryDatabase"), "Query Database", icon("download"))
                        )
                    ),
                    
                    # ------------- End of DB Module -------------------
                    
                    # ------------- Local Disk Module -------------------
                    tabPanel(
                        "Upload Data",
                        div(class = "secondaryHeaders", h3("Option 02: From Local Disk")),
                        div(
                            id = ns("inputFileDiv"),
                            class = "activeButton",
                            fileInput(
                                ns("inputFile"),
                                label = h3("CSV / DWCA ZIP file input"),
                                accept = c(
                                    "text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv",
                                    ".zip",
                                    "application/zip"
                                )
                            )
                        )
                    )
                    
                    # ------------- End of Local Disk Module -------------------
                    
                    
                ),
                
                tagList(
                    
                    br(),

                    checkboxInput(ns("darwinizerControl"),
                                  label = "Perform Header Cleaning",
                                  value = TRUE),
                    br(),
               
                    
                    helpText(
                        "To manually edit or clean headers, use ",
                        a("bdDwC", href = "https://cran.r-project.org/web/packages/bdDwC/index.html"),
                        " package. Launch bdDwC shiny app with the command 'bdDwC::run_dwc()' in R console,  or "
                    ),
                    br(),
                    br(),
                    actionButton(ns("launch_bddwc"), "Launch bddwc Shiny App Now"),
                    helpText(
                        "(Requires RStudio 1.2 and above.)"
                    ),
                    br(),
                    br()
                )
                
                
            ),
            
            # ------------- Map / Table Module -------------------
            column(9,
                   class = "upload_main",
                   tabsetPanel(
                       type = "tabs",
                       tabPanel(
                           "Map View",
                           leafletOutput(ns("mymap"), height = "700"),
                           absolutePanel(
                               top = 60,
                               right = 20,
                               selectInput(
                                   ns("mapTexture"),
                                   "Map Texture",
                                   choices = list(
                                       "OpenStreetMap.Mapnik" = "OpenStreetMap.Mapnik",
                                       "OpenStreetMap.BlackAndWhite" = "OpenStreetMap.BlackAndWhite",
                                       "Stamen.Toner" = "Stamen.Toner",
                                       "CartoDB.Positron" = "CartoDB.Positron",
                                       "Esri.NatGeoWorldMap" = "Esri.NatGeoWorldMap",
                                       "Stamen.Watercolor" = "Stamen.Watercolor",
                                       "Stamen.Terrain" = "Stamen.Terrain",
                                       "Esri.WorldImagery" = "Esri.WorldImagery",
                                       "Esri.WorldTerrain" = "Esri.WorldTerrain"
                                   ),
                                   selected = "CartoDB.Positron"
                               ),
                               selectInput(
                                   ns("mapColor"),
                                   "Points Color",
                                   choices = list(
                                       "Red" = 'red',
                                       "Green" = "green",
                                       "Blue" = "blue",
                                       "Black" = "black"
                                   )
                               )
                           )
                       ),
                       tabPanel("Table View",
                                DT::dataTableOutput(ns("inputDataTable")))
                   ))
            
            # ------------- End of Map/Table Module -------------------
        )
    )
}


# Input Module server function
bdFile <- function(input, output, session) {
    ns <- session$ns
    returnData <- data.frame()
    map <- leafletProxy(ns("mymap"))
    
    # ----------------
    
    observeEvent(input$launch_bddwc, {
        path_app <- system.file("scripts", 'bddwc.R', package = "bdclean")
        rstudioapi::jobRunScript(path = path_app)
    })
    
    observeEvent(input$queryDatabase, {
        withProgress(message = paste("Querying", input$queryDB, "..."), {
            if (input$queryDB == "gbif") {
                data <-
                    rgbif::occ_search(
                        scientificName = input$scientificName,
                        limit = input$recordSize,
                        hasCoordinate = switch(
                            input$hasCoords,
                            "1" = TRUE,
                            "2" = FALSE,
                            "3" = NULL
                        )
                    )
                returnData <<- data$data
                
            } else {
                warnings <- capture.output(
                    data <-
                        spocc::occ(
                            query = input$scientificName,
                            from = input$queryDB,
                            limit = input$recordSize,
                            has_coords = switch(
                                input$hasCoords,
                                "1" = TRUE,
                                "2" = FALSE,
                                "3" = NULL
                            )
                        ),
                    type = "message"
                )
                
                if (length(warnings) > 0) {
                    showNotification(paste(warnings, collapse = " "),
                                     duration = 6)
                }
                
                tempData <- data[[input$queryDB]]$data[[1]]
                returnData <<- tempData
            }
        })
        
        dataLoadedTask(returnData)
    })
    
    observeEvent(input$inputFile, {
        withProgress(message = paste("Reading", input$inputFile$name, "..."), {
            
            if (is.null(input$inputFile))
                return("No data to view")
            
            if (grepl("zip", tolower(input$inputFile$type))) {
                message("Reading DWCA ZIP...")
                finchRead <-
                    finch::dwca_read(input$inputFile$datapath, read = T)
                returnData <<- finchRead$data[[1]]
                
            } else {
                returnData <<-
                    data.table::fread(input$inputFile$datapath)
            }
        })
        dataLoadedTask(returnData)
        
        
    })
    
    
    observeEvent(input$mapTexture, {
        if (length(returnData) == 0) {
            return(NULL)
        }
        leafletProxy(ns("mymap"), data = returnData) %>%
            clearShapes() %>%
            addCircles(~ decimalLongitude, ~ decimalLatitude, color = input$mapColor)
    })
    
    observeEvent(input$mapColor, {
        if (length(returnData) == 0) {
            return(NULL)
        }
        leafletProxy(ns("mymap"), data = returnData) %>%
            clearShapes() %>%
            addCircles(~ decimalLongitude, ~ decimalLatitude, color = input$mapColor)
    })
    
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(input$mapTexture) %>%
            setView(0, 0, zoom = 2)
    })
    
    dataLoadedTask <- function(data) {
        if (length(data) == 0) {
            showNotification("Empty data returned! Try different setting.",
                             duration = 2)
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
                bdDwC::darwinize_names(as.data.frame(returnData), as.data.frame(customDictionary))
            
            fixed <-
                darwinizer[darwinizer$matchType == "Darwinized",]
            
            if (nrow(fixed) > 0) {
                tidyData <- bdDwC::renameUserData(returnData, darwinizer)

                returnData <<- tidyData
                
                showNotification(paste(
                    "Converted Columns:",
                    paste(
                        paste(fixed[, 1], collapse = ", "),
                        paste(fixed[, 2], collapse = ", "),
                        sep = " -> "
                    )
                ),
                duration = 7)
            }
        }
        
        if ("decimalLatitude" %in% colnames(returnData)) {
             returnData$decimalLatitude <<-
                 as.numeric(returnData$decimalLatitude)
            returnData$decimalLongitude <<-
                as.numeric(returnData$decimalLongitude)
        }
        
        # ------------ End of Darwinizing Data -------------
        
        try(leafletProxy(ns("mymap"), data = returnData) %>%
                clearShapes() %>%
                addCircles(~ decimalLongitude, ~ decimalLatitude, color = input$mapColor))
        
        output$inputDataTable <- DT::renderDataTable(DT::datatable({
            summarizeDataframe(returnData)
        }, options = list(scrollX = TRUE)))
        
        
        shinyjs::runjs(code = paste('$("#', ns("queryDatabaseDiv"), '").addClass("readyButton");', sep = ""))
        shinyjs::runjs(code = paste('$("#', ns("queryDatabaseDiv"), '").removeClass("activeButton");', sep = ""))
        shinyjs::runjs(code = paste('$("#', ns("inputFileDiv"), '").addClass("readyButton");', sep = ""))
        shinyjs::runjs(code = paste('$("#', ns("inputFileDiv"), '").removeClass("activeButton");', sep = ""))
        shinyjs::runjs(code = paste('$("#', "dataToConfigureDiv", '").addClass("completedButton");', sep = ""))
        shinyjs::runjs(code = paste('$("#', "dataToConfigureDiv", '").removeClass("activeButton");', sep = ""))
        
        
        showNotification("Read Data Successfully", duration = 2)
    
        
        # --------- Setting flag tab statistic boxes -------
        # TODO
        
    }
    
    returnDataReact <- reactive({
        # Input actions that need to trigger new dataframe return 
        input$inputFile
        input$queryDatabase
        
        returnData
    })
    
    
    return(returnDataReact)
}