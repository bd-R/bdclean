# UI Function
DarwinizingUI <- function(id) {
  ns <- NS(id)
  
  tagList(column(
    12,
    id = ns("darwinControl"),
    br(),
    
    h1("Clean Headers"),
    p("Use darwinization to standardize headers"),
    actionButton(ns("darwinizeButton"), "Perform Header Cleaning"),
    
    helpText(
      "To manually edit or clean headers, use ",
      a("bdDwC", href = "https://cran.r-project.org/web/packages/bdDwC/index.html"),
      " package. Launch bdDwC shiny app with the command 'bdDwC::run_dwc()' in R console,  or "
    ),
    
    actionButton(ns("launch_bddwc"), "Launch bddwc Shiny App Now"),
    helpText("(Requires RStudio 1.2 and above.)"),
    br()
  ))
}

# Server Function
#' @importFrom
Darwinizing <-
  function(input,
           output,
           session,
           dat) {
    ns <- session$ns
    returnState <- data.frame()
    
    print("init")
    
    observeEvent(input$darwinizeButton, {
      data <- dat()
      returnState <<- data
      
      print(data)
      
      showNotification("Cleaning Headers", duration = 4)
      dictionaryPath <-
        system.file("txts/customDwCdictionary.txt", package = "bdclean")
      customDictionary <-
        data.table::fread(file = dictionaryPath)
      
      darwinizer = tryCatch({
        bdDwC::darwinize_names(as.data.frame(data), bdDwC:::data_darwin_cloud$data)
      }, error = function(e) {
        print(e)
        showNotification("Darwinizing Erred")
      })
      
      if (class(darwinizer) != "character") {
        fixed <-
          darwinizer[darwinizer$match_type == "Darwinized",]
        
        if (nrow(fixed) > 0) {
          tidyData <- bdDwC::rename_user_data(data, darwinizer)
          
          showNotification(paste(
            "Converted Columns:",
            paste(
              paste(fixed[, 1], collapse = ", "),
              paste(fixed[, 2], collapse = ", "),
              sep = " -> "
            )
          ),
          duration = 7)
          
          print("returning tidy")
          returnState <<- tidyData
        } else {
          showNotification("No headers required darwinization", duration = 4)
        }
      }
    })

    returnDataReact <- reactive({
      # Input actions that need to trigger new dataframe return
      # reactiveWidget
      print("react")
      
      input$darwinizeButton
      
      
      return(returnState)
    })
    
    print("return")
    
    return(returnDataReact)
  }