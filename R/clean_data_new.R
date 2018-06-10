#' Data cleaning according to Questionnaire Responses.
#'
#' Use \code{run_questionnaire} to create Questionnaire Responses and pass it to this
#' function to process the data faster.
#'
#' Use \code{create_default_questionnaire} to create default Questionnaire object.
#' You can add your custom questions and then pass it to this
#' function to process the data.
#'
#'@param customQuestionnaire Custom User Created Questionnaire Responses if
#'already available to pypass asking questions each time.
#'
#'@return data frame with clean data
#'
#'@examples \dontrun{
#'library(rgbif)
#'occdat1 <- occ_data(
#'  country = "AU",     # Country code for australia
#'  classKey= 359,      # Class code for mammalia
#'  limit=5000,         # Get only 5000 records
#'  )
#'  myData<-occdat1$data
#'
#'  cleanedData <- clean_data_new(myData)
#'
#'  responses <- run_questionnaire()
#'  cleanedData <- clean_data_new(myData, responses)
#'
#'  customQuestionnaire <- create_default_questionnaire()
#'  customResponses <- run_questionnaire(customQuestionnaire)
#'  cleanedData <- clean_data_new(myData, customResponses)
#'  }
#'
#'@export
clean_data_new <- function(data, customQuestionnaire = NULL) {
    responses <- list()
    cleanedData <- data
    
    if (is.null(customQuestionnaire)) {
        responses <- run_questionnaire()
    } else {
        responses <- customQuestionnaire
    }
    
    for (question in responses$BdQuestions) {
        if (question$question.type != "Router" &&
            length(question$users.answer) > 0) {
            cleanedData <- question$cleanData(cleanedData)
        }
    }
    
    store_report_data(data, cleanedData, responses)
    
    return(cleanedData)
}

store_report_data <- function(inputData, cleanedData, responses) {
    inputSize <- dim(inputData)
    outputSize <- dim(cleanedData)
    
    inputUniqueSpecies <- length(unique(inputData$scientificName))
    outputUniqueSpecies <-
        length(unique(cleanedData$scientificName))
    
    earliestInputDate <-
        min(as.POSIXct(unique(inputData$eventDate), "%Y-%m-%dT%H:%M:%S"))
    latestInputDate <-
        max(as.POSIXct(unique(inputData$eventDate), "%Y-%m-%dT%H:%M:%S"))
    earliestOutputDate <-
        min(as.POSIXct(unique(cleanedData$eventDate), "%Y-%m-%dT%H:%M:%S"))
    latestOutputDate <-
        max(as.POSIXct(unique(cleanedData$eventDate), "%Y-%m-%dT%H:%M:%S"))
    
    InputData <-
        c(
            inputSize[1],
            inputSize[2],
            inputUniqueSpecies,
            paste(earliestInputDate, "-", latestInputDate)
        )
    CleanedData <-
        c(
            outputSize[1],
            outputSize[2],
            outputUniqueSpecies,
            paste(earliestOutputDate, "-", latestOutputDate)
        )
    
    data.summary <- data.frame(InputData, CleanedData) # One
    row.names(data.summary) <-
        c("Rows",
          "Columns",
          "Number of unique scientific names",
          "Date Range")
    
    spatialChecks <- 0
    temporalChecks <- 0
    taxonChecks <- 0
    otherChecks <- 0
    
    checks.records <- list() # Three
    index <- 1
    
    for (question in responses$BdQuestions) {
        if (question$question.type != "Router" &&
            length(question$users.answer) > 0) {
            checks.records[question$question] <-
                list(answer = question$users.answer,
                     checks = question$cleaning.details)
            
            message(question$cleaning.details)
            
            
            if (question$cleaning.details$checkCategory == "spatial") {
                spatialChecks = spatialChecks + 1
            } else if (question$cleaning.details$checkCategory == "temporal") {
                temporalChecks = spatialChecks + 1
            } else if (question$cleaning.details$checkCategory == "taxon") {
                taxonChecks = spatialChecks + 1
            } else {
                otherChecks = spatialChecks + 1
            }
        }
    }
    
    Checks <-
        c(
            "Taxonomical quality Checks",
            "Spatial quality Checks",
            "Temporal quality Checks",
            "Total quality Checks"
        )
    
    Count <-
        c(
            taxonChecks,
            spatialChecks,
            temporalChecks,
            (taxonChecks + spatialChecks + temporalChecks + otherChecks)
        )
    
    check.summary <- data.frame(Checks, Count) # Two
    
    print(kable(data.summary, format = "markdown"))
    print(kable(check.summary, format = "markdown"))
    print(kable(checks.records, format = "markdown"))
}
