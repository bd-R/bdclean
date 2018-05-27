#' Data cleaning according to Questionnaire Responses.
#'
#' Use \code{runQuestionnaire} to create Questionnaire Responses and pass it to this
#' function to process the data faster.
#'
#' Use \code{createDefaultQuestionnaire} to create default Questionnaire object.
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
#'  cleanedData <- cleanData(myData)
#'
#'  responses <- runQuestionnaire()
#'  cleanedData <- cleanData(myData, responses)
#'
#'  customQuestionnaire <- createDefaultQuestionnaire()
#'  customResponses <- runQuestionnaire(customQuestionnaire)
#'  cleanedData <- cleanData(myData, customResponses)
#'  }
#'
#'@export
cleanData <- function(data, customQuestionnaire = NA) {
    responses <- list()
    cleanedData <- data
    
    if (is.na(customQuestionnaire)) {
        responses <- runQuestionnaire()
    } else {
        responses <- customQuestionnaire
    }
    
    for (question in responses$BdQuestions) {
        if (question$question.type != "Router" && length(question$users.answer) > 0) {
            cleanedData <- question$cleanData(cleanedData)
        }
    }
    return(cleanedData)
}
