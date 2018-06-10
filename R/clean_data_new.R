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
