#' Data cleaning according to Questionnaire Responses.
#'
#' Use \code{run_questionnaire} to create Questionnaire Responses and pass it to this
#' function to process the data faster.
#'
#' Use \code{create_default_questionnaire} to create default Questionnaire object.
#' You can add your custom questions and then pass it to this
#' function to process the data.
#'
#'@param data Biodiversity data in a data frame
#'@param customQuestionnaire Custom User Created Questionnaire Responses if
#'already available to pypass asking questions each time.
#'@param verbose Verbose output if TRUE else brief output if FALSE
#'@param report Whether to print report of cleaning done.
#'@param format Formats of the cleaning report required. Options are: Markdown, HTML or / and PDF
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
#'  cleanedData <- clean_data(myData, responses)
#'
#'  customQuestionnaire <- create_default_questionnaire()
#'  customResponses <- run_questionnaire(customQuestionnaire)
#'  cleanedData <- clean_data_new(myData, customResponses)
#'  }
#'
#'@export
clean_data <-
    function(data,
             customQuestionnaire,
             verbose = FALSE,
             report = TRUE,
             format = c("html_document", "pdf_document")) {
        responses <- list()
        cleanedData <- data
        
        if (is.null(customQuestionnaire)) {
            responses <- run_questionnaire()
        } else {
            responses <- customQuestionnaire
        }
        
        if (verbose) {
            cat("\n Initial records ...", dim(cleanedData)[1], "\n")
        }
        
        for (question in responses$BdQuestions) {
            if (question$question.type != "Router" &&
                length(question$users.answer) > 0) {
                if (question$question.type == "Decision-Child")
                cleanedData <- question$cleanData(cleanedData, skipReport=TRUE)
            }
            if (verbose) {
                cat("\n Records remaining...", dim(cleanedData)[1], "\n")
            }
        }
        
        create_report_data(data, cleanedData, responses, verbose, format)
        
        return(cleanedData)
    }


#' Execute the Questionnaire and save user responses.
#'
#'
#'@param customQuestionnaire Custom User Created Questionnaire if already available.
#'
#'@return list with BdQuestionObjects containing user answers
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
#'  responses <- run_questionnaire()
#'  cleanedData <- clean_data_new(myData, responses)
#'}
#'
#'@export
run_questionnaire <- function(customQuestionnaire = NULL) {
    responses <- list()
    
    if (is.null(customQuestionnaire)) {
        message("Custom Questionnaire not given. Using package default Questionnaire...")
        responses <- create_default_questionnaire()
        
    } else {
        if (class(customQuestionnaire) != "BdQuestionContainer") {
            message(
                "Provided Custom Questionnaire is not of class BdQuestionContainer. Using package default Questionnaire"
            )
            responses <- create_default_questionnaire()
            
        } else {
            message("Custom Questionnaire detected.")
            responses <- customQuestionnaire
        }
    }
    
    message("Please answer the following questions to initiate cleaning process.")
    
    for (question in responses$BdQuestions) {
        if (question$question.type != "Child") {
            getUserResponse(question)
        }
    }
    message("Thank you! Cleaning can be started now based on your responses.")
    return(responses)
}


getUserResponse <- function(bdQuestion) {
    if (bdQuestion$question.type == "Atomic") {
        bdQuestion$printQuestion()
        bdQuestion$getResponse()
    } else {
        bdQuestion$printQuestion()
        bdQuestion$getResponse()
        if (bdQuestion$users.answer %in% bdQuestion$router.condition) {
            for (question in bdQuestion$child.questions) {
                getUserResponse(question)
            }
        }
    }
}
