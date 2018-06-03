#' Create the package default Questionnaire.
#'
#'
#'@return BdQuestionContainer object with default Questions
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
#'
#'  customQuestionnaire <- create_default_questionnaire()
#'  customResponses <- run_questionnaire(customQuestionnaire)
#'  cleanedData <- clean_data_new(myData, customResponses)
#'}
#'
#'@export
create_default_questionnaire <- function() {
    question1 <-
        BdQuestion(
            question = "What is the lowest taxonomic level you require in your data?",
            possible.responses = c(
                "CLASS",
                "ORDER",
                "FAMILY",
                "GENUS",
                "SPECIES",
                "SUBSPECIES"
            ),
            question.type = "Atomic",
            quality.checks = c(taxoLevel)
        )
    
    question2 <-
        BdQuestion(
            question = "What you want to do with data with mismatched names?",
            possible.responses = c("Keep as it is", "Remove"),
            question.type = "Atomic"
        )
    
    question3 <-
        BdQuestion(
            question = "What is the spatial resolution required for your data? (in meteres)",
            question.type = "Atomic",
            quality.checks = c(spatialResolution),
            validationFunction = function(answer) {
                answer <- suppressWarnings(as.numeric(answer))
                check <-
                    (!is.na(answer) && answer > 0 && answer < 100000)
                if (!check) {
                    message(
                        "Spatial resolution should be a number between 0 to 100 KM. Please give a correct value."
                    )
                }
                return(check)
                }
        )
    
    question4 <-
        BdQuestion(
            question = "Do you care about dates of your observations?",
            possible.responses = c("Yes" , "No"),
            question.type = "Router",
            router.condition = c("Yes", "Y", 1)
        )
    
    question5 <-
        BdQuestion(
            question = "What is the earliest date of your observations in this data set? In format (YYYY-mm-dd)",
            question.type = "Child",
            quality.checks = c(earliestDate),
            validationFunction = function(answer) {
                d <- try(as.Date(answer))
                if( class( d ) == "try-error" || is.na( d ) ) {
                    message("Invalid Date! Please follow the date format (YYYY-mm-dd)")
                    return(FALSE)
                }
                return(TRUE)
            }
        )
    
    question6 <-
        BdQuestion(
            question = "What temporal resolution are you interested in?",
            possible.responses = c("Year", "Month", "Day"),
            question.type = "Atomic",
            quality.checks = c(temporalResolution)
        )
    
    question4$addChildQuestion(c(question5, question6))
    
    
    allQuestions <-
        BdQuestionContainer(c(
            question1,
            question2,
            question3,
            question4,
            question5,
            question6
        ))
    
    return(allQuestions)
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
run_questionnaire <- function(customQuestionnaire = NA) {
    responses <- list()
    
    if (is.na(customQuestionnaire)) {
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