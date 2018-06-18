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
            question = "Do you worry about taxonomical aspect of the data?",
            possible.responses = c("Yes" , "No"),
            question.type = "Router",
            router.condition = c("Yes", "Y", 1)
        )
    
    question2 <-
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
            question.type = "Child",
            quality.checks = c("taxoLevel")
        )
    
    question1$addChildQuestion(c(question2))
    
    question3 <-
        BdQuestion(
            question = "Do you worry about spatial aspect of the data?",
            possible.responses = c("Yes" , "No"),
            question.type = "Router",
            router.condition = c("Yes", "Y", 1)
        )
    
    question4 <-
        BdQuestion(
            question = "What is the spatial resolution required for your data? (in meteres)",
            question.type = "Child",
            quality.checks = c("spatialResolution")
        )
    
    question4$addValidationFunction(function(answer) {
        answer <- suppressWarnings(as.numeric(answer))
        check <-
            (!is.na(answer) &&
                 answer > 0 && answer < 100000)
        if (!check) {
            message(
                "Spatial resolution should be a number between 0 to 100 KM. Please give a correct value."
            )
        }
        return(check)
    })
    
    question3$addChildQuestion(c(question4))
    
    question5 <-
        BdQuestion(
            question = "Do you worry about temporal aspect of your data?",
            possible.responses = c("Yes" , "No"),
            question.type = "Router",
            router.condition = c("Yes", "Y", 1)
        )
    
    question6 <-
        BdQuestion(
            question = "What is the earliest date of your observations in this data set? In format (YYYY-mm-dd)",
            question.type = "Child",
            quality.checks = c("earliestDate")
        )
    
    question6$addValidationFunction(function(answer) {
        d <- try(as.Date(answer))
        if (class(d) == "try-error" || is.na(d)) {
            message("Invalid Date! Please follow the date format (YYYY-mm-dd)")
            return(FALSE)
        }
        return(TRUE)
    })
    
    question7 <-
        BdQuestion(
            question = "What temporal resolution are you interested in?",
            possible.responses = c("Year", "Month", "Day"),
            question.type = "Child",
            quality.checks = c("temporalResolution")
        )
    
    
    question5$addChildQuestion(c(question6, question7))
    
    question8 <-
        BdQuestion(
            question = "What cleaning procedure do you want?",
            possible.responses = c("Just flagging", "Removing"),
            question.type = "Atomic-Router",
            router.condition = c("Removing", 2)
        )
    
    question9 <-
        BdQuestion(
            question = "What cleaning intensity do you require?",
            possible.responses = c("High", "Moderate", "Low"),
            question.type = "Child",
            quality.checks = c("cleaning_function")
        )
    
    question8$addChildQuestion(c(question9))
    
    allQuestions <-
        BdQuestionContainer(
            c(
                question1,
                question2,
                question3,
                question4,
                question5,
                question6,
                question7,
                question8,
                question9
            )
        )
    
    return(allQuestions)
}