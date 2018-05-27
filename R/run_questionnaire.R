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
#'  responses <- runQuestionnaire()
#'  cleanedData <- cleanData(myData, responses)
#'}
#'
#'@export
runQuestionnaire <- function(customQuestionnaire = NA) {
    responses <- list()
    
    if (is.na(customQuestionnaire)) {
        message("Custom Questionnaire not given. Using package default Questionnaire...")
        responses <- createDefaultQuestionnaire()
        
    } else {
        if (class(customQuestionnaire) != "BdQuestionContainer") {
            message(
                "Provided Custom Questionnaire is not of class BdQuestionContainer. Using package default Questionnaire"
            )
            responses <- createDefaultQuestionnaire()
            
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
#'  responses <- runQuestionnaire()
#'  cleanedData <- cleanData(myData, responses)
#'  
#'  customQuestionnaire <- createDefaultQuestionnaire()
#'  customResponses <- runQuestionnaire(customQuestionnaire)
#'  cleanedData <- cleanData(myData, customResponses)
#'}
#'
#'@export
createDefaultQuestionnaire <- function() {
    BdQuestion <-
        setRefClass(
            "BdQuestion",
            fields = list(
                question = "character",
                possible.responses = "character",
                users.answer = "character",
                child.questions = "list",
                quality.checks = "list",
                question.type = "character",
                router.condition = "character"
            ),
            methods = list(
                initialize = function(question = character(),
                                      possible.responses = character(),
                                      quality.checks = list(),
                                      question.type = character(),
                                      router.condition = character()) {
                    .self$question <- question
                    .self$possible.responses <- possible.responses
                    .self$child.questions <- child.questions
                    .self$quality.checks <- quality.checks
                    .self$question.type <- question.type
                    .self$router.condition <- router.condition
                },
                
                printQuestion = function() {
                    cat(.self$question, "\n")
                    if (length(.self$possible.responses) > 0) {
                        for (i in 1:length(.self$possible.responses)) {
                            cat(" ", i, " ", .self$possible.responses[i], "\n")
                        }
                    }
                },
                
                getResponse = function() {
                    ans <- readline()
                    if (length(.self$possible.responses) > 0) {
                        .self$users.answer <-
                            .self$possible.responses[as.numeric(ans)]
                    } else {
                        .self$users.answer <- ans
                    }
                    
                },
                
                addChildQuestion = function(questions) {
                    .self$child.questions <- questions
                },
                
                addQualityChecks = function(newChecks) {
                    cat("Adding Quality Checks.")
                    .self$quality.checks <-
                        c(.self$quality.checks, newChecks)
                },
                
                cleanData = function(data) {
                    flaggedData <- data
                    if (length(.self$quality.checks) > 0 ){
                        for (i in 1:length(.self$quality.checks)) {
                            check <- .self$quality.checks[[i]]
                            flaggedData <- check(flaggedData, .self$users.answer)
                        }
                    }
                    
                    return(flaggedData)
                },
                
                addToReport = function() {
                    cat("Adding to Report.")
                },
                
                notify = function() {
                    cat("New Question object created.")
                },
                
                printSelf = function() {
                    print(.self$question)
                    for (i in 1:length(.self$possible.responses)) {
                        cat(" ", i, " ", .self$possible.responses[i], "\n")
                    }
                    cat(" User Response: ", .self$users.answer, "\n")
                    cat("\n")
                }
            )
        )
    
    BdQuestionContainer <-
        setRefClass(
            "BdQuestionContainer",
            fields = list(BdQuestions = "list"),
            methods = list(
                initialize = function(BdQuestions = NA) {
                    "Construct an instance of sampleInfoCollectionClass after validating the type."
                    
                    if (class(BdQuestions[[1]]) != "BdQuestion") {
                        stop("Incompatible input type. Provide a list of BdQuestion")
                    }
                    .self$BdQuestions <- BdQuestions
                    .self$notify()
                },
                
                notify = function() {
                    message(paste("New BdQuestionContainer instance created with",
                              length(.self$BdQuestions),
                              "questions.")
                    )
                },
                
                printSelf = function() {
                    for (question in .self$BdQuestions) {
                        question$printSelf()
                    }
                }
            )
        )
    
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
            possible.responses = c("Keep as it is", "Remove", "Try to fix"),
            question.type = "Atomic"
        )
    
    question3 <-
        BdQuestion(question = "What is the spatial resolution required for your data? (in meteres)",
                   question.type = "Atomic",
                   quality.checks = c(spatialResolution))
    
    question4 <-
        BdQuestion(
            question = "Do you care about dates of your observations?",
            possible.responses = c("Yes" , "No"),
            question.type = "Router",
            router.condition = c("Yes", "Y", 1)
        )
    
    question5 <-
        BdQuestion(question = "What is the earliest date of your observations in this data set? In format (YYYY-mm-dd)",
                   question.type = "Child",
                   quality.checks = c(earliestDate))
    
    question6 <-
        BdQuestion(
            question = "What temporal resolution are you interested in?",
            possible.responses = c("Year", "Month", "Day"),
            question.type = "Atomic",
            quality.checks = c(temporalResolution)
        )
    
    question4$addChildQuestion(c(question5))
    
    
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