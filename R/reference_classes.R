#' The Question Reference Class
#'
#' @export BdQuestion
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
                if (length(.self$quality.checks) > 0) {
                    for (i in 1:length(.self$quality.checks)) {
                        check <- .self$quality.checks[[i]]
                        flaggedData <-
                            check(flaggedData, .self$users.answer)
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


#' The Question Container Reference Class
#'
#' @export BdQuestionContainer
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
                message(paste(
                    "New BdQuestionContainer instance created with",
                    length(.self$BdQuestions),
                    "questions."
                ))
            },
            
            printSelf = function() {
                for (question in .self$BdQuestions) {
                    question$printSelf()
                }
            }
        )
    )