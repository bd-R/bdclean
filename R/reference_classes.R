#' The Question Reference Class
#'
#' @export BdQuestion
#' @importFrom tools Rd_db
BdQuestion <-
    setRefClass(
        "BdQuestion",
        fields = list(
            question = "character",
            possible.responses = "character",
            users.answer = "character",
            validation.function = "function",
            child.questions = "list",
            quality.checks = "character",
            question.type = "character",
            router.condition = "character",
            cleaning.details = "list"
        ),
        methods = list(
            initialize = function(question = character(),
                                  possible.responses = character(),
                                  quality.checks = character(),
                                  question.type = character(),
                                  router.condition = character()) {
                .self$question <- question
                .self$possible.responses <- possible.responses
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
            
            addValidationFunction = function(valFunction) {
                .self$validation.function <- valFunction
            },
            
            getResponse = function() {
                ans <- readline()
                length <- length(.self$possible.responses)
                
                if (length > 0) {
                    # Means it was a menu question, and not an open answer
                    ans <- suppressWarnings(as.numeric(ans))
                    
                    if (!is.na(ans) &&
                        ans > 0 &&
                        ans <= length) {
                        # Validating user renponse is a menu number.
                        .self$users.answer <-
                            .self$possible.responses[as.numeric(ans)]
                    } else {
                        message("Please choose number from menu...")
                        .self$getResponse()
                    }
                    
                } else {
                    # Means answer is open ended
                    
                    if (is.null(.self$validation.function)) {
                        # If a validation function is not given
                        .self$users.answer <- ans
                    } else {
                        val = .self$validation.function(ans)
                        
                        if (val) {
                            # If the validation function passes (returns true)
                            .self$users.answer <- ans
                        } else {
                            .self$getResponse()
                        }
                    }
                }
            },
            
            addChildQuestion = function(questions) {
                .self$child.questions <- questions
            },
            
            addQualityChecks = function(newChecks) {
                cat("Adding Quality Checks.")
                .self$quality.checks <- newChecks
            },
            
            cleanData = function(data) {
                flaggedData <- data
                packageDocumentation <- tools::Rd_db("bdclean")
                
                if (length(.self$quality.checks) > 0) {
                    for (i in 1:length(.self$quality.checks)) {
                        initialRows <- nrow(flaggedData)
                        checkName <- .self$quality.checks[i]
                        flaggedData <-
                            get(checkName)(flaggedData, .self$users.answer)
                        
                        .self$addToReport(checkName,
                                          initialRows - nrow(flaggedData),
                                          packageDocumentation)
                    }
                }
                
                return(flaggedData)
            },
            
            addToReport = function(nameOfQualityCheck,
                                   countOfAffectedData,
                                   packageDocumentation) {
                functionDocumentation <-
                    packageDocumentation[grep(nameOfQualityCheck, names(packageDocumentation))]
                description <-
                    lapply(functionDocumentation,
                           tools:::.Rd_get_metadata,
                           "description")[[1]]
                
                sections <-
                    as.character(lapply(
                        functionDocumentation,
                        tools:::.Rd_get_metadata,
                        "section"
                    )[[1]])
                
                sectionsVector <-
                    gsub(", ,", "", gsub("\\\\n", "", gsub(
                        "[()\"]", "", substr(sectionsString, 5, nchar(sectionsString))
                    )))
                
                samplePassData <-
                    sectionsVector[match('samplePassData', sectionsVector) + 1]
                sampleFailData <-
                    sectionsVector[match('sampleFailData', sectionsVector) + 1]
                checkCategory <-
                    sectionsVector[match('checkCategory', sectionsVector) + 1]
                targetDWCField <-
                    sectionsVector[match('targetDWCField', sectionsVector) + 1]
                
                
                temp <- list()
                
                temp$description <-
                    paste(description, collapse = " ")
                temp$samplePassData <- samplePassData
                temp$sampleFailData <- sampleFailData
                temp$checkCategory <- checkCategory
                temp$targetDWCField <- targetDWCField
                temp$affectedData <- countOfAffectedData
                
                .self$cleaning.details[nameOfQualityCheck] <-
                    list(temp)
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
                "Construct an instance of BdQuestionContainer after validating the type."
                
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
