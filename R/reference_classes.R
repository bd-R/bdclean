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
            router.condition = "character",
            cleaning.details = "list",
            question.type = "character",
            question.id = "character",
            ui.type = "character"
        ),
        methods = list(
            initialize = function(question = character(),
                                  possible.responses = character(),
                                  quality.checks = character(),
                                  question.type = character(),
                                  router.condition = character(),
                                  question.id = character(),
                                  ui.type = character()) {
                .self$question <- question
                .self$possible.responses <- possible.responses
                .self$quality.checks <- quality.checks
                .self$question.type <- question.type
                .self$router.condition <- router.condition
                .self$question.id <- question.id
                .self$ui.type <- ui.type
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
            
            setResponse = function(response) {
                if (class(response) == "logical") {
                    .self$users.answer <- ifelse(response, "yes", "no")
                } else {
                    .self$users.answer <- as.character(response)
                }
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
            
            flagData = function(data) {
                flaggedData <- data
                
                if (length(.self$quality.checks) > 0) {
                    for (i in 1:length(.self$quality.checks)) {
                        checkName <- .self$quality.checks[i]
                        # flaggedData <-
                        #     get(checkName)(flaggedData, .self$users.answer)
                        
                        checkTemp <- bdchecks::performDataCheck(data = flaggedData, DConly = c(checkName))
                        
                        if (length(checkTemp@flags) > 0) {
                            flaggedData[, paste("bdclean", checkName, sep = ".")] <-
                                checkTemp@flags[[1]]@result
                        }
                        
                    }
                }
                
                return(flaggedData)
            },
            
            addToReport = function(flaggedData,
                                   clean = TRUE,
                                   CleaningThreshold = 5) {
                packageDocumentation <- tools::Rd_db("bdclean")
                
                
                for (i in 1:length(.self$quality.checks)) {
                    nameOfQualityCheck <- .self$quality.checks[i]
                    
                    if (!(paste("bdclean", nameOfQualityCheck, sep = ".") %in% names(flaggedData))) {
                        warning(
                            "Required column ",
                            paste("bdclean", nameOfQualityCheck, sep = "."),
                            " not found! Probably, quality check is missing from
                            environment and check was not performed."
                        )
                        return()
                    }
                    
                    flag <-
                        flaggedData[paste("bdclean", nameOfQualityCheck, sep = ".")]
                    
                    # Uncomment if using threshold
                    # countOfFlaggedData <-
                    #     sum(flag < CleaningThreshold, na.rm = TRUE)
                    
                    countOfFlaggedData <-
                        sum(flag != TRUE, na.rm = T)
                    
                    # ------ Parsing MetaData for check from .Rd file
                    functionDocumentation <-
                        packageDocumentation[grep(nameOfQualityCheck, names(packageDocumentation))]
                    
                    
                    if (length(functionDocumentation) == 0) {
                        warning(
                            "Could not find function documentation for ",
                            nameOfQualityCheck,
                            ". Skipping report."
                        )
                        next
                    }
                    
                    description <-
                        lapply(functionDocumentation,
                               tools:::.Rd_get_metadata,
                               "description")[[1]]
                    
                    sectionsString <-
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
                        gsub(" ", "", sectionsVector[match('checkCategory', sectionsVector) + 1])
                    targetDWCField <-
                        sectionsVector[match('targetDWCField', sectionsVector) + 1]
                    # ------ End of Parsing MetaData for check from .Rd file
                    
                    temp <- list()
                    
                    temp$description <-
                        paste(description, collapse = " ")
                    temp$samplePassData <- samplePassData
                    temp$sampleFailData <- sampleFailData
                    temp$checkCategory <- checkCategory
                    temp$targetDWCField <- targetDWCField
                    temp$affectedData <- countOfFlaggedData
                    
                    .self$cleaning.details[nameOfQualityCheck] <-
                        list(temp)
                }
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
            
            flagData = function(inputData) {
                message("Initial records: ", paste(dim(inputData), collapse = "x"))
                flaggedData <- inputData
                for (question in responses$BdQuestions) {
                    if (length(question$quality.checks) > 0 &&
                        length(question$users.answer) > 0) {
                        flaggedData <- question$flagData(flaggedData)
                    }
                    
                    # temp <- try({
                    #     question$flagData(tempData)
                    # })
                    #
                    # if (!is(temp, "try-error")) {
                    #     tempData <- temp
                    # }
                }
                
                
                return(flaggedData)
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
