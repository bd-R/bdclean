#' The Question Reference Class
#'
#' @export BdQuestion
#' @importFrom tools Rd_db
#' @importFrom methods new
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
            
            flagData = function(data, missing = FALSE) {
                flaggedData <- data
                
                if (length(.self$quality.checks) > 0) {
                    for (i in 1:length(.self$quality.checks)) {
                        checkName <- .self$quality.checks[i]
                        
                        if (grepl("DC_", checkName)) {
                            # bdchecks quality checks
                            checkTemp <-
                                bdchecks::performDataCheck(data = flaggedData,
                                                           DConly = c(checkName))
                            
                            
                            if (!is.null(checkTemp) &&
                                length(checkTemp@flags) > 0 &&
                                length(checkTemp@flags[[1]]@result) > 0) {
                                checkTemp <- checkTemp@flags[[1]]@result
                                
                                if (missing) {
                                    checkTemp[is.na(checkTemp)] <-
                                        FALSE # Treating mising values as fails
                                } else {
                                    checkTemp[is.na(checkTemp)] <-
                                        TRUE
                                }
                                
                                flaggedData[, paste("bdclean", checkName, sep = ".")] <-
                                    checkTemp
                            }
                            
                        } else {
                            # bdclean quality checks
                            flaggedData <-
                                get(checkName)(flaggedData, .self$users.answer)
                        }
                    }
                }
                
                return(flaggedData)
            },
            
            addToReport = function(flaggedData,
                                   clean = TRUE,
                                   CleaningThreshold = 5) {
                packageDocumentation <- tools::Rd_db("bdchecks")
                flaggedData <- as.data.frame(flaggedData)
                
                for (i in 1:length(.self$quality.checks)) {
                    nameOfQualityCheck <- .self$quality.checks[i]
                    
                    if (!(paste("bdclean", nameOfQualityCheck, sep = ".") %in% names(flaggedData))) { 
                        # both bdchecks and bdclean columns have bdcelan prefix
                        warning(
                            "Required column ",
                            paste("bdclean", nameOfQualityCheck, sep = "."),
                            " not found! Probably, quality check is missing from
                            environment and check was not performed."
                        )
                        next
                    }
                    
                    flag <-
                        flaggedData[,paste("bdclean", nameOfQualityCheck, sep = ".")]
                    
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
                    
                    
                    brokenDocumentation <-
                        unlist(strsplit(
                            paste(functionDocumentation[[1]], collapse = " "),
                            split = '\\',
                            fixed = TRUE
                        ))
                    
                    brokenDocumentation <- gsub("\\n", "",
                                                gsub(
                                                    "[{}]", "", brokenDocumentation
                                                )
                    )
                    
                    description <-
                        brokenDocumentation[grep("title", brokenDocumentation)]
                    description <-
                        gsub("title  Data check", "", description, fixed = T)
                    
                    samplePassData <-
                        brokenDocumentation[grep("samplePassData", brokenDocumentation)]
                    samplePassData <-
                        gsub("section   samplePassData", "", samplePassData, fixed = T)
                    
                    sampleFailData <-
                        brokenDocumentation[grep("sampleFailData", brokenDocumentation)]
                    sampleFailData <-
                        gsub("section   sampleFailData", "", sampleFailData, fixed = T)
                    
                    checkCategory <-
                        brokenDocumentation[grep("checkCategory", brokenDocumentation)]
                    checkCategory <-
                        gsub("section   checkCategory", "", checkCategory, fixed = T)
                    
                    targetDWCField <-
                        brokenDocumentation[grep("targetDWCField", brokenDocumentation)]
                    targetDWCField <-
                        gsub("section   targetDWCField", "", targetDWCField, fixed = T)
                    
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
            
            flagData = function(inputData, missing) {
                message("Initial records: ", paste(dim(inputData), collapse = "x"))
                flaggedData <- inputData
                for (question in .self$BdQuestions) {
                    if (length(question$quality.checks) > 0 &&
                        length(question$users.answer) > 0) {
                        if (question$question.type == "Router" &&
                            !(question$users.answer %in% question$router.condition)) {
                            # If its router and condition fails
                            next
                        } else if (question$question.type == "ChildRouter" &&
                                   !(question$users.answer %in% question$router.condition)) {
                            # If its ChildRouter and condition fails
                            next
                        }
                        
                        flaggedData <-
                            question$flagData(flaggedData, missing)
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
