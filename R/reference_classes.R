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
            print_question = function() {
                cat(.self$question, "\n")
                if (length(.self$possible.responses) > 0) {
                    for (i in 1:length(.self$possible.responses)) {
                        cat(" ", i, " ", .self$possible.responses[i], "\n")
                    }
                }
            },
            add_validation_function = function(val_function) {
                .self$validation.function <- val_function
            },
            set_response = function(response) {
                if (class(response) == "logical") {
                    .self$users.answer <- ifelse(response, "yes", "no")
                } else {
                    .self$users.answer <- as.character(response)
                }
            },
            
           
            
            get_response = function() {
                ans <- readline()
                length <- length(.self$possible.responses)
                if (length > 0) {
                    # Means it was a menu question, and not an open answer
                    ans <- suppressWarnings(as.numeric(ans))
                    if (!is.na(ans) && ans > 0 && ans <= length) {
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
                        val <- .self$validation.function(ans)
                        if (val) {
                            # If the validation function passes (returns true)
                            .self$users.answer <- ans
                        } else {
                            .self$getResponse()
                        }
                    }
                }
            },
            add_child_question = function(questions) {
                .self$child.questions <- questions
            },
            add_quality_checks = function(new_checks) {
                .self$quality.checks <- new_checks
            },
            flag_data = function(data, missing = FALSE) {
                flagged_data <- data
                
                if (length(.self$quality.checks) > 0) {
                    for (i in 1:length(.self$quality.checks)) {
                        check_name <- .self$quality.checks[i]
                        if (grepl("dc_", check_name)) {
                            # bdchecks quality checks
                            
                            check_name <- gsub("dc_", "", check_name, fixed = T)
                            check_temp <-
                                bdchecks::perform_dc(data = flagged_data,
                                                           wanted_dc = c(check_name))
                            
                            if (!is.null(check_temp) &&
                                length(check_temp@flags) > 0 &&
                                length(check_temp@flags[[1]]@result) > 0) {
                                check_temp <- check_temp@flags[[1]]@result
                                
                                if (missing) {
                                    check_temp[is.na(check_temp)] <-
                                        FALSE  # Treating mising values as fails
                                } else {
                                    check_temp[is.na(check_temp)] <- TRUE
                                }
                                flagged_data[, paste("bdclean", check_name, sep = ".")] <-
                                    check_temp
                            }
                        } else {
                            # bdclean quality checks
                            flagged_data <-
                                get(check_name)(flagged_data, .self$users.answer)
                        }
                    }
                }
                return(flagged_data)
            },
            add_to_report = function(flagged_data,
                                     clean = TRUE,
                                     cleaning_threshold = 5) {
                flagged_data <- as.data.frame(flagged_data)
                
                for (i in 1:length(.self$quality.checks)) {
                    name_of_quality_check <- .self$quality.checks[i]
                    name_of_quality_check <- gsub("DC_", "", name_of_quality_check, fixed = T)
                    
                    if (!(paste("bdclean", name_of_quality_check, sep = ".") %in% names(flagged_data))) {
                        #both bdchecks and bdclean columns have bdclean prefix
                        warning(
                            "Required column ",
                            paste("bdclean", name_of_quality_check, sep = "."),
                            " not found! Probably, quality check is missing from
                            environment and check was not performed."
                        )
                        next
                    }
                    
                    flag <-
                        flagged_data[, paste("bdclean", name_of_quality_check, sep = ".")]
                    count_of_flagged_data <- sum(!flag, na.rm = T)
                    
                    # ------ Parsing MetaData for check from .Rd file
                    package_documentation_1 <- tools::Rd_db("bdchecks")
                    package_documentation_2 <- tools::Rd_db("bdclean")
                    package_documentation <- c(package_documentation_1, package_documentation_2)
                    
                    function_documentation <-
                        package_documentation[grep(name_of_quality_check, names(package_documentation))]
                    
                    if (length(function_documentation) == 0) {
                        warning(
                            "Could not find function documentation for ",
                            name_of_quality_check,
                            ". Skipping report."
                        )
                        next
                    }
                    
                    broken_documentation <-
                        unlist(strsplit(
                            paste(function_documentation[[1]], collapse = " "),
                            split = "\\",
                            fixed = TRUE
                        ))
                    
                    broken_documentation <-
                        gsub("\\n", "", gsub("[{}]", "", broken_documentation))
                    
                    description <-
                        broken_documentation[grep("title", broken_documentation)]
                    description <-
                        gsub("title  Data check", "", description, fixed = T)
                    
                    sample_pass_data <-
                        broken_documentation[grep("samplePassData", broken_documentation)]
                    sample_pass_data <-
                        gsub("section   samplePassData", "", sample_pass_data, fixed = T)
                    
                    sample_fail_data <-
                        broken_documentation[grep("sampleFailData", broken_documentation)]
                    sample_fail_data <-
                        gsub("section   sampleFailData", "", sample_fail_data, fixed = T)
                    
                    check_category <-
                        broken_documentation[grep("checkCategory", broken_documentation)]
                    check_category <-
                        gsub("section   checkCategory", "", check_category, fixed = T)
                    
                    target_dwc_field <-
                        broken_documentation[grep("targetDWCField", broken_documentation)]
                    target_dwc_field <-
                        gsub("section   targetDWCField", "", target_dwc_field, fixed = T)
                    
                    # ------ End of Parsing MetaData for check from .Rd file
                    
                    temp <- list()
                    temp$description <- paste(description, collapse = " ")
                    temp$sample_pass_data <- sample_pass_data
                    temp$sample_fail_data <- sample_fail_data
                    temp$check_category <- check_category
                    temp$target_dwc_field <- target_dwc_field
                    temp$affected_data <- count_of_flagged_data
                    
                    .self$cleaning.details[name_of_quality_check] <- list(temp)
                }
                },
            notify = function() {
                message("New Question object created.")
            },
            print_self = function() {
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
        fields = list(bdquestions = "list"),
        methods = list(
            initialize = function(bdquestions = NA) {
                "Construct an instance of BdQuestionContainer after validating the type."
                
                if (class(bdquestions[[1]]) != "BdQuestion") {
                    stop("Incompatible input type. Provide a list of BdQuestion")
                }
                .self$bdquestions <- bdquestions
                .self$notify()
            },
            reset_responses = function(){
                for (question in .self$bdquestions) {
                    question$set_response(character())
                }
            },
            flag_data = function(input_data, missing) {
                message("Initial records: ", paste(dim(input_data), collapse = "x"))
                flagged_data <- input_data
                for (question in .self$bdquestions) {
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
                        flagged_data <-
                            question$flag_data(flagged_data, missing)
                    }
                }
                return(flagged_data)
            },
            notify = function() {
                message(paste(
                    "New BdQuestionContainer instance created with",
                    length(.self$bdquestions),
                    "questions."
                ))
            },
            print_self = function() {
                for (question in .self$bdquestions) {
                    question$print_self()
                }
            }
        )
    )
