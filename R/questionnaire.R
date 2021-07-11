#' Create the package default Questionnaire.
#'
#' @return BdQuestionContainer object with default Questions
#'
#' @examples
#'
#' customQuestionnaire <- create_default_questionnaire()
#'
#' @export
create_default_questionnaire <- function() {
    # Sample question with all possible fields
    #
    # question <-
    #     BdQuestion(
    #         question = "Do you worry about taxonomical aspect of the data?",
    #         possible.responses = c("Yes", "No"),
    #         question.type = "Router",
    #         router.condition = c("Yes", "Y", "yes", 1, TRUE, "TRUE"),
    #         question.id = "taxonMain",
    #       quality.checks = c("taxo_level"),
    #         ui.type = "single-checkbox"
    #     )
    
    
    question1 <-
        BdQuestion(
            question = "Do you worry about taxonomical aspect of the data?",
            possible.responses = c("Yes", "No"),
            question.type = "Router",
            router.condition = c("Yes", "Y", "yes", 1, TRUE, "TRUE"),
            quality.checks = c("class_valid",
                               "genus_valid",
                               "taxonid_present",
                               "phylum_valid",
                               "kingdom_valid",
                               "family_valid",
                               "taxon_present",
                               "order_valid",
                               "scientificname_present",
                               "taxonrank_standard",
                               "taxonrank_present"),
            question.id = "taxonMain",
            ui.type = "single-checkbox"
        )
    
    question2 <-
        BdQuestion(
            question = "What is the lowest taxonomic level you require in your data?",
            possible.responses = c(
                "Subspecies",
                "Species",
                "Genus",
                "Family",
                "Order",
                "Class"),
            question.type = "Child",
            quality.checks = c("taxo_level"),
            question.id = "taxonLevel",
            ui.type = "select"
        )
    
    question1$add_child_question(c(question2))
    
    question3 <-
        BdQuestion(
            question = "Do you worry about spatial aspect of the data?",
            possible.responses = c("Yes", "No"),
            question.type = "Router",
            router.condition = c("Yes", "Y", "yes", 1, TRUE, "TRUE"),
            quality.checks = c("coordinates_notzero", 
                               "decimallongitude_inrange",
                               "decimallongitude_present",
                               "decimallatitude_present",
                               "decimallatitude_inrange",
                               "eventtemporal_present",
                               "location_present",
                               "geodeticdatum_present"),
            question.id = "spatialMain",
            ui.type = "single-checkbox"
        )
    
    question4 <-
        BdQuestion(
            question = "What is the spatial resolution required for your data? (in meteres)",
            question.type = "Child",
            quality.checks = c("spatial_resolution"),
            question.id = "spatialResolution",
            ui.type = "numericInput"
        )
    
    question4$add_validation_function(function(answer) {
        answer <- suppressWarnings(as.numeric(answer))
        check <- (!is.na(answer) && answer > 0 && answer < 1e+05)
        if (!check) {
            message(
                "Spatial resolution should be a number between 0 to 100 KM. Please give a correct value."
            )
        }
        return(check)
    })
    
    question_sub_01 <-
        BdQuestion(
            question = "Do you worry about precision of coordinates?",
            possible.responses = c("Yes", "No"),
            question.type = "ChildRouter",
            router.condition = c("Yes", "Y", "yes", 1, TRUE, "TRUE"),
            quality.checks = c(
                "coordinateuncertainty_inrange"
            ),
            question.id = "precisionCoord",
            ui.type = "single-checkbox"
        )
    
    question_sub_02 <-
        BdQuestion(
            question = "Do you worry about countries of occurrences?",
            possible.responses = c("Yes", "No"),
            question.type = "ChildRouter",
            router.condition = c("Yes", "Y", "yes", 1, TRUE, "TRUE"),
            quality.checks = c("country_present",
                               "country_countrycode_consistent",
                               "countrycode_present",
                               "country_standard"),
            question.id = "countryCoord",
            ui.type = "single-checkbox"
        )
    
    question_sub_03 <-
        BdQuestion(
            question = "Do you worry about elevation of occurrences?",
            possible.responses = c("Yes", "No"),
            router.condition = c("Yes",
                                 "Y", "yes", 1, TRUE, "TRUE"),
            question.type = "ChildRouter",
            quality.checks = c("minelevation_inrange",
                               "mindepth_lessthan_maxdepth",
                               "minelevation_lessthan_maxelevation",
                               "mindepth_maxdepth_inrange",
                               "maxelevation_inrange"),
            question.id = "elevationCoord",
            ui.type = "single-checkbox"
        )
    
    question3$add_child_question(c(
        question4,
        question_sub_01,
        question_sub_02,
        question_sub_03
    ))
    
    question5 <-
        BdQuestion(
            question = "Do you worry about temporal aspect of your data?",
            possible.responses = c("Yes", "No"),
            question.type = "Router",
            router.condition = c("Yes", "Y", "yes", 1, TRUE, "TRUE"),
            quality.checks = c(
                "day_standard",
                "year_inrange",
                "eventdate_standard",
                "dateidentified_standard",
                "eventdate_inrange",
                "year_present",
                "eventdate_present",
                "month_standard"
            ),
            question.id = "temporalMain",
            ui.type = "single-checkbox"
        )
    
    question6 <-
        BdQuestion(
            question = "What is the date range of the observations in this data set? In format (YYYY-mm-dd YYYY-mm-dd)",
            question.type = "Child",
            quality.checks = c("earliest_date"),
            question.id = "temporalEarliest",
            ui.type = "date-range"
        )
    
    question6$add_validation_function(function(answer) {
        dates <- strsplit(answer, " ")[[1]]
        d <- try(as.Date(dates[1]))
        if (class(d) == "try-error" || is.na(d)) {
            message("Invalid Date! Please follow the date format (YYYY-mm-dd)")
            return(FALSE)
        }
        d <- try(as.Date(dates[2]))
        if (class(d) == "try-error" || is.na(d)) {
            message("Invalid Date! Please follow the date format (YYYY-mm-dd)")
            return(FALSE)
        }
        return(TRUE)
    })
    
    question5$add_child_question(c(question6))
    
    question_sub_05 <-
        BdQuestion(
            question = "Do you worry about other properties of occurrence? (GBIF issues/publisher/occuranceremark, etc)?",
            possible.responses = c("Yes", "No"),
            question.type = "Router",
            router.condition = c("Yes", "Y", "yes", 1, TRUE, "TRUE"),
            quality.checks = c(
                "occurrenceid_standard",
                "establishmentmeans_present",
                "occurrencestatus_standard",
                "basisofrecord_present",
                "license_present",
                "basisofrecord_standard",
                "occurrencestatus_present",
                "datageneralizations_present",
                "occurrenceid_present",
                "dctype_present"
            ),
            question.id = "smallerIssues",
            ui.type = "single-checkbox"
        )
    
    all_questions <-
        BdQuestionContainer(
            c(
                question1,
                question2,
                question3,
                question4,
                question_sub_01,
                question_sub_02,
                question5,
                question6,
                question_sub_05
            )
        )
    return(all_questions)
}



#' Create the alternate Questionnaire.
#'
#' @return BdQuestionContainer object with default Questions
#'
#' @examples
#'
#' customQuestionnaire <- create_default_questionnaire()
#'
#' @export
create_alternate_questionnaire_1 <- function() {
    question1 <-
        BdQuestion(
            question = "Flag important missing records?",
            possible.responses = c("Yes", "No"),
            question.type = "Router",
            router.condition = c("Yes", "Y", "yes", 1, TRUE, "TRUE"),
            question.id = "missing",
            quality.checks = c(
                "DC_yearMissing",
                "DC_dateNull",
                "DC_identifiedDateImprobable",
                "DC_identifiedDateImprobable",
                "DC_classUnknown"
            ),
            ui.type = "single-checkbox"
        )
    
    question2 <-
        BdQuestion(
            question = "Flag invalid values?",
            possible.responses = c("Yes", "No"),
            question.type = "Router",
            router.condition = c("Yes", "Y", "yes", 1, TRUE, "TRUE"),
            question.id = "invalid",
            quality.checks = c(
                "DC_precisionRangeMismatch",
                "DC_uncertaintyRangeMismatch",
                "DC_depthOutOfRange",
                "DC_elevationOutOfRange",
                "DC_monthInvalid",
                "DC_dayInvalid",
                "DC_countryNameUnknown",
                "DC_eventDateInFuture",
                "DC_modifiedInFuture",
                "DC_namePublishedYearInFuture",
                "DC_individualcountInvalid",
                "DC_occurrenceIdNotGuid"
            ),
            ui.type = "single-checkbox"
        )
    
    question3 <-
        BdQuestion(
            question = "Flag common digitization errors?",
            possible.responses = c("Yes", "No"),
            question.type = "Router",
            router.condition = c("Yes", "Y", "yes", 1, TRUE, "TRUE"),
            question.id = "digitization",
            quality.checks = c("taxo_level"),
            ui.type = "single-checkbox"
        )
    
    question4 <-
        BdQuestion(
            question = "Flag invalid taxon names?",
            possible.responses = c("Yes", "No"),
            question.type = "Router",
            router.condition = c("Yes", "Y", "yes", 1, TRUE, "TRUE"),
            question.id = "taxon",
            quality.checks = c("taxo_level"),
            ui.type = "single-checkbox"
        )
    
    question5 <-
        BdQuestion(
            question = "Flag common coordinate mixup errors?",
            possible.responses = c("Yes", "No"),
            question.type = "Router",
            router.condition = c("Yes", "Y", "yes", 1, TRUE, "TRUE"),
            question.id = "mixup",
            quality.checks = c("DC_coordinatesZero"),
            ui.type = "single-checkbox"
        )
    
    question6 <-
        BdQuestion(
            question = "Flag common coordinate mismatch errors?",
            possible.responses = c("Yes", "No"),
            question.type = "Router",
            router.condition = c("Yes", "Y", "yes", 1, TRUE, "TRUE"),
            question.id = "mismatch",
            quality.checks = c("taxo_level"),
            ui.type = "single-checkbox"
        )
    
    question7 <-
        BdQuestion(
            question = "Flag coordinate ~ otherFileds mismatch errors?",
            possible.responses = c("Yes", "No"),
            question.type = "Router",
            router.condition = c("Yes", "Y", "yes", 1, TRUE, "TRUE"),
            question.id = "otherfields",
            quality.checks = c("DC_coordinatePrecisionMismatch", "DC_countryMismatch"),
            ui.type = "single-checkbox"
        )
    
    question8 <-
        BdQuestion(
            question = "Flag coordinate taggging errors?",
            possible.responses = c("Yes", "No"),
            question.type = "Router",
            router.condition = c("Yes", "Y", "yes", 1, TRUE, "TRUE"),
            question.id = "tagging",
            quality.checks = c("taxo_level"),
            ui.type = "single-checkbox"
        )
    
    question9 <-
        BdQuestion(
            question = "Flag gbif issues?",
            possible.responses = c("Yes", "No"),
            question.type = "Router",
            router.condition = c("Yes", "Y", "yes", 1, TRUE, "TRUE"),
            question.id = "gbifissues",
            quality.checks = c("taxo_level"),
            ui.type = "single-checkbox"
        )
    
    question10 <-
        BdQuestion(
            question = "Flag with input based checks?",
            possible.responses = c("Yes", "No"),
            question.type = "Router",
            router.condition = c("Yes", "Y", "yes", 1, TRUE, "TRUE"),
            question.id = "inputbased",
            ui.type = "single-checkbox"
        )
    
    question10sub1 <-
        BdQuestion(
            question = "What is the lowest taxonomic level you require in your data?",
            possible.responses = c(
                "Subspecies",
                "Species",
                "Genus",
                "Family",
                "Order",
                "Class"
            ),
            question.type = "Child",
            quality.checks = c("taxo_level"),
            question.id = "taxonLevel",
            ui.type = "select"
        )
    
    question10sub2 <-
        BdQuestion(
            question = "What is the spatial resolution required for your data? (in meteres)",
            question.type = "Child",
            quality.checks = c("spatial_resolution"),
            question.id = "spatialResolution",
            ui.type = "numericInput"
        )
    
    question10sub3 <-
        BdQuestion(
            question = "What is the range of dates of the observations in this data set? In format (YYYY-mm-dd YYYY-mm-dd)",
            question.type = "Child",
            quality.checks = c("earliest_date"),
            question.id = "temporalEarliest",
            ui.type = "date-range"
        )
    
    question10$add_child_question(c(question10sub1, question10sub2, question10sub3))
    
    
    return(BdQuestionContainer(
        c(
            question1,
            question2,
            question3,
            question4,
            question5,
            question6,
            question7,
            question8,
            question9,
            question10,
            question10sub1,
            question10sub2,
            question10sub3
            
        )
    ))
}
