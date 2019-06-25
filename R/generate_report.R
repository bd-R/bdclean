#' Generate data required to create report, function required in bdclean internal usage.
#'
#' NOTE: This is an package internal function. Do not use for external uses. Exported to make it available for shiny app.
#'
#' @param input_data The input dataframe before cleaning
#' @param flagged_data The flagged data for cleaning
#' @param cleaned_data The data with flagged records removed
#' @param responses The BDQuestions object with user responses
#' @param cleaning_true Flag specifying if the cleaning should be done, or just flagging
#' @param format The format of the report to be generated
#' 
#' @examples
#' 
#' if(interactive()){
#' 
#' library(rgbif)
#' occdat <- occ_data(
#'   country = 'AU', # Country code for australia
#'   classKey = 359, # Class code for mammalia
#'   limit = 50 # Get only 50 records
#' )
#' myData <- occdat$data
#' 
#' question <- BdQuestion()
#' responses <- get_user_response(question)
#' 
#' cleaned_data <- create_report_data(myData, myData, myData, responses, T, 'pdf_document')
#' 
#' } 
#'
#' @export
create_report_data <-
    function(input_data,
             flagged_data,
             cleaned_data,
             responses,
             cleaning_true,
             format) {
        

        
        assertive::assert_is_data.frame(input_data)
        assertive::assert_has_cols(input_data)
        assertive::assert_is_data.frame(flagged_data)
        assertive::assert_has_cols(flagged_data)
        assertive::assert_is_data.frame(cleaned_data)
        assertive::assert_is_logical(cleaning_true)
        #assertive::assert_is_inherited_from(responses, "BdQuestionContainer")
        if(!all(format %in% c("html_document", "pdf_document", "md_document", "word_document"))){
            stop("Format can only be one of html_document, pdf_document")
        }
        
        for (question in responses$bdquestions) {
            if (length(question$quality.checks) > 0 &&
                length(question$users.answer) > 0) {
                question$add_to_report(flagged_data, cleaning_true)
            }
        }
        
        if (!cleaning_true) {
            cleaned_data <- flagged_data
        }
        
        # --------------- Data required for detailed report ---------------
        input_data <- as.data.frame(input_data)
        input_size <- dim(input_data)
        output_size <- dim(cleaned_data)
        
        input_unique_species <-
            length(unique(input_data[, "scientificName"]))
        output_unique_species <-
            length(unique(cleaned_data[, "scientificName"]))
        
        earliest_input_date <-
            try(as.POSIXct(unique(input_data[, "eventDate"]), tz = "UTC"), silent = T)
        
        if (class(earliest_input_date) != "try-error") {
            earliest_input_date <- min(earliest_input_date)
            latest_input_date <-
                max(as.POSIXct(unique(input_data[, "eventDate"]), tz = "UTC"))
        } else {
            earliest_input_date <- "NA"
            latest_input_date <- "NA"
        }
        
        earliest_output_date <-
            try(as.POSIXct(unique(cleaned_data[, "eventDate"]), tz = "UTC"), silent = T)
        
        if (class(earliest_output_date) != "try-error") {
            earliest_output_date <- min(earliest_input_date)
            latest_output_date <-
                max(as.POSIXct(unique(cleaned_data[, "eventDate"]), tz = "UTC"))
        } else {
            earliest_output_date <- "NA"
            latest_output_date <- "NA"
        }
        input_data_meta <-
            c(
                input_size[1],
                input_size[2],
                input_unique_species,
                paste(earliest_input_date, "-", latest_input_date)
            )
        cleaned_data_meta <-
            c(
                output_size[1],
                output_size[2],
                output_unique_species,
                paste(earliest_output_date, "-", latest_output_date)
            )
        data.summary <- data.frame(input_data_meta, cleaned_data_meta)  # One
        row.names(data.summary) <-
            c("Rows",
              "Columns",
              "Number of unique scientific names",
              "Date Range")
        
        checks.records <- list()  # Three
        index <- 1
        
        for (question in responses$bdquestions) {
            # length(question$quality.checks) > 0 &&
            if (length(question$users.answer) > 0) {
                checks.records[[paste("question", index, sep = "")]] <-
                    list(
                        question = question$question,
                        answer = question$users.answer,
                        checks = question$cleaning.details
                    )
                index <- index + 1
            }
        }
        # ------------------- End of data required for detailed report ---------------
        
        # ------------------ Data required for short report ---------------
        records_table <-
            data.frame(
                DataCleaningProcedure = "Initial Records",
                NoOfRecords = nrow(input_data),
                Action = "Initialize"
            )
        
        for (question in checks.records) {
            check_index <- 1
            for (check in question$checks) {
                records_table <-
                    rbind(
                        records_table,
                        data.frame(
                            DataCleaningProcedure = names(question$checks)[check_index],
                            NoOfRecords = ifelse(is.null(check$affectedData), 0, check$affectedData),
                            Action = ifelse(cleaning_true, "Removal", "Flagging")
                        )
                    )
                check_index <- check_index + 1
            }
        }
        remaining_records <- (nrow(cleaned_data))
        removed_records <- nrow(input_data) - nrow(cleaned_data)
        records_table <-
            rbind(
                records_table,
                data.frame(
                    DataCleaningProcedure = "Total",
                    NoOfRecords = paste(
                        "Remaining ",
                        remaining_records,
                        " Records (",
                        (remaining_records / records_table[1, 2]) * 100,
                        "%)",
                        sep = ""
                    ),
                    Action = paste(
                        ifelse(cleaning_true, "Removal of ", "Flagging of"),
                        removed_records,
                        " Records (",
                        (removed_records / records_table[1, 2]) * 100,
                        "%)",
                        sep = ""
                    )
                )
            )
        # ------------ End of data required for short report ---------------
        message(knitr::kable(records_table, format = "markdown"))
        generate_short_report(records_table, format)
        generate_detailed_report(data.summary, checks.records, format)
    }

generate_short_report <- function(records_table, format) {
    message("Generating Reports...")
    try(rmarkdown::render(
        system.file("rmd/generateShortReport.Rmd", package = "bdclean"),
        format,
        quiet = T,
        output_dir = tempdir()
    ))
    message("generated simple")
}

generate_detailed_report <-
    function(data.summary, checks.records, format) {
        try(rmarkdown::render(
            system.file("rmd/generateDetailedReport.Rmd", package = "bdclean"),
            format,
            quiet = T,
            output_dir = tempdir()
        ))
        message(paste("Saved generated reports to '", tempdir(), sep = ""))
    }
