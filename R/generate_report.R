#' Genrate data required to create report, function required in bdclean internal usage.
#'
#' NOTE: This is an package internal function. Do not use for external uses.
#'
#'@export
create_report_data <-
    function(inputData,
             flaggedData,
             cleanedData,
             responses,
             cleaningTrue,
             format) {
        for (question in responses$BdQuestions) {
            if (length(question$quality.checks) > 0 &&
                length(question$users.answer) > 0) {
                question$addToReport(flaggedData, cleaningTrue)
            }
        }
        
        # --------------- Data required for detailed report ---------------
        inputSize <- dim(inputData)
        outputSize <- dim(cleanedData)
        
        inputUniqueSpecies <-
            length(unique(inputData$scientificName))
        outputUniqueSpecies <-
            length(unique(cleanedData$scientificName))
        
        earliestInputDate <-
            min(as.POSIXct(unique(inputData$eventDate)))
        # latestInputDate <-
        #     max(suppressWarnings(as.POSIXct(
        #         unique(inputData$eventDate), "%Y-%m-%dT%H:%M:%S", tz="UTC"
        #     )))
        latestInputDate <-
            max(as.POSIXct(unique(inputData$eventDate)))
        
        earliestOutputDate <-
            min(as.POSIXct(unique(cleanedData$eventDate)))
        latestOutputDate <-
            max(as.POSIXct(unique(cleanedData$eventDate)))
        
        InputData <-
            c(
                inputSize[1],
                inputSize[2],
                inputUniqueSpecies,
                paste(earliestInputDate, "-", latestInputDate)
            )
        
        CleanedData <-
            c(
                outputSize[1],
                outputSize[2],
                outputUniqueSpecies,
                paste(earliestOutputDate, "-", latestOutputDate)
            )
        
        data.summary <- data.frame(InputData, CleanedData) # One
        row.names(data.summary) <-
            c("Rows",
              "Columns",
              "Number of unique scientific names",
              "Date Range")
        
        checks.records <- list() # Three
        index <- 1
        
        for (question in responses$BdQuestions) {
            if (length(question$quality.checks) > 0 &&
                length(question$users.answer) > 0) {
                checks.records[[paste("question", index, sep = "")]] <-
                    list(
                        "question" = question$question,
                        "answer" = question$users.answer,
                        "checks" = question$cleaning.details
                    )
                index = index + 1
            }
        }
        
        # ------------------- End of data required for detailed report ---------------
        
        # ------------------ Data required for short report ---------------
        
        recordsTable <- data.frame(
            DataCleaningProcedure = "Initial Records",
            NoOfRecords = NROW(inputData),
            Action = ""
        )
        
        checkIndex <- 1
        for (question in checks.records) {
            for (check in question$checks) {
                recordsTable <-
                    rbind(
                        recordsTable,
                        data.frame(
                            DataCleaningProcedure = names(question$checks[checkIndex]),
                            NoOfRecords = check$affectedData,
                            Action = "Removal"
                        )
                    )
                checkIndex <- checkIndex + 1
            }
        }
        
        remainingRecords <-
            (nrow(cleanedData))
        removedRecords <- nrow(inputData) - nrow(cleanedData)
        repairedRecords <- 0
        
        recordsTable <-
            rbind(
                recordsTable,
                data.frame(
                    DataCleaningProcedure = "Total",
                    NoOfRecords = paste(
                        "Remaining " ,
                        remainingRecords,
                        " Records (",
                        (remainingRecords / recordsTable[1, 2]) * 100,
                        "%)",
                        sep = ""
                    ),
                    Action = paste (
                        "Removal of ",
                        removedRecords,
                        " Records (",
                        (removedRecords / recordsTable[1, 2]) * 100,
                        "%) and Repair of ",
                        repairedRecords,
                        " Records (",
                        (repairedRecords / recordsTable[1, 2]) * 100,
                        "%)",
                        sep = ""
                    )
                )
            )
        
        # ------------ End of data required for short report ---------------
        
        print(knitr::kable(recordsTable, format = "markdown"))
        
        generateShortReport(recordsTable, format)
        generateDetailedReport(data.summary, checks.records, format)
    }

generateShortReport <- function(recordsTable, format) {
    message("Generating Reports...")
    
    dir.create(file.path(getwd(), "CleaningReports"), showWarnings = FALSE)
    
    message("Created folder")
    
    try(rmarkdown::render(
        system.file("rmd/generateShortReport.Rmd", package = "bdclean"),
        format,
        quiet = T,
        output_dir = "CleaningReports"
    ))
    
    message("generated simple")
}

generateDetailedReport <-
    function(data.summary, checks.records, format) {
        try(rmarkdown::render(
            system.file("rmd/generateDetailedReport.Rmd", package = "bdclean"),
            format,
            quiet = T,
            output_dir = "CleaningReports"
        ))
        
        message(paste(
            "Saved generated reports to '",
            getwd() ,
            "/CleaningReports'",
            sep = ""
        ))
    }
