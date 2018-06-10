store_report_data <- function(inputData, cleanedData, responses) {
    inputSize <- dim(inputData)
    outputSize <- dim(cleanedData)
    
    inputUniqueSpecies <- length(unique(inputData$scientificName))
    outputUniqueSpecies <-
        length(unique(cleanedData$scientificName))
    
    earliestInputDate <-
        min(as.POSIXct(unique(inputData$eventDate), "%Y-%m-%dT%H:%M:%S", "GMT"))
    latestInputDate <-
        max(as.POSIXct(unique(inputData$eventDate), "%Y-%m-%dT%H:%M:%S", "GMT"))
    earliestOutputDate <-
        min(as.POSIXct(unique(cleanedData$eventDate), "%Y-%m-%dT%H:%M:%S", "GMT"))
    latestOutputDate <-
        max(as.POSIXct(unique(cleanedData$eventDate), "%Y-%m-%dT%H:%M:%S", "GMT"))
    
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
        if (question$question.type != "Router" &&
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
    
    print(kable(data.summary, format = "markdown"))
    generateReport(data.summary, checks.records, format = c("md_document", "html_document", "pdf_document"))
}


generateReport <- function(data.summary, checks.records, format) {
    message("Generating Reports...")
    
    dir.create(file.path(getwd(), "CleaningReports"), showWarnings = FALSE)
    
    try(rmarkdown::render(
        system.file("rmd/generateReport.Rmd", package="bdclean"),
        format,
        quiet = T,
        output_dir = "CleaningReports"
    ))
    
    message("Saved generated reports to 'workingDirectory/CleaningReports'")
}
