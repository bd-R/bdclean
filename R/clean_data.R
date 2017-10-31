#' Data cleaning according to configuration supplied
#'
#' Use \code{get_config} to generate configuration and pass it to this
#' function to process the data accordingly.
#'
#'@param bddata biodiversity data in a data frame
#'@param config configuration generated using \code{get_config}
#'@param verbose Verbose output if TRUE else brief output if FALSE
#'@param report Whether to print report of cleaning done. Options are: Markdown, HTML or / and PDF
#'
#'@return data frame with clean data
#'
#'@import knitr rmarkdown
#'@examples \dontrun{
#'library(rgbif)
#'occdat1 <- occ_data(
#'  country = "AU",     # Country code for australia
#'  classKey= 359,      # Class code for mammalia
#'  limit=5000,         # Get only 5000 records
#'  )
#'  myData<-occdat1$data
#'  myConfig <- get_config()
#'  cleanData <- clean_data(myData,myConfig)
#'  cleanData <- clean_data(myData,myConfig, report = T)
#'}
#'
#'@export
clean_data <- function(bddata,
                       config,
                       verbose = T,
                       report = T) {
    if (verbose) {
        cat("\n Initial records ...", dim(bddata)[1], "\n")
    }
    
    ##------- Initializing dataframe and variables that hold output table details -------##
    recordsTable <- data.frame(
        DataCleaningProcedure = "Initial Records",
        NoOfRecords = NROW(bddata),
        Action = ""
    )
    selectedOption <- ""
    actionRequired <- ""
    ##------- End Of Initializing dataframe and variables that hold output table details -------##
    
    for (i in 1:dim(config)[1]) {
        cat(paste("\n", config$quest[i], config$response[i], "\n"))
        res <- as.character(config$response[i])
        
        sizeBeforeCleaning <- NROW(bddata)
        #print(res)
        switch(
            as.character(config$quest[i]),
            taxoLevel = {
                bddata <- taxoLevel(bddata, res)
                selectedOption <-
                    "Taxon Cleaning" # Storing values to be used in building output table
                actionRequired <-
                    "Removal" # Storing values to be used in building output table
            },
            misNames = {
                bddata <- misNames(bddata, res)
                selectedOption <- "Mis Match Names"
                actionRequired <- "Removal"
            },
            spatialResolution = {
                bddata <- spatialResolution(bddata, res)
                selectedOption <- "Spatial Resolution Fixing"
                actionRequired <- "Removal"
            },
            earliestDate = {
                bddata <- earliestDate(bddata, res)
                selectedOption <- "Earliest Date Events Removal"
                actionRequired <- "Removal"
            },
            temporalResolution = {
                bddata <- temporalResolution(bddata, res)
                selectedOption <- "Temporal Resolution Fixing"
                actionRequired <- "Repair"
            }
        )
        if (verbose) {
            cat("\n Records remaining...", dim(bddata)[1], "\n")
        }
        
        ## ------- Adding record of this iteration to the records dataframe ------- ##
        recordsTable <-
            rbind(
                recordsTable,
                data.frame(
                    DataCleaningProcedure = selectedOption,
                    NoOfRecords = sizeBeforeCleaning - NROW(bddata),
                    Action = actionRequired
                )
            )
        ## ------- End of Adding record of this iteration to the records dataframe ------- ##
    }
    
    ## ------- Adding Final results to the records dataframe ------- ##
    removedRecords <-
        sum(recordsTable[recordsTable$Action == "Removal", 2])
    
    recordsTable <-
        rbind(
            recordsTable,
            data.frame(
                DataCleaningProcedure = "Total",
                NoOfRecords = paste("A dataset of " , sum(recordsTable[2:NROW(recordsTable), 2]), " records"),
                Action = paste (
                    "Removal of ",
                    removedRecords,
                    " Records (",
                    (removedRecords / recordsTable[1, 2]) * 100,
                    "%)"
                )
            )
        )
    ## ------- End of Adding Final results to the records dataframe ------- ##
    
    ## ------- Exporting Outputs ------- ##
    print(kable(recordsTable, format = "markdown"))
    if (report) {
        message("Generating Reports...")
        dir.create(file.path(getwd(), "CleaningReports"), showWarnings = FALSE)
        save(recordsTable, file = "CleaningReports/cleaningReport.RData")
        download.file(
            "https://raw.githubusercontent.com/vijaybarve/bdclean/master/R/generateReport.R" ,
            destfile = "CleaningReports/generateReport.R",
            quiet = T
        )
        
       rmarkdown::render(
            "CleaningReports/generateReport.R",
            c("md_document", "html_document", "pdf_document"),
            quiet = T
        )
        suppressWarnings(suppressMessages(file.remove("CleaningReports/generateReport.R", showWarnings = FALSE)))
        suppressWarnings(suppressMessages(file.remove("CleaningReports/cleaningReport.RData", showWarnings = FALSE)))
        message("Saved generated reports to 'workingDirectory/CleaningReports'")
        
    }
    ## ------- Exporting Outputs ------- ##
    
    return(bddata)
}

# Support functions that are called within main function

taxoLevel <- function(bddata, res = "SPECIES") {
    ranks <-
        c("CLASS",
          "ORDER",
          "FAMILY",
          "GENUS",
          "SPECIES",
          "SUBSPECIES")
    if (!(res %in% ranks)) {
        print("Rank Value unknown. It should be FAMILY, GENUS, SPECIES or SUBSPECIES")
        return(bddata)
    }
    idx <- which(ranks == res)
    cat(paste("\n Removing records above :", res, "\n"))
    retmat <- NULL
    if (idx > 0) {
        for (i in idx:length(ranks)) {
            resmat <- bddata[which(bddata$taxonRank == ranks[i]), ]
            retmat <- rbind(retmat, resmat)
        }
    }
    return(retmat)
}

misNames <- function(bddata, res = "No") {
    cat("\n fxn misNames not implemented yet \n")
    return(bddata)
}

spatialResolution <- function(bddata, res = 100) {
    #print("fxn Spatial Resoultion")
    res <- as.numeric(res)
    if (res > 0) {
        retmat <-
            bddata[which(bddata$coordinateUncertaintyInMeters < res), ]
    }
    return(retmat)
}

earliestDate <- function(bddata, res = "1700-01-01") {
    bddata <- as.data.frame(bddata)
    ed <- try(as.Date(res, format = "%Y-%m-%d"))
    if (class(ed) == "try-error" || is.na(ed)) {
        print("That date wasn't correct!")
        return(bddata)
    }
    retmat <- bddata[which(as.Date(bddata$eventDate) > ed), ]
    return(retmat)
}

temporalResolution <- function(bddata, res = "Day") {
    bddata <- as.data.frame(bddata)
    if (res == "Day") {
        retmat <- bddata[which(!is.na(bddata$day)), ]
    }
    if (res == "Month") {
        retmat <- bddata[which(!is.na(bddata$month)), ]
    }
    if (res == "Year") {
        retmat <- bddata[which(!is.na(bddata$year)), ]
    }
    return(retmat)
}
