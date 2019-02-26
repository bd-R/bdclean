#' Data decision function (binary decision) required in bdclean internal usage.
#'
#' NOTE: This is an package internal function. Do not use for external uses.
#'
#' @param bddata The dataframe to clean
#' 
#'@export
cleaning_function <- function(bddata) {
    bddata <- as.data.frame(bddata)
    
    checkColumns <- which(grepl("bdclean", names(bddata)))
    
    if (length(checkColumns) == 0) {
        warning("Dataset has no flag columns! Skipping cleaning")
        return(bddata)
    }
    
    checkData <- bddata[, checkColumns]
    
    # ------------- Decision Making of Cleaning -------------
    # Cleaning criteria is binary: Pass, Fail
    # Records with cleanliness-score 10 for all checks will pass.
    # Records with cleanliness-score less than 10 in atleast 1 check will fail
    # ------------- Decision Making of Cleaning -------------
    
    if (class(checkData) == "logical") {
        failedDataLogical <- checkData != TRUE
    } else {
        failedDataLogical <- rowSums(checkData != TRUE, na.rm = T) >= 1
    }
    
    # ------------- End of Decision Making of Cleaning -------------
    
    message("Records remaining:",
            nrow(bddata) - sum(failedDataLogical))
    
    return(bddata[!failedDataLogical, !grepl("bdclean", names(bddata))])
}

#' Data decision function (threshold tuning) required in bdclean internal usage.
#'
#' NOTE: This is an package internal function. Do not use for external uses.
#'
#'@param flaggedData The dataset with flags to be cleaned.
#'@param cleaningThreshold The Cleaning tolerance. Not used in current version.
#'
perform_Cleaning <- function(flaggedData, cleaningThreshold = 5) {
    flagColumns <- which(grepl("bdclean", names(flaggedData)))
    
    if (length(flagColumns) == 0) {
        warning("Dataset has no flag columns! Skipping cleaning")
        return(flaggedData)
    }
    
    cleanedData <- flaggedData
    cleanedData$cleanlinessScore <- 0
    
    for (columnIndex in flagColumns) {
        cleanedData$cleanlinessScore <-
            cleanedData$cleanlinessScore + cleanedData[, columnIndex]
    }
    cleanedData$cleanlinessScore <-
        cleanedData$cleanlinessScore / length(flagColumns)
    cleanedData <-
        cleanedData[cleanedData$cleanlinessScore >= cleaningThreshold, c(flagColumns, length(cleanedData)) * -1]
    
    return(cleanedData)
}

#' Returning checks list, function required in bdclean internal usage.
#'
#' NOTE: This is an package internal function. Do not use for external uses.
#'
#'@export
get_checks_list <- function() {
    # Uncomment if both suppoprted in customized checks. Right now,
    # only bdcecks supported as it doenst require user input.
    # bdcleanDocumentation <- tools::Rd_db("bdclean")
    # bdchecksDocumentation <- tools::Rd_db("bdchecks")
    # packageDocumentation <- c(bdcleanDocumentation, bdchecksDocumentation)
    
    bdchecksDocumentation <- tools::Rd_db("bdchecks")
    packageDocumentation <- bdchecksDocumentation
    
    qualityChecks <- list()
    
    for (i in 1:length(packageDocumentation)) {
        string <- paste(packageDocumentation[i], collapse = " ")
        
        if (grepl("checkCategory", string)) {
            nameOfQualityCheck <-
                gsub(".Rd", "", names(packageDocumentation)[i])
            
            functionDocumentation <- packageDocumentation[i]
            
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
            
            temp <- list()
            
            temp$nameOfQualityCheck <-
                paste("DC_", nameOfQualityCheck, sep = "")
            temp$description <-
                paste(description, collapse = " ")
            temp$samplePassData <- samplePassData
            temp$sampleFailData <- sampleFailData
            temp$checkCategory <- checkCategory
            temp$targetDWCField <- targetDWCField
            
            qualityChecks[nameOfQualityCheck] <-
                list(temp)
        }
    }
    return(qualityChecks)
}
