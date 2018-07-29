#' Data decision function (binary decision) required in bdclean internal usage.
#'
#' NOTE: This is an package internal function. Do not use for external uses.
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
    
    failedDataLogical <- rowSums(checkData != TRUE) >= 1
    
    # ------------- End of Decision Making of Cleaning -------------
    
    message("Records remaining:", nrow(bddata) - sum(failedDataLogical))
    
    return(bddata[!failedDataLogical, !grepl("bdclean", names(bddata))])
}

#' Data decision function (threshold tuning) required in bdclean internal usage.
#'
#' NOTE: This is an package internal function. Do not use for external uses.
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
    packageDocumentation <- tools::Rd_db("bdclean")
    qualityChecks <- list()
    
    for (i in 1:length(packageDocumentation)) {
        string <- paste(packageDocumentation[i], collapse = " ")
        
        if (grepl("checkCategory", string)) {
            nameOfQualityCheck <-
                gsub(".Rd", "", names(packageDocumentation)[i])
            
            functionDocumentation <- packageDocumentation[i]
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
            
            temp <- list()
            
            temp$nameOfQualityCheck <- nameOfQualityCheck
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
