cleaning_function <- function(bddata, intensity){
    checkColumns <- grep("bdclean", colnames(bddata))
    checkData <- bddata[, checkColumns]
    
    # ------------- Decision Making of Cleaning -------------
    # Cleaning criteria is binary: Pass, Fail
    # Records with cleanliness-score 10 for all checks will pass.
    # Records with cleanliness-score less than 10 in atleast 1 check will fail
    # ------------- Decision Making of Cleaning -------------
    
    failedData <- which(rowSums(checkData != 10) >= 1)
    
    # ------------- End of Decision Making of Cleaning -------------
    
    return(bddata[!failedData, !checkColumns])
}


perform_Cleaning <- function(flaggedData){
    flagColumns <- which(grepl("bdclean", names(flaggedData)))
    cleanedData <- flaggedData
    cleanedData$cleanlinessScore <- 0
    
    for (columnIndex in flagColumns) {
        cleanedData$cleanlinessScore <-
            cleanedData$cleanlinessScore + cleanedData[, columnIndex]
        
    }
    cleanedData$cleanlinessScore <-
        cleanedData$cleanlinessScore / length(flagColumns)
    cleanedData <-
        cleanedData[cleanedData$cleanlinessScore > 5, c(flagColumns, length(cleanedData)) * -1]
    
    return(cleanedData)
}
