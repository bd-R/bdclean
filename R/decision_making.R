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
