cleaning_function <- function(bddata, intensity){
    checkColumns <- grep("bdclean", colnames(bddata))
    checkData <- bddata[, checkColumns]
    failedData <- which(rowSums(checkData != 10) >= 1)
    
    return(bddata[!passedData, !checkColumns])
}