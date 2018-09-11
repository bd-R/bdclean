summarizeDataframe <- function(data) {
    if (nrow(data) == 0) {
        return(data)
    }
    tempData <- as.data.frame(data)
    tempData <-
        tempData[, names(tempData) %in% c(
            "scientificName",
            "taxonRank",
            "eventDate",
            "country",
            "decimalLatitude",
            "decimalLongitude"
        )]
    tempData <- cbind(tempData, data)
    
    hidingCols <- c()
    tempData[] <- lapply(tempData, as.character)
    
    for (i in 1:length(names(tempData))) {
        sample <-
            sample(1:nrow(tempData), size = ifelse(nrow(tempData) > 1000, 1000, nrow(tempData)))
        f <-
            mean(sapply(tempData[sample, i], function(x)
                nchar(x)), na.rm = T)
        
        if (!is.nan(f)) {
            if (f > 50) {
                hidingCols <- c(hidingCols, i)
            }
        }
    }
    
    if (length(hidingCols) > 0) {
        tempData <- tempData[, c(hidingCols * -1)]
    }
    tempData
}
