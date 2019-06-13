summarizeDataframe <- function(data) {
    if (nrow(data) == 0) {
        return(data)
    }
    temp_data <- as.data.frame(data)
    temp_data <-
        temp_data[, names(temp_data) %in% c(
            "scientificName",
            "taxonRank",
            "eventDate",
            "country",
            "decimalLatitude",
            "decimalLongitude"
        )]
    temp_data <- cbind(temp_data, data)
    hiding_cols <- c()
    temp_data[] <- lapply(temp_data, as.character)
    
    for (i in 1:length(names(temp_data))) {
        size <- ifelse(nrow(temp_data) > 1000, 1000, nrow(temp_data))
        sample <-
            sample(1:nrow(temp_data), size = size)
        f <-
            mean(sapply(temp_data[sample, i], function(x)
                nchar(x)), na.rm = T)
        
        if (!is.nan(f)) {
            if (f > 50) {
                hiding_cols <- c(hiding_cols, i)
            }
        }
    }
    
    if (length(hiding_cols) > 0) {
        temp_data <- temp_data[, c(hiding_cols * -1)]
    }
    temp_data
}
