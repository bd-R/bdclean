#' Data decision function (binary decision) required in bdclean internal usage.
#'
#' NOTE: This is an package internal function. Do not use for external uses. Exported to make it available for shiny app.
#'
#' @param bddata The dataframe to clean
#' 
#' @examples
#' 
#' if(interactive()){
#' 
#' library(rgbif)
#' occdat <- occ_data(
#'   country = 'AU', # Country code for australia
#'   classKey = 359, # Class code for mammalia
#'   limit = 50 # Get only 50 records
#' )
#' myData <- occdat$data
#' cleaned_data <- cleaning_function(myData)
#' 
#' } 
#'
#' @export
cleaning_function <- function(bddata) {
    bddata <- as.data.frame(bddata)
    
    check_columns <- which(grepl("bdclean", names(bddata)))
    
    if (length(check_columns) == 0) {
        warning("Dataset has no flag columns! Skipping cleaning")
        return(bddata)
    }
    
    check_data <- bddata[, check_columns]
    
    # ------------- Decision Making of Cleaning -------------
    # Cleaning criteria is binary: Pass, Fail Records with cleanliness-score 10 for all
    # checks will pass.  Records with cleanliness-score less than 10 in atleast 1 check will fail
    # ------------- Decision Making of Cleaning -------------
    
    if (class(check_data) == "logical") {
        failed_data_logical <- check_data != TRUE
    } else {
        failed_data_logical <- rowSums(check_data != TRUE, na.rm = T) >= 1
    }
    
    # ------------- End of Decision Making of Cleaning -------------
    message("Records remaining:",
            nrow(bddata) - sum(failed_data_logical))
    
    return(bddata[!failed_data_logical, !grepl("bdclean", names(bddata))])
}

#' Data decision function (threshold tuning) required in bdclean internal usage.
#'
#' NOTE: This is an package internal function. Do not use for external uses.
#'
#' @param flagged_data The dataset with flags to be cleaned.
#' @param cleaning_threshold The Cleaning tolerance. Not used in current version.
#'
#' @examples
#'
#' if(interactive()){
#' 
#' library(rgbif)
#' occdat <- occ_data(
#'   country = 'AU', # Country code for australia
#'   classKey = 359, # Class code for mammalia
#'   limit = 50 # Get only 50 records
#' )
#' myData <- occdat$data
#' cleaned_data <- perform_Cleaning(myData)
#' 
#' } 
perform_Cleaning <- function(flagged_data, cleaning_threshold = 5) {
    flag_columns <- which(grepl("bdclean", names(flagged_data)))
    
    if (length(flag_columns) == 0) {
        warning("Dataset has no flag columns! Skipping cleaning")
        return(flagged_data)
    }
    assert_is_numeric(cleaning_threshold)
    
    cleaned_data <- flagged_data
    cleaned_data$cleanliness_score <- 0
    
    for (column_index in flag_columns) {
        cleaned_data$cleanliness_score <-
            cleaned_data$cleanliness_score + cleaned_data[, column_index]
    }
    cleaned_data$cleanliness_score <-
        cleaned_data$cleanliness_score / length(flag_columns)
    cleaned_data <-
        cleaned_data[cleaned_data$cleanliness_score >= cleaning_threshold, c(flag_columns, length(cleaned_data)) * -1]
    return(cleaned_data)
}

#' Returning checks list, function required in bdclean internal usage.
#'
#' NOTE: This is an package internal function. Do not use for external uses.
#' 
#' @examples
#' 
#' if(interactive()){
#' 
#' all_checks <- get_checks_list()
#' 
#' } 
#'
#' @export
get_checks_list <- function() {
    bdchecks_documentation <- tools::Rd_db("bdchecks")
    package_documentation <- bdchecks_documentation
    
    quality_checks <- list()
    for (i in 1:length(package_documentation)) {
        string <- paste(package_documentation[i], collapse = " ")
        
        if (grepl("checkCategory", string)) {
            name_of_quality_check <-
                gsub(".Rd", "", names(package_documentation)[i])
            
            function_documentation <- package_documentation[i]
            broken_documentation <-
                unlist(strsplit(
                    paste(function_documentation[[1]], collapse = " "),
                    split = "\\",
                    fixed = TRUE
                ))
            broken_documentation <-
                gsub("\\n", "", gsub("[{}]", "", broken_documentation))
            
            description <-
                broken_documentation[grep("title", broken_documentation)]
            description <-
                gsub("title  Data check", "", description, fixed = T)
            
            sample_pass_data <-
                broken_documentation[grep("samplePassData", broken_documentation)]
            sample_pass_data <-
                gsub("section   samplePassData",
                     "",
                     sample_pass_data,
                     fixed = T)
            
            sample_fail_data <-
                broken_documentation[grep("sampleFailData", broken_documentation)]
            sample_fail_data <-
                gsub("section   sampleFailData",
                     "",
                     sample_fail_data,
                     fixed = T)
            
            check_category <-
                broken_documentation[grep("checkCategory", broken_documentation)]
            check_category <-
                gsub("section   checkCategory",
                     "",
                     check_category,
                     fixed = T)
            
            target_dwc_field <-
                broken_documentation[grep("targetDWCField", broken_documentation)]
            target_dwc_field <-
                gsub("section   targetDWCField",
                     "",
                     target_dwc_field,
                     fixed = T)
            
            temp <- list()
            temp$name_of_quality_check <- name_of_quality_check
            temp$description <- paste(description, collapse = " ")
            temp$sample_pass_data <- sample_pass_data
            temp$sample_fail_data <- sample_fail_data
            temp$check_category <- check_category
            temp$target_dwc_field <- target_dwc_field
            quality_checks[name_of_quality_check] <- list(temp)
        }
    }
    return(quality_checks)
}
