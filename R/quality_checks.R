#' Clean data based on lower taxon level
#'
#' Clean data based on lower taxon level
#'
#' @section samplePassData:
#' When resolution is Species, Subspecies and Species will pass.
#'
#' @section sampleFailData:
#' When resolution is Species, Family or Genus or any lower ranks will fail.
#'
#' @section targetDWCField:
#' taxonRank
#'
#' @section checkCategory:
#' taxonomic
#'
#' @param bddata Bio diversity data in a data frame
#' @param res The low rank of species required
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
#' 
#' responses <- taxo_level(myData, 'SPECIES')
#' 
#' }
#'
#' @export
taxo_level <- function(bddata, res = "SPECIES") {
    assertive::assert_is_data.frame(bddata)
    assertive::assert_has_cols(bddata)
    
    ranks <-
        c("CLASS",
          "ORDER",
          "FAMILY",
          "GENUS",
          "SPECIES",
          "SUBSPECIES")
    res <- toupper(res)
    
    if (!(res %in% ranks)) {
        warning("Rank Value unknown. It should be FAMILY, GENUS, SPECIES or SUBSPECIES")
        return(bddata)
    }
    
    idx <- which(ranks == res)
    message(paste("taxoLevel:", "\n Removing records above :", res, "\n"))
    bddata[, "bdclean.taxoLevel"] <- FALSE
    if (idx > 0) {
        for (i in idx:length(ranks)) {
            bddata[which(bddata[, "taxonRank"] == ranks[i]), "bdclean.taxoLevel"] <-
                TRUE
        }
    }
    
    return(bddata)
}

#' Clean data based on spatial resolution
#'
#' Clean data based on spatial resolution
#'
#' @section samplePassData:
#' When resolution is 100 meters, Coordinate Uncertainties below 100 meters will pass.
#'
#' @section sampleFailData:
#' When resolution is 100 meters, Coordinate Uncertainties above 100 meters will fail.
#'
#' @section targetDWCField:
#' coordinateUncertaintyInMeters
#'
#' @section checkCategory:
#' spatial
#'
#' @param bddata Bio diversity data in a data frame
#' @param res The highest coordinate uncertainty required
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
#' 
#' responses <- spatial_resolution(myData, 1500)
#' 
#' }
#'
#' @export
spatial_resolution <- function(bddata, res = 100) {
    assertive::assert_is_data.frame(bddata)
    assertive::assert_has_cols(bddata)
    
    message(paste(
        "spatialResolution:",
        "\n Removing records above :",
        res,
        "\n"
    ))
    res <- as.numeric(res)
    bddata[, "bdclean.spatialResolution"] <- FALSE
    if (res > 0) {
        bddata[which(bddata[, "coordinateUncertaintyInMeters"] < res), "bdclean.spatialResolution"] <-
            TRUE
    }
    return(bddata)
}

#' Clean data based on earliest date.
#'
#' Clean data based on earliest date.
#'
#' @section samplePassData:
#' When resolution is 20-Jan-2005, records recorded after the date will pass.
#'
#' @section sampleFailData:
#' When resolution is 20-Jan-2005, records recorded before the date will fail.
#'
#' @section targetDWCField:
#' eventDate
#'
#' @section checkCategory:
#' temporal
#'
#' @param bddata Bio diversity data in a data frame
#' @param res The earliest data required
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
#' 
#' responses <- earliest_date(myData, '2000-01-01')
#' 
#' }
#'
#' @export
earliest_date <- function(bddata, res = "1700-01-01") {
    assertive::assert_is_data.frame(bddata)
    assertive::assert_has_cols(bddata)
    
    message(paste("earliestDate:", "\n Removing records above :", res, "\n"))
    dates <- strsplit(res, " ")[[1]]
    bddata <- as.data.frame(bddata)
    ed <- try(as.Date(dates[1], format = "%Y-%m-%d"))
    if (class(ed) == "try-error" || is.na(ed)) {
        warning("That date wasn't correct!")
        return(bddata)
    }
    bddata[, "bdclean.earliestDate"] <- FALSE
    bddata[which(as.Date(bddata[, "eventDate"]) > ed), "bdclean.earliestDate"] <-
        TRUE
    return(bddata)
}

#' Clean data based on temporal resolution
#'
#' Clean data based on temporal resolution
#'
#' @section samplePassData:
#' When resolution is day, records with day specified will pass.
#'
#' @section sampleFailData:
#' When resolution is month, records with NA/empty month specified will fail.
#'
#' @section targetDWCField:
#' day, month, year
#'
#' @section checkCategory:
#' temporal
#'
#' @param bddata Bio diversity data in a data frame
#' @param res restriction of records with/without data, month, year fields
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
#' 
#' responses <- temporal_resolution(myData, 'Day')
#' 
#' }
#'
#' @export
temporal_resolution <- function(bddata, res = "Day") {
    assertive::assert_is_data.frame(bddata)
    assertive::assert_has_cols(bddata)
    
    message(paste(
        "temporalResolution:",
        "\n Removing records above :",
        res,
        "\n"
    ))
    bddata <- as.data.frame(bddata)
    bddata[, "bdclean.temporalResolution"] <- FALSE
    if (res == "Day") {
        bddata[which(!is.na(bddata$day)), "bdclean.temporalResolution"] <-
            TRUE
    }
    if (res == "Month") {
        bddata[which(!is.na(bddata$month)), "bdclean.temporalResolution"] <-
            TRUE
    }
    if (res == "Year") {
        bddata[which(!is.na(bddata$year)), "bdclean.temporalResolution"] <-
            TRUE
    }
    return(bddata)
}
