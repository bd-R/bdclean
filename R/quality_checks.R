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
#' @export
taxoLevel <- function(bddata, res = "SPECIES") {
    ranks <-
        c("CLASS",
          "ORDER",
          "FAMILY",
          "GENUS",
          "SPECIES",
          "SUBSPECIES")
    res <- toupper(res)
    
    if (!(res %in% ranks)) {
        print("Rank Value unknown. It should be FAMILY, GENUS, SPECIES or SUBSPECIES")
        return(bddata)
    }
    
    idx <- which(ranks == res)
    cat(paste("\n Removing records above :", res, "\n"))
    bddata$bdclean.taxoLevel <- 0
    if (idx > 0) {
        for (i in idx:length(ranks)) {
            bddata[which(bddata$taxonRank == ranks[i]), 'bdclean.taxoLevel'] <- 10
        }
    }
    return(bddata)
}

#' Clean data based on spatial resolution
#'
#' Clean data based on spatial resolution
#' 
#' @section samplePassData:
#' When resolution is 100 meters, Coordinate Uncertainities below 100 meteres will pass.
#' 
#' @section sampleFailData:
#' When resolution is 100 meters, Coordinate Uncertainities above 100 meteres will fail.
#' 
#' @section targetDWCField:
#' coordinateUncertaintyInMeters
#' 
#' @section checkCategory:
#' spatial
#'
#'@export
spatialResolution <- function(bddata, res = 100) {
    #print("fxn Spatial Resoultion")
    cat("spatialResolution")
    bddata$bdclean.taxoLevel <- 0
    res <- as.numeric(res)
    bddata$bdclean.spatialResolution <- 0
    if (res > 0) {
        bddata[which(bddata$coordinateUncertaintyInMeters < res), 'bdclean.spatialResolution'] <- 10
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
#'@export
earliestDate <- function(bddata, res = "1700-01-01") {
    cat("earliestDate")
    bddata <- as.data.frame(bddata)
    ed <- try(as.Date(res, format = "%Y-%m-%d"))
    if (class(ed) == "try-error" || is.na(ed)) {
        print("That date wasn't correct!")
        return(bddata)
    }
    bddata$bdclean.earliestDate <- 0
    bddata[which(as.Date(bddata$eventDate) > ed), 'bdclean.earliestDate'] <- 10
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
#'@export
temporalResolution <- function(bddata, res = "Day") {
    cat("temporalResolution")
    bddata <- as.data.frame(bddata)
    bddata$bdclean.temporalResolution <- 0
    if (res == "Day") {
        bddata[which(!is.na(bddata$day)), "bdclean.temporalResolution"] <- 10
    }
    if (res == "Month") {
        bddata[which(!is.na(bddata$month)), "bdclean.temporalResolution"] <- 10
    }
    if (res == "Year") {
        bddata[which(!is.na(bddata$year)), "bdclean.temporalResolution"] <- 10
    }
    return(bddata)
}

# repeating_digits
# coordinates_decimal_mismatch
# coordinate_precision_outofrange_flag
# uncertainty_outofrange_flag
# country_coordinate_mismatch_flag
# precision_uncertainty_mismatch_flag
# center_of_the_country_coordinates_flag
# coordinate_negated_flag
# scrubr::COORDINATES_ZERO
# scrubr::LATITUDE_OUT_OF_RANGE
#
# georeference_protocol_flag
# georeference_verification_status_flag
# georeference_post_occurrence_flag
# locality_coordinate_mismatch_flag
# stateProvinceCoordinateMismatchFlag
# country_code_unknown_flag
# occurrence_establishment_flag
# county_coordinate_mismatch_flag
# depth_out_of_range_flag
#
#
# first_of_year_flag
#
# identified_pre_event_flag
# impropable_identified_date_flag
