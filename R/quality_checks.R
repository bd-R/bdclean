#' Clean data based on lower taxon level
#'
#' Clean data based on lower taxon level
#'
#'@export
taxoLevel <- function(bddata, res = "SPECIES") {
    ranks <-
        c("CLASS",
          "ORDER",
          "FAMILY",
          "GENUS",
          "SPECIES",
          "SUBSPECIES")
    if (!(res %in% ranks)) {
        print("Rank Value unknown. It should be FAMILY, GENUS, SPECIES or SUBSPECIES")
        return(bddata)
    }
    idx <- which(ranks == res)
    cat(paste("\n Removing records above :", res, "\n"))
    retmat <- NULL
    if (idx > 0) {
        for (i in idx:length(ranks)) {
            resmat <- bddata[which(bddata$taxonRank == ranks[i]), ]
            retmat <- rbind(retmat, resmat)
        }
    }
    return(retmat)
}

#' Clean data based on spatial resolution
#'
#' Clean data based on spatial resolution
#'
#'@export
spatialResolution <- function(bddata, res = 100) {
    #print("fxn Spatial Resoultion")
    res <- as.numeric(res)
    if (res > 0) {
        retmat <-
            bddata[which(bddata$coordinateUncertaintyInMeters < res), ]
    }
    return(retmat)
}

#' Clean data based on learliest date.
#'
#' Clean data based on learliest date.
#'
#'@export
earliestDate <- function(bddata, res = "1700-01-01") {
    bddata <- as.data.frame(bddata)
    ed <- try(as.Date(res, format = "%Y-%m-%d"))
    if (class(ed) == "try-error" || is.na(ed)) {
        print("That date wasn't correct!")
        return(bddata)
    }
    retmat <- bddata[which(as.Date(bddata$eventDate) > ed), ]
    return(retmat)
}

#' Clean data based on temporal resolution
#'
#' Clean data based on temporal resolution
#'
#'@export
temporalResolution <- function(bddata, res = "Day") {
    bddata <- as.data.frame(bddata)
    if (res == "Day") {
        retmat <- bddata[which(!is.na(bddata$day)), ]
    }
    if (res == "Month") {
        retmat <- bddata[which(!is.na(bddata$month)), ]
    }
    if (res == "Year") {
        retmat <- bddata[which(!is.na(bddata$year)), ]
    }
    return(retmat)
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
