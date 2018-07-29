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
    cat(paste("taxoLevel:", "\n Removing records above :", res, "\n"))
    bddata$bdclean.taxoLevel <- 0
    if (idx > 0) {
        for (i in idx:length(ranks)) {
            bddata[which(bddata$taxonRank == ranks[i]), 'bdclean.taxoLevel'] <-
                10
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
    cat(paste("spatialResolution:", "\n Removing records above :", res, "\n"))
    bddata$bdclean.taxoLevel <- 0
    res <- as.numeric(res)
    bddata$bdclean.spatialResolution <- 0
    if (res > 0) {
        bddata[which(bddata$coordinateUncertaintyInMeters < res), 'bdclean.spatialResolution'] <-
            10
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
    cat(paste("earliestDate:", "\n Removing records above :", res, "\n"))
    dates <- strsplit(res, " ")[[1]]
    bddata <- as.data.frame(bddata)
    ed <- try(as.Date(dates[1], format = "%Y-%m-%d"))
    if (class(ed) == "try-error" || is.na(ed)) {
        print("That date wasn't correct!")
        return(bddata)
    }
    bddata$bdclean.earliestDate <- 0
    bddata[which(as.Date(bddata$eventDate) > ed), 'bdclean.earliestDate'] <-
        10
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
    cat(paste("temporalResolution:", "\n Removing records above :", res, "\n"))
    bddata <- as.data.frame(bddata)
    bddata$bdclean.temporalResolution <- 0
    if (res == "Day") {
        bddata[which(!is.na(bddata$day)), "bdclean.temporalResolution"] <-
            10
    }
    if (res == "Month") {
        bddata[which(!is.na(bddata$month)), "bdclean.temporalResolution"] <-
            10
    }
    if (res == "Year") {
        bddata[which(!is.na(bddata$year)), "bdclean.temporalResolution"] <-
            10
    }
    return(bddata)
}


# ------------------- Additional Checks ---------------------------

#' Flag repeating digits
#'
#' Runs quality check of finding repeated digits and flags accordingly.
#'
#' The function runs a quality check on two fields of GBIF data - decimalLatitude and decimalLongitude.
#' Checks if the decimal values have repeated values at the end of the value. Repeated values might
#' mean error in digitisation or conversions of records
#' @import  data.table
#' @export
#' @author thiloshon <thiloshon@@gmail.com>
#' @param gbif_data Dataframe from GBIF with two mandatory fields; decimalLatitude and decimalLongitude.
#' @return Same dataframe with two additional columns; latRepeatCount and longRepeatCount. Both shows the number of digits that are repeating
#' @examples
#' dat <- rgbif::occ_data(scientificName = 'Ursus americanus')
#' flagged_dat <- repeating_digits(dat$data)
#' 
#' @section samplePassData:
#' lat - 19.5518, long 113.8497.
#'
#' @section sampleFailData:
#' lat - 19.5555, long 113.3777.
#'
#' @section targetDWCField:
#' decimalLongitude, decimalLatitude
#'
#' @section checkCategory:
#' spatial
repeating_digits <- function(gbif_data) {
    t <- Sys.time()
    
    gbif_data <- format_checking(gbif_data,
                                 c("decimalLatitude", "decimalLongitude"))
    
    # -------------- Finding records with repeated digits ---------------------------------------------- #
    latRepeat <-
        sapply(gbif_data$decimalLatitude, function(lat) {
            x <- as.character.numeric_version(lat)
            rmword <- grepl("(\\w)\\1{2, }", x)
            return(rmword)
        })
    
    longRepeat <-
        sapply(gbif_data$decimalLongitude, function(long) {
            x <- as.character.numeric_version(long)
            rmword <- grepl("(\\w)\\1{2, }", x)
            return(rmword)
        })
    
    # -------------- End of Finding records with repeated digits --------------------------------------- #
    
    # -------------- Finding number of repeated digits of Latitude and Flagging------------------------- #
    gbif_data$latRepeatCount  <-
        sapply(1:dim(gbif_data)[1], function(counter) {
            if (latRepeat[counter]) {
                lat <-
                    as.character.numeric_version(gbif_data[counter, c("decimalLatitude")])
                
                list = as.vector(strsplit(lat, ""))
                table <- data.table::as.data.table(list)
                frameCount <-
                    table[, count := sequence(.N), by = rleid(V1)][V1 == "No", count := 0][]
                
                max(frameCount$count)
            } else{
                0
            }
        })
    
    # -------------- End of Finding number of repeated digits of Latitude and Flagging-------------------- #
    
    # -------------- Finding number of repeated digits of Longitude and Flagging------------------------- #
    gbif_data$longRepeatCount <-
        sapply(1:dim(gbif_data)[1], function(counter) {
            if (longRepeat[counter]) {
                long <-
                    as.character.numeric_version(gbif_data[counter, c("decimalLongitude")])
                
                list = as.vector(strsplit(long, ""))
                table <- data.table::as.data.table(list)
                frameCount <-
                    table[, count := sequence(.N), by = rleid(V1)][V1 == "No", count := 0][]
                
                max(frameCount$count)
            } else{
                0
            }
        })
    # -------------- End of Finding number of repeated digits of Longitude and Flagging------------------- #
    
    message(paste("Time difference of " , Sys.time() - t, " seconds", sep = ""))
    return(gbif_data)
}


#' Flag coordinates with decimal points mismatch.
#'
#' Runs quality check of checking coordinates with more than 3 decimal point difference.
#'
#' The function runs a quality check on coordinates of GBIF data to check if latitude and longitude have
#' different number of decimal points. If coordinate was recorded by an accepted protocol, its unlikely to
#' identify both coordinate with varying precision.
#' @export
#' @author thiloshon <thiloshon@@gmail.com>
#' @param gbif_data Dataframe from GBIF with two mandatory fields; decimalLatitude, decimalLongitude
#' @return Same dataframe with one additional column; decimalPointDifference
#' @examples
#' dat <- rgbif::occ_data(scientificName = 'Ursus americanus')
#' flagged_dat <- coordinates_decimal_mismatch(dat$data)
#' 
#' @section samplePassData:
#' lat - 19.5518, long 113.8497.
#'
#' @section sampleFailData:
#' lat - 19.55, long 113.3777.
#'
#' @section targetDWCField:
#' decimalLatitude, decimalLongitude
#'
#' @section checkCategory:
#' spatial
coordinates_decimal_mismatch <- function(gbif_dataFrame) {
    t <- Sys.time()
    
    gbif_data <- format_checking(gbif_dataFrame,
                                 c("decimalLatitude", "decimalLongitude"))
    
    
    lat <- sapply(gbif_dataFrame$decimalLatitude, function(lat) {
        list <- strsplit(sub('0+$', '', as.character.numeric_version(lat)),
                         ".",
                         fixed = TRUE)
        
        if (length(list[[1]]) < 2) {
            return (0)
        } else {
            return (nchar(list[[1]][[2]]))
        }
    })
    
    long <- sapply(gbif_dataFrame$decimalLongitude, function(long) {
        list <- strsplit(sub('0+$', '', as.character.numeric_version(long)),
                         ".",
                         fixed = TRUE)
        if (length(list[[1]]) < 2) {
            return (0)
        } else {
            return (nchar(list[[1]][[2]]))
        }
    })
    
    gbif_dataFrame$lat <- lat
    gbif_dataFrame$lon <- long
    
    gbif_dataFrame$decimalPointDifference <- abs(lat - long)
    
    message(paste("Time difference of " , Sys.time() - t, " seconds", sep = ""))
    return(gbif_dataFrame)
}



#' Flag records georeferenced after the event date
#'
#' Runs quality check of checking if georeferencing was done after event date.
#'
#' The function collects the time in years between the occurrence date and the georeferenced date.
#' If the record was georeferenced on the day it was occurred, then the record will most likely to be correct. But,
#' if the record was georeferenced several years later, then the reliability will be low. This flag brings that out.
#' @export
#' @import parsedate
#' @author thiloshon <thiloshon@@gmail.com>
#' @param gbif_data Dataframe from GBIF with two mandatory fields; georeferencedDate and eventDate
#' @return Same dataframe with one additional column; georeferencePostOccurrenceFlag
#' @examples
#' dat <- rgbif::occ_data(scientificName = 'Ursus americanus')
#' flagged_dat <- georeference_post_occurrence_flag(dat$data)
#' 
#' @section samplePassData:
#' If the record was georeferenced on the day it was occurred
#'
#' @section sampleFailData:
#' If the record was georeferenced several years later
#'
#' @section targetDWCField:
#' georeferencedDate, eventDate
#'
#' @section checkCategory:
#' temporal
georeference_post_occurrence_flag <- function(gbif_data) {
    t <- Sys.time()
    
    gbif_data <- format_checking(gbif_data,
                                 c("georeferencedDate", "eventDate"))
    
    logical <- gbif_data$georeferencedDate != "" & !is.na(gbif_data$georeferencedDate)
    subset <- gbif_data[logical, ]
    
    
    eventDate <- parsedate::parse_date(subset$eventDate)
    referencedDate <- parsedate::parse_date(subset$georeferencedDate)
    
    diffInYears <- as.numeric(referencedDate - eventDate) / 365.242
    diffInYears <- as.integer(diffInYears)
    gbif_data$georeferencePostOccurrenceFlag <- NA
    gbif_data$georeferencePostOccurrenceFlag[logical] <- diffInYears
    
    message(paste("Time difference of " , Sys.time() - t, " seconds", sep = ""))
    return(gbif_data)
}



#' Flag records with incorrect coordinatePrecision
#'
#' Runs quality check of checking if coordinatePrecision is wthin possible range.
#'
#' The coordinate precision (dwc:coordinatePrecision), as a decimal representation, is outside the range of zero (minimum) and
#' one (maximum) coordinatePrecision /=>0<=1.
#' coordinatePrecision is a measure of precision of the coordinates. It can take only values between 0 and 1 for 1 being
#' highly precise and 0 being no precise at all. This check will flag all records with precision out of this range.
#' @export
#' @author thiloshon <thiloshon@@gmail.com>
#' @param gbif_data Dataframe from GBIF with one mandatory field; coordinatePrecision
#' @return Same dataframe with one additional column; coordinatePrecisionOutofRangeFlag
#' @examples
#' dat <- finch::dwca_read("sources/GBIF Downloaded Files/DwC/0090371-160910150852091.zip", read=TRUE)
#' flagged_dat <- coordinate_precision_outofrange_flag(dat$data$occurrence.txt)
#' 
#' @section samplePassData:
#' 0 <= coordinatePrecision <= 1
#'
#' @section sampleFailData:
#' 0 <= coordinatePrecision <= 1
#'
#' @section targetDWCField:
#' coordinatePrecision
#'
#' @section checkCategory:
#' spatial
coordinate_precision_outofrange_flag <- function(gbif_data) {
    t <- Sys.time()
    
    gbif_data <- format_checking(gbif_data,
                                 c("coordinatePrecision"))
    
    gbif_data$coordinatePrecisionOutofRangeFlag <-
        gbif_data$coordinatePrecision < 0 |
        gbif_data$coordinatePrecision > 1
    
    message(paste("Time difference of " , Sys.time() - t, " seconds", sep = ""))
    return(gbif_data)
}



#' Flag records with coordinates of center of the country
#'
#' Flag records with decimalLatitude/decimalLongitude=spatial buffered centre of country
#'
#' When coordinates are not known auto georefences makes the center of the country the coordinate. This flag brings that out
#' @export
#' @import ggmap
#' @author thiloshon <thiloshon@@gmail.com>
#' @param gbif_data Dataframe from GBIF with two mandatory fields; "decimalLatitude", "decimalLongitude"
#' @return Same dataframe with one additional column; centerofTheCountryCoordinatesFlag
#' @examples
#' dat <- rgbif::occ_data(scientificName = 'Ursus americanus')
#' flagged_dat <- center_of_the_country_coordinates_flag(dat$data)
#' 
#' @section samplePassData:
#' decimalLatitude/decimalLongitude=spatial buffered centre of country
#'
#' @section sampleFailData:
#' decimalLatitude/decimalLongitude=spatial buffered centre of country
#'
#' @section targetDWCField:
#' decimalLatitude, decimalLongitude
#'
#' @section checkCategory:
#' spatial
center_of_the_country_coordinates_flag <- function(gbif_data) {
    t <- Sys.time()
    
    gbif_data <- format_checking(gbif_data,
                                 c("decimalLatitude", "decimalLongitude"))
    
    center <- ggmap::geocode("Australia")
    
    lat <- as.integer(center$lat)
    lon <- as.integer(center$lon)
    
    gbif_data$centerofTheCountryCoordinatesFlag <-
        (
            as.integer(gbif_data$decimalLatitude) == lat &
                as.integer(gbif_data$decimalLongitude) == lon
            
            # > sumFac(australianMammals$decimalLatitude==-24  & australianMammals$decimalLongitude==134)
            # FALSE    TRUE    NA's
            # 1722404      10   35779
            
            # > geocode("Australia")
            # lon      lat
            # 1 133.7751 -25.2744
            
            # (gbif_data$decimalLatitude) == lat &
            #     (gbif_data$decimalLongitude) == lon
        )
    
    message(paste("Time difference of " , Sys.time() - t, " seconds", sep = ""))
    return(gbif_data)
}

#' Flag records with coordinates mismatching country
#'
#' Geographic coordinates fall outside the area defined by the referenced terrestrial boundary of the country
#'
#' @export
#' @import maps
#' @author thiloshon <thiloshon@@gmail.com>
#' @param gbif_data Dataframe from GBIF with two mandatory fields; "decimalLatitude", "decimalLongitude"
#' @return Same dataframe with two additional columns; countryCoordinateMismatchFlag, generatedCountries
#' @examples
#' dat <- rgbif::occ_data(scientificName = 'Ursus americanus')
#' flagged_dat <- country_coordinate_mismatch_flag(dat$data)
#' 
#' @section samplePassData:
#' doordinates mismatching country
#'
#' @section sampleFailData:
#' oordinates mismatching country
#'
#' @section targetDWCField:
#' decimalLatitude, decimalLongitude, country
#'
#' @section checkCategory:
#' spatial
country_coordinate_mismatch_flag <- function(gbif_data) {
    t <- Sys.time()
    
    gbif_data <- format_checking(gbif_data,
                                 c("decimalLatitude", "decimalLongitude"))
    
    gbif_data$countryCoordinateMismatchFlag <- NA
    gbif_data$generatedCountries <- NA
    
    logical <- !is.na(gbif_data$decimalLatitude)
    
    gbif_data[logical, ]$generatedCountries <-
        maps::map.where(database = "world",
                        gbif_data[logical, ]$decimalLongitude,
                        gbif_data[logical, ]$decimalLatitude)
    
    gbif_data[logical, ]$countryCoordinateMismatchFlag <-
        !grepl("Australia", gbif_data[logical, ]$generatedCountries)
    
    message(paste("Time difference of " , Sys.time() - t, " seconds", sep = ""))
    return(gbif_data)
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
