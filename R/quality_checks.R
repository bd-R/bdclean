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
    bddata$bdclean.taxoLevel <- FALSE
    if (idx > 0) {
        for (i in idx:length(ranks)) {
            bddata[which(bddata$taxonRank == ranks[i]), 'bdclean.taxoLevel'] <-
                TRUE
        }
    }
    
    print(bddata$bdclean.taxoLevel)
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
    cat(paste(
        "spatialResolution:",
        "\n Removing records above :",
        res,
        "\n"
    ))
    res <- as.numeric(res)
    bddata$bdclean.spatialResolution <- FALSE
    if (res > 0) {
        bddata[which(bddata$coordinateUncertaintyInMeters < res), 'bdclean.spatialResolution'] <-
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
    bddata$bdclean.earliestDate <- FALSE
    bddata[which(as.Date(bddata$eventDate) > ed), 'bdclean.earliestDate'] <-
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
#'@export
temporalResolution <- function(bddata, res = "Day") {
    cat(paste(
        "temporalResolution:",
        "\n Removing records above :",
        res,
        "\n"
    ))
    bddata <- as.data.frame(bddata)
    bddata$bdclean.temporalResolution <- FALSE
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