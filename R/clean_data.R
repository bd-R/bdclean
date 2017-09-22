#' Data cleaning according to configuration supplied
#'
#' Use \code{get_config} to generate configuration and pass it to this
#' function to process the data accordingly.
#'
#'@param bddata biodiversity data in a data frame
#'@param config configuration generated using \code{get_config}
#'@param verbose Verbose output if TRUE else brief output if FALSE

#'
#'@return data frame with clean data
#'
#'@examples \dontrun{
#'library(rgbif)
#'occdat1 <- occ_data(
#'  country = "AU",     # Country code for australia
#'  classKey= 359,      # Class code for mammalia
#'  limit=5000,         # Get only 5000 records
#'  )
#'  myData<-occdat1$data
#'  myConfig <- get_config()
#'  cleanData <- clean_data(myData,myConfig)
#'}
#'
#'@export
clean_data <- function(bddata,config,verbose=T){
  if(verbose){
    cat("\n Initial records ...",dim(bddata)[1],"\n")
  }

  for (i in 1:dim(config)[1]){
    cat(paste("\n",config$quest[i],config$response[i],"\n"))
    res <- as.character(config$response[i])
    #print(res)
    switch(as.character(config$quest[i]),
           taxoLevel = {bddata <- taxoLevel(bddata, res)},
           misNames = {bddata <- misNames(bddata, res)},
           spatialResolution = {bddata <- spatialResolution(bddata, res)},
           earliestDate = {bddata <- earliestDate(bddata, res)},
           temporalResolution = {bddata <- temporalResolution(bddata,res)}
    )
    if(verbose){
      cat("\n Records remaining...",dim(bddata)[1],"\n")
    }
  }

  return(bddata)
}

# Support functions that are called within main function

taxoLevel <- function(bddata, res="SPECIES"){
  ranks <- c("CLASS", "ORDER", "FAMILY", "GENUS", "SPECIES", "SUBSPECIES")
  if(!(res %in% ranks)){
    print("Rank Value unknown. It should be FAMILY, GENUS, SPECIES or SUBSPECIES")
    return(bddata)
  }
  idx <- which(ranks==res)
  cat(paste("\n Removing records above :",res,"\n"))
  retmat <- NULL
  if(idx > 0){
    for(i in idx:length(ranks)){
      resmat <- bddata[which(bddata$taxonRank==ranks[i]),]
      retmat <- rbind(retmat,resmat)
    }
  }
  return(retmat)
}

misNames <- function(bddata, res="No"){
  cat("\n fxn misNames not implemented yet \n")
  return(bddata)
}

spatialResolution <- function(bddata, res=100){
  #print("fxn Spatial Resoultion")
  res <- as.numeric(res)
  if(res>0){
    retmat <- bddata[which(bddata$coordinateUncertaintyInMeters<res),]
  }
  return(retmat)
}

earliestDate <- function(bddata, res="1700-01-01"){
  bddata <- as.data.frame(bddata)
  ed <- try( as.Date( res, format= "%Y-%m-%d" ) )
  if( class( ed ) == "try-error" || is.na( ed ) ) {
    print( "That date wasn't correct!" )
    return(bddata)
  }
  retmat <- bddata[which(as.Date(bddata$eventDate)>ed),]
  return(retmat)
}

temporalResolution <- function(bddata,res="Day"){
  bddata <- as.data.frame(bddata)
  if(res=="Day"){
    retmat <- bddata[which(!is.na(bddata$day)),]
  }
  if(res=="Month"){
    retmat <- bddata[which(!is.na(bddata$month)),]
  }
  if(res=="Year"){
    retmat <- bddata[which(!is.na(bddata$year)),]
  }
  return(retmat)
}
