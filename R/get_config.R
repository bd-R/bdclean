#' Get user inputs about data cleaning protocol to follow
#'
#' \code{get_config} asks user a set of questions and the answers are stored
#' in configuration variable. This variable will be used to clean the data
#' depending on the responses
#'
#'@importFrom utils menu
#'
#'@param quest data.frame containing set of questions to be asked {not implemented  yet}
#'@param options list of data frames with options for each questions {not implemented yet}
#'
#'@return data frame with the responses
#'
#'@examples \dontrun{
#' myConfig <- get_config()
#'}
#'
#'@export

get_config <- function(quest=NULL,options=NULL){
  if (is.null(quest)){
    quest <- data.frame(qvar=c("taxoLevel", "misNames", "spatialResolution", "dateCheck",
                               "earliestDate", "temporalResolution"),
                        question=c("What is the lowest taxonomic level you require in your data? ",
                                   "What you want to do with data with mismatched names? ",
                                   "What is the spatial resolution required for your data ",
                                   "Do you care about dates of your observations",
                                   "What is the earliest date of your observations in this data set (date format-> %Y-%m-%d) ",
                                   "What temporal resolution are you interested in? "),
                        rtype=c("I_Numeric","I_Numeric","Numeric","I_Numeric","Date","I_Numeric"),
                        rlimit=c(4,2,0,2,0,3),
                        mtype=c("m","m","v","m","v","m"),stringsAsFactors = F)
  }
  if (is.null(options)){  options<-list(taxoLevel=data.frame(choice=c(1,2,3,4),value=c("SUBSPECIES", "SPECIES","GENUS","FAMILY")),
                misNames=data.frame(choice=c(1,2),value=c("Yes","No")),
                spatialResolution=NULL,
                dateCheck=data.frame(choice=c(1,2),value=c("Yes","No")),
                earliestDate=NULL,
                temporalResolution=data.frame(choice=c(1,2,3),value=c("Year","Month","Day")))
  }
  quest1<-(quest)
  res<-vector(mode="character",length = nrow(quest))
  for (i in 1:nrow(quest)){
    if(quest$mtype[i]=="m"){
      rval <- menu(options[[quest[i,1]]]$value,title = quest[i,2] )
    } else {
      rval <- readline(prompt=quest[i,2])
    }
    # --- Needs to be checked. Directly borrowed form Ashwin ---
    if(quest[i,3]=="Numeric"){
      res[i]=rval
    }
    if(quest[i,3]=="Date"){
      if(IsDate(rval)){
        res[i]=rval
      }
      else{
        stop(paste("The input in wrong or the date format is not correct"))
      }
    }

    if(quest[i,3]=="I_Numeric"){

      if(rval<=quest[i,4] && rval>=1){
        qvar<-quest[i,1]
        data<-as.data.frame(options[qvar])
        colnames(data)<-c("choice","value")
        res[i]<-as.character((data[data$choice==rval,2]))
        quest1[i,3]="Numeric"
      }
      else{
        stop(paste("The entered choice is wrong."))
      }
    }
    # --- upto here ---
  }
  response<-data.frame(quest=quest1[,1],response=res)
  return(response)
}


# Functions for input.
IsDate <- function(mydate, date.format = "%Y-%m-%d") {
  tryCatch(!is.na(as.Date(mydate, date.format)),
           error = function(err) {FALSE})
}
