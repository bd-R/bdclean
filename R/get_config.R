#' Get user inputs about data cleaning protocol to follow
#'
#' \code{get_config} asks user a set of questions and the answers are stored
#' in configuration variable. This variable will be used to clean the data
#' depending on the responces
#'
#'@importFrom utils menu
#'
#'@param quest data.frame containing set of questions to be asked
#'
#'@return data frame with the responces, which would be passed on to \code{clean_data}
#'
#'@examples \dontrun{
#' myConfig <- get_config()
#'}
#'
#'@export
get_config <- function(quest = NULL) {
    data(quest, envir = environment())
    data(responses, envir = environment())
    quest1 <- (quest)
    res <- vector(mode = "character", length = nrow(quest))
    for (i in 1:nrow(quest)) {
        if (quest$qlink[i] > 0) {
            if (res[quest$qlink[i]] != quest$rescond[i]) {
                next
            }
        }
        if (quest$mtype[i] == "m") {
            rval <- menu(responses[[quest[i, 1]]]$value, title = quest[i, 2])
        } else {
            rval <- readline(prompt = quest[i, 2])
        }
        if (quest[i, 3] == "Numeric") {
            res[i] = rval
        }
        if (quest[i, 3] == "Date") {
            if (IsDate(rval)) {
                res[i] = rval
            }
            else{
                stop(paste(
                    "The input in wrong or the date format is not correct"
                ))
            }
        }
        if (quest[i, 3] == "I_Numeric") {
            if (rval <= quest[i, 4] && rval >= 1) {
                qvar <- quest[i, 1]
                data <- as.data.frame(responses[qvar])
                colnames(data) <- c("choice", "value")
                res[i] <- as.character((data[data$choice == rval, 2]))
                quest1[i, 3] = "Numeric"
            }
            else{
                stop(paste("The entered choice is wrong."))
            }
        }
    }
    response <- data.frame(quest = quest1[, 1], response = res)
    return(response)
}


# Functions for input.
IsDate <- function(mydate, date.format = "%Y-%m-%d") {
    tryCatch(
        !is.na(as.Date(mydate, date.format)),
        error = function(err) {
            FALSE
        }
    )
}
