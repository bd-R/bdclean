#' Data cleaning according to Questionnaire Responses.
#'
#' Use \code{run_questionnaire} to add Questionnaire Responses and pass it to this
#' function to process the data faster.
#'
#' Use \code{create_default_questionnaire} to create default questionnaire object.
#' You can add your custom questions to this questionnaire and then pass it to this
#' function to process the data.
#'
#' @param data Biodiversity data in a data frame
#' @param custom_questionnaire Custom user created questionnaire responses if to pypass answering questions each time.
#' @param clean Whether to clean after flagging. If false only flagging will be done.
#' @param missing How to treat data with missing values. Default: false - will be treated as bad data.
#' @param report Whether to print report of cleaning done.
#' @param format Formats of the cleaning report required. Options are: Markdown, HTML or / and PDF
#'
#' @return data frame with clean data
#'
#' @examples
#' 
#' custom_questionnaire <- create_default_questionnaire()
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
#' responses <- run_questionnaire()
#' cleaned_data <- clean_data(myData, responses)
#' 
#' cleaned_data2 <- clean_data(myData)
#' 
#' } 
#'
#' @export
clean_data <-
    function(data,
             custom_questionnaire = NULL,
             clean = TRUE,
             missing = FALSE,
             report = TRUE,
             format = c("html_document", "pdf_document")) {
        
        assertive::assert_is_data.frame(data)
        assertive::assert_has_cols(data)
        assertive::assert_is_logical(clean)
        assertive::assert_is_logical(missing)
        assertive::assert_is_logical(report)
        assertive::assert_is_character(format)
        if(!all(format %in% c("html_document", "pdf_document"))){
            stop("Format can only be one of html_document, pdf_document")
        }
        if(!assertive::is_inherited_from(custom_questionnaire, "BdQuestionContainer") & !is.null(custom_questionnaire)){
            stop("custom_questionnaire should be of type BdQuestionContainer")
        }
        
        responses <- list()
        input_data <- data
        flagged_data <- data
        cleaned_data <- data
        
        # Questionnaire
        if (is.null(custom_questionnaire)) {
            responses <- run_questionnaire()
        } else {
            responses <- custom_questionnaire
        }
        
        # Flagging
        flagged_data <- responses$flag_data(input_data, missing)
        
        # Decision Making
        if (clean) {
            cleaned_data <- cleaning_function(flagged_data)
        }
        
        # Report
        if (report) {
            create_report_data(data,
                               flagged_data,
                               cleaned_data,
                               responses,
                               clean,
                               format)
        }
        
        # Cleaning
        if (clean) {
            assertive::assert_is_data.frame(cleaned_data)
            return(cleaned_data)
        }
        
        assertive::assert_is_data.frame(cleaned_data)
        return(flagged_data)
    }


#' Execute the Questionnaire and save user responses.
#'
#'
#' @param custom_questionnaire Custom User Created Questionnaire if already available.
#'
#' @return list with BdQuestionObjects containing user answers
#'
#' @examples
#' 
#' if(interactive()){ 
#'
#' responses <- run_questionnaire()
#' 
#' }
#'
#' @export
run_questionnaire <- function(custom_questionnaire = NULL) {
    responses <- list()
    if (is.null(custom_questionnaire)) {
        message("Custom Questionnaire not given. Using package default Questionnaire...")
        responses <- create_default_questionnaire()
    } else {
        if (class(custom_questionnaire) != "BdQuestionContainer") {
            message(
                "Provided Custom Questionnaire is not of class BdQuestionContainer.
                Using package default Questionnaire"
            )
            responses <- create_default_questionnaire()
        } else {
            message("Custom Questionnaire detected.")
            responses <- custom_questionnaire
        }
    }
    message("Please answer the following questions to initiate cleaning process.")
    for (question in responses$bdquestions) {
        if (question$question.type != "Child" &&
            question$question.type != "ChildRouter") {
            get_user_response(question)
        }
    }
    message("Thank you! Cleaning can be started now based on your responses.")
    return(responses)
}

#' Internal function for getting user response
#'
#' @param bd_question The BDQuestion object to get users responses.
#'
#' @examples
#' 
#' if(interactive()){ 
#'
#' question <- BdQuestion()
#' responses <- get_user_response(question)
#' 
#' }
get_user_response <- function(bd_question) {
    assertive::assert_is_inherited_from(bd_question, "BdQuestion")
    
    # Child & ChildRouter already filtered in first loop above
    if (bd_question$question.type == "Atomic") {
        # Atomic is filtered
        bd_question$print_question()
        bd_question$get_response()
    } else {
        # Router , Child as child & ChildRouter as child is filtered
        bd_question$print_question()
        bd_question$get_response()
        if (bd_question$users.answer %in% bd_question$router.condition) {
            for (question in bd_question$child.questions) {
                get_user_response(question)
            }
        }
    }
}
