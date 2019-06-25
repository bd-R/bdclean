# UI Function
questionnaireUI <- function(id) {
    ns <- NS(id)
    
    tagList(
        div(class = "secondaryHeaders", h3("Option 01: Questionnaire")),
        helpText(
            "Note: If you have limited knowledge in Biodiversity data,
                                        this option is preferred.",
            "Answer a few questions and let bdclean take care of the cleaning."
        ),
        
        
        # -------------------------------
        
        uiOutput(ns("questionnaire"))
        
        # -------------------------------
    )
}

# Server Function
questionnaire <- function(input, output, session, bdquestions) {
    ns <- session$ns
    
    output$questionnaire <- renderUI({
        components <- list()
        
        for (question in bdquestions) {
            if (question$question.type != "Child" &&
                question$question.type != "ChildRouter") {
                components <- createUIContainer(question, components, input)
            }
        }
        
        return(components)
    })
}


# Helper Functions


createUIContainer <- function(bdQuestion, components, input) {
    val <- length(components) + 1
    components[[val]] <- createQuestionsUI(bdQuestion, val)
    val <- val + 1
    
    for (question in bdQuestion$child.questions) {
        components[[val]] <- conditionalPanel(
            condition = paste("input.",
                              bdQuestion$question.id,
                              " == true",
                              sep = ""),
            div(class = "subSpan", createQuestionsUI(question, val, input))
            
        )
        val <- val + 1
    }
    
    return(components)
}

createQuestionsUI <- function(question, index, input) {
    return(switch(
        question$ui.type,
        "single-checkbox" = tagList(
            h4(paste(index, question$question, sep = ") ")),
            checkboxInput(question$question.id,
                          label = "Yes",
                          value = FALSE),
            br()
        ),
        
        "select" = tagList(
            h4(paste(index, question$question, sep = ") ")),
            selectInput(
                question$question.id,
                label = "",
                choices = setNames(
                    as.character(question$possible.responses),
                    question$possible.responses
                )
            ),
            br()
        ),
        "radio" = tagList(
            h4(paste(index, question$question, sep = ") ")),
            radioButtons(
                question$question.id,
                label = "",
                choices = setNames(
                    as.character(question$possible.responses),
                    question$possible.responses
                )
            ),
            br()
        ),
        "numericInput" = tagList(
            h4(paste(index, question$question, sep = ") ")),
            checkboxInput(
                paste(question$question.id, "_ctrl", sep = ""),
                label = "Enabled Question",
                value = TRUE
            ),
            
            conditionalPanel(
                paste(
                    'input[["',
                    paste(question$question.id, "_ctrl", sep = ""),
                    '"]] == true',
                    sep = ""
                ),
                numericInput(question$question.id,
                             label = "",
                             value = 1)
            ),
            
            br()
        ),
        
        "date-range" = tagList(
            h4(paste(index, question$question, sep = ") ")),
            dateRangeInput(question$question.id,
                           label = ""),
            br()
        )
    ))
}
