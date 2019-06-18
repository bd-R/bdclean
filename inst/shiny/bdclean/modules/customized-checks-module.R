# UI Function
customizedCheckUI <- function(id) {
    ns <- NS(id)
    
    tagList(
        div(class = "secondaryHeaders", h3("Option 02: Customized Checks")),
        helpText(
            "Note: Select the quality checks you prefer and
                                        continue cleaning with just those checks"
        ),
        
        # -------------------------------
        
        uiOutput(ns("qualityChecks"))
        
        # -------------------------------
    )
}

# Server Function
customizedCheck <- function(input, output, session) {
    ns <- session$ns
    qualityChecks <- bdclean::get_checks_list()
    
    output$qualityChecks <- renderUI({
        components <- list()
        
        for (i in 1:length(qualityChecks)) {
            components[[i]] <- tagList(
                            HTML(
                                paste(
                                    "<input type=checkbox
                                    name=typeInput value=",
                                    data_store$qualityChecks[[i]]$name_of_quality_check,
                                    ">"
                                )
                            ),
                            div(
                                class = "checksListContent",
                                h4(data_store$qualityChecks[[i]]$name_of_quality_check),

                                div(class = "checksListTopic col-sm-3", p("Description: ")),
                                div(
                                    class = "checksListTitle",
                                    p(data_store$qualityChecks[[i]]$description)
                                ),

                                div(class = "checksListTopic col-sm-3", p("Sample Passing Data: ")),
                                div(
                                    class = "checksListTitle",
                                    p(data_store$qualityChecks[[i]]$sample_pass_data)
                                ),

                                div(class = "checksListTopic col-sm-3", p("Sample Failing Data: ")),
                                div(
                                    class = "checksListTitle",
                                    p(data_store$qualityChecks[[i]]$sample_fail_data)
                                ),

                                div(class = "checksListTopic col-sm-3", p("Category of Quality Check: ")),
                                div(
                                    class = "checksListTitle",
                                    p(data_store$qualityChecks[[i]]$check_category)
                                ),

                                div(class = "checksListTopic col-sm-3", p(
                                    "DWC Field Targetted by Check: "
                                )),
                                div(
                                    class = "checksListTitle",
                                    p(data_store$qualityChecks[[i]]$target_dwc_field)
                                )
                            ),
                            br(),
                            br()
                        )
        }
        
        return(
            div(
                id = "typeInput",
                class = "form-group shiny-input-checkboxgroup shiny-input-container shiny-bound-input",
                tags$br(),
                tags$br(),
                column(width = 12,
                       components)
            )
        )
    })
    
}