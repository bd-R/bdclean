# UI Function
domainCleaningUI <- function(id) {
    ns <- NS(id)
    
    tagList(
        
      renderUI({
            components <- list()
            
            components[[1]] <- tagList(
                HTML(
                    paste("<input type=radio
                      name=domainInput value=",
                          "as",
                          ">")
                ),
                div(
                    class = "checksListContent",
                    h4("Marine Research"),
                    
                    div(class = "checksListTopic col-sm-3", p("Description: ")),
                    div(
                        class = "checksListTitle",
                        p(
                            "Researches focused on marine species and marine occarance distribution"
                        )
                    ),
                    
                    div(class = "checksListTopic col-sm-3", p("Quality checks performed: ")),
                    div(
                        class = "checksListTitle",
                        p(
                            "depth_out_of_range_flag, country_coordinate_mismatch_flag, precision_uncertainty_mismatch_flag
                        , center_of_the_country_coordinates_flag
                        , coordinate_negated_flag"
                        )
                    ),
                    
                    div(class = "checksListTopic col-sm-3", p("DWC Fields Targetted by Checks: ")),
                    div(class = "checksListTitle", p("coordinates"))
                ),
                br(),
                br()
            )
            
            return(
                div(
                    id = "domainInput",
                    class = "form-group shiny-input-radiogroup shiny-input-container shiny-bound-input",
                    tags$br(),
                    tags$br(),
                    column(width = 12,
                           components)
                )
            )
        })
        
        
    )
}