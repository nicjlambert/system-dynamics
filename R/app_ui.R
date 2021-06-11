#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny shinydashboard
#' @noRd
app_ui <- function(request) {
  
  bslib::bs_global_theme()
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    thematic::thematic_shiny(),
    
    # Your application UI logic 
    navbarPage(theme = bslib::bs_theme(primary = "teal", secondary = "orange", bg = "white", fg = "black", success = "green", version = 4),
               "System Dynamics Model",
               tabPanel("About",
      mod_About_ui("About_ui_1")),
               tabPanel("Output",
                            mod_Output_ui("Output_ui_1")
          )
        )
      )

}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'systemdynamics'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}



