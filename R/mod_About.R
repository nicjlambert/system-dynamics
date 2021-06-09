#' About UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_About_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4("A system dynamics representation of the health sector"),
    br(),
    p("This is an implemention of the model from Jim Duggan's paper: System 
    Dynamics Modeling with R (Lecture Notes in Social Networks), Duggan, Jim."),
    p("Systems thinking is a departure from linear thinking, as system dynamics 
    attempts to understand nonlinear behaviour of participants over time using 
    stocks, flows, feedback loops, table functions and other functions such as 
    time delays.")
  )
}
    
#' About Server Functions
#'
#' @noRd 
mod_About_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
    
## To be copied in the UI
# mod_About_ui("About_ui_1")
    
## To be copied in the server
# mod_About_server("About_ui_1")
