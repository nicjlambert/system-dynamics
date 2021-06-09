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
    plotOutput(ns("plot"))
  )
}
    
#' About Server Functions
#'
#' @noRd 
mod_About_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$plot <- renderPlot({
      plot(mtcars$mpg)
    })
  })
}
    
## To be copied in the UI
# mod_About_ui("About_ui_1")
    
## To be copied in the server
# mod_About_server("About_ui_1")
