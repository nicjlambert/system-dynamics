#' Output UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Output_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    
    plotOutput(ns("plot"))
 
  )
}
    
#' Output Server Functions
#'
#' @noRd 
mod_Output_server <- function(id){
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    output$plot <- renderPlot({
      plot(mtcars$mpg)
    })
 
  })
}
    
## To be copied in the UI
# mod_Output_ui("Output_ui_1")
    
## To be copied in the server
# mod_Output_server("Output_ui_1")
