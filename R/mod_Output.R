#' Output UI Function
#'
#' @description A shiny Module for business logic
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Output_ui <- function(id){
  ns <- NS(id)
  
  tagList(
  
    sidebarLayout(
      sidebarPanel(
        
        
        sliderInput(ns("aCrudeBirthRate"),  "Birth rate per 1,000 population", min=9, max=15, value=12.5, step=0.5),
        sliderInput(ns("aCrudeDeathRate"),  "Death rate per 1,000 population", min=5, max=10, value=6.6, step=0.5),
        sliderInput(ns("aStandardGPProductivity"),  "Average no. of patients seen per day", min=20, max=30, value=24, step=1),
        sliderInput(ns("aAverageGPCareerDuration"),  "Average career duration", min=20, max=50, value=40, step=5),     
        sliderInput(ns("aStandardGPWorkYear"),  "Work Year", min=200, max=360, value=250, step=10),
        checkboxInput(ns("SystemPressureFlag"), "Allow response to system pressure", FALSE),
        actionButton(inputId = ns("simulate"), "Simulate")
      ),
      mainPanel(
        shinycssloaders::withSpinner(
          plotOutput(ns("plot")),
          color = "#008080"
        )
      )
    )
 
  )
}
    
#' Output Server Functions
#'
#' @noRd 
mod_Output_server <- function(id){
  
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns

    data <- reactive({
      
      # setup simulation times and time steps
      begin = 2017
      end = 2053
      timeStep = 1
      # create time vector
      times <- seq(begin, end, by = timeStep)
      
      # create stocks vector with initial values. Inflows and outflows
      # can increase or decrease the stock's value over time
      stocks  <- c(
        
        sPopulation0_14 = 4364000, 
        sPopulation15_39 = 7918000,
        sPopulation40_64 = 7442000,
        sPopulation65_plus = 3676000,
        
        sGeneralPractitioners = 4000,
        ExpectedRetirement = 100,
        sPatientsBeingTreated = 24000000
        
      )
      # create exogenous vector
      parms    <- c(aCrudeBirthRate = as.numeric(input$aCrudeBirthRate),    
                    aCrudeDeathRate = as.numeric(input$aCrudeDeathRate),    
                    aAverageGPVisits0_14 = 3, 
                    aAverageGPVisits15_39 = 4, 
                    aAverageGPVisits40_64 = 5, 
                    aAverageGPVisits65plus = 10, 
                    aStandardGPProductivity = 1.4,    
                    aStandardGPWorkYear = 325,       
                    aTargetCompletionTime = 1,
                    aAverageGPCareerDuration = 40,
                    aDesiredGPsPer1000sPopulation = 0.8 / 1000,
                    aAdjustmentTime = 5,
                    WorkYearFlag = 1,
                    ProductivtyFlag = 1,
                    D1 = 15,
                    D2 = 25,
                    D3 = 25,
                    DC = 3,
                    SystemPressureFlag = TRUE
      )
      
      # in order to simulate the model needs a set of equations that describe the relationship. The are
      # defined in the model above and called in the ode `func`    
      o <- data.frame(deSolve::ode(y = stocks, 
                                   times = times, 
                                   func = systemdynamics::sdm, 
                                   parms = parms, 
                                   method = "euler"))
 
    })
    
    v <- reactiveValues(plot = NULL)
    
    # Whenever the "runMod" button is pressed, render the plot
    observeEvent(input$simulate, {
      
      label <- paste0("Simulate ", input$simulate, " times")
      updateActionButton(inputId = "simulate", label = label)
      
        v$p1 <- ggplot2::ggplot()+
          ggplot2::geom_line(data=data(), ggplot2::aes(x=time, y=sPopulation, color="Population"), size=1.15) +
          ggplot2::geom_point() +
          ggplot2::scale_y_continuous(labels = scales::comma) +
          ggplot2::ylab("Population")+
          ggplot2::xlab("Year") +
          ggplot2::labs(color="")
        
        v$p2 <- ggplot2::ggplot()+
          ggplot2::geom_line(data=o, ggplot2::aes(x=time, y=DesiredGPs, color="DesiredGPs"), linetype="dashed", size=1.15) +
          ggplot2::geom_line(data=o, ggplot2::aes(x=time, y=sGeneralPractitioners, color="GPs"), size=1.15) +
          ggplot2::scale_y_continuous(labels = scales::comma) +
          ggplot2::ylab("GPs (Target & Actual)") +
          ggplot2::xlab("Year") +
          ggplot2::labs(color="")

      })
      
    output$plot <- renderPlot({
      if (is.null(v$p1)) return()
      gridExtra::grid.arrange(v$p1, v$p2, 
                              ncol=2)
    })
})
  

  
}
    
## To be copied in the UI
# mod_Output_ui("Output_ui_1")
    
## To be copied in the server
# mod_Output_server("Output_ui_1")
