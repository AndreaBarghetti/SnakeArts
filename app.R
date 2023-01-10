library(tidyverse)
library(shiny)

source("R/snake_player.R")

# space <- Space$new(30,30)
# space$fill_space()

ui <- fluidPage(
  
    numericInput(inputId = "nrows",
                 label = "Height",
                 value = 10,
                 min = 2, 
                 max = 200),
    
    numericInput(inputId = "ncols",
                 label = "Width",
                 value = 10,
                 min = 2, 
                 max = 200),
    
    actionButton("fill",label = "Fill"),
    
    plotOutput(outputId = "plot")
)

server <- function(input, output) {
 
  output$plot <- shiny::renderPlot({
    
    req(input$fill)
    
    space <- Space$new(input$nrows,input$ncols)
    space$fill_space()
    space$show()
  })
  
}

shinyApp(ui, server)
