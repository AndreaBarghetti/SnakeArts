library(tidyverse)
library(shiny)

source("R/snake_player.R")

ui <- fluidPage(
  
  sidebarLayout(sidebarPanel = sidebarPanel(
    sliderInput(inputId = "nrows",
                label = "Height",
                value = 20,
                min = 2, 
                max = 150),
    
    sliderInput(inputId = "ncols",
                label = "Width",
                value = 20,
                min = 2, 
                max = 150),
    
    sliderInput(inputId = "linewidth",
                label = "linewidth",
                value = 3,
                min = .1, 
                max = 10),
    
    sliderInput(inputId = "dotsize",
                label = "dotsize",
                value = 3,
                min = .1, 
                max = 10),
    
    selectInput("palette",label = "Palatte",choices = LETTERS[1:8],selected = "A"),
    
    actionButton("move_head","Move Head"),
    actionButton("new_snake","New Snake"),
    actionButton("grow_snake","Grow Snake"),
    actionButton("fill_space","Snake it all!"),
    
    actionButton("refresh","Refresh")
  ),
  mainPanel = mainPanel(
    plotOutput(outputId = "plot",width = "1000px", height = "600px")
  ))

)

server <- function(input, output) {
 
  space <- reactive({
    req(input$nrows,input$ncols)
    space <- Space$new(input$nrows,input$ncols)
    space$new_snake()
  })
  
  observeEvent(input$new_snake, {
    space()$new_snake()
  })
  
  observeEvent(input$grow_snake, {
    space()$grow_snake()
  })
  
  observeEvent(input$move_head, {
    space()$move_head()
  })
  
  observeEvent(input$fill_space, {
    space()$fill_space()
  })
  
  output$plot <- shiny::renderPlot({
    
    input$move_head
    input$new_snake
    input$grow_snake
    input$fill_space
    
    space()$show(
      linewidth=input$linewidth, 
      background_color="black", 
      border_color="white",
      dotsize=input$dotsize
    ) + scale_color_viridis_c(direction = 1, begin = 0, end = 1, option = input$palette)
  })
}

shinyApp(ui, server)
