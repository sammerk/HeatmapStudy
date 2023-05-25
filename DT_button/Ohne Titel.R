library(shiny)
library(tidyverse)
library(shinyjs)
library(DT)
library(glue)

ui <- fluidPage(
  # front end interface
  useShinyjs(),
  uiOutput('ui')
)

server <- function(input, output, session) {
  output$ui <- renderUI({
    tags$div(
      style = 'padding: 40px',
      dataTableOutput('df'),
      tags$br(),
      verbatimTextOutput('text')
    )
  })
  
  output$df <- renderDataTable({
    mpg <- mpg %>%
      mutate(
        serial_no = row_number(), 
        actionable = glue('<button id="custom_btn" onclick="Shiny.onInputChange(\'button_id\', \'{serial_no}\')">Click</button>')
      )
    
    DT::datatable(
      mpg,
      escape = FALSE,
      options = list(
        searching = FALSE, 
        pageLength = 10
      )
    )
  })
  
  output$text <- renderText('Nothing is selected yet')
  
  observeEvent(input$button_id, {
    output$text <- renderText(glue('Row number {input$button_id} is selected recently'))
  })
}

shinyApp(ui, server)