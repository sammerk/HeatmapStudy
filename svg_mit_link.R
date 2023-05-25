library(shiny)
library(htmltools)

# see Shiny docs
#  http://shiny.rstudio.com/reference/shiny/latest/htmlOutput.html
#  http://shiny.rstudio.com/reference/shiny/latest/renderUI.html
ui <-  basicPage(
  uiOutput("svgout"),
  verbatimTextOutput("debug1")
)

server <- function(input, output, session){
  output$svgout <- renderUI({
    HTML(

"<svg>
  <a <a id='test' href='#' class='action-button'>
    <circle cx='60' cy='60' r='50'/>
  </a>
</svg>"

    )
  })
  
  observeEvent(input$test, {
    showModal(modalDialog(
      title = "Somewhat important message",
      "This is a somewhat important message.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  output$debug1 <- 
    renderText({
      input$test
    })
  
}

shinyApp(ui, server)