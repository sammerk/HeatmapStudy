library(shiny)
library(gt)
library(tidyverse)

data_gt <-
  tibble(
    Gruppe = selectInput("i1",
                         "",
                         c("-", as.character(1:5))) |>
      as.character(),
    A1 = c(1, 0, 1),
    A2 = c(0, 0, 1)
  ) |>
  gt(id = "mygt") |>
  fmt_markdown(columns = vars(Gruppe)) |>
  cols_label(
    A1 = gt::html(
          paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv erkennen ', 
                actionButton("b1",
                               "",
                             icon = icon("search")) |>
                    as.character(),
                "</div></div>"
               )
          ),
    A2 = gt::html(
          paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">',
                actionButton("b2",
                         "Imperativ benennen") |>
                    as.character(),
            "</div></div>"
           )
    ),
  ) |>
  opt_css(
    css = "
      #mygt  th.gt_col_heading {
  /* Something you can count on */
  height: 230px;
  white-space: nowrap;
  overflow-x: inherit;
  
}

#mygt th.gt_col_heading > div {
  transform: 
    translate(0px, -10px)
    rotate(270deg);
  width: 40px;
  overflow-x: inherit;
  
}
#mygt th.gt_col_heading > div > span {
  border-bottom: 1px solid #ccc;
  padding: 5px 10px;
  overflow-x: inherit;
}"
    )
  
  


# define ui ####################################################################
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "dragndrop.css")
  ),
  
  # header complete width
  wellPanel(
    fluidRow(
      h4("Header"),
      p("text"),
      actionButton("b4",
                   "Test",
                   icon = icon("search"))
    )
  ), 
  
  # table
  fluidRow(
    
    # Tabelle 
    column(5, 
           wellPanel(
             gt_output('gt')
           ))
  ),
  
  fluidRow(
    wellPanel(
      verbatimTextOutput("debug01"),
      verbatimTextOutput("debug02"),
    )
  )
)


server <- function(input, output, session) {
  
  output$gt <-  
    render_gt({data_gt}) 
  
  output$debug01 <- 
    renderText({
      input$b1
    })
  
  output$debug02 <- 
    renderText({
      input$i1
    })
  
}

  
  

shinyApp(ui, server)