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
                  <div class="rotation-wrapper-inner">', 
                actionButton("b1",
                               "Imperativ erkennen") |>
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
    .rotation-wrapper-outer {
         display: table;
      }
    .rotation-wrapper-inner {
        padding: 50% 0;
        height: 0;
      }

     #mygt .gt_col_heading {
        display: block;
  transform-origin: top left;
  /* Note: for a CLOCKWISE rotation, use the commented-out
     transform instead of this one. */
  transform: rotate(-90deg) translate(-100%);
  /* transform: rotate(90deg) translate(0, -100%); */
  margin-top: -50%;

  /* Not vital, but possibly a good idea if the element you're rotating contains
     text and you want a single long vertical line of text and the pre-rotation
     width of your element is small enough that the text wraps: */
  white-space: nowrap;
}




           "
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
      p("text")
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