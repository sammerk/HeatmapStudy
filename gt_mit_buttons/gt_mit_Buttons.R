library(shiny)
library(gt)
library(tidyverse)
library(readxl)
library(readxl)
library(gtExtras)
 # data <- read_csv("gt_mit_buttons/data.csv") # for interactive debugging with 
data <- read_csv("data.csv")
data_gt <- 
  data |> 
  rowwise() |> 
  mutate(`Richtig pro SuS [%]` = mean(c_across(1:20))) |> 
  ungroup() |> 
  add_row(summarise(data,
                      across(where(is.numeric), function(x) sum(x)/20),
                      across(where(is.character), ~ "Richtig pro Aufgabe [%]"))) |> 
  relocate(`Schüler:in`, `Richtig pro SuS [%]`) |> 
  gt(id = "mygt") |>
  gt_color_rows(columns = c(`Richtig pro SuS [%]`:21), 
                domain = c(0, 100),
                palette = c("#4B0092", "#1AFF1A"), # color blind friendly
                na.color = "transparent") |> 
  #fmt_markdown(columns = vars(Gruppe)) |>
  sub_values(columns = c(`Konjunktiv Formgleichheit`:`Konjunktiv erkennen e)`),
             fn = function(x) between(x, 100, 100), 
             replacement = "✓") |> 
  sub_values(columns = c(`Konjunktiv Formgleichheit`:`Konjunktiv erkennen e)`),
             fn = function(x) between(x, 0, 0), 
             replacement = "⨉") |> 
  
  cols_label(
    `Richtig pro SuS [%]` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Richtig pro SuS [%]')),
    `Konjunktiv Formgleichheit` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv Formgleichheit', 
            actionButton("Konjunktiv Formgleichheit",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")))|>
  opt_css(
    css = "
      #mygt  th.gt_col_heading {
  /* Something you can count on */
  height: 260px;
  white-space: nowrap;
  overflow-x: inherit;
  
}

#mygt th.gt_col_heading > div {
  transform: 
    translate(0px, -10px)
    rotate(270deg);
  width: 30px;
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