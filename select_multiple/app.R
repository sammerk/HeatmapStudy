## Example shiny app with bucket list

library(shiny)
library(tidyverse)
library(gt)
library(shinyWidgets)

# generate dummy data ##########################################################
data <- 
  tibble(
    sus = paste("S", 1:25, sep = ""),
    `A1` =  sample(0:1, 25, replace = T),
    `A2` =  sample(0:1, 25, replace = T),
    `A3` =  sample(0:1, 25, replace = T),
    `A4` =  sample(0:1, 25, replace = T),
    `A5` =  sample(0:1, 25, replace = T),
    `A6` =  sample(0:1, 25, replace = T),
    `A7` =  sample(0:1, 25, replace = T),
    `A8` =  sample(0:1, 25, replace = T),
    `A9` =  sample(0:1, 25, replace = T),
    `A10` =  sample(0:1, 25, replace = T),
    `A11` =  sample(0:1, 25, replace = T),
    `A12` =  sample(0:1, 25, replace = T),
    `A13` =  sample(0:1, 25, replace = T),
    `A14` =  sample(0:1, 25, replace = T),
    `A15` =  sample(0:1, 25, replace = T),
    `A16` =  sample(0:1, 25, replace = T),
    `A17` =  sample(0:1, 25, replace = T),
    `A18` =  sample(0:1, 25, replace = T),
    `A19` =  sample(0:1, 25, replace = T),
    `A20` =  sample(0:1, 25, replace = T))


# generate table ###############################################################
data_gt <-
  data |>
  gt(id = "mygt") |>
  opt_css(
    css = "
    #mygt .gt_col_heading {
      text-align: center;
      transform: rotate(-90deg);
      font-weight: bold;
    }
    "
  ) |> 
  tab_options(
    table.font.size = px(8L),
    data_row.padding = px(1),
    table.align="left"
  )


# define ui ####################################################################
ui <- fluidPage(
  wellPanel(
    fluidRow(
      h4("Header"),
      p("text")
      )
    ), 
  
  # Tabelle 
  wellPanel(
    fluidRow(
      gt_output('gt')
      )
  ),
  
  # Gruppenanzahl
  wellPanel(
    numericInput("number_groups", 
                "Wieviele Gruppen sollten gebildet werden?",
                value = 1,
                min = 1,
                max = 12,
                step = 1)
  ),
  
  # Gruppe 1
  wellPanel(
    fluidRow(
      column(6,
             pickerInput(
               inputId = "sus_gr01",
               label = "Welche Schüler*innen sollen in Gruppe 1", 
               choices = data$sus,
               multiple = TRUE
             ) 
             ),
      column(6,
             textAreaInput("goals_gr01", 
                       "Welche Lernziele sollte Gruppe 1 verfolgen?",
                       width = "100%"))
    )
  ),
  
  # Gruppe 1
  conditionalPanel(condition = "input.number_groups > 1",
                   
                   wellPanel(fluidRow(
                     column(
                       6,
                       pickerInput(
                         inputId = "sus_gr02",
                         label = "Welche Schüler*innen sollen in Gruppe 2",
                         choices = data$sus,
                         multiple = TRUE
                       )
                     ),
                     column(
                       6,
                       textAreaInput(
                         "goals_gr02",
                         "Welche Lernziele sollte Gruppe 2 verfolgen?",
                         width = "100%"
                       )
                     )
                   )))
  
)

server <- function(input, output, session) {
  
  output$gt <-  
    render_gt(data_gt) 
}  

shinyApp(ui, server)