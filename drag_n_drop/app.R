## Example shiny app with bucket list

library(shiny)
library(tidyverse)
library(gt)
library(shinyWidgets)
library(sortable)

# generate dummy data ##########################################################
data <- 
  tibble(
    sus = paste("S", 1:20, sep = ""),
    `A1` =  sample(0:1, 20, replace = T),
    `A2` =  sample(0:1, 20, replace = T),
    `A3` =  sample(0:1, 20, replace = T),
    `A4` =  sample(0:1, 20, replace = T),
    `A5` =  sample(0:1, 20, replace = T),
    `A6` =  sample(0:1, 20, replace = T),
    `A7` =  sample(0:1, 20, replace = T),
    `A8` =  sample(0:1, 20, replace = T),
    `A9` =  sample(0:1, 20, replace = T),
    `A10` =  sample(0:1, 20, replace = T),
    `A11` =  sample(0:1, 20, replace = T),
    `A12` =  sample(0:1, 20, replace = T),
    `A13` =  sample(0:1, 20, replace = T),
    `A14` =  sample(0:1, 20, replace = T),
    `A15` =  sample(0:1, 20, replace = T),
    `A16` =  sample(0:1, 20, replace = T),
    `A17` =  sample(0:1, 20, replace = T),
    `A18` =  sample(0:1, 20, replace = T))


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
  
  # table | slider / drag n. drop
  fluidRow(
    
  # Tabelle 
    column(5, 
      wellPanel(
          gt_output('gt')
      )),
  
  # Gruppenanzahl
    column(7, 
      wellPanel(
        sliderInput("number_groups",
                    "Wieviele Gruppen sollten gebildet werden?",
                    1, 7, 1, 1)
        ),
      
      # Drag and drop 1 Gruppe
      conditionalPanel(condition = "input.number_groups == 1",
                       fluidRow(column(
                         width = 12,
                         bucket_list(
                           header = "Ordnen Sie die Schüler*innen den Gruppen zu!",
                           group_name = "bucket_list_group",
                           orientation = "horizontal",
                           add_rank_list(
                             text = "Noch zuzuordnen",
                             labels = data$sus,
                             input_id = "rank_list_10"
                           ),
                           add_rank_list(
                             text = "Gruppe 1",
                             labels = NULL,
                             input_id = "rank_list_11"
                           )
                         )
                       ))), 
      
      # Drag and drop 2 Gruppen
      conditionalPanel(condition = "input.number_groups == 2",
                       fluidRow(column(
                         width = 12,
                         bucket_list(
                           header = "Ordnen Sie die Schüler*innen den Gruppen zu!",
                           group_name = "bucket_list_group",
                           orientation = "horizontal",
                           add_rank_list(
                             text = "Noch zuzuordnen",
                             labels = data$sus,
                             input_id = "rank_list_20"
                           ),
                           add_rank_list(
                             text = "Gruppe 1",
                             labels = NULL,
                             input_id = "rank_list_21"
                           ),
                           add_rank_list(
                             text = "Gruppe 2",
                             labels = NULL,
                             input_id = "rank_list_22"
                           )
                         )
                       ))), 
      
      # Drag and drop 3 Gruppen
      conditionalPanel(condition = "input.number_groups == 3",
                       fluidRow(column(
                         width = 12,
                         bucket_list(
                           header = "Ordnen Sie die Schüler*innen den Gruppen zu!",
                           group_name = "bucket_list_group",
                           orientation = "horizontal",
                           add_rank_list(
                             text = "Noch zuzuordnen",
                             labels = data$sus,
                             input_id = "rank_list_30"
                           ),
                           add_rank_list(
                             text = "Gruppe 1",
                             labels = NULL,
                             input_id = "rank_list_31"
                           ),
                           add_rank_list(
                             text = "Gruppe 2",
                             labels = NULL,
                             input_id = "rank_list_32"
                           ),
                           add_rank_list(
                             text = "Gruppe 3",
                             labels = NULL,
                             input_id = "rank_list_33"
                           )
                         )
                       ))), 
      
      # Drag and drop 4 Gruppen
      conditionalPanel(condition = "input.number_groups == 4",
                       fluidRow(column(
                         width = 12,
                         bucket_list(
                           header = "Ordnen Sie die Schüler*innen den Gruppen zu!",
                           group_name = "bucket_list_group",
                           orientation = "horizontal",
                           add_rank_list(
                             text = "Noch zuzuordnen",
                             labels = data$sus,
                             input_id = "rank_list_40"
                           ),
                           add_rank_list(
                             text = "Gruppe 1",
                             labels = NULL,
                             input_id = "rank_list_41"
                           ),
                           add_rank_list(
                             text = "Gruppe 2",
                             labels = NULL,
                             input_id = "rank_list_42"
                           ),
                           add_rank_list(
                             text = "Gruppe 3",
                             labels = NULL,
                             input_id = "rank_list_43"
                           ),
                           add_rank_list(
                             text = "Gruppe 4",
                             labels = NULL,
                             input_id = "rank_list_44"
                           )
                         )
                       ))), 
      
      
      # Drag and drop 5 Gruppen
      conditionalPanel(condition = "input.number_groups == 5",
      fluidRow(
        column(
          width = 12,
          bucket_list(
            header = "Ordnen Sie die Schüler*innen den Gruppen zu!",
            group_name = "bucket_list_group",
            orientation = "horizontal",
            add_rank_list(
              text = "Noch zuzuordnen",
              labels = data$sus,
              input_id = "rank_list_50"
            ),
            add_rank_list(
              text = "Gruppe 1",
              labels = NULL,
              input_id = "rank_list_51"
            ),
            add_rank_list(
              text = "Gruppe 2",
              labels = NULL,
              input_id = "rank_list_52"
            ),
            add_rank_list(
              text = "Gruppe 3",
              labels = NULL,
              input_id = "rank_list_53"
            ),
            add_rank_list(
              text = "Gruppe 4",
              labels = NULL,
              input_id = "rank_list_54"
            ),
            add_rank_list(
              text = "Gruppe 5",
              labels = NULL,
              input_id = "rank_list_55"
            ))))),
      
      # Drag and drop 6 Gruppen
      conditionalPanel(condition = "input.number_groups == 6",
                       fluidRow(
                         column(
                           width = 12,
                           bucket_list(
                             header = "Ordnen Sie die Schüler*innen den Gruppen zu!",
                             group_name = "bucket_list_group",
                             orientation = "horizontal",
                             add_rank_list(
                               text = "Noch zuzuordnen",
                               labels = data$sus,
                               input_id = "rank_list_60"
                             ),
                             add_rank_list(
                               text = "Gruppe 1",
                               labels = NULL,
                               input_id = "rank_list_61"
                             ),
                             add_rank_list(
                               text = "Gruppe 2",
                               labels = NULL,
                               input_id = "rank_list_62"
                             ),
                             add_rank_list(
                               text = "Gruppe 3",
                               labels = NULL,
                               input_id = "rank_list_63"
                             ),
                             add_rank_list(
                               text = "Gruppe 4",
                               labels = NULL,
                               input_id = "rank_list_64"
                             ),
                             add_rank_list(
                               text = "Gruppe 5",
                               labels = NULL,
                               input_id = "rank_list_65"
                             ),
                             add_rank_list(
                               text = "Gruppe 6",
                               labels = NULL,
                               input_id = "rank_list_66"
                             ))))),
      
      
      # Drag and drop 7 Gruppen
      conditionalPanel(condition = "input.number_groups == 7",
                       fluidRow(
                         column(
                           width = 12,
                           bucket_list(
                             header = "Ordnen Sie die Schüler*innen den Gruppen zu!",
                             group_name = "bucket_list_group",
                             orientation = "horizontal",
                             add_rank_list(
                               text = "Noch zuzuordnen",
                               labels = data$sus,
                               input_id = "rank_list_70"
                             ),
                             add_rank_list(
                               text = "Gruppe 1",
                               labels = NULL,
                               input_id = "rank_list_71"
                             ),
                             add_rank_list(
                               text = "Gruppe 2",
                               labels = NULL,
                               input_id = "rank_list_72"
                             ),
                             add_rank_list(
                               text = "Gruppe 3",
                               labels = NULL,
                               input_id = "rank_list_73"
                             ),
                             add_rank_list(
                               text = "Gruppe 4",
                               labels = NULL,
                               input_id = "rank_list_74"
                             ),
                             add_rank_list(
                               text = "Gruppe 5",
                               labels = NULL,
                               input_id = "rank_list_75"
                             ),
                             add_rank_list(
                               text = "Gruppe 6",
                               labels = NULL,
                               input_id = "rank_list_76"
                             ),
                             add_rank_list(
                               text = "Gruppe 7",
                               labels = NULL,
                               input_id = "rank_list_77"
                             )))))
    )
    )
)

server <- function(input, output, session) {
  
  output$gt <-  
    render_gt(data_gt) 
  
  output$results_1 <-
    renderPrint(
      input$rank_list_1 # This matches the input_id of the first rank list
    )
  output$results_2 <-
    renderPrint(
      input$rank_list_2 # This matches the input_id of the second rank list
    )
  output$results_3 <-
    renderPrint(
      input$bucket_list_group # Matches the group_name of the bucket list
    )

  # test updating the bucket list label
  counter_bucket <- reactiveVal(1)
  observe({
    update_bucket_list(
      "bucket_list_group",
      text = paste("You pressed the button", counter_bucket(), "times"),
      session = session
    )
    counter_bucket(counter_bucket() + 1)
  }) %>%
    bindEvent(input$btnUpdateBucket)

  # test updating the rank list label
  counter_rank <- reactiveVal(1)
  observe({
    update_rank_list(
      "rank_list_1",
      text = paste("You pressed the button", counter_rank(), "times"),
      session = session
    )
    counter_rank(counter_rank() + 1)
  }) %>%
    bindEvent(input$btnUpdateRank)
}


shinyApp(ui, server)