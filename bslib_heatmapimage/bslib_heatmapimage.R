library(shiny)
library(gt)
library(tidyverse)
library(readxl)
library(readxl)
library(gtExtras)
library(shinyWidgets)
library(sortable)
library(bslib)



# define ui ####################################################################
ui <- 
  page_fillable(
  # css for sortable widget ####
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "dragndrop.css")
  ),
  
  # Global Instruction ####
  wellPanel(
    h4("Bildung homogener Lerngruppen anhand einer Lernzielkontrolle"),
    p("Eine Lehrerin hat eine Lernzielkontrolle durchgeführt und möchte darauf basierend binnendifferenziert unterrichten. Deshalb will sie ihre Klasse in homogene Lerngruppen anhand der Ergebnisse einteilen. Sie ist gemeinsam mit weiteren pädagogischen Fachkräften in der Klasse, sodass alle Lerngruppen gut begleitet werden können."), 
    p("Die Lehrerin überlegt, wie sie die homogenen Gruppen auf Basis der Lernzielkontrolle einteilen soll und welche Lernziele die Gruppen verfolgen sollen.")
  ),
  
  # Tasks ####
  wellPanel(
    
    fluidRow(h5("Lernzielkontrolle"), 
             em("Aus Datenschutzgründen sind die Namen der Schüler:innen nicht zu sehen. Stattdessen wurden die Schüler:innen zufällig durchnummeriert.")), 
    
    fluidRow(
      column(9,
             wellPanel(
               h6("Ergebnisse"),
               img(
                 src = 'hm.png',
                 align = "center",
                 width = "100%"
               )
      )
    ), 
      column(3,
             wellPanel(
               h6("Aufgaben"),
               actionButton(inputId = 'modal_all_tasks', 
                            label = 'Alle Aufgaben anschauen'),
               h6(""),
               selectInput(
                 selected = NULL,
                 "modal_single_task",
                 "Ausgewählte Aufgaben anschauen",
                 choices = c(
                   "Aufgabe ...",
                   "Indikativ erkennen d)",
                   "Indikativ erkennen b)",
                   "Hund"
                 )
               )
             )))), 
  
  # Group Building ####
  wellPanel(
    h5("Gruppenbildung"),
    fluidRow(
      wellPanel(
      sliderInput(
        "number_groups",
        "Wie viele homogene Lerngruppen sollte die Lehrerin bilden?",
        1,
        8,
        1,
        1
      )),
         
  # Conditional Panels ############       
         # Drag and drop 1 Gruppe
  wellPanel(
         conditionalPanel(condition = "input.number_groups == 1",
                          fluidRow(column(
                            width = 12,
                            bucket_list(
                              header = "Ordnen Sie die Schüler*innen den Gruppen zu!",
                              group_name = "bucket_list_group",
                              orientation = "horizontal",
                              add_rank_list(
                                text = "Noch zuzuordnen",
                                labels = LETTERS[1:18],
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
                              header = "",
                              group_name = "bucket_list_group",
                              orientation = "horizontal",
                              add_rank_list(
                                text = "Noch zuzuordnen",
                                labels = LETTERS[1:24],
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
                              header = "",
                              group_name = "bucket_list_group",
                              orientation = "horizontal",
                              add_rank_list(
                                text = "Noch zuzuordnen",
                                labels = LETTERS[1:24],
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
                              header = "",
                              group_name = "bucket_list_group",
                              orientation = "horizontal",
                              add_rank_list(
                                text = "Noch zuzuordnen",
                                labels = LETTERS[1:24],
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
                                header = "",
                                group_name = "bucket_list_group",
                                orientation = "horizontal",
                                add_rank_list(
                                  text = "Noch zuzuordnen",
                                  labels = LETTERS[1:24],
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
                                header = "",
                                group_name = "bucket_list_group",
                                orientation = "horizontal",
                                add_rank_list(
                                  text = "Noch zuzuordnen",
                                  labels = LETTERS[1:24],
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
                                header = "",
                                group_name = "bucket_list_group",
                                orientation = "horizontal",
                                add_rank_list(
                                  text = "Noch zuzuordnen",
                                  labels = LETTERS[1:24],
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
                                ))))), 
         # Drag and drop 8 Gruppen
         conditionalPanel(condition = "input.number_groups == 8",
                          fluidRow(
                            column(
                              width = 12,
                              bucket_list(
                                header = "",
                                group_name = "bucket_list_group",
                                orientation = "horizontal",
                                add_rank_list(
                                  text = "Noch zuzuordnen",
                                  labels = LETTERS[1:24],
                                  input_id = "rank_list_80"
                                ),
                                add_rank_list(
                                  text = "Gruppe 1",
                                  labels = NULL,
                                  input_id = "rank_list_81"
                                ),
                                add_rank_list(
                                  text = "Gruppe 2",
                                  labels = NULL,
                                  input_id = "rank_list_82"
                                ),
                                add_rank_list(
                                  text = "Gruppe 3",
                                  labels = NULL,
                                  input_id = "rank_list_83"
                                ),
                                add_rank_list(
                                  text = "Gruppe 4",
                                  labels = NULL,
                                  input_id = "rank_list_84"
                                ),
                                add_rank_list(
                                  text = "Gruppe 5",
                                  labels = NULL,
                                  input_id = "rank_list_85"
                                ),
                                add_rank_list(
                                  text = "Gruppe 6",
                                  labels = NULL,
                                  input_id = "rank_list_86"
                                ),
                                add_rank_list(
                                  text = "Gruppe 7",
                                  labels = NULL,
                                  input_id = "rank_list_87"
                                ),
                                add_rank_list( 
                                  text = "Gruppe 8",
                                  labels = NULL,
                                  input_id = "rank_list_88"
                                ))))))),
  
  fluidRow(
    wellPanel(
      verbatimTextOutput("debug01"),
      verbatimTextOutput("debug02"),
    )
  )
 ))



server <- function(input, output, session) {
  
  # modal window all tasks ####
  observeEvent(input$modal_all_tasks, {
    showModal(modalDialog(
     card(
       card_image(
         file = "www/tasks_demo.png"
       )
     ),
      easyClose = T)
      )
  })
  
  # modal window specific tasks ####
  observeEvent(input$modal_single_task, {
    if(input$modal_single_task != "Aufgabe ...")
    showModal(modalDialog(
      card(
        card_image(
          file = case_when(
            input$modal_single_task == "Indikativ erkennen d)" ~ "www/a1.svg",
            input$modal_single_task == "Indikativ erkennen b)" ~ "www/a2.svg",
            T ~ "www/a3.svg" )
        )
      ),
      easyClose = T)
    )
  })
  
  
  # render gt ####  output$gt <-  
    render_gt({data_gt_unsorted_heatmap}) 
  
  
  # debuggings ####
  output$debug01 <- 
    renderText({
      input$b1
    })
  
  output$debug02 <- 
    renderText({
      input$i1
    })
  
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
  
  # updating the bucket list label ####
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
  
  # updating the rank list label #####
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


