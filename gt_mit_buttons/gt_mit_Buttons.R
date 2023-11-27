library(shiny)
library(gt)
library(tidyverse)
library(readxl)
library(readxl)
library(gtExtras)
library(shinyWidgets)
library(sortable)

 # data <- read_csv("gt_mit_buttons/data.csv") # for interactive debugging with 
data <- read_csv("data.csv") # ggf. Dateipfad anpassen und Überordner mit dazu nehmen


# sorted order of students
ordered_students <- c("Schüler:in10", "Schüler:in13", "Schüler:in15", "Schüler:in12", "Schüler:in04", "Schüler:in01", "Schüler:in14", 
                    "Schüler:in02", "Schüler:in06", "Schüler:in05", "Schüler:in18", "Schüler:in17", "Schüler:in07", "Schüler:in09", 
                    "Schüler:in03", "Schüler:in08", "Schüler:in11", "Schüler:in16", "Richtig pro SuS [%]") # for ordering rows: sorted heatmap

# unsorted order of students
unordered_students <- c("Schüler:in13", "Schüler:in18", "Schüler:in17", "Schüler:in15", "Schüler:in02", "Schüler:in16", "Schüler:in11", 
                        "Schüler:in10", "Schüler:in08", "Schüler:in09", "Schüler:in06", "Schüler:in07", "Schüler:in01", "Schüler:in05", 
                        "Schüler:in04", "Schüler:in12", "Schüler:in14", "Schüler:in03", "Richtig pro SuS [%]") 


data_gt_sorted_heatmap <- # gt object: ordered heatmap
  data |> 
  rowwise() |> 
  mutate(`Richtig pro SuS [%]` = mean(c_across(1:20))) |> 
  ungroup() |> 
  add_row(summarise(data,
                      across(where(is.numeric), function(x) sum(x)/18), 
                    across(where(is.numeric), function(x) round(x, digits = 0)),
                      across(where(is.character), ~ "Richtig pro Aufgabe [%]"))) |> 
  relocate(`Schüler_in`, `Richtig pro SuS [%]`) |> 
  arrange(match(Schüler_in, ordered_students))%>% # ordering rows: sorted heatmap
  relocate(any_of(c("Schüler_in", "Richtig pro SuS [%]", "Unterschied Konjunktiv I/II", "Imperativ erkennen d)", 
                    "Konjunktiv erkennen b)", "Imperativ erkennen a)", "Imperativ erkennen e)", "Konjunktiv erkennen a)", 
                    "Indikativ erkennen d)", "Indikativ erkennen a)", "Konjunktiv erkennen c)", "Indikativ erkennen e)", 
                    "Indikativ erkennen b)", "Indikativ erkennen c)", "Konjunktiv erkennen e)", "Imperativ erkennen c)", 
                    "Imperativ erkennen b)", "Konjunktiv Formgleichheit", "Konjunktiv II bilden c)", "Konjunktiv II bilden a)",
                    "Konjunktiv II bilden b)", "Konjunktiv erkennen d)")))%>% # ordering colums: sorted heatmap
  gt(id = "mygt") |>
  gt_color_rows(columns = c(`Richtig pro SuS [%]`:22), 
                domain = c(0, 100),
                palette = c("#482677", "#55C667FF"), # also color blind friendly: "#4B0092", "#1AFF1A"
                na.color = "transparent") |> 
  #fmt_markdown(columns = vars(Gruppe)) |>
  sub_values(columns = c(`Unterschied Konjunktiv I/II`:`Konjunktiv erkennen d)`), 
             rows = c(1:18),
             fn = function(x) between(x, 100, 100), 
             replacement = "✓") |> 
  sub_values(columns = c(`Unterschied Konjunktiv I/II`:`Konjunktiv erkennen d)`),
             fn = function(x) between(x, 0, 0), 
             replacement = "x") |> 
  sub_missing(columns = everything(),
              rows = everything(),
              missing_text = "")%>%
  opt_table_font(font = list(google_font(name = "Source Sans Pro"))) |> 
  tab_footnote(footnote = "✓ = Aufgabe richtig; x = Aufgabe falsch") %>%
  #cols_width(columns = c(`Konjunktiv Formgleichheit`:`Konjunktiv erkennen e)`) ~ px(20)) %>% # hat keinen Einfluss auf Spaltenbreite
  cols_label(
    `Richtig pro SuS [%]` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Richtig pro SuS [%]')),
    `Konjunktiv Formgleichheit` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv Formgleichheit', 
            #actionButton("Konjunktiv Formgleichheit",
             #            "", style='padding:1px; font-size:90%', 
              #           icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Konjunktiv erkennen a)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv erkennen a)', 
            actionButton("Konjunktiv erkennen a)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Imperativ erkennen a)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Imperativ erkennen a)', 
            actionButton("Imperativ erkennen a)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Imperativ erkennen b)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Imperativ erkennen b)', 
            actionButton("Imperativ erkennen b)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Imperativ erkennen c)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Imperativ erkennen c)', 
            actionButton("Imperativ erkennen c)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Imperativ erkennen d)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Imperativ erkennen d)', 
            actionButton("Imperativ erkennen d)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Imperativ erkennen e)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Imperativ erkennen e)', 
            actionButton("Imperativ erkennen e)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Indikativ erkennen a)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Indikativ erkennen a)', 
            actionButton("Indikativ erkennen a)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Indikativ erkennen b)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Indikativ erkennen b)', 
            actionButton("Indikativ erkennen b)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Indikativ erkennen c)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Indikativ erkennen c)', 
            actionButton("Indikativ erkennen c)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Indikativ erkennen d)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Indikativ erkennen d)', 
            actionButton("Indikativ erkennen d)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Konjunktiv II bilden a)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv II bilden a)', 
            actionButton("Konjunktiv II bilden a)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Indikativ erkennen e)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Indikativ erkennen e)', 
            actionButton("Indikativ erkennen e)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Konjunktiv II bilden b)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv II bilden b)', 
            actionButton("Konjunktiv II bilden b)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Konjunktiv II bilden c)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv II bilden c)', 
            actionButton("Konjunktiv II bilden c)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Unterschied Konjunktiv I/II` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Unterschied Konjunktiv I/II', 
            actionButton("Unterschied Konjunktiv I/II",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Konjunktiv erkennen b)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv erkennen b)', 
            actionButton("Konjunktiv erkennen b)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Konjunktiv erkennen c)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv erkennen c)', 
            actionButton("Konjunktiv erkennen c)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Konjunktiv erkennen d)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv erkennen d)', 
            actionButton("Konjunktiv erkennen d)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Konjunktiv erkennen e)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv erkennen e)', 
            actionButton("Konjunktiv erkennen e)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    )|>
  opt_css(
    css = "
      #mygt  th.gt_col_heading {
  /* Something you can count on */
  height: 170px;
  white-space: nowrap;
  overflow-x: inherit;
  
}

#mygt th.gt_col_heading > div {
  transform: 
    translate(0px, -10px)
    rotate(270deg);
  width: 20px;
  overflow-x: inherit;
  
}
#mygt th.gt_col_heading > div > span {
  border-bottom: 1px solid #ccc;
  padding-top: 1px;
padding-bottom: 1px;
padding-left: 1px;
padding-right: 1px;
  padding: 1px 1px;
  overflow-x: inherit;
} 
#mygt .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 1px;
  padding-right: 1px;
}
#mygt .gt_col_heading {
    padding-top: 1px;
    padding-bottom: 1px;
    padding-left: 1px;
    padding-right: 1px;
}
#mygt .gt_right {
    text-align: center;
    font-variant-numeric: tabular-nums;
    padding-top: 1px;
    padding-bottom: 1px;
    padding-left: 1px;
    padding-right: 1px;
}
#mygt .gt_table {
    display: table;
    border-collapse: collapse;
    line-height: normal;
    margin-left: auto;
    margin-right: auto;
    color: #333333;
    font-size: 12px
}
"
  )
  

data_gt_unsorted_heatmap <- # gt object: unordered heatmap
  data |> 
  rowwise() |> 
  mutate(`Richtig pro SuS [%]` = mean(c_across(1:20))) |> 
  ungroup() |> 
  add_row(summarise(data,
                    across(where(is.numeric), function(x) sum(x)/18), 
                    across(where(is.numeric), function(x) round(x, digits = 0)),
                    across(where(is.character), ~ "Richtig pro Aufgabe [%]"))) |> 
  relocate(`Schüler_in`, `Richtig pro SuS [%]`) |> 
  arrange(match(Schüler_in, unordered_students))%>% # ordering rows: unsorted heatmap
  relocate(any_of(c("Schüler_in", "Richtig pro SuS [%]", "Indikativ erkennen d)", "Indikativ erkennen b)", "Indikativ erkennen c)", "Imperativ erkennen c)", 
                    "Konjunktiv erkennen d)", "Konjunktiv Formgleichheit", "Imperativ erkennen a)", "Indikativ erkennen a)", "Konjunktiv II bilden a)", "Konjunktiv erkennen c)", 
                    "Imperativ erkennen b)", "Konjunktiv erkennen a)", "Konjunktiv erkennen b)", "Konjunktiv II bilden b)", "Indikativ erkennen e)", 
                    "Konjunktiv erkennen e)", "Unterschied Konjunktiv I/II", "Imperativ erkennen d)", "Konjunktiv II bilden c)", "Imperativ erkennen e)")))%>% # ordering colums: unsorted heatmap
  gt(id = "mygt") |>
  gt_color_rows(columns = c(`Richtig pro SuS [%]`:22), 
                domain = c(0, 100),
                palette = c("#482677", "#55C667FF"), # also color blind friendly: "#4B0092", "#1AFF1A"
                na.color = "transparent") |> 
  #fmt_markdown(columns = vars(Gruppe)) |>
  sub_values(columns = c(`Indikativ erkennen d)`:`Imperativ erkennen e)`), 
             rows = c(1:18),
             fn = function(x) between(x, 100, 100), 
             replacement = "✓") |> 
  sub_values(columns = c(`Indikativ erkennen d)`:`Imperativ erkennen e)`),
             fn = function(x) between(x, 0, 0), 
             replacement = "x") |> 
  sub_missing(columns = everything(),
              rows = everything(),
              missing_text = "")%>%
  opt_table_font(font = list(google_font(name = "Source Sans Pro"))) |> 
  tab_footnote(footnote = "✓ = Aufgabe richtig; x = Aufgabe falsch") %>%
  #cols_width(columns = c(`Konjunktiv Formgleichheit`:`Konjunktiv erkennen e)`) ~ px(20)) %>% # hat keinen Einfluss auf Spaltenbreite
  cols_label(
    `Richtig pro SuS [%]` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Richtig pro SuS [%]')),
    `Konjunktiv Formgleichheit` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv Formgleichheit', 
            #actionButton("Konjunktiv Formgleichheit",
             #            "", style='padding:1px; font-size:90%', 
              #           icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Konjunktiv erkennen a)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv erkennen a)', 
            actionButton("Konjunktiv erkennen a)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Imperativ erkennen a)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Imperativ erkennen a)', 
            actionButton("Imperativ erkennen a)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Imperativ erkennen b)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Imperativ erkennen b)', 
            actionButton("Imperativ erkennen b)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Imperativ erkennen c)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Imperativ erkennen c)', 
            actionButton("Imperativ erkennen c)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Imperativ erkennen d)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Imperativ erkennen d)', 
            actionButton("Imperativ erkennen d)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Imperativ erkennen e)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Imperativ erkennen e)', 
            actionButton("Imperativ erkennen e)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Indikativ erkennen a)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Indikativ erkennen a)', 
            actionButton("Indikativ erkennen a)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Indikativ erkennen b)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Indikativ erkennen b)', 
            actionButton("Indikativ erkennen b)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Indikativ erkennen c)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Indikativ erkennen c)', 
            actionButton("Indikativ erkennen c)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Indikativ erkennen d)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Indikativ erkennen d)', 
            actionButton("Indikativ erkennen d)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Konjunktiv II bilden a)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv II bilden a)', 
            actionButton("Konjunktiv II bilden a)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Indikativ erkennen e)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Indikativ erkennen e)', 
            actionButton("Indikativ erkennen e)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Konjunktiv II bilden b)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv II bilden b)', 
            actionButton("Konjunktiv II bilden b)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Konjunktiv II bilden c)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv II bilden c)', 
            actionButton("Konjunktiv II bilden c)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Unterschied Konjunktiv I/II` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Unterschied Konjunktiv I/II', 
            actionButton("Unterschied Konjunktiv I/II",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Konjunktiv erkennen b)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv erkennen b)', 
            actionButton("Konjunktiv erkennen b)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Konjunktiv erkennen c)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv erkennen c)', 
            actionButton("Konjunktiv erkennen c)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Konjunktiv erkennen d)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv erkennen d)', 
            actionButton("Konjunktiv erkennen d)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
    `Konjunktiv erkennen e)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv erkennen e)', 
            actionButton("Konjunktiv erkennen e)",
                         "", style='padding:1px; font-size:90%', 
                         icon = icon("search")) |>
              as.character(),
            "</div></div>")), 
  )|>
  opt_css(
    css = "
      #mygt  th.gt_col_heading {
  /* Something you can count on */
  height: 170px;
  white-space: nowrap;
  overflow-x: inherit;
  
}

#mygt th.gt_col_heading > div {
  transform: 
    translate(0px, -10px)
    rotate(270deg);
  width: 20px;
  overflow-x: inherit;
  
}
#mygt th.gt_col_heading > div > span {
  border-bottom: 1px solid #ccc;
  padding-top: 1px;
padding-bottom: 1px;
padding-left: 1px;
padding-right: 1px;
  padding: 1px 1px;
  overflow-x: inherit;
} 
#mygt .gt_row {
  padding-top: 1px;
  padding-bottom: 1px;
  padding-left: 1px;
  padding-right: 1px;
}
#mygt .gt_col_heading {
    padding-top: 1px;
    padding-bottom: 1px;
    padding-left: 1px;
    padding-right: 1px;
}
#mygt .gt_right {
    text-align: center;
    font-variant-numeric: tabular-nums;
    padding-top: 1px;
    padding-bottom: 1px;
    padding-left: 1px;
    padding-right: 1px;
}
#mygt .gt_table {
    display: table;
    border-collapse: collapse;
    line-height: normal;
    margin-left: auto;
    margin-right: auto;
    color: #333333;
    font-size: 12px
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
      h4("Bildung homogener Lerngruppen anhand einer Lernzielkontrolle"),
      p("Eine Lehrerin hat eine Lernzielkontrolle durchgeführt und möchte darauf basierend binnendifferenziert unterrichten. Deshalb will sie ihre Klasse in homogene Lerngruppen anhand der Ergebnisse einteilen. Sie ist gemeinsam mit weiteren pädagogischen Fachkräften in der Klasse, sodass alle Lerngruppen gut begleitet werden können."), 
      p("Die Lehrerin überlegt, wie sie die homogenen Gruppen auf Basis der Lernzielkontrolle einteilen soll und welche Lernziele die Gruppen verfolgen sollen."),
      actionButton("b4",
                   "Test - Platzhalter google",
                   icon = icon("search"),
                   onclick ="window.open('http://google.com', '_blank')"),
    )
  ), 
  
  # table
  fluidRow(

    # Tabelle 
    column(7, 
           wellPanel(
             h5("Ergebnisse der Lernzielkontrolle"),
             em("Wenn Sie auf eine Lupe klicken, sehen Sie die Aufgabe der Lernzielkontrolle. 
               Aus Datenschutzgründen sind die Namen der Schüler:innen nicht zu sehen. Stattdessen wurden die Schüler:innen zufällig durchnummeriert."),
             gt_output('gt')
           )
  ), 
  # Gruppenanzahl
  column(5, 
         wellPanel(
           sliderInput("number_groups",
                       "Wie viele homogene Lerngruppen sollte die Lehrerin bilden?",
                       1, 8, 1, 1)
         )),
  column(5, 
         wellPanel(
           h5("Ordnen Sie die Schüler*innen den Gruppen zu!")
         ),  
         
         # Drag and drop 1 Gruppe
         conditionalPanel(condition = "input.number_groups == 1",
                          fluidRow(column(
                            width = 12,
                            bucket_list(
                              header = "",
                              group_name = "bucket_list_group",
                              orientation = "horizontal",
                              add_rank_list(
                                text = "Noch zuzuordnen",
                                labels = data$Schüler_in,
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
                                labels = data$Schüler_in,
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
                                labels = data$Schüler_in,
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
                                labels = data$Schüler_in,
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
                                  labels = data$Schüler_in,
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
                                  labels = data$Schüler_in,
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
                                  labels = data$Schüler_in,
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
                                  labels = data$Schüler_in,
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
                                ))))),
  
  fluidRow(
    wellPanel(
      verbatimTextOutput("debug01"),
      verbatimTextOutput("debug02"),
    )
  )
)))


server <- function(input, output, session) {
  
  output$gt <-  
    render_gt({data_gt_unsorted_heatmap}) 
  
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


