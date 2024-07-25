library(shiny)
library(gt)
library(tidyverse)
library(readxl)
library(gtExtras)
library(shinyWidgets)
library(sortable)

# data <- read_csv("gt_mit_buttons/data.csv") # for interactive debugging with 
data_soscisurvey <- read_xlsx("bslib_heatmapimage/data/data_soscisurvey.xlsx") # ggf. Dateipfad anpassen und Überordner mit dazu nehmen


# sorted order of students
ordered_students <- c("Schüler:in10", "Schüler:in13", "Schüler:in15", "Schüler:in12", "Schüler:in04", "Schüler:in01", "Schüler:in14", 
                      "Schüler:in02", "Schüler:in06", "Schüler:in05", "Schüler:in18", "Schüler:in17", "Schüler:in07", "Schüler:in09", 
                      "Schüler:in03", "Schüler:in08", "Schüler:in11", "Schüler:in16", "Richtig pro SuS [%]") # for ordering rows: sorted heatmap

# unsorted order of students
unordered_students <- c("Schüler:in13", "Schüler:in18", "Schüler:in17", "Schüler:in15", "Schüler:in02", "Schüler:in16", "Schüler:in11", 
                        "Schüler:in10", "Schüler:in08", "Schüler:in09", "Schüler:in06", "Schüler:in07", "Schüler:in01", "Schüler:in05", 
                        "Schüler:in04", "Schüler:in12", "Schüler:in14", "Schüler:in03", "Richtig pro SuS [%]") 

## ordering columns: shiny (unsorted heatmap)
#   relocate(any_of(c("Schüler_in", "Richtig pro SuS [%]", "a19", "a17", "a18", "a13", 
#"a8", "a1", "a11", "a16", "a2", "a7", 
#"a12", "a10", "a6", "a3", "a20", 
#"a9", "a5", "a14", "a4", "a15", "Gruppenzuordnung")))%>% # ordering colums: unsorted heatmap shiny 
#
## ordering columns: soscisurvey (unsorted heatmap)
#relocate(any_of(c("Schüler_in", "Richtig pro SuS [%]", "a9", "a6", "a17", "a10", 
# "a19", "a2", "a16", "a20", "a4", "a15", 
# "a14", "a13", "a7", "a5", "a12", 
# "a11", "a1", "a18", "a3", "a8", "Gruppenzuordnung")))%>% # ordering colums: unsorted heatmap soscisurvey 

data_gt_unsorted_heatmap <- # gt object: unordered heatmap
  data_soscisurvey |> 
  mutate(across(c(1:20), ~ .* 100)) |> 
  rowwise() |> 
  mutate(`Richtig pro SuS [%]` = mean(c_across(1:20))) |> 
  ungroup() |> 
  add_row(summarise(data_soscisurvey,
                    across(where(is.numeric), function(x) sum(x)/18*100), 
                    across(where(is.numeric), function(x) round(x, digits = 0)),
                    across(where(is.character), ~ "Richtig pro Aufgabe [%]"))) |> 
  relocate(`Schüler_in`, `Richtig pro SuS [%]`) |> 
  arrange(match(Schüler_in, unordered_students))%>% # ordering rows: unsorted heatmap
  relocate(any_of(c("Schüler_in", "Richtig pro SuS [%]", "a9", "a6", "a17", "a10", 
              "a19", "a2", "a16", "a20", "a4", "a15", 
               "a14", "a13", "a7", "a5", "a12", 
               "a11", "a1", "a18", "a3", "a8", "Gruppenzuordnung")))%>% # ordering colums: unsorted heatmap soscisurvey 
  gt(id = "mygt") |>
  gt_color_rows(columns = c(`Richtig pro SuS [%]`:22), 
                domain = c(0, 100),
                palette = c("#482677", "#55C667FF"), # also color blind friendly: "#4B0092", "#1AFF1A"
                na.color = "transparent") |> 
  #fmt_markdown(columns = vars(Gruppe)) |>
  sub_values(columns = c(`a9`:`a8`), 
             rows = c(1:18),
             fn = function(x) between(x, 100, 100), 
             replacement = "✓") |> 
  sub_values(columns = c(`a9`:`a8`),
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
    `a1` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">1. Unterschied Konjunktiv I/II', 
            #actionButton("Konjunktiv Formgleichheit",
            #            "", style='padding:1px; font-size:90%', 
            #           icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `a2` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">2. Konjunktiv Formgleichheit', 
            #actionButton("Konjunktiv erkennen a)",
            #            "", style='padding:1px; font-size:90%', 
            #           icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `a3` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">3. Konjunktiv II bilden', 
            # actionButton("Imperativ erkennen a)",
            #             "", style='padding:1px; font-size:90%', 
            #            icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `a4` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">4. Konjunktiv II bilden', 
            # actionButton("Imperativ erkennen b)",
            #             "", style='padding:1px; font-size:90%', 
            #            icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `a5` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">5. Konjunktiv II bilden', 
            # actionButton("Imperativ erkennen c)",
            #             "", style='padding:1px; font-size:90%', 
            #            icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `a6` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">6. Indikativ erkennen', 
            # actionButton("Imperativ erkennen d)",
            #             "", style='padding:1px; font-size:90%', 
            #            icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `a7` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">7. Konjunktiv erkennen', 
            # actionButton("Imperativ erkennen e)",
            #             "", style='padding:1px; font-size:90%', 
            #            icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `a8` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">8. Imperativ erkennen', 
            #  actionButton("Indikativ erkennen a)",
            #              "", style='padding:1px; font-size:90%', 
            #             icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `a9` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">9. Indikativ erkennen', 
            #  actionButton("Indikativ erkennen b)",
            #              "", style='padding:1px; font-size:90%', 
            #             icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `a10` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">10. Imperativ erkennen', 
            # actionButton("Indikativ erkennen c)",
            #             "", style='padding:1px; font-size:90%', 
            #            icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `a11` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">11. Konjunktiv erkennen', 
            # actionButton("Indikativ erkennen d)",
            #             "", style='padding:1px; font-size:90%', 
            #            icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `a12` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">12. Indikativ erkennen', 
            # actionButton("Konjunktiv II bilden a)",
            #             "", style='padding:1px; font-size:90%', 
            #            icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `a13` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">13. Konjunktiv erkennen', 
            #  actionButton("Indikativ erkennen e)",
            #              "", style='padding:1px; font-size:90%', 
            #             icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `a14` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">14. Imperativ erkennen', 
            #  actionButton("Konjunktiv II bilden b)",
            #              "", style='padding:1px; font-size:90%', 
            #             icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `a15` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">15. Konjunktiv erkennen', 
            # actionButton("Konjunktiv II bilden c)",
            #             "", style='padding:1px; font-size:90%', 
            #            icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `a16` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">16. Imperativ erkennen', 
            # actionButton("Unterschied Konjunktiv I/II",
            #             "", style='padding:1px; font-size:90%', 
            #            icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `a17` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">17. Indikativ erkennen', 
            # actionButton("Konjunktiv erkennen b)",
            #             "", style='padding:1px; font-size:90%', 
            #            icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `a18` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">18. Imperativ erkennen', 
            # actionButton("Konjunktiv erkennen c)",
            #             "", style='padding:1px; font-size:90%', 
            #            icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `a19` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">19. Konjunktiv erkennen', 
            # actionButton("Konjunktiv erkennen d)",
            #             "", style='padding:1px; font-size:90%', 
            #            icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `a20` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">20. Indikativ erkennen', 
            #actionButton("Konjunktiv erkennen e)",
            #            "", style='padding:1px; font-size:90%', 
            #           icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `Gruppenzuordnung` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Gruppenzuordnung', 
            #actionButton("Konjunktiv erkennen e)",
            #            "", style='padding:1px; font-size:90%', 
            #           icon = icon("search")) |>
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
  padding-right: 3px;
  width: 20px;
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

data_gt_unsorted_heatmap |> gtsave("test_gtexport_unsorted.png")
