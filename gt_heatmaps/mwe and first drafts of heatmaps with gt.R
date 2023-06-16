##############################################
# mwe and first drafts heatmaps with gt (unordered and ordered) 
#############################################


# libraries
library(shiny)
library(gt)
library(tidyverse)
library(readxl)
library(gtExtras)
library(gfonts)

##################
# mwe: proof of concept 
##################
dummy_data_colsum <- tibble(students = c("student1", "student2", "student3", "colsum"), 
                     rowsum = c(50, 0, 100, 300), 
                     a1 = c(100, 0, 100, 66), 
                     a2 = c(0, 0, 100, 33))

dummy_data <- tibble(students = c("student1", "student2", "student3"), 
                            rowsum = c(50, 0, 100), 
                            a1 = c(100, 0, 100), 
                            a2 = c(0, 0, 100))

## approach 1: margins with summary_rows 
dummy_data%>%
  gt()%>%
  gt_color_rows(columns = c(a1, a2, rowsum), #with color rows -> über data_color scheinbar ein bug, weil bei a14 die Farben getauscht werden (?), daher über color_rows
                domain = c(0, 100),
                palette = c("#E5E5F0", "#2D204C"), 
                na.color = "transparent")%>%
  #data_color(columns = c(a1, a2, rowsum), 
             #palette = c("#E5E5F0", "#2D204C"))%>%
  summary_rows(fns = list("Anteil richtig gelöst pro Aufgabe" = ~ sum(.)/3), 
               groups = ":GRAND_SUMMARY:", #> scheint bug zu sein, https://github.com/rstudio/gt/issues/1292
                     columns = c(a2, rowsum), #c(any_of(c("a1", "a2"))),
                     missing_text = "", 
                     fmt = ~ fmt_number(., decimals = 0))%>% 
  sub_values(columns = c(rowsum, a1, a2),
             fn = function(x) between(x, 0, 100), 
             replacement = "")%>%
  opt_table_lines(extent = c("default"))%>% # hierüber kann man sichtbarkeit der Linien definieren
  tab_style(
    locations = cells_grand_summary(columns = a1), 
    style = cell_fill(color = "#665B7F"))%>%
  tab_style(
    locations = cells_grand_summary(columns = a2), 
    style = list(cell_fill(color = "red"),
                 cell_text(color = "red"))) # not elegant but works
  #opt_stylize(style = 2) # hierüber kann man grundstyle definieren, aber es gibt kein plain, numeric zwischen 1-6. Besser: gtExtras

# approach 2: margins in table body and "styling"
dummy_data_colsum%>%
  gt()%>%
  gt_color_rows(columns = c(a1, a2, rowsum), 
                domain = c(0, 100),
                palette = c("#E5E5F0", "#2D204C"), 
                na.color = "transparent")%>%
  #data_color(columns = c(a1, a2, rowsum), 
  #palette = c("#E5E5F0", "#2D204C"))%>%
  sub_values(columns = c(rowsum, a1, a2),
             fn = function(x) between(x, 0, 100), 
             replacement = "")%>%
  opt_table_lines(extent = c("default"))%>% # hierüber kann man sichtbarkeit der Linien definieren
  tab_style(locations = list(cells_body(columns = c(rowsum), 
                                        rows = everything())), 
                            style = list(cell_borders(sides = c("all"), 
                                                      color = 'black', 
                                                      weight = px(5))))%>%
  tab_style(locations = list(cells_body(columns = everything(), 
                                        rows = c(4))), 
            style = list(cell_borders(sides = c("bottom", "top"), 
                                      color = 'grey', 
                                      weight = px(5))))
  
  
  
############
## preprocessing heatmaps with gt
###########

# raw data
data_nonametask <- read_excel("gt_heatmaps/data/rohdaten_aufgabensummen_aufgabennamen nicht sprechend.xlsx")


# color legend as ggplot element
legend <- 
  tibble(n = 0:100) %>%
  ggplot(aes(x = n, y = "", fill = n)) +
  geom_tile() + 
  scale_fill_gradient(high = "#2D204C", low = "#E5E5F0") + 
  theme_void() +
  theme(legend.position = "none", axis.text.y = element_blank(),
        axis.text.x = element_text(size = 90, family = "Lato"), 
        plot.title = element_text(size = 90, hjust = 0.5, family = "Lato")) + # lato is font in espn-theme
  scale_x_continuous(breaks = seq(0, 100, by = 25), 
                     labels = c("0%", "25%", "50%", "75%", "100%")) + 
  labs(title = "Anteil richtiger Lösungen pro Aufgabe/Schüler:in")

# extracting hex codes for manually inputting in grand_summary_rows -> only needed if approach 1
layer_data(legend)


###########################
# mwe | first draft sorted heatmap
###########################


# sorted order of students
order_students <- c("Schüler:in10", "Schüler:in13", "Schüler:in15", "Schüler:in12", "Schüler:in04", "Schüler:in01", "Schüler:in14", 
                   "Schüler:in02", "Schüler:in06", "Schüler:in05", "Schüler:in18", "Schüler:in17", "Schüler:in07", "Schüler:in09", 
                   "Schüler:in03", "Schüler:in08", "Schüler:in11", "Schüler:in16", "Prozent_richtig") 


# sorted heatmap
data_nonametask%>%
  select(-rowsums)%>%
  mutate(a1 = ifelse(a1 == 1, 100, a1),
         a2 = ifelse(a2 == 1, 100, a2),
         a3 = ifelse(a3 == 1, 100, a3), 
         a4 = ifelse(a4 == 1, 100, a4), 
         a5 = ifelse(a5 == 1, 100, a5), 
         a6 = ifelse(a6 == 1, 100, a6), 
         a7 = ifelse(a7 == 1, 100, a7), 
         a8 = ifelse(a8 == 1, 100, a8), 
         a9 = ifelse(a9 == 1, 100, a9), 
         a10 = ifelse(a10 == 1, 100, a10), 
         a11 = ifelse(a11 == 1, 100, a11), 
         a12 = ifelse(a12 == 1, 100, a12), 
         a13 = ifelse(a13 == 1, 100, a13), 
         a14 = ifelse(a14 == 1, 100, a14),
         a15 = ifelse(a15 == 1, 100, a15), 
         a16 = ifelse(a16 == 1, 100, a16), 
         a17 = ifelse(a17 == 1, 100, a17), 
         a18 = ifelse(a18 == 1, 100, a18), 
         a19 = ifelse(a19 == 1, 100, a19), 
         a20 = ifelse(a20 == 1, 100, a20))%>%
  arrange(match(Schüler_innen, order_students))%>%
  relocate(any_of(c("Schüler_innen", "Prozent_richtig", "a5", "a14", "a6", "a11", "a15", "a10", "a19", "a16", 
                    "a7", "a20", "a17", "a18", "a9", "a13", "a12", "a1", "a4", "a2", "a3", "a8")))%>% # ordnering tasks
  gt()%>%
  gt_color_rows(columns = c(Prozent_richtig, a5, a14, a6, a11, a15, a10, a19, a16, 
                            a7, a20, a17, a18, a9, a13, a12, a1, a4, a2, a3, a8), 
                domain = c(0, 100),
             palette = c("#E5E5F0", "#2D204C"), 
             na.color = "transparent")%>%
  sub_values(columns = c(Prozent_richtig, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20),
             fn = function(x) between(x, 0, 100), 
             replacement = "")%>%
  sub_missing(columns = everything(),
    rows = everything(),
    missing_text = "")%>%
  opt_table_lines(extent = c("default"))%>%
  gt_theme_espn()%>% # kann auch raus, ist optionales Theme aus gtExtra package
  #tab_source_note(gt::html(gt::ggplot_image(legend, aspect_ratio = 6, height = px(55))))%>% # if legend as source note
  tab_header(title = md("Aufgabenergebnisse"), subtitle = gt::html(gt::ggplot_image(legend, aspect_ratio = 6, height = px(55))))%>%
  tab_spanner(columns = 1:2, label = "Anteil richtiger Aufgaben pro Schüler:in (%)", id = "spannerA")%>% # id falls nachher noch gestylt wird 
  tab_spanner(columns = 3:22, label = "Aufgaben", id = "spannerB")%>%
  text_replace(locations = cells_column_labels(columns = c(Prozent_richtig)),
               pattern = "Prozent_richtig",
               replacement = "Anteil richtiger Aufgaben (%)")%>%
  text_replace(locations = cells_body(columns = c(Schüler_innen)),
               pattern = "Prozent_richtig",
               replacement = "ANTEIL RICHTIGER AUFGABEN PRO AUFGABE (%)")%>%
  tab_style(locations = list(cells_body(columns = everything(), 
                                        rows = everything())), 
            style = list(cell_borders(sides = c("all"), 
                                      color = 'white', 
                                      weight = px(0))))%>% #remmoving lines to get non-spaced heatmap
  tab_style(locations = list(cells_body(columns = everything(), 
                                        rows = c(19))), 
            style = list(cell_borders(sides = c("bottom", "top"), 
                                      color = 'white', 
                                      weight = px(4))))%>%
  tab_style(locations = list(cells_body(columns = c(Schüler_innen), 
                                        rows = c(19))), 
            style = list(cell_borders(sides = c("bottom", "top"), 
                                      color = "#e2e2e2", 
                                      weight = px(3))))%>% # can`t google exact grey tone in espn-theme  
  tab_style(locations = list(cells_body(columns = c(Schüler_innen), 
                                        rows = c(19))), 
            style = list(cell_text(style = "oblique"),
                         color = "black",
                         weight = "bold"))%>% #font = c(google_font(name = "Lato") doesn`t work -> default in espn-theme
  tab_style(locations = list(cells_body(columns = c(Prozent_richtig), 
                                        rows = everything())), 
            style = list(cell_borders(sides = c("left", "right"), 
                                      color = 'white', 
                                      weight = px(4))))%>%
  cols_label(
    a1 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Formgleichheit bei Konjunktiv ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ),
    a2 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv 2 bilden ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a3 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv 2 bilden ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a4 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv 2 bilden ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a5 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv 1 und 2 unterscheiden ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a6 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv erkennen ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a7 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv erkennen ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ),   
    a8 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv erkennen ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ),   
    a9 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv erkennen ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a10 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv erkennen ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a11 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Imperativ erkennen ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a12 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Imperativ erkennen ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a13 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Imperativ erkennen ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a14 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Imperativ erkennen ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a15 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Imperativ erkennen ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a16 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Indikativ erkennen ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a17 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Indikativ erkennen ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a18 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Indikativ erkennen ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a19 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Indikativ erkennen ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a20 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Indikativ erkennen ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ))%>%
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
  ) # ? doesn`t work but no error? 


###########################
# mwe | first draft unsorted heatmap
########################### 


# unsorted order of students
unordered_students <- c("Schüler:in13", "Schüler:in18", "Schüler:in17", "Schüler:in15", "Schüler:in02", "Schüler:in16", "Schüler:in11", 
                    "Schüler:in10", "Schüler:in08", "Schüler:in09", "Schüler:in06", "Schüler:in07", "Schüler:in01", "Schüler:in05", 
                    "Schüler:in04", "Schüler:in12", "Schüler:in14", "Schüler:in03", "Prozent_richtig") 

# unsorted heatmap
data_nonametask%>%
  select(-rowsums)%>%
  mutate(a1 = ifelse(a1 == 1, 100, a1),
         a2 = ifelse(a2 == 1, 100, a2),
         a3 = ifelse(a3 == 1, 100, a3), 
         a4 = ifelse(a4 == 1, 100, a4), 
         a5 = ifelse(a5 == 1, 100, a5), 
         a6 = ifelse(a6 == 1, 100, a6), 
         a7 = ifelse(a7 == 1, 100, a7), 
         a8 = ifelse(a8 == 1, 100, a8), 
         a9 = ifelse(a9 == 1, 100, a9), 
         a10 = ifelse(a10 == 1, 100, a10), 
         a11 = ifelse(a11 == 1, 100, a11), 
         a12 = ifelse(a12 == 1, 100, a12), 
         a13 = ifelse(a13 == 1, 100, a13), 
         a14 = ifelse(a14 == 1, 100, a14),
         a15 = ifelse(a15 == 1, 100, a15), 
         a16 = ifelse(a16 == 1, 100, a16), 
         a17 = ifelse(a17 == 1, 100, a17), 
         a18 = ifelse(a18 == 1, 100, a18), 
         a19 = ifelse(a19 == 1, 100, a19), 
         a20 = ifelse(a20 == 1, 100, a20))%>%
  arrange(match(Schüler_innen, unordered_students))%>%
  relocate(any_of(c("Schüler_innen", "Prozent_richtig", "a19", "a17", "a18", "a13", "a8", "a1", "a11", "a16", 
                    "a2", "a7", "a12", "a10", "a6", "a3", "a20", "a9", "a5", "a14", "a4", "a15")))%>% # ordnering tasks
  gt()%>%
  gt_color_rows(columns = c(Prozent_richtig, a5, a14, a6, a11, a15, a10, a19, a16, 
                            a7, a20, a17, a18, a9, a13, a12, a1, a4, a2, a3, a8), 
                domain = c(0, 100),
                palette = c("#E5E5F0", "#2D204C"), 
                na.color = "transparent")%>%
  sub_values(columns = c(Prozent_richtig, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20),
             fn = function(x) between(x, 0, 100), 
             replacement = "")%>%
  sub_missing(columns = everything(),
              rows = everything(),
              missing_text = "")%>%
  opt_table_lines(extent = c("default"))%>%
  gt_theme_espn()%>% # kann auch raus, ist optionales Theme aus gtExtra package
  #tab_source_note(gt::html(gt::ggplot_image(legend, aspect_ratio = 6, height = px(55))))%>% # falls Legend als Note
  tab_header(title = md("Aufgabenergebnisse"), subtitle = gt::html(gt::ggplot_image(legend, aspect_ratio = 6, height = px(55))))%>%
  tab_spanner(columns = 1:2, label = "Anteil richtiger Aufgaben pro Schüler:in (%)", id = "spannerA")%>% # id falls nachher noch gestylt wird 
  tab_spanner(columns = 3:22, label = "Aufgaben", id = "spannerB")%>%  #id falls nachher noch gestylt wird
  text_replace(locations = cells_column_labels(columns = c(Prozent_richtig)),
               pattern = "Prozent_richtig",
               replacement = "Anteil richtiger Aufgaben (%)")%>%
  text_replace(locations = cells_body(columns = c(Schüler_innen)),
               pattern = "Prozent_richtig",
               replacement = "ANTEIL RICHTIGER AUFGABEN PRO AUFGABE (%)")%>%
  tab_style(locations = list(cells_body(columns = everything(), 
                                        rows = everything())), 
            style = list(cell_borders(sides = c("all"), 
                                      color = 'white', 
                                      weight = px(0))))%>% #remmoving lines to get non-spaced heatmap
  tab_style(locations = list(cells_body(columns = everything(), 
                                        rows = c(19))), 
            style = list(cell_borders(sides = c("bottom", "top"), 
                                      color = 'white', 
                                      weight = px(4))))%>%
  tab_style(locations = list(cells_body(columns = c(Schüler_innen), 
                                        rows = c(19))), 
            style = list(cell_borders(sides = c("bottom", "top"), 
                                      color = "#e2e2e2", 
                                      weight = px(3))))%>% # can`t google exact grey tone in espn-theme  
  tab_style(locations = list(cells_body(columns = c(Schüler_innen), 
                                        rows = c(19))), 
            style = list(cell_text(style = "oblique"),
                         color = "black",
                         weight = "bold"))%>% #font = c(google_font(name = "Lato") doesn`t work -> default in espn-theme
  tab_style(locations = list(cells_body(columns = c(Prozent_richtig), 
                                        rows = everything())), 
            style = list(cell_borders(sides = c("left", "right"), 
                                      color = 'white', 
                                      weight = px(4))))%>%
  cols_label(
    a1 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Formgleichheit bei Konjunktiv ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ),
    a2 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv 2 bilden ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a3 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv 2 bilden ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a4 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv 2 bilden ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a5 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv 1 und 2 unterscheiden ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a6 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv erkennen ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a7 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv erkennen ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ),   
    a8 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv erkennen ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ),   
    a9 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv erkennen ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a10 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv erkennen ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a11 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Imperativ erkennen ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a12 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Imperativ erkennen ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a13 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Imperativ erkennen ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a14 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Imperativ erkennen ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a15 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Imperativ erkennen ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a16 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Indikativ erkennen ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a17 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Indikativ erkennen ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a18 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Indikativ erkennen ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a19 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Indikativ erkennen ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ), 
    a20 = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Indikativ erkennen ', 
            actionButton("b1",
                         "",
                         icon = icon("search")) |>
              as.character(),
            "</div></div>"
      )
    ))%>%
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
  ) # ? doesn`t work but no error? 
