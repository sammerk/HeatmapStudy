
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
            #  actionButton("Konjunktiv erkennen a)",
            #              "", style='padding:1px; font-size:90%', 
            #             icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `Imperativ erkennen a)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Imperativ erkennen a)', 
            #  actionButton("Imperativ erkennen a)",
            #              "", style='padding:1px; font-size:90%', 
            #             icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `Imperativ erkennen b)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Imperativ erkennen b)', 
            #  actionButton("Imperativ erkennen b)",
            #              "", style='padding:1px; font-size:90%', 
            #             icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `Imperativ erkennen c)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Imperativ erkennen c)', 
            #  actionButton("Imperativ erkennen c)",
            #              "", style='padding:1px; font-size:90%', 
            #             icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `Imperativ erkennen d)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Imperativ erkennen d)', 
            #  actionButton("Imperativ erkennen d)",
            #              "", style='padding:1px; font-size:90%', 
            #             icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `Imperativ erkennen e)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Imperativ erkennen e)', 
            #   actionButton("Imperativ erkennen e)",
            #               "", style='padding:1px; font-size:90%', 
            #              icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `Indikativ erkennen a)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Indikativ erkennen a)', 
            # actionButton("Indikativ erkennen a)",
            #             "", style='padding:1px; font-size:90%', 
            #            icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `Indikativ erkennen b)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Indikativ erkennen b)', 
            #  actionButton("Indikativ erkennen b)",
            #              "", style='padding:1px; font-size:90%', 
            #             icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `Indikativ erkennen c)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Indikativ erkennen c)', 
            # actionButton("Indikativ erkennen c)",
            #             "", style='padding:1px; font-size:90%', 
            #            icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `Indikativ erkennen d)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Indikativ erkennen d)', 
            #actionButton("Indikativ erkennen d)",
            #            "", style='padding:1px; font-size:90%', 
            #           icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `Konjunktiv II bilden a)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv II bilden a)', 
            # actionButton("Konjunktiv II bilden a)",
            #             "", style='padding:1px; font-size:90%', 
            #            icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `Indikativ erkennen e)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Indikativ erkennen e)', 
            #actionButton("Indikativ erkennen e)",
            #            "", style='padding:1px; font-size:90%', 
            #           icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `Konjunktiv II bilden b)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv II bilden b)', 
            #  actionButton("Konjunktiv II bilden b)",
            #              "", style='padding:1px; font-size:90%', 
            #             icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `Konjunktiv II bilden c)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv II bilden c)', 
            #  actionButton("Konjunktiv II bilden c)",
            #              "", style='padding:1px; font-size:90%', 
            #             icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `Unterschied Konjunktiv I/II` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Unterschied Konjunktiv I/II', 
            # actionButton("Unterschied Konjunktiv I/II",
            #             "", style='padding:1px; font-size:90%', 
            #            icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `Konjunktiv erkennen b)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv erkennen b)', 
            #  actionButton("Konjunktiv erkennen b)",
            #              "", style='padding:1px; font-size:90%', 
            #             icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `Konjunktiv erkennen c)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv erkennen c)', 
            #  actionButton("Konjunktiv erkennen c)",
            #              "", style='padding:1px; font-size:90%', 
            #             icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `Konjunktiv erkennen d)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv erkennen d)', 
            #  actionButton("Konjunktiv erkennen d)",
            #              "", style='padding:1px; font-size:90%', 
            #             icon = icon("search")) |>
            as.character(),
            "</div></div>")), 
    `Konjunktiv erkennen e)` = gt::html(
      paste('<div class="rotation-wrapper-outer">
                  <div class="rotation-wrapper-inner">Konjunktiv erkennen e)', 
            #  actionButton("Konjunktiv erkennen e)",
            #              "", style='padding:1px; font-size:90%', 
            #             icon = icon("search")) |>
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


