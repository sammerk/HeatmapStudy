tibble(
  `Column Label` = 1:3,
  `Long Column Label` = 1:3
  ) |>
  gt(id = "mygt") |> 
  opt_css(
    css = "#mygt .gt_col_heading {
               transform: rotate(-90deg);
           }"
  )
  