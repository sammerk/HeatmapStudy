tibble(
  `Column Label` = 1:3,
  `Long Column Label` = 1:3
  ) |>
  gt(id = "mygt") |> 
  cols_label(
    `Column Label` = gt::html(
      paste(
        '<div class="rotation-wrapper-outer">
                  <span class="rotation-wrapper-inner">',
        "Column Label",
        "</span></div>"
      )),
      `Long Column Label` = gt::html(
        paste(
          '<div class="rotation-wrapper-outer">
                  <span class="rotation-wrapper-inner">',
          "Long Column Label",
          "</span></div>"
        ))
  ) |> 
  opt_css(
    css = "
    
  #mygt  th.gt_col_heading {
  /* Something you can count on */
  height: 140px;
  white-space: nowrap;
  overflow-x: inherit;
  
}

#mygt th.gt_col_heading > div {
  transform: 
    /* Magic Numbers */
    translate(25px, -10px)
    /* 45 is really 360 - 45 */
    rotate(315deg);
  width: 30px;
  overflow-x: inherit;
  
}
#mygt th.gt_col_heading > div > span {
  border-bottom: 1px solid #ccc;
  padding: 5px 10px;
  overflow-x: inherit;
}

"
  )
  