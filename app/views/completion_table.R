box::use(
  Tplyr[
    add_column_headers, add_layer, add_total_group, build, f_str,
    group_count, header_n, set_distinct_by, set_format_strings,
    set_pop_data, set_pop_treat_var, tplyr_table
  ],
  dplyr[distinct, filter, left_join, mutate, select, tibble],
  huxtable[
    as_hux, set_align, set_bold, set_bottom_border,
    set_col_width, set_escape_contents, set_valign, set_width, to_html
  ],
  rtables[ncol],
  shiny[HTML, NS, p, renderUI, tagList, uiOutput],
)

#' @export
ui <- function(id, datasets) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("table")),
    p("Table is based on participants within the ITT population")
  )
}

#' @export
server <- function(input, output, session, datasets) {
  output$table <- renderUI({
    filtered_adsl <- datasets$get_data("ADSL", filtered = FALSE)
    filtered_adlb <- datasets$get_data("ADLB", filtered = FALSE)
    adsl <- filtered_adsl
    adlbc <- filtered_adlb

    # use adlbc data set to remain consistent with efficacy table input data
    visit_df <- adlbc |>
      filter(PARAMCD == "GLUC") |>
      filter(AVISITN != 98) |>
      filter(!is.na(AVISITN)) |>
      select(USUBJID, AVISITN) |>
      distinct() |>
      left_join(
        select(adsl, USUBJID, TRT01P),
        by = "USUBJID"
      )

    # visit number and week lookup
    v_week_df <- tibble(
      AVISITN = c(0, 2, 4, 6, 8, 12, 16, 20, 24, 26, 99),
      VISIT = c("Baseline ", paste("Week", c(2, 4, 6, 8, 12, 16, 20, 24, 26)), "End of Treatment")
    ) |>
      mutate(
        VISIT = factor(
          VISIT,
          levels = c(
            "Baseline ",
            paste("Week", c(2, 4, 6, 8, 12, 16, 20, 24, 26)),
            "End of Treatment"
          )
        )
      )

    # build Tplyr table
    t_visit <- visit_df |>
      left_join(v_week_df, by = "AVISITN") |>
      tplyr_table(TRT01P) |>
      set_pop_data(adsl) |>
      set_pop_treat_var(TRT01P) |>
      add_total_group() |>
      add_layer(
        group_count(VISIT) |>
          set_distinct_by(USUBJID) |>
          set_format_strings(
            f_str("xx (xx%)", distinct_n, distinct_pct)
          )
      )

    b_t_visit <- t_visit |>
      build() |>
      select(
        row_label1, var1_Placebo, `var1_Xanomeline High Dose`,
        `var1_Xanomeline Low Dose`, var1_Total
      ) |>
      add_column_headers(
        paste0(
          "|Placebo</br>(N=**Placebo**)",
          "| Xanomeline High Dose</br>(N=**Xanomeline High Dose**) ",
          "| Xanomeline Low Dose</br>(N=**Xanomeline Low Dose**) ",
          "| Total</br>(N=**Total**) "
        ),
        header_n(t_visit)
      )

    ht <- as_hux(b_t_visit, add_colnames = FALSE) |>
      set_bold(1, seq_len(ncol(b_t_visit)), TRUE) |>
      set_align(1, seq_len(ncol(b_t_visit)), "center") |>
      set_valign(1, seq_len(ncol(b_t_visit)), "bottom") |>
      set_bottom_border(1, seq_len(ncol(b_t_visit)), 1) |>
      set_width(0.9) |>
      set_escape_contents(FALSE) |>
      set_col_width(c(.5, 1 / 8, 1 / 8, 1 / 8, 1 / 8))
    HTML(to_html(ht))
  })
}
