box::use(
  Tplyr[
    add_column_headers, add_layer, build, f_str, group_desc, header_n, set_desc_layer_formats,
    set_distinct_by, set_pop_data, set_pop_treat_var, set_pop_where, tplyr_table
  ],
  dplyr[bind_rows, select, starts_with],
  huxtable[
    as_hux, set_align, set_bold, set_bottom_border, set_col_width,
    set_escape_contents, set_valign, set_width, to_html
  ],
  rtables[ncol],
  shiny[HTML, NS, p, renderUI, tagList, uiOutput],
)

box::use(
  .. / logic / eff_models[efficacy_models],
  .. / logic / Tplyr_helpers[nest_rowlabels],
  .. / logic / helpers[filter_active],
)

#' @export
ui <- function(id, datasets) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("table")),
    p(
      "Statistical model and comparison p-values removed when applying data filters.
      Refer to the application information for additional details."
    ),
    p(
      "[1] Based on Analysis of covariance (ANCOVA) model with treatment and
      site group as factors and baseline value as a covariate."
    ),
    p(
      "[2] Test for a non-zero coefficient for treatment (dose) as a continuous variable."
    ),
    p(
      "[3] Pairwise comparison with treatment as a categorical variable:
      p-values without adjustment for multiple comparisons."
    )
  )
}

#' @export
server <- function(input, output, session, datasets) {
  output$table <- renderUI({
    if (is.null(datasets)) {
      return(NULL)
    }

    filtered_adsl <- datasets$get_data("ADSL", filtered = FALSE)
    filtered_adas <- datasets$get_data("ADAS", filtered = FALSE)
    adas <- filtered_adas

    t <- tplyr_table(adas, TRTP) |>
      set_pop_data(filtered_adsl) |>
      set_pop_treat_var(TRT01P) |>
      set_pop_where(EFFFL == "Y" & ITTFL == "Y") |>
      set_distinct_by(USUBJID) |>
      set_desc_layer_formats(
        "n" = f_str("xx", n),
        "Mean (SD)" = f_str("xx.x (xx.xx)", mean, sd),
        "Median (Min; Max)" = f_str("xx.x (xxx;xx)", median, min, max)
      ) |>
      add_layer(
        group_desc(AVAL, where = AVISITN == 0, by = "Baseline")
      ) |>
      add_layer(
        group_desc(AVAL, where = AVISITN == 24, by = "Week 24")
      ) |>
      add_layer(
        group_desc(CHG, where = AVISITN == 24, by = "Change from Baseline")
      )

    sum_data <- t |>
      build() |>
      nest_rowlabels() |>
      select(-starts_with("ord")) |>
      add_column_headers(
        paste0(
          "|Placebo</br>(N=**Placebo**)| Xanomeline Low Dose</br>(N=**Xanomeline Low Dose**) ",
          "| Xanomeline High Dose</br>(N=**Xanomeline High Dose**)"
        ),
        header_n(t)
      )
    model_portion <- efficacy_models(adas, "CHG", 24, !filter_active(datasets))

    final <- bind_rows(sum_data, model_portion)

    ht <- as_hux(final, add_colnames = FALSE) |>
      set_bold(1, seq_len(ncol(final)), TRUE) |>
      set_align(1, seq_len(ncol(final)), "center") |>
      set_valign(1, seq_len(ncol(final)), "bottom") |>
      set_bottom_border(1, seq_len(ncol(final)), 1) |>
      set_width(1) |>
      set_escape_contents(FALSE) |>
      set_col_width(c(.5, 1 / 6, 1 / 6, 1 / 6))
    HTML(to_html(ht))
  })
}
