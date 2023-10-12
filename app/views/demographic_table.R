box::use(
  rtables[
    add_colcounts, analyze, as_html, basic_table,
    build_table, in_rows, list_wrap_x, split_cols_by
  ],
  shiny[NS, renderUI, uiOutput],
  stats[median, sd],
)

#' ui_t_demographic UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @export
ui <- function(id, datasets) {
  ns <- NS(id)
  uiOutput(ns("table"), class = "top-margin")
}

#' srv_t_demographic Server Functions
#'
#' @export
server <- function(input, output, session, datasets) {
  output$table <- renderUI({
    if (is.null(datasets)) {
      return(NULL)
    }

    filtered_adsl <- datasets$get_data("ADSL", filtered = FALSE)
    vars <- c("AGE", "AGEGR1", "RACE", "HEIGHTBL", "WEIGHTBL", "BMIBL")
    labels <- datasets$get_varlabels("ADSL", vars)
    labels <- vapply(vars, function(x) {
      ifelse(is.na(labels[[x]]),
        x, labels[[x]]
      )
    }, character(1))
    labels["AGEGR1"] <- "Age group"
    labels["AGE"] <- "Age (year)"
    labels["RACE"] <- "Race"
    lyt <- basic_table(
      title = "Protocol: CDISCPILOT01",
      subtitles = "Population: Intent-to-Treat",
      main_footer = paste0(Sys.time())
    ) |>
      split_cols_by("TRT01P") |>
      add_colcounts() |>
      analyze(vars, function(x, ...) {
        if (is.numeric(x)) {
          in_rows(
            "Mean (SD)" = c(mean(x), sd(x)),
            "Median" = median(x),
            "Min - Max" = range(x),
            .formats = c("xx.xx (xx.xx)", "xx.xx", "xx.xx - xx.xx")
          )
        } else if (is.factor(x) || is.character(x)) {
          in_rows(.list = list_wrap_x(table)(x))
        } else {
          stop("type not supproted")
        }
      },
      var_labels = labels
      )
    tbl <- build_table(lyt, filtered_adsl)
    as_html(tbl)
  })
}
