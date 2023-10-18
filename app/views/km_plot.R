box::use(
  cowplot[draw_label, ggdraw, plot_grid],
  dplyr[filter, inner_join, mutate, select],
  ggplot2[element_text, geom_hline, rel, theme, theme_bw, theme_set],
  shiny[NS, p, plotOutput, renderPlot, tagList, tags],
  visR[add_CI, add_CNSR, estimate_KM, visr],
)

box::use(
  .. / logic / kmplot_helpers[add_risktable2],
)

#' @export
ui <- function(id, datasets) {
  ns <- NS(id)

  tagList(
    tags$div(
      class = "alert alert-info alert-dismissible top-margin top-margin",
      tagList(
        tags$b("Important Information:"),
        tags$p(
          "The analyses performed when utilizing subgroups or
          other subsets of the source data sets are considered ",
          tags$b("exploratory.")
        ),
        tags$ul(
          tags$li(
            "Treatment information variables from the",
            tags$b("ADTTE"),
            "data set are excluded from the variable list.
            Use the treatment variables present in the",
            tags$b("ADSL"),
            "set to perform treatment-related filters."
          ),
          tags$li(
            "In rare situations, applying filters with variables from both",
            tags$b("ADSL"), "and", tags$b("ADTTE"),
            "that overlap in content could result in an invalid data subset.
            When possible, select variables with distinct content."
          )
        )
      )
    ),
    plotOutput(ns("plot"), height = "800px")
  )
}

#' @export
server <- function(input, output, session, datasets) {
  output$plot <- renderPlot({
    adsl <- datasets$get_data("ADSL", filtered = TRUE)
    adtte <- datasets$get_data("ADTTE", filtered = TRUE)
    anl <- adsl |>
      filter(
        SAFFL == "Y",
        STUDYID == "CDISCPILOT01"
      ) |>
      select(STUDYID, USUBJID, TRT01A) |>
      inner_join(
        filter(
          adtte, STUDYID == "CDISCPILOT01"
        ) |> select(STUDYID, USUBJID, AVAL, CNSR, PARAM, PARAMCD),
        by = c("STUDYID", "USUBJID")
      ) |>
      mutate(
        TRT01A = factor(
          TRT01A,
          levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")
        ),
        AVAL = AVAL / 30.4167
      )

    surv_mod <- estimate_KM(data = anl, strata = "TRT01A")
    theme_set(theme_bw())

    km_plot <- visr(surv_mod,
      y_label = "Survival Probability (%)",
      x_label = "Time (Months)",
      fun = "pct",
      legend_position = "bottom"
    ) |>
      add_CNSR() |>
      add_CI()

    km_plot <- km_plot +
      theme(
        axis.text = element_text(size = rel(1.3)),
        axis.title = element_text(size = rel(1.4)),
        legend.text = element_text(size = rel(1.3)),
        legend.title = element_text(size = rel(1.4))
      ) +
      geom_hline(yintercept = 0.5, linetype = "dashed")

    km_plot <- km_plot |>
      add_risktable2(group = "statlist")

    title <- ggdraw() +
      draw_label(
        "KM plot for Time to First Dermatologic Event: Safety population\n",
        fontface = "bold",
        size = 16
      )

    caption <- ggdraw() +
      draw_label(
        paste(
          "The shaded areas are 95% CI of the survival probability for each group",
          "\n",
          paste0(Sys.time())
        ),
        size = 12
      )

    # km_plot <- plot_grid(
    #   title, km_plot, caption,
    #   ncol = 1,
    #   rel_heights = c(0.1, 0.8, 0.1)
    # )
    km_plot
  })
}
