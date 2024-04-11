box::use(
  dplyr[filter, inner_join, mutate, select],
  ggplot2[element_text, geom_hline, rel, theme, theme_bw, theme_set, labs],
  shiny[
    NS, observeEvent, p, plotOutput, reactiveValues, renderPlot, renderUI, selectInput,
    tagAppendAttributes, tagList, tags, uiOutput, updateSelectInput,
    fluidRow, column
  ],
  ggsurvfit[
    Surv, survfit2, ggsurvfit, add_confidence_interval, add_risktable,
    add_censor_mark, scale_ggsurvfit
  ],
  scales[breaks_width]
)

box::use(
  .. / logic / kmplot_helpers[add_risktable2],
  . / km_plot_filter
)

alert_message <- function() {
  tags$div(
    class = "alert alert-info alert-dismissible top-margin top-margin",
    style = "min-height: 100px",

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
  )
}

#' @export
ui <- function(id, datasets) {
  ns <- NS(id)

  fluidRow(
    column(
      width = 9,

      tags$div(
        class = "background-wrapper",

        alert_message(),
        uiOutput(ns("plot_title") ,class = "plot-message"),
        plotOutput(ns("plot"), height = "600px"),
        uiOutput(ns("plot_footer"), class = "plot-message")
      )
    ),
    column(
      width = 3,

      tags$div(
        class = "background-wrapper filter-card",
        km_plot_filter$ui(ns("adsl"), "ADSL")
      ),

      tags$div(
        class = "background-wrapper filter-card",
        km_plot_filter$ui(ns("adtte"), "ADTTE")
      )
    )
  )
}

#' @export
server <- function(input, output, session, datasets) {
  adsl_ <- km_plot_filter$server("adsl", "ADSL", datasets$get_data("ADSL"))
  adtte_ <- km_plot_filter$server("adtte", "ADTTE", datasets$get_data("ADTTE"))

  output$plot <- renderPlot({
    adsl <- adsl_()
    adtte <- adtte_()
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

    surv_mod <- survfit2(Surv(AVAL, 1 - CNSR) ~ TRT01A, data = anl)
    theme_set(theme_bw())

    km_plot <- ggsurvfit(surv_mod) +
      add_censor_mark() +
      add_confidence_interval() +
      add_risktable(
        risktable_stats = "n.risk",
        size = rel(5)
      ) +
      labs(
        y = "Survival Probability",
        x = "Time (Months)",
      )

    km_plot <- km_plot +
      theme(
        axis.text = element_text(size = rel(1.3)),
        axis.title = element_text(size = rel(1.4)),
        legend.text = element_text(size = rel(1.3)),
        legend.title = element_text(size = rel(1.4))
      ) +
      scale_ggsurvfit(
        x_scales = list(
          breaks = scales::breaks_width(0.5)
        )
      ) +
      geom_hline(yintercept = 0.05, linetype = "dashed")

    output$plot_title <- renderUI({
      tags$div(
        style = "font-weight: bold; font-size: 16px",
        "KM plot for Time to First Dermatologic Event: Safety population",
      )
    })

    output$plot_footer <- renderUI({
      tagList(
        tags$div(style = "font-size: 12px", "The shaded areas are 95% CI of the survival probability for each group"),
        tags$div(style = "font-size: 12px", Sys.time())
      )
    })

    km_plot
  })
}
