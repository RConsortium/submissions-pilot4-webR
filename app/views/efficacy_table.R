box::use(
  dplyr[filter, group_by, mutate, n, right_join, select, summarise],
  emmeans[emmeans],
  graphics[pairs],
  purrr[map2],
  reactable[colDef, colGroup, reactable, reactableOutput, renderReactable],
  rtables[ncol],
  shiny[NS, br, column, fluidPage, fluidRow, h4, h6, hr, p, reactive, tags],
  stats[lm, sd],
  tippy[tippy],
)
box::use(
  .. / logic / formatters[fmt_est, fmt_ci, fmt_pval],
  .. / logic / helpers[tooltip_text, filter_active],
)

#' @export
ui <- function(id, datasets) {
  ns <- NS(id)
  fluidPage(
    tags$br(),
    tags$br(),
    fluidRow(
      tippy(
        h4("Primary Endpoint Analysis: Glucose (mmol/L) - Summary at Week 20 LOCF"),
        tooltip = tooltip_text(
          "Table is based on participants who have observable data at Baseline and Week 20",
          font_size = 16
        ),
        allowHTML = TRUE
      ),
      tags$br(), tags$br(),
      column(
        width = 10,
        reactableOutput(ns("tbl_efficacy_1"))
      )
    ),
    tags$br(),
    tags$br(),
    tags$hr(),
    fluidRow(
      tippy(
        h4("Pairwise Comparison"),
        tooltip = tooltip_text(
          "Inference in this table is based on a Analysis of Covariance (ANCOVA) model
          with treatment and baseline value as covariates.",
          font_size = 16
        ),
        allowHTML = TRUE
      ),
      tags$br(),
      tags$br(),
      column(
        width = 10,
        reactableOutput(ns("tbl_efficacy_2"))
      )
    ),
    tags$br(),
    tags$br(),
    tags$hr(),
    fluidRow(
      h6(tags$i(
        "Abbreviations: CI=Confidence Interval; LS=Least Squares; SD=Standard Deviation"
      )),
      h6(tags$p(
        "Table is based on participants who had observable data at Baseline and Week 20"
      )),
      h6(tags$p(
        "Based on an Analysis of Covariance (ANCOVA) model
        with treatment and baseline value as covariates"
      ))
    )
  )
}

#' @export
server <- function(input, output, session, datasets) {
  efficacy_results <- reactive({
    adsl <- datasets$get_data("ADSL", filtered = FALSE)

    itt <- adsl |>
      filter(ITTFL == "Y") |>
      select("STUDYID", "USUBJID")

    adlb <- datasets$get_data("ADLB", filtered = FALSE)

    # prepare labs data for pairwise comparison
    adlb1 <- adlb |>
      right_join(itt, by = c("STUDYID", "USUBJID")) |>
      filter(TRTPN %in% c(0, 81), PARAMCD == "GLUC", !is.na(AVISITN)) |>
      mutate(TRTPN = ifelse(TRTPN == 0, 99, TRTPN))

    gluc_lmfit <- lm(
      CHG ~ BASE + TRTPN,
      data = adlb1 |>
        filter(AVISITN == 20)
    )

    t10 <- adlb1 |>
      filter(AVISITN == 0) |>
      group_by(TRTPN, TRTP) |>
      summarise(
        N = n(),
        mean_bl = mean(BASE),
        sd_bl = sd(BASE)
      )

    ## Raw summary statistics
    t11 <- adlb1 |>
      filter(AVISITN == 20, !is.na(CHG), !is.na(BASE)) |>
      group_by(TRTPN, TRTP) |>
      summarise(
        N_20 = n(),
        mean_chg = mean(CHG),
        sd_chg = sd(CHG),
        mean = mean(AVAL),
        sd = sd(AVAL)
      )

    ## Calculate LS mean
    t12 <- emmeans(gluc_lmfit, "TRTPN")

    ## Merge and format data for reporting
    apr0ancova1 <- merge(t10, t11) |>
      merge(t12) |>
      mutate(emmean_sd = SE * sqrt(df)) |>
      mutate(
        Trt = c("Xanomeline High Dose", "Placebo"),
        N1 = N,
        Mean1 = fmt_est(mean_bl, sd_bl),
        N2 = N_20,
        Mean2 = fmt_est(mean, sd),
        N3 = N_20,
        Mean3 = fmt_est(mean_chg, sd_chg),
        CI = fmt_ci(emmean, lower.CL, upper.CL)
      ) |>
      select(Trt:CI)

    t2 <- data.frame(pairs(t12))

    ## Treatment Comparison
    apr0ancova2 <- t2 |>
      mutate(
        lower = estimate - 1.96 * SE,
        upper = estimate + 1.96 * SE
      ) |>
      mutate(
        comp = "Study Drug vs. Placebo",
        mean = fmt_ci(estimate, lower, upper),
        p = ifelse(filter_active(datasets), "Not Applicable", fmt_pval(p.value))
      ) |>
      select(comp:p)

    ### Calculate root mean square and save data in output folder
    apr0ancova3 <- data.frame(rmse = paste0(
      "Root Mean Squared Error of Change = ",
      formatC(sqrt(mean((gluc_lmfit$residuals)^2)), digits = 2, format = "f", flag = "0")
    ))
    list(
      apr0ancova1 = apr0ancova1,
      apr0ancova2 = apr0ancova2,
      apr0ancova3 = apr0ancova3
    )
  })
  output$tbl_efficacy_1 <- renderReactable({
    efficacy_results <- efficacy_results()
    apr0ancova1 <- efficacy_results$apr0ancova1
    coln <- c(
      "Treatment",
      "N", "Mean (SD)",
      "N", "Mean (SD)",
      "N", "Mean (SD)", "LS Mean (95% CI)"
    )
    colgr <- c(1, 2, 2, 3, 3, 4, 4, 4)
    colwidths <- c(rep(100, 7), 150)
    colgrn <- c("", "Baseline", "Week 20", "Change from Baseline")
    collist <- map2(seq_len(ncol(apr0ancova1)), colwidths, ~ {
      colDef(name = coln[.x], minWidth = .y)
    })
    names(collist) <- names(apr0ancova1)
    reactable(
      apr0ancova1,
      columns = collist,
      columnGroups = list(
        colGroup(name = colgrn[2], columns = names(apr0ancova1)[colgr == 2]),
        colGroup(name = colgrn[3], columns = names(apr0ancova1)[colgr == 3]),
        colGroup(name = colgrn[4], columns = names(apr0ancova1)[colgr == 4])
      )
    )
  })
  output$tbl_efficacy_2 <- renderReactable({
    efficacy_results <- efficacy_results()
    apr0ancova2 <- efficacy_results$apr0ancova2
    apr0ancova3 <- efficacy_results$apr0ancova3
    coln <- c(
      "",
      "Difference in LS Mean (95% CI)",
      "p-Value"
    )
    collist <- lapply(seq_len(ncol(apr0ancova2)), function(xx) {
      if (xx > 1) {
        colDef(name = coln[xx])
      } else {
        colDef(name = coln[xx], footer = apr0ancova3$rmse)
      }
    })
    names(collist) <- names(apr0ancova2)

    reactable(
      apr0ancova2,
      columns = collist,
      defaultColDef = colDef(footerStyle = list(fontStyle = "italic"))
    )
  })
}
