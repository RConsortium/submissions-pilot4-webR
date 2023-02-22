box::use(
  shiny[...],
  rtables[...],
  ggplot2[...],
  Tplyr[...],
  reactable[...],
  dplyr[...],
  tippy[...],
)

fmt_num <- function(x, digits, width = digits + 4) {
  formatC(x,
    digits = digits,
    format = "f",
    width = width
  )
}

fmt_est <- function(.mean,
                    .sd,
                    digits = c(1, 2)) {
  .mean <- fmt_num(.mean, digits[1], width = digits[1] + 4)
  .sd <- fmt_num(.sd, digits[2], width = digits[2] + 3)
  paste0(.mean, " (", .sd, ")")
}

fmt_ci <- function(.est,
                   .lower,
                   .upper,
                   digits = 2,
                   width = digits + 3) {
  .est <- fmt_num(.est, digits, width)
  .lower <- fmt_num(.lower, digits, width)
  .upper <- fmt_num(.upper, digits, width)
  paste0(.est, " (", .lower, ",", .upper, ")")
}

fmt_pval <- function(.p, digits = 3) {
  scale <- 10^(-1 * digits)
  p_scale <- paste0("<", digits)
  ifelse(.p < scale, p_scale, fmt_num(.p, digits = digits))
}

pad_row <- function(.data, n = 1) {
  .data[(nrow(.data) + 1):(nrow(.data) + n), ] <- ""
  .data
}

num_fmt <- Vectorize(function(var, digits = 0, size = 10, int_len = 3) {
  # Formats summary stat strings to align display correctly

  if (is.na(var)) {
    return("")
  }

  # Set nsmall to input digits
  nsmall <- digits

  # Incremement digits for to compensate for display
  if (digits > 0) {
    digits <- digits + 1
  }

  # Form the string
  return(stringr::str_pad(
    format(
      # Round
      round(var, nsmall),
      # Set width of format string
      width = (int_len + digits),
      # Decimals to display
      nsmall = nsmall
    ),
    # Overall width padding
    side = "right", size
  ))
})


efficacy_models <- function(data, var = NULL, wk = NULL, show_pvalue = TRUE) {
  # Need to set contrasts to work for Type III SS. See analysis results metadata for
  # table 14-3.01. Reference for R here: https://www.r-bloggers.com/anova-%E2%80%93-type-iiiiii-ss-explained/
  op <- options(contrasts = c("contr.sum", "contr.poly"))

  # Subset to analyze
  data <- data %>%
    filter(AVISITN == wk)

  data <- data %>%
    mutate(
      TRTPCD = case_when(
        TRTPN == 0 ~ "Pbo",
        TRTPN == 54 ~ "Xan_Lo",
        TRTPN == 81 ~ "Xan_Hi"
      )
    )

  # Create an ordered factor variable for the models
  data["TRTPCD_F"] <- factor(data$TRTPCD, levels = c("Xan_Hi", "Xan_Lo", "Pbo"))
  data["AWEEKC"] <- factor(data$AVISIT)

  # Set up the models
  if (var == "CHG") {
    model1 <- lm(CHG ~ TRTPN + SITEGR1 + BASE, data = data)
    model2 <- lm(CHG ~ TRTPCD_F + SITEGR1 + BASE, data = data)
  } else {
    model1 <- lm(AVAL ~ TRTPN + SITEGR1, data = data)
    model2 <- lm(AVAL ~ TRTPCD_F + SITEGR1, data = data)
  }

  ## Dose Response --- NOTE: For statistics portions, I purposefully did not
  # import the libraries to make it explicitly clear which packages were being
  # used to match P-values.
  ancova <- drop1(model1, . ~ ., test = "F")

  # Pull it out into a table
  sect1 <- tibble::tibble(
    row_label = c("p-value(Dose Response) [1][2]"),
    `81` = ifelse(show_pvalue, c(num_fmt(ancova[2, "Pr(>F)"], int_len = 4, digits = 3, size = 12)), "Not Applicable")
  ) %>%
    pad_row()

  ## Pairwise Comparisons ----
  # Here's a reference for the emmeans package and how to use it:
  #   https://cran.r-project.org/web/packages/emmeans/vignettes/confidence-intervals.html
  # Adjustments made are in line with the analysis results metadata in the analysis define
  # and PROC GLM documentation.

  # Linear model but use treatment group as a factor now
  # LS Means and weight proportionately to match OM option on PROC GLM in SAS
  lsm <- emmeans::lsmeans(model2, ~TRTPCD_F, weights = "proportional")

  # Here on out - it's all the same data manipulation
  # Get pairwise contrast and remove P-values adjustment for multiple groups
  cntrst_p <- emmeans::contrast(lsm, method = "pairwise", adjust = NULL)
  # 95% CI
  cntrst_ci <- confint(cntrst_p)

  # merge and convert into dataframe
  pw_data <- tibble::as_tibble(summary(cntrst_p)) %>%
    merge(tibble::as_tibble(cntrst_ci)) %>%
    rowwise() %>%
    # Create the display strings
    mutate(
      p = ifelse(show_pvalue, num_fmt(p.value, int_len = 4, digits = 3, size = 12), "Not Applicable"),
      diff_se = as.character(
        glue::glue("{num_fmt(estimate, int_len=2, digits=1, size=4)} ({num_fmt(SE, int_len=1, digits=2, size=4)})")
      ),
      ci = as.character(
        glue::glue("({num_fmt(lower.CL, int_len=2, digits=1, size=4)};{num_fmt(upper.CL, int_len=1, digits=1, size=3)})")
      )
    ) %>%
    # Clean out the numeric variables
    select(contrast, p, diff_se, ci) %>%
    # Transpose
    tidyr::pivot_longer(c("p", "diff_se", "ci"), names_to = "row_label")

  # Subset Xan_Lo - Pbo into table variables
  xan_lo <- pw_data %>%
    filter(contrast == "Xan_Lo - Pbo") %>%
    # Rename to the table display variable
    select(`54` = value) %>%
    pad_row()

  # Add in row_label
  xan_lo["row_label"] <- c("p-value(Xan - Placebo) [1][3]", "  Diff of LS Means (SE)", "  95% CI", "")

  # Subset Xan_hi - Pbo into table variables
  xan_hi <- pw_data %>%
    filter(contrast == "Xan_Hi - Pbo") %>%
    # Rename to the table display variable
    select(`81` = value) %>%
    pad_row()
  # Add in row_label
  xan_hi["row_label"] <- c("p-value(Xan - Placebo) [1][3]", "  Diff of LS Means (SE)", "  95% CI", "")
  xan_hi["ord"] <- c(1, 2, 3, 4) # Order for sorting

  # Subset Xan_Hi - Xan_Lo into table variable
  xan_xan <- pw_data %>%
    filter(contrast == "Xan_Hi - Xan_Lo") %>%
    # Rename to the table display variable
    select(`81` = value)
  # Add in row_label
  xan_xan["row_label"] <- c("p-value(Xan High - Xan Low) [1][3]", "  Diff of LS Means (SE)", "  95% CI")
  xan_xan["ord"] <- c(5, 6, 7) # Order for sorting

  # Pack it all together
  pw_final <- merge(xan_lo, xan_hi, by = "row_label") %>%
    bind_rows(xan_xan) %>%
    arrange(ord)

  # Bind and clean up
  bind_rows(sect1, pw_final) %>%
    select(row_label,
      `var1_Xanomeline Low Dose` = `54`,
      `var1_Xanomeline High Dose` = `81`
    )
}


tooltip_text <- function(text, font_size = 16) {
  glue::glue("<span style='font-size:{font_size}px;'>{text}<span>")
}

set_data_path <- function(path) {
  # assertions on path
  path <- normalizePath(path, mustWork = FALSE)

  if (!dir.exists(path)) stop(paste("The path", path, "does not exist. Please use a valid directory path"), call. = FALSE)

  # check if data files are present
  data_files <- c("adsl.xpt", "adadas.xpt", "adtte.xpt", "adlbc.xpt")
  data_check <- sapply(data_files, function(x) file.exists(file.path(path, x)))

  if (!all(data_check)) {
    # determine which files are missing
    missing_files <- data_files[!data_check]
    stop(paste("The following data files are missing in the specified path", path, ":", paste(missing_files, collapse = ", ")), call. = FALSE)
  }

  # set golem config option
  golem::amend_golem_config("adam_path", path, talkative = FALSE)
  invisible(TRUE)
}

filter_active <- function(datasets) {
  result <- FALSE
  if (length(names(datasets$get_filter_state()) > 0)) {
    filter_use <- purrr::map_lgl(names(datasets$get_filter_state()), ~ {
      # grab call of filter code
      f_call <- datasets$get_call(.x)$filter
      f_call != glue::glue("{.x}_FILTERED <- {.x}")
    })
    result <- any(filter_use)
  }

  return(result)
}


add_risktable2 <- function(gg,
                           times = NULL,
                           statlist = "n.risk",
                           label = NULL,
                           group = "strata",
                           collapse = FALSE,
                           rowgutter = .16,
                           risk_font_size = 6.0,
                           risk_label_font_size = 12,
                           ...) {
  # User input validation ---------------------------------------------------

  if (!(is.numeric(rowgutter) == TRUE) || (rowgutter < 0) || (rowgutter > 1)) {
    stop("rowgutter should be a numeric value in range [0, 1]")
  }

  # Obtain the relevant table --------------------------------------------------
  tidy_object <- gg$data
  estimate_object <- visR:::.extract_estimate_object(gg)

  ggbld <- ggplot2::ggplot_build(gg)

  graphtimes <- as.numeric(ggbld$layout$panel_params[[1]]$x$get_labels())

  if (is.null(times)) times <- graphtimes

  final <-
    visR:::get_risktable(estimate_object,
      times = times,
      statlist = statlist,
      label = label,
      group = group,
      collapse = collapse
    )

  times <- as.numeric(unique(final$time))
  statlist <- attributes(final)$statlist
  title <- attributes(final)$title

  attr(final, "time_ticks") <- NULL
  attr(final, "statlist") <- NULL
  attr(final, "title") <- NULL

  # Plot requested tables below using list approach with map function -------

  tbls <-
    base::Map(
      function(statlist, title = NA) {
        ggrisk <- ggplot2::ggplot(
          final,
          ggplot2::aes(
            x = time,
            y = stats::reorder(y_values, dplyr::desc(y_values)),
            label = format(get(statlist), nsmall = 0) # = value columns
          )
        ) +
          ggplot2::geom_text(size = risk_font_size, hjust = 0.5, vjust = 0.5, angle = 0, show.legend = FALSE) +
          ggplot2::theme_bw() +
          ggplot2::scale_x_continuous(
            breaks = graphtimes,
            limits = c(min(graphtimes), max(graphtimes))
          ) +
          ggplot2::theme(
            axis.title.x = ggplot2::element_text(
              size = 8,
              vjust = 1,
              hjust = 1
            ),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.border = ggplot2::element_blank(),
            axis.line = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_text(size = risk_label_font_size, colour = "black", face = "plain"),
            plot.margin = ggplot2::unit(c(1, 0, 0, 0), "lines"),
            plot.title = ggplot2::element_text(hjust = 0, vjust = 0),
            legend.position = "none"
          ) +
          ggplot2::xlab(NULL) +
          ggplot2::ylab(NULL)

        if (!is.na(title) && !is.null(title)) {
          ggrisk <- ggrisk +
            ggplot2::ggtitle(title) +
            ggplot2::theme(plot.title = ggplot2::element_text(size = 10))
        }

        return(ggrisk)
      },
      statlist = as.list(statlist),
      title = as.list(title)
    )

  # Align plot and table by adjusting width ---------------------------------

  gglist <- list(gg) %>%
    base::append(tbls)

  ggA <- gglist %>%
    visR:::align_plots()

  # Create plot and add class -----------------------------------------------

  ## cowplot allows to align according to an axis (+left) and change the heigth
  ggB <- cowplot::plot_grid(
    plotlist = ggA,
    align = "none",
    nrow = length(ggA),
    rel_heights = c(1 - (rowgutter * (length(ggA) - 1)), rep(rowgutter, length(ggA) - 1))
  )

  class(ggB) <- c(class(ggB), intersect(class(gg), c("ggsurvfit", "ggtidycmprsk")))

  # Add individual components -----------------------------------------------

  components <- append(list(gg), tbls)
  names(components) <- c("visR_plot", title)
  ggB[["components"]] <- components

  return(ggB)
}

nest_rowlabels <- function(.dat) {
  stubs <- .dat %>%
    distinct(row_label1, ord_layer_index) %>%
    rename(row_label = row_label1) %>%
    mutate(
      ord_layer_1 = 0,
      ord_layer_2 = 0
    )

  .dat %>%
    select(-row_label1, row_label = row_label2) %>%
    bind_rows(stubs) %>%
    arrange(ord_layer_index, ord_layer_1, ord_layer_2) %>%
    mutate(
      across(starts_with("var"), ~ tidyr::replace_na(., ""))
    )
}

# pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
# options("golem.app.prod" = TRUE)
# pilot2wrappers::run_app() # add parameters here (if any)

app_sys <- function(...) {
  system.file(..., package = "pilot2wrappers")
}


adam_path <- "data/adam"
adsl <- haven::read_xpt(file.path(adam_path, "adsl.xpt"))
adsl <- adsl |>
  dplyr::mutate(
    TRT01P = factor(TRT01P, levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")),
    AGEGR1 = factor(AGEGR1, levels = c("<65", "65-80", ">80")),
    RACE = factor(RACE, levels = c("WHITE", "BLACK OR AFRICAN AMERICAN", "AMERICAN INDIAN OR ALASKA NATIVE"))
  )
adas <- haven::read_xpt(file.path(adam_path, "adadas.xpt")) |>
  dplyr::filter(
    EFFFL == "Y",
    ITTFL == "Y",
    PARAMCD == "ACTOT",
    ANL01FL == "Y"
  )
adtte <- haven::read_xpt(file.path(adam_path, "adtte.xpt")) |>
  dplyr::filter(PARAMCD == "TTDE")
adlb <- haven::read_xpt(file.path(adam_path, "adlbc.xpt")) |>
  dplyr::filter(PARAMCD == "GLUC" & !is.na(AVISITN))


srv_t_demographic <- function(input, output, session, datasets) {
  output$table <- shiny::renderUI({
    ADSL_FILTERED <- datasets$get_data("ADSL", filtered = TRUE)
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
    ) %>%
      split_cols_by("TRT01P") %>%
      add_colcounts() %>%
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
    tbl <- build_table(lyt, ADSL_FILTERED)
    as_html(tbl)
  })
}

ui_t_demographic <- function(id, datasets) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("table"))
}

srv_g_kmplot <- function(input, output, session, datasets) {
  output$plot <- shiny::renderPlot({
    adsl <- datasets$get_data("ADSL", filtered = TRUE)
    adtte <- datasets$get_data("ADTTE", filtered = TRUE)
    anl <<- adsl %>%
      dplyr::filter(
        SAFFL == "Y",
        STUDYID == "CDISCPILOT01"
      ) %>%
      dplyr::select(STUDYID, USUBJID, TRT01A) %>%
      dplyr::inner_join(
        filter(
          adtte, STUDYID == "CDISCPILOT01"
        ) %>% select(STUDYID, USUBJID, AVAL, CNSR, PARAM, PARAMCD),
        by = c("STUDYID", "USUBJID")
      ) %>%
      dplyr::mutate(
        TRT01A = factor(TRT01A, levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")),
        AVAL = AVAL / 30.4167
      )


    ## -----------------------------------------------------------------------------------------------------------------------------------
    # estimate survival
    surv_mod <- visR::estimate_KM(data = anl, strata = "TRT01A")

    #
    # # save plot
    ggplot2::theme_set(theme_bw())

    KM <- visR::visr(surv_mod,
      y_label = "Survival Probability (%)",
      x_label = "Time (Months)",
      fun = "pct",
      legend_position = "bottom"
    ) %>%
      visR::add_CNSR() %>%
      visR::add_CI()

    KM <- KM +
      ggplot2::theme(
        axis.text = element_text(size = rel(1.3)),
        axis.title = element_text(size = rel(1.4)),
        legend.text = element_text(size = rel(1.3)),
        legend.title = element_text(size = rel(1.4))
      ) +
      ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed")

    KM <- KM %>%
      add_risktable2(group = "statlist")
    # visR::add_risktable(group = "statlist")

    title <- cowplot::ggdraw() +
      cowplot::draw_label(
        "KM plot for Time to First Dermatologic Event: Safety population\n",
        fontfamily = "sans",
        fontface = "bold",
        size = 16
      )

    caption <- cowplot::ggdraw() +
      cowplot::draw_label(
        paste(
          "The shaded areas are 95% CI of the survival probability for each group",
          "\n",
          paste0(Sys.time())
        ),
        fontfamily = "sans",
        size = 12
      )

    KM <- cowplot::plot_grid(
      title, KM, caption,
      ncol = 1,
      rel_heights = c(0.1, 0.8, 0.1)
    )
    KM
  })
}

ui_g_kmplot <- function(id, datasets) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyWidgets::alert(
      tagList(tags$b("Important Information:"), tags$p("The analyses performed when utilizing subgroups or other subsets of the source data sets are considered ", tags$b("exploratory."))),
      status = "info",
      dismissible = TRUE
    ),
    shiny::plotOutput(ns("plot"), height = "800px")
  )
}

srv_t_primary <- function(input, output, session, datasets) {
  output$table <- shiny::renderUI({
    ADSL_FILTERED <- datasets$get_data("ADSL", filtered = TRUE)
    ADAS_FILTERED <- datasets$get_data("ADAS", filtered = TRUE)
    adas <- ADAS_FILTERED

    ## -----------------------------------------------------------------------------------------------------------------------------------
    t <- tplyr_table(adas, TRTP) %>%
      set_pop_data(ADSL_FILTERED) %>%
      set_pop_treat_var(TRT01P) %>%
      set_pop_where(EFFFL == "Y" & ITTFL == "Y") %>%
      set_distinct_by(USUBJID) %>%
      set_desc_layer_formats(
        "n" = f_str("xx", n),
        "Mean (SD)" = f_str("xx.x (xx.xx)", mean, sd),
        "Median (Min; Max)" = f_str("xx.x (xxx;xx)", median, min, max)
      ) %>%
      add_layer(
        group_desc(AVAL, where = AVISITN == 0, by = "Baseline")
      ) %>%
      add_layer(
        group_desc(AVAL, where = AVISITN == 24, by = "Week 24")
      ) %>%
      add_layer(
        group_desc(CHG, where = AVISITN == 24, by = "Change from Baseline")
      )

    sum_data <- t %>%
      build() %>%
      nest_rowlabels() %>%
      dplyr::select(-starts_with("ord")) %>%
      add_column_headers(
        paste0(
          "|Placebo</br>(N=**Placebo**)| Xanomeline High Dose</br>(N=**Xanomeline High Dose**) ",
          "| Xanomeline Low Dose</br>(N=**Xanomeline Low Dose**)"
        ),
        header_n(t)
      )


    ## -----------------------------------------------------------------------------------------------------------------------------------
    model_portion <- efficacy_models(adas, "CHG", 24, !filter_active(datasets))


    ## -----------------------------------------------------------------------------------------------------------------------------------
    final <- dplyr::bind_rows(sum_data, model_portion)

    ht <- huxtable::as_hux(final, add_colnames = FALSE) %>%
      huxtable::set_bold(1, 1:ncol(final), TRUE) %>%
      huxtable::set_align(1, 1:ncol(final), "center") %>%
      huxtable::set_valign(1, 1:ncol(final), "bottom") %>%
      huxtable::set_bottom_border(1, 1:ncol(final), 1) %>%
      huxtable::set_width(1) %>%
      huxtable::set_escape_contents(FALSE) %>%
      huxtable::set_col_width(c(.5, 1 / 6, 1 / 6, 1 / 6))
    htmltools::HTML(huxtable::to_html(ht))
  })
}

ui_t_primary <- function(id, datasets) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("table")),
    shiny::p("Statistical model and comparison p-values removed when applying data filters. Refer to the application information for additional details."),
    shiny::p("[1] Based on Analysis of covariance (ANCOVA) model with treatment and site group as factors and baseline value as a covariate."),
    shiny::p("[2] Test for a non-zero coefficient for treatment (dose) as a continuous variable."),
    shiny::p("[3] Pairwise comparison with treatment as a categorical variable: p-values without adjustment for multiple comparisons.")
  )
}

srv_t_efficacy <- function(input, output, session, datasets) {
  efficacy_results <- reactive({
    adsl <- datasets$get_data("ADSL", filtered = TRUE)

    itt <- adsl %>%
      filter(ITTFL == "Y") %>%
      select("STUDYID", "USUBJID")

    adlb <- datasets$get_data("ADLB", filtered = TRUE)

    # prepare labs data for pairwise comparison
    adlb1 <- adlb %>%
      right_join(itt, by = c("STUDYID", "USUBJID")) %>%
      filter(TRTPN %in% c(0, 81), PARAMCD == "GLUC", !is.na(AVISITN)) %>%
      mutate(TRTPN = ifelse(TRTPN == 0, 99, TRTPN))

    gluc_lmfit <- adlb1 %>%
      filter(AVISITN == 20) %>%
      stats::lm(CHG ~ BASE + TRTPN, data = .)

    t10 <- adlb1 %>%
      filter(AVISITN == 0) %>%
      group_by(TRTPN, TRTP) %>%
      summarise(
        N = n(),
        mean_bl = mean(BASE),
        sd_bl = sd(BASE)
      )

    ## Raw summary statistics
    t11 <- adlb1 %>%
      filter(AVISITN == 20, !is.na(CHG), !is.na(BASE)) %>%
      group_by(TRTPN, TRTP) %>%
      summarise(
        N_20 = n(),
        mean_chg = mean(CHG),
        sd_chg = sd(CHG),
        mean = mean(AVAL),
        sd = sd(AVAL)
      )

    ## Calculate LS mean
    t12 <- emmeans::emmeans(gluc_lmfit, "TRTPN")

    ## Merge and format data for reporting
    apr0ancova1 <- merge(t10, t11) %>%
      merge(t12) %>%
      mutate(emmean_sd = SE * sqrt(df)) %>%
      mutate(
        Trt = c("Xanomeline High Dose", "Placebo"),
        N1 = N,
        Mean1 = fmt_est(mean_bl, sd_bl),
        N2 = N_20,
        Mean2 = fmt_est(mean, sd),
        N3 = N_20,
        Mean3 = fmt_est(mean_chg, sd_chg),
        CI = fmt_ci(emmean, lower.CL, upper.CL)
      ) %>%
      select(Trt:CI)


    ## -----------------------------------------------------------------------------------------------------------------------------------
    t2 <- data.frame(pairs(t12))

    ## Treatment Comparison
    apr0ancova2 <- t2 %>%
      mutate(
        lower = estimate - 1.96 * SE,
        upper = estimate + 1.96 * SE
      ) %>%
      mutate(
        comp = "Study Drug vs. Placebo",
        mean = fmt_ci(estimate, lower, upper),
        p = ifelse(filter_active(datasets), "Not Applicable", fmt_pval(p.value))
      ) %>%
      select(comp:p)

    ## -----------------------------------------------------------------------------------------------------------------------------------
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
  output$tbl_efficacy_1 <- reactable::renderReactable({
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
    collist <- purrr::map2(1:ncol(apr0ancova1), colwidths, ~ {
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
  output$tbl_efficacy_2 <- reactable::renderReactable({
    efficacy_results <- efficacy_results()
    apr0ancova2 <- efficacy_results$apr0ancova2
    apr0ancova3 <- efficacy_results$apr0ancova3
    coln <- c(
      "",
      "Difference in LS Mean (95% CI)",
      "p-Value"
    )
    collist <- lapply(1:ncol(apr0ancova2), function(xx) {
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

ui_t_efficacy <- function(id, datasets) {
  ns <- NS(id)
  fluidPage(
    tags$br(),
    tags$br(),
    fluidRow(
      tippy::tippy(
        h4("Primary Endpoint Analysis: Glucose (mmol/L) - Summary at Week 20 LOCF"),
        tooltip = tooltip_text("Table is based on participants who have observable data at Baseline and Week 20", 16),
        allowHTML = TRUE
      ),
      tags$br(), tags$br(),
      column(
        width = 10,
        reactable::reactableOutput(ns("tbl_efficacy_1"))
      )
    ),
    tags$br(),
    tags$br(),
    tags$hr(),
    fluidRow(
      tippy::tippy(
        h4("Pairwise Comparison"),
        tooltip = tooltip_text("Inference in this table is based on a Analysis of Covariance (ANCOVA) model with treatment and baseline value as covariates.", 16),
        allowHTML = TRUE
      ),
      tags$br(),
      tags$br(),
      column(
        width = 10,
        reactable::reactableOutput(ns("tbl_efficacy_2"))
      )
    ),
    tags$br(),
    tags$br(),
    tags$hr(),
    fluidRow(
      h6(tags$i("Abbreviations: CI=Confidence Interval; LS=Least Squares; SD=Standard Deviation")),
      h6(tags$p("Table is based on participants who had observable data at Baseline and Week 20")),
      h6(tags$p("Based on an Analysis of Covariance (ANCOVA) model with treatment and baseline value as covariates"))
    )
  )
}

srv_t_disposition <- function(input, output, session, datasets) {
  output$table <- renderUI({
    ADSL_FILTERED <- datasets$get_data("ADSL", filtered = TRUE)
    ADLB_FILTERED <- datasets$get_data("ADLB", filtered = TRUE)
    adsl <- ADSL_FILTERED
    adlbc <- ADLB_FILTERED

    # use adlbc data set to remain consistent with efficacy table input data
    visit_df <- adlbc %>%
      filter(PARAMCD == "GLUC") %>%
      filter(AVISITN != 98) %>%
      filter(!is.na(AVISITN)) %>%
      select(USUBJID, AVISITN) %>%
      distinct() %>%
      left_join(
        select(adsl, USUBJID, TRT01P),
        by = "USUBJID"
      )

    # visit number and week lookup
    v_week_df <- tibble::tibble(
      AVISITN = c(0, 2, 4, 6, 8, 12, 16, 20, 24, 26, 99),
      VISIT = c("Baseline ", paste("Week", c(2, 4, 6, 8, 12, 16, 20, 24, 26)), "End of Treatment")
    ) %>%
      mutate(VISIT = factor(VISIT, levels = c("Baseline ", paste("Week", c(2, 4, 6, 8, 12, 16, 20, 24, 26)), "End of Treatment")))

    # build Tplyr table
    t_visit <- visit_df %>%
      left_join(v_week_df, by = "AVISITN") %>%
      tplyr_table(TRT01P) %>%
      set_pop_data(adsl) %>%
      set_pop_treat_var(TRT01P) %>%
      add_total_group() %>%
      add_layer(
        group_count(VISIT) %>%
          set_distinct_by(USUBJID) %>%
          set_format_strings(
            f_str("xx (xx%)", distinct_n, distinct_pct)
          )
      )

    b_t_visit <- t_visit %>%
      build() %>%
      dplyr::select(row_label1, var1_Placebo, `var1_Xanomeline High Dose`, `var1_Xanomeline Low Dose`, var1_Total) %>%
      add_column_headers(
        paste0(
          "|Placebo</br>(N=**Placebo**)",
          "| Xanomeline High Dose</br>(N=**Xanomeline High Dose**) ",
          "| Xanomeline Low Dose</br>(N=**Xanomeline Low Dose**) ",
          "| Total</br>(N=**Total**) "
        ),
        header_n(t_visit)
      )

    ht <- huxtable::as_hux(b_t_visit, add_colnames = FALSE) %>%
      huxtable::set_bold(1, 1:ncol(b_t_visit), TRUE) %>%
      huxtable::set_align(1, 1:ncol(b_t_visit), "center") %>%
      huxtable::set_valign(1, 1:ncol(b_t_visit), "bottom") %>%
      huxtable::set_bottom_border(1, 1:ncol(b_t_visit), 1) %>%
      huxtable::set_width(0.9) %>%
      huxtable::set_escape_contents(FALSE) %>%
      huxtable::set_col_width(c(.5, 1 / 8, 1 / 8, 1 / 8, 1 / 8))
    htmltools::HTML(huxtable::to_html(ht))
  })
}

ui_t_disposition <- function(id, datasets) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("table")),
    p("Table is based on participants within the ITT population")
  )
}


app <- teal::init(
  data = teal.data::cdisc_data(
    teal.data::cdisc_dataset("ADSL", adsl),
    teal.data::cdisc_dataset("ADAS", adas, keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT", "QSSEQ")),
    teal.data::cdisc_dataset("ADTTE", adtte),
    teal.data::cdisc_dataset("ADLB", adlb)
  ),
  modules = teal::modules(
    teal::module(
      label = "App Information",
      server = function(input, output, session, datasets) {},
      ui = function(id, ...) {
        shiny::includeMarkdown("docs/about.md")
      },
      filters = NULL
    ),
    teal::module(
      label = "Demographic Table",
      ui = ui_t_demographic,
      server = srv_t_demographic,
      filters = NULL
    ),
    teal::module(
      label = "KM plot for TTDE",
      ui = ui_g_kmplot,
      server = srv_g_kmplot,
      filters = c("ADSL", "ADTTE")
    ),
    teal::module(
      label = "Primary Table",
      ui = ui_t_primary,
      server = srv_t_primary,
      filters = NULL
    ),
    teal::module(
      label = "Efficacy Table",
      ui = ui_t_efficacy,
      server = srv_t_efficacy,
      filters = NULL
    ),
    teal::module(
      label = "Visit Completion Table",
      ui = ui_t_disposition,
      server = srv_t_disposition,
      filters = NULL
    )
  ),
  header = "Pilot 2 Shiny Application",
  footer = tags$p(class = "text-muted", "Source: R Consortium")
)

shinyApp(ui = app$ui, server = app$server)
