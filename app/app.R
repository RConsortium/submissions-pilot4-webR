box::use(
  shiny[shinyApp, tags, includeMarkdown],
  teal[init, modules, module],
  teal.data[cdisc_data, cdisc_dataset],
)

box::use(
  app / logic / adam_data[get_adsl, get_adas, get_adtte, get_adlb],
  app / logic / formatters[fmt_num, fmt_est, fmt_ci, fmt_pval],
  app / logic / helpers[pad_row, num_fmt, tooltip_text, set_data_path, filter_active],
  app / logic / eff_models[efficacy_models],
  app / logic / kmplot_helpers[add_risktable2],
  app / logic / Tplyr_helpers[nest_rowlabels],
  app / view / demographic_table,
  app / view / km_plot,
  app / view / primary_table,
  app / view / efficacy_table,
  app / view / completion_table,
)

adsl <- get_adsl()
adas <- get_adas()
adtte <- get_adtte()
adlb <- get_adlb()

app <- init(
  data = cdisc_data(
    cdisc_dataset("ADSL", adsl),
    cdisc_dataset("ADAS", adas, keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT", "QSSEQ")),
    cdisc_dataset("ADTTE", adtte),
    cdisc_dataset("ADLB", adlb)
  ),
  modules = modules(
    module(
      label = "App Information",
      server = function(input, output, session, datasets) {},
      ui = function(id, ...) {
        includeMarkdown("docs/about.md")
      },
      filters = NULL
    ),
    module(
      label = "Demographic Table",
      ui = demographic_table$ui,
      server = demographic_table$server,
      filters = NULL
    ),
    module(
      label = "KM plot for TTDE",
      ui = km_plot$ui,
      server = km_plot$server,
      filters = c("ADSL", "ADTTE")
    ),
    module(
      label = "Primary Table",
      ui = primary_table$ui,
      server = primary_table$server,
      filters = NULL
    ),
    module(
      label = "Efficacy Table",
      ui = efficacy_table$ui,
      server = efficacy_table$server,
      filters = NULL
    ),
    module(
      label = "Visit Completion Table",
      ui = completion_table$ui,
      server = completion_table$server,
      filters = NULL
    )
  ),
  header = "Pilot 2 Shiny Application",
  footer = tags$p(class = "text-muted", "Source: R Consortium")
)

shinyApp(ui = app$ui, server = app$server)
