box::use(
  shiny[
    shinyApp, tags, includeMarkdown, moduleServer, NS, bootstrapPage,
    textOutput, renderText
  ],
  teal[ui_teal_with_splash, modules, module, srv_teal_with_splash],
  teal.data[cdisc_data, cdisc_dataset],
)

box::use(
  app / logic / adam_data[get_adsl, get_adas, get_adtte, get_adlb],
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

teal_data <- cdisc_data(
  cdisc_dataset("ADSL", adsl),
  cdisc_dataset("ADAS", adas, keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT", "QSSEQ")),
  cdisc_dataset("ADTTE", adtte),
  cdisc_dataset("ADLB", adlb)
)

teal_modules <- modules(
  module(
    label = "App Information",
    server = function(id, datasets) {
      moduleServer(id, function(input, output, session) {

      })
    },
    ui = function(id, ...) {
      includeMarkdown("app/docs/about.md")
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
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  ui_teal_with_splash(
    title = "Some title",
    id = ns("teal_wrapper"),
    data = teal_data,
    header = "Pilot 2 Shiny Application using Rhino framework",
    footer = tags$p(class = "text-muted", "Source: R Consortium")
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    srv_teal_with_splash(id = "teal_wrapper", data = teal_data, modules = teal_modules)
  })
}
