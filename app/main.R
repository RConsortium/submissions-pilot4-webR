box::use(
  shiny[
    shinyApp, tagList, tags, includeMarkdown, moduleServer, NS, bootstrapPage,
    textOutput, renderText, actionButton, observe, bindEvent, icon,
  ],
  teal[ui_teal_with_splash, modules, module, srv_teal_with_splash],
)

box::use(
  app / view / user_guide,
  app / view / demographic_table,
  app / view / km_plot,
  app / view / primary_table,
  app / view / efficacy_table,
  app / view / completion_table,
)

teal_data <- readRDS("teal_data.rds")


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
    label = "Usage Guide",
    ui = user_guide$ui,
    server = user_guide$server,
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
  tags$div(
    class = "dark color-mode",
    ui_teal_with_splash(
      title = "Pilot 2 Shiny Rhino Application",
      id = ns("teal_wrapper"),
      data = teal_data,
      header = tags$div(
        class = "flex",
        tags$h1(
          "Pilot 2 Shiny Application",
          tags$span(
            class = "text-muted text-smaller text-italic",
            "(using Rhino framework)"
          )
        ),
        tags$div(
          class = "logos-wrapper",
          tags$div(
            class = "center-wrap",
            actionButton(
              ns("theme_mode_toggle"),
              class = "color-mode-toggle",
              label = tagList(
                tags$span(
                  class = "color-mode dark",
                  title = "Switch to light mode",
                  "☀️"
                ),
                tags$span(
                  class = "color-mode light",
                  title = "Switch to light mode",
                  "🌑"
                )
              )
            ),
            tags$a(
              href = "https://rconsortium.github.io/submissions-wg/",
              target = "_blank",
              tags$img(class = "logo", src = "static/logos/rconsortium.svg")
            )
          )
        )
      ),
      footer = tagList(
        tags$p(
          class = "text-muted",
          "Source: R Consortium. Adapted to a Rhino application by Appsilon."
        ),
        tags$div(
          class = "logos-wrapper",
          tags$a(
            href = "https://rconsortium.github.io/submissions-wg/",
            target = "_blank",
            tags$img(class = "logo", src = "static/logos/rconsortium.svg")
          ),
          tags$a(
            href = "https://appsilon.com",
            target = "_blank",
            tags$img(class = "logo", src = "static/logos/appsilon.svg")
          )
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    srv_teal_with_splash(id = "teal_wrapper", data = teal_data, modules = teal_modules)

    observe({
      session$sendCustomMessage("toggle_dark", input$theme_mode_toggle)
    }) |>
      bindEvent(input$theme_mode_toggle, once = FALSE, ignoreInit = TRUE)
  })
}
