library(config)
library(cowplot)
library(dplyr)
library(emmeans)
library(ggplot2)
library(glue)
library(haven)
library(htmltools)
library(huxtable)
library(magrittr)
library(markdown)
library(pkgload)
library(purrr)
library(reactable)
# library(rhino)
library(rtables)
library(shiny)
library(stringr)
# library(teal)
# library(teal.data)
library(tibble)
library(tidyr)
library(tippy)
library(Tplyr)
library(utils)
library(visR)
library(shinyWidgets)

box::use(
  app / view / user_guide,
  app / view / demographic_table,
  app / view / km_plot,
  app / view / primary_table,
  app / view / efficacy_table,
  app / view / completion_table,
)

teal_data <- readRDS("teal_data.rds")
teal_modules <- readRDS("teal_modules.rds")

modules <- list
module <- list

ui_teal_with_splash <- function(title, id, header, footer, ...) {
  tabs <- lapply(
    teal_modules$children[1],
    function(m) {
      shiny::tabPanel(
        m$label,
        m$ui(paste0(tolower(gsub(" ", "-", m$label))))
      )
    }
  )
  names(tabs) <- NULL

  shiny::fluidPage(
    title = title,
    id = id,
    header,
    do.call(
      shiny::tabsetPanel,
      tabs
    ),
    footer
  )
}

ui <- function() {
  ns <- NS("app")

  tags$div(
    class = "dark color-mode",
    ui_teal_with_splash(
      title = "Pilot 2 Shiny Rhino Application",
      id = "",
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

ui <- fluidPage("example")

server <- function(input, output, session) {
  datasets <- list(
    get_data = function(name, ...) teal_data$get_dataset(name)$get_raw_data()
  )
  lapply(
    teal_modules$children[1],
    function(m) {
      m$server(tolower(gsub(" ", "-", m$label)), datasets)
    }
  )
}

shinyApp(ui, server)
