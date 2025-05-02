library(shiny)
library(markdown)

# TealShim
library(R6)
library(formatters)

# user_guide
library(reactable)
library(tibble)

# demographic_table
library(rtables)
library(stats)

# km_plot
library(dplyr)
library(ggplot2)
library(visR)

# primary_table
library(Tplyr)
library(huxtable)
library(tidyr)
library(glue)
library(purrr)
library(stringr)

# efficacy_table
library(emmeans)
library(graphics)
library(tippy)

# completion_table
##

# adam_data
library(haven)

box::use(
  logic / adam_data[get_adsl, get_adas, get_adtte, get_adlb],
  views / user_guide,
  views / demographic_table,
  views / km_plot,
  views / primary_table,
  views / efficacy_table,
  views / completion_table
)

adsl <- get_adsl()
adas <- get_adas()
adtte <- get_adtte()
adlb <- get_adlb()

TealShim <- R6::R6Class(
  "TealShim",
  public = list(
    initialize = function(adsl, adas, adtte, adlb) {
      private$data$adsl <- adsl
      private$data$adas <- adas
      private$data$adtte <- adtte
      private$data$adlb <- adlb
    },
    get_data = function(name, ...) {
      switch(name,
        ADSL = private$data$adsl,
        ADAS = private$data$adas,
        ADTTE = private$data$adtte,
        ADLB = private$data$adlb
      )
    },
    get_varlabels = function(name, cols) {
      formatters::var_labels(self$get_data(name))[cols]
    },
    get_filter_state = function() {
      c()
    }
  ),
  private = list(
    data = NULL
  )
)
datasets <- datasets_km <- TealShim$new(adsl, adas, adtte, adlb)

if (file.exists("www/static/about.md")) {
  md_path <- "www/static/about.md"
} else {
  md_path <- tempfile(fileext = ".md")
  download.file("/static/about.md", destfile = md_path, mode = "wb")
}
app_information <- includeMarkdown(md_path)

get_page_dependencies <- function() {
  tagList(
    tags$link(rel = "icon", href = "/static/favicon.ico"),
    tags$link(rel = "stylesheet", type = "text/css", href = "/static/css/styles.css"),
    tags$script(src = "/static/js/scripts.js"),
    fontawesome::fa_html_dependency(),
  )
}

get_page_header <- function() {
  tags$header(
    tags$div(
      class = "app-header flex",

      tags$h1(
        "Pilot 4 Shiny Application",
        tags$span(
          class = "text-muted text-smaller text-italic",
          "(using webR framework)"
        )
      ),
      tags$div(
        class = "logos-wrapper",
        tags$div(
          class = "center-wrap",
          actionButton(
            "theme_mode_toggle",
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
            tags$img(class = "logo", src = "/static/logos/rconsortium.svg")
          )
        )
      )
    )
  )
}

get_page_footer <- function() {
  tags$div(
    class = "app-footer",

    tags$p(
      class = "text-muted",
      "Source: R Consortium. Adapted to a webR application by Appsilon."
    ),
    tags$div(
      class = "logos-wrapper",
      tags$a(
        href = "https://rconsortium.github.io/submissions-wg/",
        target = "_blank",
        tags$img(class = "logo", src = "/static/logos/rconsortium.svg")
      ),
      tags$a(
        href = "https://appsilon.com",
        target = "_blank",
        tags$img(class = "logo", src = "/static/logos/appsilon.svg")
      )
    )
  )
}

ui <- fluidPage(
  get_page_dependencies(),
  title = "Pilot 2 Shiny webR Application",
  class = "app-wrapper dark color-mode",

  get_page_header(),

  tabsetPanel(
    id = "moduleTabs",
    type = "tabs",

    tabPanel("App Information", app_information),
    tabPanel("User Guide", user_guide$ui("user_guide", datasets)),
    tabPanel("Demographic Table",
      demographic_table$ui("demographic_table", datasets)
    ),
    tabPanel("KM Plot for TTDE", km_plot$ui("km_plot", datasets)) |>
      tagAppendAttributes(class = "no-background-tab"),
    tabPanel("Primary Table", primary_table$ui("primary_table", datasets)),
    tabPanel("Efficacy Table", efficacy_table$ui("efficacy_table", datasets)),
    tabPanel("Visit Completion Table",
      completion_table$ui("visit_completion_table", datasets)
    )
  ),

  get_page_footer()
)

server <- function(input, output, session) {
  moduleServer("user_guide",
    function(input, output, session) {
      user_guide$server(input, output, session, datasets)
    }
  )
  moduleServer("demographic_table",
    function(input, output, session) {
      demographic_table$server(input, output, session, datasets)
    }
  )
  moduleServer("km_plot",
    function(input, output, session) {
      km_plot$server(input, output, session, datasets)
    }
  )
  moduleServer("primary_table",
    function(input, output, session) {
      primary_table$server(input, output, session, datasets)
    }
  )
  moduleServer("efficacy_table",
    function(input, output, session) {
      efficacy_table$server(input, output, session, datasets)
    }
  )
  moduleServer("visit_completion_table",
    function(input, output, session) {
      completion_table$server(input, output, session, datasets)
    }
  )

  observe({
    session$sendCustomMessage("toggle_dark", input$theme_mode_toggle)
  }) |>
  bindEvent(input$theme_mode_toggle, once = FALSE, ignoreInit = TRUE)
}

shinyApp(ui, server)
