# To run the app normally set environment variable LOCAL=TRUE
library(shiny)

# TealShim
library(R6)
library(formatters)

# user_guide
library(reactable)
library(tibble)

#demographic_table
library(rtables)
library(stats)

# km_plot
library(cowplot)
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

# Currently unused
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

ui <- bootstrapPage(
  tabsetPanel(
    id = "moduleTabs",
    type = "tabs",

    tabPanel("User Guide", user_guide$ui("user_guide", datasets)),
    tabPanel("Demographic Table", demographic_table$ui("demographic_table", datasets)),
    tabPanel("KM Plot", km_plot$ui("km_plot", datasets_km)),
    tabPanel("Primary Table", primary_table$ui("primary_table", datasets)),
    tabPanel("Efficacy Table", efficacy_table$ui("efficacy_table", datasets)),
    tabPanel("Visit Completion Table", completion_table$ui("visit_completion_table", datasets))
  )
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
      km_plot$server(input, output, session, datasets_km)
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
}

shinyApp(ui, server)
