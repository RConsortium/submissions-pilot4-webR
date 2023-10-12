library(shiny)

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

box::use(
  views / user_guide,
  views / demographic_table,
  views / km_plot,
  views / primary_table,
  views / efficacy_table,
  views / completion_table
)


ui <- bootstrapPage(
  tabsetPanel(
    id = "moduleTabs",
    type = "tabs",

    tabPanel("User Guide", user_guide$ui("user_guide", NULL)),
    tabPanel("Demographic Table", demographic_table$ui("demographic_table", NULL)),
    tabPanel("KM Plot", km_plot$ui("km_plot", NULL)),
    tabPanel("Primary Table", primary_table$ui("primary_table", NULL)),
    tabPanel("Efficacy Table", efficacy_table$ui("efficacy_table", NULL)),
    tabPanel("Visit Completion Table", completion_table$ui("visit_completion_table", NULL))
  )
)

server <- function(input, output, session) {
  moduleServer("user_guide",
    function(input, output, session) {
      user_guide$server(input, output, session, NULL)
    }
  )
  moduleServer("demographic_table",
    function(input, output, session) {
      demographic_table$server(input, output, session, NULL)
    }
  )
  moduleServer("km_plot",
    function(input, output, session) {
      km_plot$server(input, output, session, NULL)
    }
  )
  moduleServer("primary_table",
    function(input, output, session) {
      primary_table$server(input, output, session, NULL)
    }
  )
  moduleServer("efficacy_table",
    function(input, output, session) {
      efficacy_table$server(input, output, session, NULL)
    }
  )
  moduleServer("visit_completion_table",
    function(input, output, session) {
      completion_table$server(input, output, session, NULL)
    }
  )
}

shinyApp(ui, server)
