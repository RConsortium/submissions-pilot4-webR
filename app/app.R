library(shiny)
library(markdown)

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
# adsl <- get_adsl()
# adas <- get_adas()
# adtte <- get_adtte()
# adlb <- get_adlb()

# teal_data <- cdisc_data(
#   cdisc_dataset("ADSL", adsl),
#   cdisc_dataset("ADAS", adas, keys = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT", "QSSEQ")),
#   cdisc_dataset("ADTTE", adtte),
#   cdisc_dataset("ADLB", adlb)
# )

# Use me for compiling to webR
temp_file_path <- tempfile(fileext = ".rds")
download.file("/adam/datasets.rds", destfile = temp_file_path, mode = "wb")
datasets <- readRDS(temp_file_path)

temp_file_path <- tempfile(fileext = ".rds")
download.file("/adam/datasets_km.rds", destfile = temp_file_path, mode = "wb")
datasets_km <- readRDS(temp_file_path)

temp_file_path <- tempfile(fileext = ".md")
download.file("/static/about.md", destfile = temp_file_path, mode = "wb")
app_information <- includeMarkdown(temp_file_path)

# Use me to run the app as a nomral shiny app
# datasets <- readRDS("www/adam/datasets.rds")
# datasets_km <- readRDS("www/adam/datasets_km.rds")

ui <- bootstrapPage(
  tabsetPanel(
    id = "moduleTabs",
    type = "tabs",

    tabPanel("App Information", app_information),
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
