box::use(
  reactable[reactable, reactableOutput, renderReactable],
  shiny[
    NS, br, column, fluidRow, h1, h2, imageOutput,
    p, renderImage, tabPanel, tabsetPanel, tagList, tags
  ],
  tibble[tibble],
)

ui <- function(id, datasets) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        tags$h1("Application Guide"),
        tags$p(
          "The Pilot 2 Shiny Application contains five distinct interfaces,
          each displaying a different analysis output as described in the App Information page."
        ),
        reactableOutput(ns("pilot1_table"))
      )
    ),
    tags$br(),
    fluidRow(
      column(
        width = 12,
        tags$h2("Dynamic Filters"),
        tags$p(
          "The", tags$b("KM Plot for TTDE"),
          "module allows for filters to be applied based on variables in the",
          tags$b("ADSL"), "and", tags$b("ADTTE"), "data sets.
          Below is an example of performing subpopulation analysis
          for an age group within the module:"
        ),
        tags$br(),
        tabsetPanel(
          id = "userSteps",
          type = "tabs",
          tabPanel(
            title = "Step 1",
            fluidRow(
              column(
                width = 4,
                tags$img(src = "/static/app_screenshot1.png", height = "auto")
              ),
              column(
                class = "top-margin",
                width = 8,
                tags$p(
                  "Within the", tags$b("Add Filter Variables"),
                  "widget, click the box with the placeholder",
                  tags$b("Select variables to filter")
                )
              )
            )
          ),
          tabPanel(
            title = "Step 2",
            column(
              width = 4,
                tags$img(src = "/static/app_screenshot2.png", height = "auto")
            ),
            column(
              class = "top-margin",
              width = 8,
              tags$p(
                "Scroll up/down or use the search bar to find the variable for subpopulation.
                Click the desired variable, ",
                tags$b("AGEGR1"), "in this example"
              )
            )
          ),
          tabPanel(
            title = "Step 3",
            column(
              width = 4,
                tags$img(src = "/static/app_screenshot3.png", height = "auto")
            ),
            column(
              width = 8,
              class = "top-margin",
              tags$p(
                "In the", tags$b("Active Filter Variables"),
                "widget, the selected variable with its available categories or levels
                will display,",
                tags$b("AGEGR1"), "in this example, is displayed with three categories.
                If the selected variable in the previous step is a continuous variable,
                then a slider will appear for selecting a range of values."
              ),
              tags$br(),
              tags$p(
                "Select the target subpopulation (e.g. >80) and
                the analysis output displayed on the left hand side will be updated
                in real-time according to the selection,
                which in this example is equivalent to performing a filter on the",
                tags$b("ADSL"), "data by AGEGR1 == '>80'"
              )
            )
          )
        )
      )
    )
  )
}

server <- function(input, output, session, datasets) {
  output$pilot1_table <- renderReactable({
    pilot1_table <- tibble(
      tab = c(
        "Demographic Table", "KM Plot for TTDE",
        "Primary Table", "Efficacy Table",
        "Visit Completion Table"
      ),
      output = c(
        "Table 14-2.01 Summary of Demographic and Baseline Characteristics",
        "Figure 14-1 Time to Dermatologic Event by Treatment Group",
        "Table 14-3.01 Primary Endpoint Analysis: ADAS Cog(11) - Change from Baseline to Week 24 - LOCF", # nolint
        "Table 14-3.02 Primary Endpoint Analysis: Glucose (mmol/L) - Summary at Week 20 - LOCF",
        "Not Applicable"
      )
    )

    reactable(pilot1_table)
  })
}
