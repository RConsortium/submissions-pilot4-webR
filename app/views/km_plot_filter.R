box::use(
  shiny[
    NS, bindEvent, moduleServer, observe, reactiveVal, reactiveValues, selectInput, tags, uiOutput,
    updateSelectInput
  ],
  purrr[walk],
  tibble[type_sum],
  stats[setNames]
)

ui <- function(id, dataset_name) {
  ns <- NS(id)
  tags$div(
    tags$h3("Add filter variables"),
    selectInput(ns("variables"), dataset_name, NULL, multiple = TRUE)
  )
}

server <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    filters <- reactiveValues()

    filtered_data <- reactiveVal(dataset)
    updateSelectInput(
      session,
      "variables",
      choices = setNames(
        colnames(dataset),
        sapply(colnames(dataset), function(col) {
          col_type <- type_sum(dataset[[col]])
          sprintf("%s (%s)", col, col_type)
        })
      )
    )

    observe({
      setdiff(names(filters), input$variables) |>
        walk(function(col) {
          filters[[col]] <- NULL
        })

      walk(input$variables, function(col) {
        if (!is.null(filters[[col]])) { # active filter
          return()
        }
        filters[[col]] <- switch(type_sum(dataset[[col]]),
          chr = sort(unique(dataset[[col]])),
          fct = sort(unique(dataset[[col]])),
          dbl = range(dataset[[col]]),
          date = range(dataset[[col]])
        )
      })
    }) |>
      bindEvent(input$variables)

    return(filtered_data)
  })
}
