box::use(
  dplyr[between, filter],
  purrr[discard, imap, reduce, walk],
  shiny[
    NS, bindEvent, dateRangeInput, isolate, moduleServer, observe, reactiveVal, reactiveValues,
    reactiveValuesToList, renderUI, selectInput, sliderInput, tags, uiOutput, updateSelectInput
  ],
  stats[setNames],
  tibble[type_sum],
)

ui <- function(id, dataset_name) {
  ns <- NS(id)
  tags$div(
    tags$h3("Add filter variables"),
    selectInput(ns("variables"), dataset_name, NULL, multiple = TRUE),
    uiOutput(ns("filters"))
  )
}

server <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    filters <- reactiveValues()
    filters_values <- reactiveValues()
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
        discard(function(col) is.null(filters[[col]])) |>
        walk(function(col) {
          filters[[col]]$obs$destroy()
          filters[[col]] <- NULL
          filters_values[[col]] <- NULL
        })

      walk(input$variables, function(col) {
        if (!is.null(filters[[col]])) { # active filter
          return()
        }
        obs <- observe({
          filters_values[[col]] <- input[[paste0("filter_", col)]]
        }) |>
          bindEvent(input[[paste0("filter_", col)]], ignoreInit = TRUE)
        values <- switch(type_sum(dataset[[col]]),
          chr = sort(unique(dataset[[col]])),
          fct = sort(unique(dataset[[col]])),
          dbl = range(dataset[[col]]),
          date = range(dataset[[col]])
        )
        filters_values[[col]] <- values
        filters[[col]] <- list(obs = obs, choices = values)
      })
    }) |>
      bindEvent(input$variables, ignoreNULL = FALSE)

    output$filters <- renderUI({
      reactiveValuesToList(filters) |>
        imap(function(meta, col) {
          if (is.null(meta)) {
            return(NULL)
          }
          values <- isolate(filters_values[[col]])
          choices <- meta$choices
          switch(type_sum(dataset[[col]]),
            chr = selectInput(ns(paste0("filter_", col)), col, choices, values, multiple = TRUE),
            fct = selectInput(ns(paste0("filter_", col)), col, choices, values, multiple = TRUE),
            dbl = sliderInput(ns(paste0("filter_", col)), col, choices[1], choices[2], values),
            date = dateRangeInput(ns(paste0("filter_", col)), col, choices[1], choices[2], values[1], values[2])
          )
        }) |>
        discard(is.null)
    })

    observe({
      reactiveValuesToList(filters_values) |>
        names() |>
        reduce(.init = dataset, function(df, col) {
          values <- filters_values[[col]]
          if (is.null(values) || NROW(df) == 0) {
            return(df)
          }

          switch(type_sum(df[[col]]),
            chr = filter(df, .data[[col]] %in% values),
            fct = filter(df, .data[[col]] %in% values),
            dbl = filter(df, between(.data[[col]], values[1], values[2])),
            date = filter(df, between(.data[[col]], values[1], values[2]))
          )
        }) |>
        filtered_data()
    }) |>
      bindEvent(reactiveValuesToList(filters_values))

    return(filtered_data)
  })
}
