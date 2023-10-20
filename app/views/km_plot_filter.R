box::use(
  dplyr[between, filter],
  formatters[var_labels],
  ggplot2[aes, geom_density, ggplot, scale_x_continuous, scale_y_continuous, theme_void],
  grDevices[rgb],
  purrr[discard, imap, iwalk, reduce, walk],
  shiny[
    HTML, NS, bindEvent, dateRangeInput, debounce, isolate, moduleServer, observe, plotOutput,
    reactive, reactiveVal, reactiveValues, reactiveValuesToList, renderPlot, renderUI, selectInput,
    sliderInput, tagAppendAttributes, tagList, tags, uiOutput, updateSelectInput
  ],
  stats[setNames],
  tibble[type_sum]
)

ui <- function(id, dataset_name) {
  ns <- NS(id)

  uiOutput(ns("main")) |>
    tagAppendAttributes(class = "km-plot-filters")
}

server <- function(id, dataset_name, dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    filters <- reactiveValues()
    filters_values <- reactiveValues()
    filtered_data <- reactiveVal(dataset)

    output$main <- renderUI({
      choices <- setNames(
        sapply(colnames(dataset), \(col) paste(col, type_sum(dataset[[col]]), sep = "-")),
        sapply(colnames(dataset), \(col) {
          label <- var_labels(dataset[, col])
          if (is.na(label)) {
            return(col)
          }
          sprintf("%s (%s)", col, label)
        })
      )

      tags$div(
        tags$h3("Add Filter Variables"),
        selectInput(ns("variables"), dataset_name, c("Select variables to filter" = "", choices), multiple = TRUE) |>
          tagAppendAttributes(class = "variable-input"),
        uiOutput(ns("filters"))
      )
    })

    variables <- reactive({
      gsub("-(chr|fct|dbl|date)$", "", input$variables)
    })

    observe({
      # clean filters that just got disabled
      setdiff(names(filters), variables()) |>
        discard(function(col) is.null(filters[[col]])) |>
        walk(function(col) {
          filters[[col]]$obs$destroy()
          filters[[col]] <- NULL
          filters_values[[col]] <- NULL
        })

      # set up filters that just got enabled
      walk(variables(), function(col) {
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
      bindEvent(variables(), ignoreNULL = FALSE)

    output$filters <- renderUI({
      reactiveValuesToList(filters) |>
        imap(function(meta, col) {
          if (is.null(meta)) { # filter that was active but is now disabled
            return(NULL)
          }
          values <- isolate(filters_values[[col]])
          choices <- meta$choices
          input <- switch(type_sum(dataset[[col]]),
            chr = selectInput(ns(paste0("filter_", col)), NULL, choices, values, multiple = TRUE),
            fct = selectInput(ns(paste0("filter_", col)), NULL, choices, values, multiple = TRUE),
            dbl = tags$div(
              class = "overlay-slider",
              plotOutput(ns(paste0("filter_", col, "_plot")), height = "auto"),
              sliderInput(ns(paste0("filter_", col)), NULL, choices[1], choices[2], values)
            ),
            date = dateRangeInput(ns(paste0("filter_", col)), NULL, choices[1], choices[2], values[1], values[2])
          )
          label <- var_labels(dataset[, col])
          tagList(
            tags$label(
              col,
              if (!is.na(label)) tags$span(paste0("(", label, ")"))
            ),
            input
          )
        }) |>
        discard(is.null)
    })

    observe({
      reactiveValuesToList(filters) |>
        iwalk(function(meta, col) {
          if (is.null(meta) || type_sum(dataset[[col]]) != "dbl") {
            return(NULL)
          }
          output[[paste0("filter_", col, "_plot")]] <- renderPlot(
            range_slider_overlay(dataset[[col]]),
            height = 25,
            bg = "transparent"
          )
        })
    }) |>
      bindEvent(reactiveValuesToList(filters))

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

range_slider_overlay <- function(values) {
  ggplot(data.frame(x = values), aes(x = x)) +
    geom_density(
      fill = rgb(66 / 255, 139 / 255, 202 / 255),
      color = NA,
      alpha = 0.2
    ) +
    theme_void() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
}
