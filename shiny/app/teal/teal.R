box::use(
  shiny[...]
)

ui_teal <- function(id,
                    splash_ui = tags$h2("Starting the Teal App"),
                    title = NULL,
                    header = tags$p(""),
                    footer = tags$p("")) {
  ns <- NS(id)
  splash_ui <- div(
    id = ns("main_ui_container"),
    # just the first item of the tagList)
    div(splash_ui)
  )

  # show busy icon when shiny session is busy computing stuff
  # based on https://stackoverflow.com/questions/17325521/r-shiny-display-loading-message-while-function-is-running/22475216#22475216 #nolint
  shiny_busy_message_panel <- conditionalPanel(
    condition = "(($('html').hasClass('shiny-busy')) && (document.getElementById('shiny-notification-panel') == null))", # nolint
    div(
      icon("arrows-rotate", "spin fa-spin"),
      "Computing ...",
      # CSS defined in `custom.css`
      class = "shinybusymessage"
    )
  )

  res <- fluidPage(
    title = title,
    theme = get_teal_bs_theme(),
    include_teal_css_js(),
    tags$header(header),
    tags$hr(class = "my-2"),
    shiny_busy_message_panel,
    splash_ui,
    tags$hr(),
    tags$footer(
      div(
        footer,
        textOutput(ns("identifier"))
      )
    )
  )
  return(res)
}

#' @export
ui_teal_with_splash <- function(id,
                                data,
                                title,
                                header = tags$p("Add Title Here"),
                                footer = tags$p("Add Footer Here")) {
  is_pulled_data <- teal.data::is_pulled(data)
  ns <- NS(id)

  splash_ui <- if (is_pulled_data) {
    # blank ui if data is already pulled
    div()
  } else {
    message("App was initialized with delayed data loading.")
    data$get_ui(ns("startapp_module"))
  }

  ui_teal(
    id = ns("teal"),
    splash_ui = splash_ui,
    title = title,
    header = header,
    footer = footer
  )
}

#' @export
modules <- function(..., label = "root") {
  submodules <- list(...)
  if (any(vapply(submodules, is.character, FUN.VALUE = logical(1)))) {
    stop(
      "The only character argument to modules() must be 'label' and it must be named, ",
      "change modules('lab', ...) to modules(label = 'lab', ...)"
    )
  }
  # name them so we can more easily access the children
  # beware however that the label of the submodules should not be changed as it must be kept synced
  labels <- vapply(submodules, function(submodule) submodule$label, character(1))
  names(submodules) <- make.unique(gsub("[^[:alnum:]]+", "_", labels), sep = "_")
  structure(
    list(
      label = label,
      children = submodules
    ),
    class = "teal_modules"
  )
}

#' @export
module <- function(label = "module",
                   server = function(id, ...) {
                     moduleServer(id, function(input, output, session) {}) # nolint
                   },
                   ui = function(id, ...) {
                     tags$p(paste0("This module has no UI (id: ", id, " )"))
                   },
                   filters,
                   datanames = "all",
                   server_args = NULL,
                   ui_args = NULL) {
  if (!missing(filters)) {
    checkmate::assert_character(filters, min.len = 1, null.ok = TRUE, any.missing = FALSE)
    datanames <- filters
    msg <-
      "The `filters` argument is deprecated and will be removed in the next release. Please use `datanames` instead."
    logger::log_warn(msg)
    warning(msg)
  }

  if (label == "global_filters") {
    stop(
      sprintf("module(label = \"%s\", ...\n  ", label),
      "Label 'global_filters' is reserved in teal. Please change to something else.",
      call. = FALSE
    )
  }
  if (label == "Report previewer") {
    stop(
      sprintf("module(label = \"%s\", ...\n  ", label),
      "Label 'Report previewer' is reserved in teal.",
      call. = FALSE
    )
  }
  server_formals <- names(formals(server))
  if (!(
    "id" %in% server_formals ||
      all(c("input", "output", "session") %in% server_formals)
  )) {
    stop(
      "\nmodule() `server` argument requires a function with following arguments:",
      "\n - id - teal will set proper shiny namespace for this module.",
      "\n - input, output, session (not recommended) - then shiny::callModule will be used to call a module.",
      "\n\nFollowing arguments can be used optionaly:",
      "\n - `data` - module will receive list of reactive (filtered) data specified in the `filters` argument",
      "\n - `datasets` - module will receive `FilteredData`. See `help(teal.slice::FilteredData)`",
      "\n - `reporter` - module will receive `Reporter`. See `help(teal.reporter::Reporter)`",
      "\n - `filter_panel_api` - module will receive `FilterPanelAPI`. (See [teal.slice::FilterPanelAPI]).",
      "\n - `...` server_args elements will be passed to the module named argument or to the `...`"
    )
  }

  if (!is.element("data", server_formals) && !is.null(datanames)) {
    message(sprintf("module \"%s\" server function takes no data so \"datanames\" will be ignored", label))
    datanames <- NULL
  }

  srv_extra_args <- setdiff(names(server_args), server_formals)
  if (length(srv_extra_args) > 0 && !"..." %in% server_formals) {
    stop(
      "\nFollowing `server_args` elements have no equivalent in the formals of the `server`:\n",
      paste(paste(" -", srv_extra_args), collapse = "\n"),
      "\n\nUpdate the `server` arguments by including above or add `...`"
    )
  }

  ui_formals <- names(formals(ui))
  if (!"id" %in% ui_formals) {
    stop(
      "\nmodule() `ui` argument requires a function with following arguments:",
      "\n - id - teal will set proper shiny namespace for this module.",
      "\n\nFollowing arguments can be used optionaly:",
      "\n - `data` - module will receive list of reactive (filtered) data specied in the `filters` argument",
      "\n - `datasets` - module will receive `FilteredData`. See `help(teal.slice::FilteredData)`",
      "\n - `...` ui_args elements will be passed to the module argument of the same name or to the `...`"
    )
  }

  ui_extra_args <- setdiff(names(ui_args), ui_formals)
  if (length(ui_extra_args) > 0 && !"..." %in% ui_formals) {
    stop(
      "\nFollowing `ui_args` elements have no equivalent in the formals of `ui`:\n",
      paste(paste(" -", ui_extra_args), collapse = "\n"),
      "\n\nUpdate the `ui` arguments by including above or add `...`"
    )
  }

  structure(
    list(
      label = label,
      server = server, ui = ui, datanames = datanames,
      server_args = server_args, ui_args = ui_args
    ),
    class = "teal_module"
  )
}

srv_teal <- function(id, modules, raw_data, filter = teal_slices()) {
  stopifnot(is.reactive(raw_data))
  moduleServer(id, function(input, output, session) {
    logger::log_trace("srv_teal initializing the module.")

    output$identifier <- renderText(
      paste0("Pid:", Sys.getpid(), " Token:", substr(session$token, 25, 32))
    )

    teal.widgets::verbatim_popup_srv(
      "sessionInfo",
      verbatim_content = utils::capture.output(utils::sessionInfo()),
      title = "SessionInfo"
    )

    # `JavaScript` code
    # run_js_files(files = "init.js") # `JavaScript` code to make the clipboard accessible
    # set timezone in shiny app
    # timezone is set in the early beginning so it will be available also
    # for `DDL` and all shiny modules
    session$userData$timezone <- "Europe/Paris"

    # loading the data -----
    env <- environment()
    datasets_reactive <- reactive({
      if (is.null(raw_data())) {
        return(NULL)
      }
      env$progress <- shiny::Progress$new(session)
      env$progress$set(0.25, message = "Setting data")

      # create a list of data following structure of the nested modules list structure.
      # Because it's easier to unpack modules and datasets when they follow the same nested structure.
      datasets_singleton <- teal.slice::init_filtered_data(raw_data())
      # Singleton starts with only global filters active.
      filter_global <- Filter(function(x) x$id %in% attr(filter, "mapping")$global_filters, filter)
      datasets_singleton$set_filter_state(filter_global)
      module_datasets <- function(modules) {
        if (inherits(modules, "teal_modules")) {
          datasets <- lapply(modules$children, module_datasets)
          labels <- vapply(modules$children, `[[`, character(1), "label")
          names(datasets) <- labels
          datasets
        } else if (isTRUE(attr(filter, "module_specific"))) {
          # we should create FilteredData even if modules$datanames is null
          # null controls a display of filter panel but data should be still passed
          datanames <- if (is.null(modules$datanames)) raw_data()$get_datanames() else modules$datanames
          data_objects <- sapply(
            datanames,
            function(dataname) {
              dataset <- raw_data()$get_dataset(dataname)
              list(
                dataset = dataset$get_raw_data(),
                metadata = dataset$get_metadata(),
                label = dataset$get_dataset_label()
              )
            },
            simplify = FALSE
          )
          datasets_module <- teal.slice::init_filtered_data(
            data_objects,
            join_keys = raw_data()$get_join_keys(),
            code = raw_data()$get_code_class(),
            check = raw_data()$get_check()
          )

          # set initial filters
          slices <- Filter(x = filter, f = function(x) {
            x$id %in% unique(unlist(attr(filter, "mapping")[c(modules$label, "global_filters")])) &&
              x$dataname %in% datanames
          })
          include_varnames <- attr(slices, "include_varnames")[names(attr(slices, "include_varnames")) %in% datanames]
          exclude_varnames <- attr(slices, "exclude_varnames")[names(attr(slices, "exclude_varnames")) %in% datanames]
          slices$include_varnames <- include_varnames
          slices$exclude_varnames <- exclude_varnames
          datasets_module$set_filter_state(slices)
          datasets_module
        } else {
          datasets_singleton
        }
      }
      datasets <- module_datasets(modules)

      logger::log_trace("srv_teal@4 Raw Data transferred to FilteredData.")
      datasets
    })

    reporter <- teal.reporter::Reporter$new()
    is_any_previewer <- function(modules) {
      if (inherits(modules, "teal_modules")) {
        any(unlist(lapply(modules$children, is_any_previewer), use.names = FALSE))
      } else if (inherits(modules, "teal_module_previewer")) {
        TRUE
      } else {
        FALSE
      }
    }
    # if (is_arg_used(modules, "reporter") && !is_any_previewer(modules)) {
    #   modules <- append_module(modules, reporter_previewer_module())
    # }

    # Replace splash / welcome screen once data is loaded ----
    # ignoreNULL to not trigger at the beginning when data is NULL
    # just handle it once because data obtained through delayed loading should
    # usually not change afterwards
    # if restored from bookmarked state, `filter` is ignored
    observeEvent(datasets_reactive(), ignoreNULL = TRUE, once = TRUE, {
      logger::log_trace("srv_teal@5 setting main ui after data was pulled")
      env$progress$set(0.5, message = "Setting up main UI")
      on.exit(env$progress$close())
      # main_ui_container contains splash screen first and we remove it and replace it by the real UI

      removeUI(sprintf("#%s:first-child", session$ns("main_ui_container")))
      insertUI(
        selector = paste0("#", session$ns("main_ui_container")),
        where = "beforeEnd",
        # we put it into a div, so it can easily be removed as a whole, also when it is a tagList (and not
        # just the first item of the tagList)
        ui = div(ui_tabs_with_filters(
          session$ns("main_ui"),
          modules = modules,
          datasets = datasets_reactive(),
          filter = filter
        )),
        # needed so that the UI inputs are available and can be immediately updated, otherwise, updating may not
        # have any effect as they are ignored when not present
        immediate = TRUE
      )

      # must make sure that this is only executed once as modules assume their observers are only
      # registered once (calling server functions twice would trigger observers twice each time)
      active_module <- srv_tabs_with_filters(
        id = "main_ui",
        datasets = datasets_reactive(),
        modules = modules,
        reporter = reporter,
        filter = filter
      )
      return(active_module)
    })
  })
}


#' @export
srv_teal_with_splash <- function(id, data, modules, filter = teal_slices()) {
  moduleServer(id, function(input, output, session) {
    logger::log_trace(
      "srv_teal_with_splash initializing module with data { paste(data$get_datanames(), collapse = ' ')}."
    )

    is_pulled_data <- teal.data::is_pulled(data)
    # raw_data contains TealDataAbstract, i.e. R6 object and container for data
    # reactive to get data through delayed loading
    # we must leave it inside the server because of callModule which needs to pick up the right session
    if (is_pulled_data) {
      raw_data <- reactiveVal(data) # will trigger by setting it
    } else {
      raw_data <- data$get_server()(id = "startapp_module")
      if (!is.reactive(raw_data)) {
        stop("The delayed loading module has to return a reactive object.")
      }
    }

    res <- srv_teal(id = "teal", modules = modules, raw_data = raw_data, filter = filter)
    return(res)
  })
}