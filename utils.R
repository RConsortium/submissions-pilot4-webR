#' Assemble web-assembly version of application
#'
#' @param dir_source path to the directory containing the traditional (R)
#'   version of the application files. By default the path is to a directory
#'   called `app` in the current working directory where this function is
#'   executed.
#' @param dir_build path to the new directory that will contain the compiled
#'   application files. By default this path is to a directory called `_site`
#'   in the current working directory where this function is executed.
#' @param overwrite flag to overwrite the existing compiled application files.
#'   Default is `TRUE`.
#' @param quiet flag to suppress compilation messages and progress indicators
#'   in the R console while the function executes. Default is `FALSE`.
#' @return None, as the function is used for the "side effect" of compiling
#'   the application
#' @example
#' \dpntrun{
#' build_app()
#' }
build_app <- function(dir_source = "app", dir_build = "_site", overwrite = TRUE, quiet = FALSE) {
  if (isFALSE(overwrite) && fs::dir_exists(dir_build)) {
    withr::with_options(
      list(rlang_backtrace_on_error = "none"),
      cli::cli_abort(
        c("Output directory {dir_build} already exists.",
          "Delete the directory or set {.code overwrite=TRUE}"
        ),
        call = NULL
      )
    )
  }

  temp_dir <- fs::file_temp("fda-pilot")
  on.exit(fs::dir_delete(temp_dir))
  temp_dir_source <- fs::path_join(c(temp_dir, "src"))
  temp_dir_source_www <- fs::path_join(c(temp_dir_source, "www"))
  temp_dir_www <- fs::path_join(c(temp_dir, "www"))
  temp_dir_out <- fs::path_join(c(temp_dir, "out"))

  fs::dir_create(temp_dir)
  fs::dir_copy(dir_source, temp_dir_source)
  if (fs::dir_exists(temp_dir_source_www)) {
    fs::file_move(temp_dir_source_www, temp_dir_www) # Move www out for shinylive::export to work cleanly
  }

  shinylive::export(temp_dir_source, temp_dir_out, quiet = quiet, wasm_packages = FALSE)
  if (fs::dir_exists(temp_dir_www)) {
    fs::dir_walk(temp_dir_www, \(f) fs::file_move(f, temp_dir_out))
  }

  if (isTRUE(overwrite) && fs::dir_exists(dir_build)) {
    fs::dir_delete(dir_build)
  }
  fs::dir_copy(temp_dir_out, dir_build)
}

#' Execute web assembly version of the Shiny application
#'
#' @param dir path to the directory containing the compiled web-assembly
#'   application files. By default this path is to a directory called
#'   `_site` in the current working directory where this function is executed.
#' @param port integer denoting the port used by the web process. Default is
#'   `7654`, however this can be changed to any available port on your system
#'   (typically in the 1000-9999 range).
#' @return interactive web server process created by `httpuv::runStaticServer()`.
#' @example
#' \dontrun{
#' run_app_webassembly()
#' }
run_app_webassembly <- function(dir = "_site", port = 7654) {
  if (!fs::file_exists(fs::path(dir, "app.json"))) {
    withr::with_options(
      list(rlang_backtrace_on_error = "none"),
      cli::cli_abort(
        c("Webassembly application not detected in directory {dir}.",
          "Run {.code build_app()} and try again"
        ),
        call = NULL
      )
    )
  }

  httpuv::runStaticServer(dir = dir, port = port, browse = FALSE)
}

#' Execute traditional version of the Shiny application
#'
#' @param dir path to the directory containing the traditional (R) version
#'   of the application files. By default this path is to a directory called
#'   `app` in the current working directory where this function is executed.
#' @param port integer denoting the port used by the web process. Default is
#'   `7654`, however this can be changed to any available port on your system
#'   (typically in the 1000-9999 range).
#' @return interactive web server process created by `shiny::runApp()`.
#' @example
#' \dontrun{
#' run_app_shiny()
#' }
run_app_shiny <- function(dir = "app", port = 7654) {
  if (!fs::file_exists(fs::path(dir, "app.R"))) {
    withr::with_options(
      list(rlang_backtrace_on_error = "none"),
      cli::cli_abort(
        c("Shiny application not detected in directory {dir}.",
          "Ensure application files were copied successfully as documented in the ADRG."
        ),
        call = NULL
      )
    )
  }

  shiny::runApp(appDir = dir, port = port)
}
