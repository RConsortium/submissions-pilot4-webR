app_dir_exists <- function(dir = "_site") {
  fs::dir_exists(dir) && length(fs::dir_ls(dir) > 0)
}

build_app <- function(dir_source = "app", dir_build = "_site", overwrite = TRUE) {
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

  shinylive::export(temp_dir_source, temp_dir_out, quiet = FALSE, wasm_packages = FALSE)
  if (fs::dir_exists(temp_dir_www)) {
    fs::dir_walk(temp_dir_www, \(f) fs::file_move(f, temp_dir_out))
  }

  if (isTRUE(overwrite) && fs::dir_exists(dir_build)) {
    fs::dir_delete(dir_build)
  }
  fs::dir_copy(temp_dir_out, dir_build)
}

run_app_webassembly <- function(dir = "_site", port = 7654) {
  if (!fs::file_exists(fs::path(dir, "app.json"))) {
    withr::with_options(
      list(rlang_backtrace_on_error = "none"),
      cli::cli_abort(
        c("Webassembly application not detected in directory {dir}.",
          "Run {.code build_app()} or {.code extract_app_bundle()} and try again"
        ),
        call = NULL
      )
    )
  }

  httpuv::runStaticServer(dir = dir, port = port, browse = FALSE)
}

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

create_ectd_bundle <- function(archive_name = "r4app.zip") {
  archive::archive_write_files(
    archive = fs::path("ectd_bundle", archive_name),
    files = c(
      fs::dir_ls("app", recurse = TRUE),
      fs::dir_ls("app_bundle", recurse = TRUE),
      "renv/.gitignore",
      "renv/activate.R",
      "renv/settings.json",
      "utils.R",
      "renv.lock",
      "submissions-pilot4-webr.Rproj",
      ".Rprofile"
    ),
    format = "zip"
  )
  invisible(TRUE)
}

create_app_bundle <- function(archive_name = "shinyapp.zip", dir_build = "_site") {
  if (!app_dir_exists(dir_build)) {
    build_app()
  }
  archive::archive_write_files(
    archive = fs::path("app_bundle", archive_name),
    files = c(
      fs::dir_ls("_site", recurse = TRUE)
    ),
    format = "zip"
  )
  invisible(TRUE)
}

extract_app_bundle <- function(archive_name = "shinyapp.zip", dir_build = "_site", overwrite = TRUE) {
  if (isTRUE(overwrite) && fs::dir_exists(dir_build)) {
    fs::dir_delete(dir_build)
  }
  archive::archive_extract(
    archive = fs::path("app_bundle", archive_name),
    dir = "."
  )
  invisible(TRUE)
}

create_pkglite_bundle <- function(source_app_dir = "app", pkglite_file = "pilot4_webR_pkglite.txt") {
  # application source code
  pkg <- source_app_dir

  pkg |>
    pkglite::collate(
      # logic R scripts
      pkglite::file_spec(
        path = "logic/",
        format = "text",
        pattern = "*.R$",
        recursive = TRUE
      ),
      # view R scripts
      pkglite::file_spec(
        path = "views/",
        format = "text",
        pattern = "*.R",
        recursive = TRUE
      ),
      # png images
      pkglite::file_spec(
        path = "www/static",
        format = "binary",
        pattern = "*.png$",
        recursive = TRUE
      ),
      # svg and other web asset files
      pkglite::file_spec(
        path = "www/static",
        format = "text",
        pattern = "*.svg$|*.md$|*.css$|*.js$",
        recursive = TRUE
      ),
      # index html page
      pkglite::file_spec(
        path = "www/",
        format = "text",
        pattern = "index.html",
        recursive = FALSE
      ),
      # app.R file
      pkglite::file_spec(
        path = ".",
        format = "text",
        pattern = "app.R",
        recursive = FALSE
      )
    ) |>
    pkglite::pack(output = pkglite_file, quiet = FALSE)
}

unpack_pkglite_bundle <- function(pkglite_file = "pilot4_webR_pkglite.txt", output_dir = "pilot4_webR_files") {
  pkglite::unpack(input = pkglite_file, output = output_dir)
}
