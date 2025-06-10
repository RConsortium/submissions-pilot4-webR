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

create_app_pkglite_bundle <- function(
  source_app_dir = "app", 
  renv_lockfile = "renv.lock",
  rstudio_project_file = "submissions-pilot4-webr.Rproj",
  utils_file = "utils.R",
  pkglite_file = "pilot4_webR_pkglite.txt"
) {

  # create temp directory and move relevant files
  working_dir <- tempdir()
  file.copy(file.path(source_app_dir, "DESCRIPTION"), working_dir)
  file.copy(renv_lockfile, working_dir)
  file.copy(rstudio_project_file, working_dir)
  file.copy(utils_file, working_dir)

  # application source code
  app_source_pkg <- source_app_dir |>
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
        path = "www/static/",
        format = "binary",
        pattern = "*.png$",
        recursive = TRUE
      ),
      # misc data files in rds format
      pkglite::file_spec(
        path = "www/adam/",
        format = "binary",
        pattern = "*.rds$",
        recursive = FALSE
      ),
      # svg and other web asset files
      pkglite::file_spec(
        path = "www/static/",
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
    )
  
  # supporting files
  renv_spec <- pkglite::file_spec(
    ".",
    pattern = "\\.lock",
    format = "text",
    recursive = FALSE
  )

  rstudio_proj_spec <- pkglite::file_spec(
    ".",
    pattern = "\\.Rproj",
    format = "text",
    recursive = FALSE
  )

  utils_spec <- pkglite::file_spec(
    ".",
    pattern = "^utils",
    format = "text",
    recursive = FALSE
  )

  app_support_pkg <- working_dir |>
    pkglite::collate(
      renv_spec,
      rstudio_proj_spec,
      utils_spec
    )

  # assemble everything to a single bundle
  pkglite::pack(
    app_source_pkg,
    app_support_pkg,
    output = file.path("dev", pkglite_file), 
    quiet = FALSE
  )
}

create_env_pkglite_bundle <- function(
  source_app_dir = "app", 
  renv_lockfile = "renv.lock",
  rstudio_project_file = "submissions-pilot4-webr.Rproj",
  utils_file = "utils.R",
  pkglite_file = "pilot4_webR_env.txt"
) {

  # create temp directory and move relevant files
  working_dir <- tempdir()
  file.copy(file.path(source_app_dir, "DESCRIPTION"), working_dir)
  file.copy(renv_lockfile, working_dir)
  file.copy(rstudio_project_file, working_dir)
  file.copy(utils_file, working_dir)

  renv_spec <- pkglite::file_spec(
    ".",
    pattern = "\\.lock",
    format = "text",
    recursive = FALSE
  )

  rstudio_proj_spec <- pkglite::file_spec(
    ".",
    pattern = "\\.Rproj",
    format = "text",
    recursive = FALSE
  )

  utils_spec <- pkglite::file_spec(
    ".",
    pattern = "^utils",
    format = "text",
    recursive = FALSE
  )

  pkglite::collate(
    pkg = working_dir,
    renv_spec,
    rstudio_proj_spec,
    utils_spec
  ) |>
    pkglite::pack(output = file.path("dev", pkglite_file))
}

unpack_pkglite_bundle <- function(
  pkglite_file = "pilot4_webR_pkglite.txt",
  datasets_dir = "dev/datasets",
  renv_lockfile = "renv.lock",
  rstudio_project_file = "submissions-pilot4-webr.Rproj",
  utils_file = "utils.R",
  output_dir = "pilot4_webR_files"
) {
  # unpack all files to output_dir
  pkglite::unpack(input = pkglite_file, output = output_dir)

  # manually move non-app source files to root of output_dir
  file.rename(
    from = file.path(output_dir, "pilot4webR", renv_lockfile),
    to = file.path(output_dir, renv_lockfile)
  )

  file.rename(
    from = file.path(output_dir, "pilot4webR", rstudio_project_file),
    to = file.path(output_dir, rstudio_project_file)
  )

  file.rename(
    from = file.path(output_dir, "pilot4webR", utils_file),
    to = file.path(output_dir, utils_file)
  )

  # rename pilot4webR directory to app
  file.rename(
    from = file.path(output_dir, "pilot4webR"),
    to = file.path(output_dir, "app")
  )

  # copy xpt data files to appropriate directory (app/www/adam)
  file.copy(
    from = file.path(datasets_dir, "adadas.xpt"),
    to = file.path(output_dir, "app", "www", "adam", "adadas.xpt")
  )

  file.copy(
    from = file.path(datasets_dir, "adlbc.xpt"),
    to = file.path(output_dir, "app", "www", "adam", "adlbc.xpt")
  )

  file.copy(
    from = file.path(datasets_dir, "adsl.xpt"),
    to = file.path(output_dir, "app", "www", "adam", "adsl.xpt")
  )

  file.copy(
    from = file.path(datasets_dir, "adtte.xpt"),
    to = file.path(output_dir, "app", "www", "adam", "adtte.xpt")
  )
}
