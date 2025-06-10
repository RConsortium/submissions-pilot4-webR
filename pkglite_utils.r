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

unpack_pkglite_bundle <- function(
  pkglite_file = "pilot4_webR_pkglite.txt",
  datasets_dir = "datasets",
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
