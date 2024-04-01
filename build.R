library(fs)
library(shinylive)

dir_source <- "app"
dir_build <- "_site"
overwrite <- TRUE

if (isFALSE(overwrite) && dir_exists(dir_build)) {
  stop(sprintf("Output directory '%s' already exists", dir_build), call. = FALSE)
}

temp_dir <- file_temp("fda-pilot")
on.exit(dir_delete(temp_dir))
temp_dir_source <- path_join(c(temp_dir, "src"))
temp_dir_source_www <- path_join(c(temp_dir_source, "www"))
temp_dir_www <- path_join(c(temp_dir, "www"))
temp_dir_out <- path_join(c(temp_dir, "out"))

dir_create(temp_dir)
dir_copy(dir_source, temp_dir_source)
if (dir_exists(temp_dir_source_www)) {
  file_move(temp_dir_source_www, temp_dir_www) # Move www out for shinylive::export to work cleanly
}

export(temp_dir_source, temp_dir_out, verbose = FALSE)
if (dir_exists(temp_dir_www)) {
  dir_walk(temp_dir_www, \(f) file_move(f, temp_dir_out))
}

if (isTRUE(overwrite) && dir_exists(dir_build)) {
  dir_delete(dir_build)
}
dir_copy(temp_dir_out, dir_build)

msg <- 'Run the following in an R session to serve the app:
  httpuv::runStaticServer("%s")'
cat(sprintf(msg, dir_build), fill = TRUE)
