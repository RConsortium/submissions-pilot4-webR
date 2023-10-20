library(readr)
library(fs)
library(dplyr)

# Original online location for downloading the files
original_repo_location <- "https://repo.r-wasm.org/bin/emscripten/contrib/4.3/"

# Output folder for the weR build
build_directory <- "site"

# Local folder (relative to the output folder of the build) to use as the local repo root
local_repo_folder <- "packages"

remote_package_index <- "PACKAGES.rds"

# List of all files required for the local repo
required_packages <- list(
    "askpass",
    "assertthat",
    "backports",
    "base64enc",
    "bit",
    "bit64",
    "box",
    "broom" ,
    "bslib",
    "cachem",
    "callr",
    "checkmate",
    "cli",
    "clipr",
    "codetools",
    "colorspace",
    "commonmark",
    "cowplot",
    "cpp11",
    "crayon",
    "digest",
    "dplyr",
    "ellipsis",
    "emmeans",
    "estimability",
    "evaluate",
    "fansi",
    "farver",
    "fastmap",
    "fontawesome",
    "forcats",
    "formatters",
    "fs",
    "generics",
    "ggplot2",
    "glue",
    "gridExtra",
    "gtable",
    "haven",
    "highr",
    "hms",
    "htmltools",
    "htmlwidgets",
    "httpuv",
    "httr",
    "huxtable",
    "isoband",
    "jquerylib",
    "jsonlite",
    "kableExtra",
    "knitr",
    "labeling",
    "later",
    "lattice",
    "lifecycle",
    "magrittr",
    "markdown",
    "MASS",
    "Matrix",
    "memoise",
    "mgcv",
    "mime",
    "munsell",
    "mvtnorm",
    "nlme",
    "numDeriv",
    "openssl",
    "pillar",
    "pkgconfig",
    "prettyunits",
    "processx",
    "progress",
    "promises",
    "purrr",
    "R6",
    "rappdirs",
    "RColorBrewer",
    "Rcpp",
    "reactable",
    "reactR",
    "readr",
    "renv",
    "rlang",
    "rmarkdown",
    "rstudioapi",
    "rtables",
    "rvest",
    "sass",
    "scales",
    "selectr",
    "shiny",
    "sourcetools",
    "stringi",
    "stringr",
    "survival",
    "svglite",
    "sys",
    "tibble",
    "tidyr",
    "tidyselect",
    "tinytex",
    "tippy",
    "Tplyr",
    "tzdb",
    "utf8",
    "vctrs",
    "viridisLite",
    "visR",
    "vroom",
    "webshot",
    "withr",
    "xfun",
    "xml2",
    "xtable",
    "yaml"
)

if (!dir.exists(build_directory)) {
    stop("Build directory does not exist")
}

full_package_path <- path(build_directory, local_repo_folder, "bin/emscripten/contrib/4.3/")
if (!dir.exists(full_package_path)) {
    dir.create(full_package_path, recursive = TRUE)
}

remote_package_info <- download.file(path(original_repo_location, "PACKAGES.rds"), destfile = path(full_package_path, "PACKAGES.rds"), mode = "wb")

remote_package_info <- readRDS(path(full_package_path, "PACKAGES.rds")) %>%
    as.data.frame()


for (name in required_packages) {
    remote_version <- remote_package_info %>%
        filter(Package == name) %>%
        .$Version

    remote_file <- paste0(name, "_", remote_version, ".tgz")

    if (!file.exists(path(full_package_path, remote_file))) {
        download.file(path(original_repo_location, remote_file), destfile = path(full_package_path, remote_file), mode = "wb")
    }
}

# Replace the repo location on the shinylive js file
tx <- read_file(path(build_directory, "shinylive/shinylive.js"))
tx2  <- gsub(pattern = "https://repo.r-wasm.org", replace = paste0("/", local_repo_folder, "/"), x = tx)
write_file(tx2, path(build_directory, "shinylive/shinylive.js"))
