box::use(
  # config[get],
  haven[read_xpt],
  dplyr[mutate, filter, select],
  utils[download.file]
)

get_file <- function(url) {
  if (file.exists(paste0("www/", url))) {
    xpt_path <- paste0("www/", url)
  } else {
    xpt_path <- tempfile(fileext = ".xpt")
    download.file(paste0("/", url), destfile = xpt_path, mode = "wb")
  }

  read_xpt(xpt_path)
}

#' @export
get_adsl <- function() {
  get_file("adam/adsl.xpt") |>
  # read_xpt(file.path("adam", "adsl.xpt")) |>
    mutate(
      TRT01P = factor(TRT01P, levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")),
      AGEGR1 = factor(AGEGR1, levels = c("<65", "65-80", ">80")),
      RACE = factor(
        RACE,
        levels = c("WHITE", "BLACK OR AFRICAN AMERICAN", "AMERICAN INDIAN OR ALASKA NATIVE")
      )
    )
}

#' @export
get_adas <- function() {
  get_file("adam/adadas.xpt") |>
  # read_xpt(file.path("adam", "adadas.xpt")) |>
    filter(
      EFFFL == "Y",
      ITTFL == "Y",
      PARAMCD == "ACTOT",
      ANL01FL == "Y"
    )
}

#' @export
get_adtte <- function() {
  get_file("adam/adtte.xpt") |>
  # read_xpt(file.path("adam", "adtte.xpt")) |>
    filter(PARAMCD == "TTDE") |>
    select(-c(TRTDUR, TRTP, TRTA, TRTAN))
}

#' @export
get_adlb <- function() {
  get_file("adam/adlbc.xpt") |>
  # read_xpt(file.path("adam", "adlbc.xpt")) |>
    filter(PARAMCD == "GLUC" & !is.na(AVISITN))
}
