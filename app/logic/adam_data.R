box::use(
  config[get],
  haven[read_xpt],
  dplyr[mutate, filter, select],
)

#' @export
get_adsl <- function() {
  read_xpt(file.path(get("adam_path"), "adsl.xpt")) |>
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
  read_xpt(file.path(get("adam_path"), "adadas.xpt")) |>
    filter(
      EFFFL == "Y",
      ITTFL == "Y",
      PARAMCD == "ACTOT",
      ANL01FL == "Y"
    )
}

#' @export
get_adtte <- function() {
  read_xpt(file.path(get("adam_path"), "adtte.xpt")) |>
    filter(PARAMCD == "TTDE") |>
    select(-c(TRTDUR, TRTP, TRTA, TRTAN))
}

#' @export
get_adlb <- function() {
  read_xpt(file.path(get("adam_path"), "adlbc.xpt")) |>
    filter(PARAMCD == "GLUC" & !is.na(AVISITN))
}
