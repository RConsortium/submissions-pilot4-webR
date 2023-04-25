box::use(
  glue[glue],
  purrr[map_lgl],
  rtables[nrow],
  stringr[str_pad],
)

#' Add a padding row below data
#'
#' @param .data Data to pad
#' @param n Number of rows to pad
#'
#' @return Dataframe with extra blank rows
#' @export
pad_row <- function(.data, n = 1) {
  .data[(nrow(.data) + 1):(nrow(.data) + n), ] <- ""
  .data
}

#' Number formatter
#'
#' Format numbers for presentation, with proper rounding of data
#'
#' @param var Variable to format
#' @param digits Desired number of decimal places
#' @param size String size
#' @param int_len Space allotted for integer side of the decimal
#'
#' @return Formatted string
#' @export
num_fmt <- Vectorize(function(var, digits = 0, size = 10, int_len = 3) {
  # Formats summary stat strings to align display correctly

  if (is.na(var)) {
    return("")
  }

  # Set nsmall to input digits
  nsmall <- digits

  # Incremement digits for to compensate for display
  if (digits > 0) {
    digits <- digits + 1
  }

  # Form the string
  return(str_pad(
    format(
      # Round
      round(var, nsmall),
      # Set width of format string
      width = (int_len + digits),
      # Decimals to display
      nsmall = nsmall
    ),
    # Overall width padding
    side = "right", size
  ))
})

#' style a tooltip produced by the tippy package
#'
#' @param text String for text in tooltip
#' @param font_size Font size (in pixels)
#'
#' @return HTML with font size applied
#' @export
tooltip_text <- function(text, font_size = 16) {
  glue("<span style='font-size:{font_size}px;'>{text}<span>")
}

#' check if a filter is active in a teal module
#'
#' @param datasets instance of teal filtered datasets class
#'
#' @return boolean, TRUE if a filter is applied, FALSE otherwise
#' @export
filter_active <- function(datasets) {
  result <- FALSE
  if (length(names(datasets$get_filter_state()) > 0)) {
    filter_use <- map_lgl(names(datasets$get_filter_state()), ~ {
      # grab call of filter code
      f_call <- datasets$get_call(.x)$filter
      f_call != glue("{.x}_FILTERED <- {.x}")
    })
    result <- any(filter_use)
  }

  return(result)
}
