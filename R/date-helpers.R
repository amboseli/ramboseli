test_char_date <- function(test_col) {
  is.Date(suppressWarnings(lubridate::ymd(test_col))) | is.POSIXct(suppressWarnings(lubridate::ymd_hms(test_col)))
}

#' Function to add "year_of" and "month_of" columns to a dataframe
#'
#' @param df A data frame with a date column
#' @param date_col The unquoted name of the date column
#'
#' @return A data frame with the new date columns.
#' @export
#'
#' @examples
#' df <- data_frame(date_of = seq.Date(as.Date("2000-01-01"),
#'                                     as.Date("2000-12-01"),
#'                                     by = "1 month"),
#'                  x = runif(12))
#' make_date_cols(df, date_of)
make_date_cols <- function(df, date_col) {

  date_col <- dplyr::enquo(date_col)

  if (!(dplyr::quo_name(date_col) %in% names(df))) {
    stop("Date column not found.")
  }

  test_col <- dplyr::select(df, !!date_col)[[1]]

  if (lubridate::is.POSIXct(test_col) | lubridate::is.Date(test_col)) {
    res <- df %>%
      dplyr::mutate(year_of = lubridate::year(!!date_col),
                    month_of = lubridate::month(!!date_col))
  }
  else if (is.character(test_col) | test_char_date(test_col)) {
    res <- df %>%
      dplyr::mutate(year_of = lubridate::year(as.Date(!!date_col)),
                    month_of = lubridate::month(as.Date(!!date_col)))
  }

  res$month_of <- factor(res$month_of, labels = month.abb)

  res <- res %>%
    dplyr::select(!!date_col, year_of, month_of, everything())

  return(res)

}
