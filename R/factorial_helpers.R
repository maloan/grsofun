#' Build a Fixed Day-of-Year Climatology
#'
#' Builds a mean annual cycle for one variable from the first 5 years present
#' in the input and maps that cycle back to the full time series by day of year.
#'
#' @param df A data frame for one site/grid cell containing a `date` column and
#'   the variable to be frozen.
#' @param varname Character string giving the column name of the variable to
#'   freeze.
#' @return A numeric vector with length `nrow(df)` containing the fixed
#'   day-of-year climatology.
build_fixed_cycle <- function(df, varname) {
  stopifnot("date" %in% names(df))
  stopifnot(varname %in% names(df))

  if (!any(!is.na(df$date))) {
    stop("build_fixed_cycle: column 'date' has no non-missing values.")
  }

  year0 <- min(lubridate::year(df$date), na.rm = TRUE)
  years_keep <- seq(year0, length.out = 5)

  year <- lubridate::year(df$date)
  doy <- lubridate::yday(df$date)

  keep <- year %in% years_keep
  clim <- tapply(
    X = df[[varname]][keep],
    INDEX = doy[keep],
    FUN = function(x) {
      mean(x, na.rm = TRUE)
    }
  )

  out <- rep(NA_real_, nrow(df))
  idx <- !is.na(doy)
  out[idx] <- unname(clim[as.character(doy[idx])])
  out
}

#' Freeze One Forcing Driver
#'
#' Replaces one forcing variable with a repeated day-of-year climatology built
#' from the first 5 years of the input time series.
#'
#' @param df A forcing data frame for one site/grid cell.
#' @param driver Character string selecting the driver to freeze. Must be one of
#'   `"co2"`, `"precip"`, `"vpd"`, or `"fapar"`.
#'
#' @return The input data frame with the selected forcing variable replaced by
#'   its fixed climatology.
freeze_driver <- function(df, driver) {
  var_lookup <- c(
    co2    = "co2",
    precip = "rain",
    vpd    = "vpd",
    fapar  = "fapar"
  )

  varname <- var_lookup[[driver]]

  if (is.null(varname)) {
    stop("Unknown driver: ", driver)
  }

  df[[varname]] <- build_fixed_cycle(df, varname = varname)

  df
}

#' Apply Factorial Forcing to Nested Inputs
#'
#' Applies a factorial forcing experiment across all nested forcing tables in
#' `df_forcing$data`.
#'
#' If `settings$factorial$enabled` is `TRUE` and a supported
#' `settings$factorial$driver` is provided, the selected driver is replaced by a
#' fixed climatology for each site/grid cell. If factorial mode is disabled, the
#' input is returned unchanged.
#'
#' @param df_forcing A nested forcing data frame containing a `data` list-column
#'   with one forcing table per site/grid cell.
#'
#' @return The modified nested forcing data frame.
apply_factorial_forcing <- function(df_forcing, settings) {
  factorial <- settings$factorial

  if (is.null(factorial) ||
        !isTRUE(factorial$enabled) ||
        is.null(factorial$driver) ||
        factorial$driver == "none") {
    return(df_forcing)
  }

  driver <- factorial$driver

  df_forcing |>
    dplyr::mutate(data = purrr::map(data, freeze_driver, driver = driver))
}