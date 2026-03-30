#' Writes a gridded monthly NetCDF from tidy monthly output.
#'
#' Input df must contain columns lon, lat, year, month and the variables listed
#' in settings$save with value "mon".
#'
#' @param df Tidy data frame.
#' @param settings List with dir_out_nc, fileprefix, grid, save.
#' @export
#'

grsofun_save_nc <- function(df, settings) {
  stopifnot(is.data.frame(df))

  dir.create(settings$dir_out_nc,
             recursive = TRUE,
             showWarnings = FALSE)

  vars <- names(settings$save)[unlist(settings$save) == "mon"]
  stopifnot(length(vars) > 0)
  stopifnot(all(vars %in% names(df)))

  g <- settings$grid
  nlon <- g$len_ilon
  nlat <- g$len_ilat

  # Regular grid coordinates
  lon_axis <- g$lon_start + (0:(nlon - 1)) * g$dlon
  lat_axis <- g$lat_start + (0:(nlat - 1)) * g$dlat

  # Monthly time axis represented by the 15th of each month
  dates <- sort(unique(as.Date(
    sprintf("%04d-%02d-15", df$year, df$month)
  )))
  origin <- as.Date("1900-01-01")
  time_vals <- as.integer(dates - origin)
  ntime <- length(dates)

  # Precompute indices once; this avoids repeated join/pivot work per month.
  df$date <- as.Date(sprintf("%04d-%02d-15", df$year, df$month))
  df$ilon <- match(df$lon, lon_axis)
  df$ilat <- match(df$lat, lat_axis)
  df$itime <- match(df$date, dates)

  idx_ok <- stats::complete.cases(df[, c("ilon", "ilat", "itime")])
  df_idx <- df[idx_ok, , drop = FALSE]

  # NetCDF definitions
  dlon  <- ncdf4::ncdim_def("lon", "degrees_east", lon_axis)
  dlat  <- ncdf4::ncdim_def("lat", "degrees_north", lat_axis)
  dtime <- ncdf4::ncdim_def("time", "days since 1900-01-01", time_vals)

  fill_value <- -9999
  vdefs <- lapply(vars, function(v) {
    ncdf4::ncvar_def(
      name     = v,
      units    = "1",
      dim      = list(dlon, dlat, dtime),
      missval  = fill_value,
      longname = v,
      prec     = "float"
    )
  })

  fn <- file.path(settings$dir_out_nc,
                  paste0(settings$fileprefix, "_mon.nc"))
  nc <- ncdf4::nc_create(fn, vdefs)

  # Write each variable into [lon, lat, time]
  for (k in seq_along(vars)) {
    v <- vars[k]
    arr <- array(fill_value, dim = c(nlon, nlat, ntime))

    if (nrow(df_idx) > 0) {
      vals <- df_idx[[v]]
      vals[is.na(vals)] <- fill_value
      arr[cbind(df_idx$ilon, df_idx$ilat, df_idx$itime)] <- vals
    }

    ncdf4::ncvar_put(nc, vdefs[[k]], arr)
  }

  ncdf4::nc_close(nc)
}
