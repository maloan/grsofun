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

  # Construct full regular coordinate vectors
  lon_axis <- g$lon_start + (0:(nlon - 1)) * g$dlon
  lat_axis <- g$lat_start + (0:(nlat - 1)) * g$dlat

  # time axis (monthly) -> 15. of month
  dates <- sort(unique(as.Date(
    sprintf("%04d-%02d-15", df$year, df$month)
  )))
  origin <- as.Date("1900-01-01")
  time_vals <- as.integer(dates - origin)
  ntime <- length(dates)

  # Add a date column to match against the dates vector
  df <- df |>
    dplyr::mutate(date = as.Date(sprintf("%04d-%02d-15", year, month)))

  # tidy df -> lon-lat grid matrix (lon x lat)
  df_to_grid <- function(d, varnam) {
    # Create a full grid skeleton of all lon–lat combinations
    grid_df <- tidyr::expand_grid(lon = lon_axis, lat = lat_axis)

    val_df <- d |>
      dplyr::transmute(lon = .data$lon,
                       lat = .data$lat,
                       value = .data[[varnam]])

    # complete grid + pivot
    mat_lat_lon <- grid_df |>
      dplyr::left_join(val_df, by = c("lon", "lat")) |>
      dplyr::arrange(lat, lon) |>
      tidyr::pivot_wider(names_from = lon, values_from = value) |>
      dplyr::arrange(lat) |>
      dplyr::select(-lat) |>
      as.matrix()

    # transpose to lon x lat
    t(mat_lat_lon)
  }

  # NetCDF defs -> Define the three dimensions for NetCDF
  dlon  <- ncdf4::ncdim_def("lon", "degrees_east", lon_axis)
  dlat  <- ncdf4::ncdim_def("lat", "degrees_north", lat_axis)
  dtime <- ncdf4::ncdim_def("time", "days since 1900-01-01", time_vals)

  fill_value <- -9999  # finite numeric fillvalue
  vdefs <- lapply(vars, function(v) {
    # Create NetCDF variable definitions for all variables with identical dims
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

  # write each variable
  for (k in seq_along(vars)) {
    v <- vars[k]
    # Initialize 3D array [lon, lat, time] filled with NA value
    arr <- array(fill_value, dim = c(nlon, nlat, ntime))

    for (t in seq_along(dates)) {
      # Subset df for one month
      d_t <- df[df$date == dates[t], , drop = FALSE]
      if (nrow(d_t) == 0) {
        next
      }

      m <- df_to_grid(d_t, v) # Convert tidy monthly data -> 2D lon–lat matrix
      m[is.na(m)] <- fill_value
      arr[, , t] <- m # Insert 2D slice into 3D array at time index
    }

    ncdf4::ncvar_put(nc, vdefs[[k]], arr)
  }

  ncdf4::nc_close(nc)
}
