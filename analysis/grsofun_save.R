#' Write monthly NetCDF files from grsofun tidy output
#'
#' Reads monthly RDS files written by `grsofun_collect()` and converts them
#' to gridded NetCDF files (one file per year-month).
#'
#' @param settings List containing grid definition, output directories,
#'                 file prefix, and save configuration.
#'
#' @export
#'

grsofun_save_nc_monthly <- function(settings) {
  # variables to write
  vars <- names(settings$save)[unlist(settings$save) == "mon"]
  stopifnot(length(vars) > 0)

  #locate monthly RDS files
  files <- list.files(
    settings$dir_out,
    pattern = paste0("^", settings$fileprefix, "_monLON_.*\\.rds$"),
    full.names = TRUE
  )
  stopifnot(length(files) > 0)

  # define raster template from grid settings
  # lon_start / lat_start are grid-cell centres → shift by 0.5*resolution
  g <- settings$grid

  r0 <- rast(
    xmin = g$lon_start - 0.5 * g$dlon,
    xmax = g$lon_start + (g$len_ilon - 0.5) * g$dlon,
    ymin = g$lat_start - 0.5 * g$dlat,
    ymax = g$lat_start + (g$len_ilat - 0.5) * g$dlat,
    ncols = g$len_ilon,
    nrows = g$len_ilat,
    crs   = "EPSG:4326"
  )

  # read all monthly tidy data
  dat <- map_dfr(files, ~ read_rds(.x))
  if (!nrow(dat)) {
    return(invisible(NULL))
  }

  dir.create(settings$dir_out_nc,
             recursive = TRUE,
             showWarnings = FALSE)

  # unique year-month combinations
  ym <- distinct(dat, year, month)

  # loop over months and write NetCDF
  pwalk(ym, function(year, month) {
    sub <- filter(dat, year == !!year, month == !!month)
    if (!nrow(sub)) {
      return()
    }

    # build multilayer raster (one layer per variable)
    r <- map(vars, function(v) {
      pts <- filter(select(sub, lon, lat, all_of(v)), !is.na(.data[[v]]))
      rr <- if (!nrow(pts)) {
        setValues(r0, NA_real_)
      } else {
        rasterize(pts[, 1:2], r0, pts[[v]], fun = "mean")
      }

      names(rr) <- v
      rr

    }) |> (\(x) do.call(c, x))()

    # write NetCDF file
    writeCDF(r, file.path(
      settings$dir_out_nc,
      sprintf("%s_%04d_%02d.nc", settings$fileprefix, year, month)
    ), overwrite = TRUE)

  })

  invisible(TRUE)
}
