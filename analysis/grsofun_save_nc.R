#' Writes yearly global monthly NetCDF files from tidy monthly RDS outputs
#' produced by grsofun_collect_byLON().
#'

grsofun_save_nc <- function(settings) {
  # Variables to write (monthly outputs)
  vars <- names(settings$save)[unlist(settings$save) == "mon"]

  # Monthly tidy files by longitudinal band
  files <- list.files(
    settings$dir_out,
    pattern = paste0("^", settings$fileprefix, "_mon_LON_.*\\.rds$"),
    full.names = TRUE
  )
  stopifnot(length(files) > 0)

  # Grid definition (regular lon/lat)
  g <- settings$grid
  nlon <- g$len_ilon
  nlat <- g$len_ilat
  lon  <- g$lon_start + (0:(nlon - 1)) * g$dlon
  lat  <- g$lat_start + (0:(nlat - 1)) * g$dlat

  # Output directory for NetCDF files
  dir.create(settings$dir_out_nc,
             recursive = TRUE,
             showWarnings = FALSE)

  # NetCDF spatial dimensions
  dim_lon <- ncdim_def("lon", "degrees_east", lon)
  dim_lat <- ncdim_def("lat", "degrees_north", lat)

  # Write one NetCDF per year
  for (year in settings$year_start:settings$year_end) {
    # Allocate fill arrays (lon × lat × 12 months) for each variable
    arr <- lapply(vars, \(.) array(1e20, c(nlon, nlat, 12)))
    names(arr) <- vars

    # Fill arrays from all longitudinal-band files
    for (f in files) {
      x <- readr::read_rds(f)
      if (!nrow(x)) {
        next
      }

      # Subset to the current year
      x <- x[x$year == year, , drop = FALSE]
      if (!nrow(x)) {
        next
      }

      # Validate required columns
      miss <- setdiff(c("lon", "lat", "month", vars), names(x))
      if (length(miss)) {
        stop("Missing in ", basename(f), ": ", paste(miss, collapse = ", "))
      }

      # Map longitude to grid index (each file should correspond to one lon band)
      lo <- unique(x$lon)
      if (length(lo) != 1) {
        lo <- lo[1]
      }
      i <- as.integer(round((lo - g$lon_start) / g$dlon) + 1L)
      if (i < 1L || i > nlon) {
        next
      }

      # Map latitude and month to grid indices
      j <- as.integer(round((x$lat - g$lat_start) / g$dlat) + 1L)
      m <- as.integer(x$month)

      # Keep only valid indices
      ok <- !is.na(j) & j >= 1L & j <= nlat & m >= 1L & m <= 12L
      if (!any(ok)) {
        next
      }
      j <- j[ok]
      m <- m[ok]

      # Write values into arrays
      for (v in vars) {
        val <- x[[v]][ok]
        keep <- !is.na(val)
        if (any(keep))
          arr[[v]][i, j[keep], m[keep]] <- val[keep]
      }
    }

    # Time dimension (12 monthly steps for the current year)
    time_dates <- as.Date(sprintf("%04d-%02d-01", year, 1:12))
    time_vals  <- as.numeric(time_dates - as.Date("1900-01-01"))
    dim_time   <- ncdim_def("time", "days since 1900-01-01", time_vals, unlim = FALSE)

    # NetCDF variable definitions (units left blank)
    var_def <- function(name)
      ncvar_def(name,
                "",
                list(dim_lon, dim_lat, dim_time),
                missval = 1e20,
                prec = "float")

    # Create and write the yearly NetCDF
    nc_path <- file.path(dir_out_nc,
                         sprintf("%s_mon_%04d.nc", settings$fileprefix, year))
    nc <- nc_create(nc_path, lapply(vars, var_def), force_v4 = TRUE)

    for (v in vars)
      ncvar_put(nc, v, arr[[v]])
    nc_close(nc)

    message("Wrote: ", nc_path)
  }

  invisible(TRUE)
}
