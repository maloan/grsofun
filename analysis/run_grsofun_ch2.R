#' Global simulation using grsofun
#'
#' Demonstrates a full global rsofun simulation, using WATCH-WFDEI climate and
#' MODIS fAPAR forcing. This script prepares tidy input files, runs the P-model
#' over a global grid, and visualizes GPP output as monthly maps.
#'
#' @details The grsofun framework allows running {rsofun} over large gridded
#'   inputs by first tidying NetCDF inputs into gridcell-specific time series,
#'   running simulations in parallel, and collecting outputs back into a tidy
#'   structure.
#'
# -----------------------------------------------------------
# Global rsofun simulation with grsofun: WATCH-WFDEI + MODIS
# -----------------------------------------------------------
suppressPackageStartupMessages({
  library(BayesianTools)
  library(ggplot2)
  library(dplyr)
  library(rnaturalearth)
  library(cowplot)
  library(map2tidy)
  library(purrr)
  library(parallel)
  library(here)
  library(sf)
  library(tictoc)
  library(terra)
  library(readr)
  library(ncdf4)
})

message("Starting program..")

# Load all R scripts from the R/ directory
source_files <- list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE)
purrr::walk(source_files, source)

ncores <- max(as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", 1)) - 1, 1)
ncores <- min(ncores, 8) # or 4, depending on memory

base_data_path <- "/storage/research/giub_geco/data_2"
source_fapar <- "fpar_masked"
experiment <- "transient"         # "transient", "fix_co2", "fix_precip", "fix_vpd", "fix_fapar"
fileprefix <- "prefix"

# -----------------------------------------------------------
# Model runs
# -----------------------------------------------------------
set_factorial_experiment <- function(settings, experiment, output_root) {
  experiment_cfg <- list(
    transient = list(
      fileprefix = "PM-S0_TRANSIENT",
      factorial = list(enabled = FALSE, driver = "none")
    ),
    fix_co2 = list(
      fileprefix = "PM-S0_FIX_CO2",
      factorial = list(enabled = TRUE, driver = "co2")
    ),
    fix_precip = list(
      fileprefix = "PM-S0_FIX_PRECIP",
      factorial = list(enabled = TRUE, driver = "precip")
    ),
    fix_vpd = list(
      fileprefix = "PM-S0_FIX_VPD",
      factorial = list(enabled = TRUE, driver = "vpd")
    ),
    fix_fapar = list(
      fileprefix = "PM-S0_FIX_FPAR",
      factorial = list(enabled = TRUE, driver = "fapar")
    )
  )

  cfg <- experiment_cfg[[experiment]]
  if (is.null(cfg)) {
    stop("Unknown experiment: ", experiment)
  }

  settings$fileprefix <- cfg$fileprefix
  settings$factorial <- cfg$factorial

  settings$dir_out <- file.path(output_root, "grsofun_output", settings$fileprefix, "")
  settings$dir_out_nc <- file.path(output_root, "grsofun_output", settings$fileprefix)
  settings$dir_out_drivers <- file.path(output_root, "grsofun_input", settings$fileprefix)

  settings
}

# -----------------------------------------------------------
# Model and I/O configuration
# -----------------------------------------------------------
base_out <- if (source_fapar == "modis") {
  "/storage/research/giub_geco/data_2/scratch/akurth"
} else {
  "/storage/research/giub_geco/data_2/scratch/akurth/chapter_2"
}

settings <- list(
  factorial = list(enabled = TRUE, driver  = "none"),
  # simulation config
  model       = "pmodel",
  year_start  = 1982,
  # note: not yet handled
  year_end    = 2024,
  # note: not yet handled
  spinupyears = 10,
  recycle     = 1,

  # HPC config
  nnodes     = 1,
  ncores_max = ncores,

  # tidy model input config
  grid = list(
    lon_start = -179.75,
    dlon      = 0.5,
    len_ilon  = 720,
    lat_start = -89.75,
    dlat      = 0.5,
    len_ilat  = 360
  ),

  # Simulation parameters
  params_siml = list(
    use_gs     = FALSE,
    use_pml    = FALSE,
    use_phydro = FALSE
  ),

  # Model output
  save = list(
    gpp   = "mon",
    aet   = "mon",
    pet   = "mon",
    wcont = "mon",
    iwue  = "mon",
    fapar = "mon",
    wscal = "mon",
    snow  = "mon",
    cond  = "mon",
    le    = "mon"
  ),
  overwrite = TRUE,

  fileprefix = fileprefix,

  # Source
  source_fapar   = source_fapar,
  source_climate = "watch-wfdei",

  # Path settings
  dir_in_ssr = file.path(base_data_path, "ERA5Land", "remap"),
  dir_in_str = file.path(base_data_path, "ERA5Land", "remap"),

  file_in_canopy = file.path(
    base_data_path,
    "vegheight_lang_2023",
    "canopy_mean_0.5deg.nc"
  ),
  file_in_co2    = file.path(base_data_path, "global", "co2_annmean_mlo.csv"),
  dir_in_climate = file.path(base_data_path, "wfdei_weedon_2014", "data"),

  file_in_fapar = if (source_fapar == "modis") {
    file.path(
      base_data_path,
      "modis_lai_fpar",
      "MODIS-C061_MOD15A2H__LAI_FPAR__LPDAAC__GLOBAL_0.5degree__UHAM-ICDC__2000_2024__MON__fv0.03.nc"
    )
  } else {
    file.path(base_data_path,
              "fpar_masked",
              "fpar_masked_0p5_monthly_1982-2024.nc")
  },

  file_in_whc = file.path(
    base_data_path,
    "scratch",
    "fbernhard",
    "whc_stocker_2023",
    "data",
    "remap",
    "cwdx80_forcing_0.5degbil.nc"
  ),
  file_in_landmask = file.path(
    base_data_path,
    "wfdei_weedon_2014",
    "data",
    "WFDEI-elevation.nc"
  ),
  file_in_elv      = file.path(
    base_data_path,
    "wfdei_weedon_2014",
    "data",
    "WFDEI-elevation.nc"
  ),
  file_in_gicew    = file.path(base_data_path, "gicew", "gicew_halfdeg.cdf"),

  # tidy outputs
  dir_out_tidy_ssr     = file.path(base_data_path, "ERA5Land", "remap", "tidy"),
  dir_out_tidy_str     = file.path(base_data_path, "ERA5Land", "remap", "tidy"),
  dir_out_tidy_canopy  = file.path(base_data_path, "vegheight_lang_2023", "tidy"),
  dir_out_tidy_climate = file.path(base_data_path, "watch_wfdei", "tidy"),
  dir_out_tidy_fapar   = if (source_fapar == "modis") {
    file.path(base_data_path, "modis_lai_fpar", "global", "tidy")
  } else {
    file.path(base_data_path, "fpar_masked", "tidy")
  },
  dir_out_tidy_whc      = file.path(base_data_path, "mct_data", "tidy"),
  dir_out_tidy_landmask = file.path(base_data_path, "watch_wfdei", "tidy"),
  dir_out_tidy_elv      = file.path(base_data_path, "watch_wfdei", "tidy"),
  dir_out_tidy_gicew    = file.path(base_data_path, "gicew", "tidy"),

  # final model output
  dir_out        = file.path(base_out, "grsofun_output", paste0(fileprefix, "/")),
  dir_out_nc     = file.path(base_out, "grsofun_output", fileprefix),
  dir_out_drivers = file.path(base_out, "grsofun_input", fileprefix)
)
# -----------------------------------------------------------
# Model settings
# -----------------------------------------------------------
settings <- set_factorial_experiment(settings, experiment, output_root = base_out)
print(settings$factorial)
print(settings$fileprefix)

# -----------------------------------------------------------
# Model parameters
# -----------------------------------------------------------

# PM-S0 parameter set
par <- list(
  kphio              = 0.04756,
  kphio_par_a        = -0.00099,
  kphio_par_b        = 24.06332,
  soilm_thetastar    = 398.99790,
  #err_gpp            = 2.31061,
  #err_le             = 24.25638,
  gw_calib           = 0.74075,
  beta_unitcostratio = 146.0,
  rd_to_vcmax        = 0.014,
  tau_acclim         = 20.0,
  kc_jmax            = 0.41
)

settings$params_siml <- list(use_gs = TRUE,
                             use_pml = TRUE,
                             use_phydro = FALSE)

print(settings)


# -----------------------------------------------------------
# Preprocess tidy input data from NetCDF
# -----------------------------------------------------------
message("Tidying data...")
tictoc::tic("Tidying input")
# tidy_out <- grsofun_tidy(settings)
tictoc::toc()
gc()
# -----------------------------------------------------------
# Run grsofun model simulation
# -----------------------------------------------------------
message("Running Model...")
tictoc::tic("Run model")
error <- grsofun_run(par, settings)
tictoc::toc()
gc()

# -----------------------------------------------------------
# Collect model output data
# -----------------------------------------------------------
message("Collecting model output...")
tictoc::tic("Collect model output")
df <- grsofun_collect(settings, return_data = TRUE)
tictoc::toc()
gc()

# -----------------------------------------------------------
# Save model output data
# -----------------------------------------------------------
message("Saving model output...")
tictoc::tic("Save model output")
df <- grsofun_save_nc(df, settings)
tictoc::toc()
gc()

# ------------------------------------------------------------
# Plot model output
# -----------------------------------------------------------
message("Plotting model output...")
tictoc::tic("Plot model output")
df_plot <- df |>
  filter(year >= 2000, year <= 2020) |>
  group_by(lon, lat) |>
  summarise(gpp = mean(gpp, na.rm = TRUE), .groups = "drop")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

p1 <- ggplot(df_plot) +
  geom_raster(aes(x = lon, y = lat, fill = gpp)) +
  geom_sf(data = world, fill = NA, color = "gray30", size = 0.1) +
  scale_fill_viridis_c(name = "GPP (gC m-2 yr-1)") +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  labs(title = "Mean GPP (2000-2020)")

p2 <- ggplot(df_plot) +
  geom_raster(aes(x = lon, y = lat, fill = aet)) +
  geom_sf(data = world, fill = NA, color = "gray30", size = 0.1) +
  scale_fill_viridis_c(name = "AET (mm yr-1)") +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  labs(title = "Mean AET (2000-2020)")

ggsave(file.path(settings$dir_out_nc, "gpp_map.png"), p1, width = 14, height = 8, dpi = 150)
ggsave(file.path(settings$dir_out_nc, "aet_map.png"), p2, width = 14, height = 8, dpi = 150)
tictoc::toc()
gc()