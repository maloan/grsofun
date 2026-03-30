#!/bin/bash
#SBATCH --job-name=test_0_grsofun
#SBATCH --output=log/%x_%j.log
#SBATCH --error=log/%x_%j.err
#SBATCH --time=02:00:00
#SBATCH --mail-type=end,fail
#SBATCH --partition=icpu-stocker
#SBATCH --account=invest
#SBATCH --qos=job_icpu-stocker
#SBATCH --cpus-per-task=16

set -euo pipefail

module purge
module load R/4.4.2-gfbf-2024a UDUNITS/2.2.28-GCCcore-13.3.0 \
PROJ/9.4.1-GCCcore-13.3.0 GDAL/3.10.0-foss-2024a CDO/2.4.4-gompi-2024a

## To change
PREFIX=test_0
START=2000
END=2018

export GRSOFUN_DIR="$HOME/grsofun" # grsofun repository needs to be cloned
export R_LIBS_USER=$HOME/R/x86_64-pc-linux-gnu-library/4.4

# --- Run the script -----------------------------------------------------------
# Rscript "$GRSOFUN_DIR/analysis/run_grsofun_modis.R" # For remote
Rscript run_grsofun_modis.R # For within grsofun