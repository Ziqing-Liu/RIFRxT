########################################################################
# Reshape Supplementary_Table_6.xlsx into the tidy format grow96 expects,
# then run grow96's growth-curve analysis on it directly.
#
# This skips grow96::processODData() (which parses raw BioTek/Epoch
# plate-reader exports + a spec file) because Supplementary_Table_6.xlsx
# is already a cleaned summary table, not a raw plate-reader export.
# Instead we build the tibble that processODData() would normally
# produce, by hand, and feed that straight into blankODs()/analyseODData().
#
# ASSUMPTION: each sheet's 42 OD columns are 6 repeated blocks of the
# same 7 strains (WT, V146F, Q513L, Q513R, S531F, S522F, H526Y), and each
# block = one replicate. Check this against the sheet before trusting
# the Replicate numbering below.
########################################################################

library(readxl)
library(dplyr)
library(tidyr)
library(grow96)     # devtools::install_github("JanEngelstaedter/grow96")

filePath    <- "Supplementary_Table_6.xlsx"          # adjust path as needed
strainOrder <- c("WT", "V146F", "Q513L", "Q513R", "S531F", "S522F", "H526Y")

# --- Read one sheet and reshape to tidy format -------------------------
read_growth_sheet <- function(path, sheet, setTemperature) {
  # skip the title/blank rows and the header row; read raw values only
  raw <- read_excel(path, sheet = sheet, skip = 4, col_names = FALSE)
  names(raw) <- c("Time_h", paste0("col", seq_len(ncol(raw) - 1)))

  nStrains    <- length(strainOrder)
  nReplicates <- (ncol(raw) - 1) / nStrains
  if (nReplicates != round(nReplicates)) {
    stop("Number of OD columns isn't a whole multiple of the strain list - check strainOrder.")
  }

  strainLabels <- rep(strainOrder, times = nReplicates)
  repLabels    <- rep(seq_len(nReplicates), each = nStrains)
  names(raw)[-1] <- paste(strainLabels, repLabels, sep = "__")

  raw |>
    pivot_longer(cols = -Time_h, names_to = "colID", values_to = "OD") |>
    separate(colID, into = c("Strain", "Replicate"), sep = "__") |>
    mutate(
      Replicate      = as.integer(Replicate),
      SetTemperature = setTemperature,
      Plate          = paste0("T", setTemperature),
      Well           = paste(Strain, Replicate, sep = "_"),  # synthetic well ID
      WellType       = "DATA",
      Time_min       = Time_h * 60
    ) |>
    select(Plate, Replicate, Well, WellType, Strain, SetTemperature,
           Time_min, Time_h, OD)
}

# --- Combine both temperature sheets ------------------------------------

getwd()
list.files() 

data <- bind_rows(
  read_growth_sheet(filePath, "growth at 37C", 37),
  read_growth_sheet(filePath, "growth at 42C", 42)
)

# --- Blank the data -------------------------------------------------------
# t=0 readings are already close to zero, so this table looks pre-blanked.
# A fixed blank of 0 just copies OD through to blankedOD (required by
# analyseODData). Replace `values = 0` with a real blank OD if you have one.
data <- blankODs(data, method = "fixed", values = 0)

# --- Run grow96's growth-curve analysis ------------------------------------
growthAnalysis <- analyseODData(data)

# growthAnalysis$pars  -> per-well estimates (mumax, lag, r2, maxOD)
# growthAnalysis$means -> means per Strain x SetTemperature
# growthAnalysis$SDs   -> standard deviations
# growthAnalysis$SEs   -> standard errors
# growthAnalysis$n     -> number of non-NA replicates used

print(growthAnalysis$pars)
