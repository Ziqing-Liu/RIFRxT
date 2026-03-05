# This script creates all spec files, including pdfs, for the Edward's RIFRxT masters project.
# Once run in eanest to produce the final spec files, it should never be run again.
# Author: Jan Engelstaedter
# Date: 28 January 2026

# 0) Load libraries and specify path and mutant names

# install packages if not done already:
#install.packages("devtools")
#devtools::install_github("JanEngelstaedter/grow96")
# install.packages("pdftools")

library(tidyverse)
library(grow96)
library(pdftools)

spec_path <- "design/specs"

# mutant IDs, for testing only!
mutant_IDs <- c("WT", paste0("M", 1:28))

# actual mutant IDs:
# mutant_IDs <- c("WT", read_csv("data/mutants.csv") |> pull(mutant_ID))

# 1) make all spec files for the growth assays:
for(temp in 22:42) {
  makeSpec_wrapping(
    plateName = paste0("RIFxT", temp),
    wrapName = "mutant_ID",
    wraps = mutant_IDs,
    groupName = "growth_medium",
    groups = c("M9gluc", "LB"),
    border = "EMPTY",
    replicates = 3,
    randomise = "wraps",
    specPath = spec_path,
    makePlot = TRUE
  )
}

# 2) bind pdfs into single file for printing:
pdf_files <- list.files(spec_path) |>
  str_subset(".pdf") |>
  str_subset("specplot_")

pdf_combine(input = file.path(spec_path, pdf_files), 
            output = file.path(spec_path, "all_specplots.pdf"))

# 3) copy spec files to new, identical spec files for the overnight cultures:
spec_files <- list.files(spec_path) |>
  str_subset(".csv") |>
  str_subset("spec_")

for(i in 1:length(spec_files)) {
  path <- file.path(spec_path, spec_files[i])
  plate <- str_extract(spec_files[i], "^[^_]*_([^_]*)")
  new_plate <- paste0(plate, "_ONC")
  spec <- read_csv(path) |>
    mutate(Plate = new_plate)
  new_path <- file.path(spec_path, str_replace(spec_files[i], "^[^_]*_([^_]*)", new_plate))
  write_csv(spec, new_path)
}
