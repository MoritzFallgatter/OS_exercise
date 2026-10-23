# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c("tibble", "tidyverse", "here", "biomes", "sf", "quarto") # Packages that your targets need for their tasks.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  tar_target(
    name = coordinates_path,
    command = here("data/GLORIA_mid_coordinates.csv"),
    format = "file"
  ),
  
  tar_target(
    name = lgm_path,
    command = here("data/LGM_glaciated_extent_combined.shp"),
    format = "file"
  ),
  
  tar_target(
    name = coordinates,
    command = load_coordinates(coordinates_path)
  ),
  
  tar_target(
    name = lgm_extent,
    command = st_read(lgm_path)
  ),
  
  tar_target(
    name = coordinates_biomes,
    command = assign_biomes(coordinates, scheme = 3)
  ),
  
  tar_target(
    name = glaciated_yes_no,
    command = access_glaciation(coordinates, lgm_extent)
  ),
  
  tar_target(
    name = biomes_glaciation_model,
    command = fit_glaciation_model(
      coordinates_biomes,
      glaciated_yes_no
    )
  ),
  
  tar_target(
    name = results_model,
    command = summarize_model(biomes_glaciation_model)
  ),

  tar_target(
    name = map_biomes_glaciation,
    command = plot_biomes_glaciation(
      coordinates_biomes,
      lgm_extent
    )
  ),
  
  tar_target(
    name = barplot,
    command = plot_glaciation_model(
      model = biomes_glaciation_model,
      results_model = results_model
    )
  ),
  
  tar_quarto(name = manuscript, path = "biomes_glaciation_quarto.qmd")
)


