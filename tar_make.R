library(targets)

tar_make()

tar_load(coordinates)
tar_load(coordinates_biomes)
tar_load(glaciated_yes_no)
tar_load(model)
tar_load(map_biomes_glaciation)


tar_load(results_model)


summary(model)
windows(
)
plot(map_biomes_glaciation)

tar_visnetwork()
