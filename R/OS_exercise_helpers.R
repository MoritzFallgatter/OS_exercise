load_coordinates <- function(path) {
  
  coordinates <- read_delim(path) |>
    rename(
      longitude = avg_longitude,
      latitude = avg_latitude
    )
  
  return(coordinates)
}


assign_biomes <- function(coordinates, scheme = 3) {
  
  result <- biomes::biomes_classify(
    x = coordinates,
    scheme = scheme,
    lon = "longitude",
    lat = "latitude",
    value = "name",
    append = TRUE
  ) |>
    dplyr::mutate(
      biome = substr(Biome_Inventory_layer_03_name, 1, 1)
    )
  
  return(result)
}

access_glaciation <- function(coordinates, lgm_extent) {
  
  sf::sf_use_s2(FALSE)
  
  summits <- st_as_sf(
    coordinates,
    coords = c("longitude", "latitude"),
    crs = 4326
  )
  
  coordinates$LGM_glaciated <- as.integer(
    lengths(st_intersects(summits, lgm_extent)) > 0
  )
  
  sf::sf_use_s2(TRUE)
  
  return(coordinates)
}


fit_glaciation_model <- function(coordinates_biomes, glaciated_yes_no) {
  
  model_data <- data.frame(
    biome = coordinates_biomes$biome,
    LGM_glaciated = glaciated_yes_no$LGM_glaciated
  )
  
  glm(
    LGM_glaciated ~ biome,
    data = model_data,
    family = binomial
  )
}

summarize_model <- function(model) {
  
  test <- drop1(model, test = "Chisq")
  
  results <- list(
    n = nobs(model),
    AIC = AIC(model),
    null_deviance = model$null.deviance,
    residual_deviance = deviance(model),
    p_value = test["biome", "Pr(>Chi)"],
    coefficients = summary(model)$coefficients
  )
  
  return(results)
}

plot_biomes_glaciation <- function(coordinates_biomes, lgm_extent) {
  
  world <- rnaturalearth::ne_countries(
    scale = "medium",
    returnclass = "sf"
  )
  
  summits <- sf::st_as_sf(
    coordinates_biomes,
    coords = c("longitude", "latitude"),
    crs = 4326
  )
  
  ggplot2::ggplot() +
    
    # World map
    ggplot2::geom_sf(
      data = world,
      fill = "grey90",
      color = "white",
      linewidth = 0.1
    ) +
    
    # LGM glaciated extent
    ggplot2::geom_sf(
      data = lgm_extent,
      fill = "lightblue",
      color = NA,
      alpha = 0.7
    ) +
    
    # GLORIA regions coloured by biome
    ggplot2::geom_sf(
      data = summits,
      ggplot2::aes(color = biome),
      size = 2.5
    ) +
    
    ggplot2::scale_color_manual(
      values = c(
        "A" = "#D73027",
        "B" = "#FDAE61",
        "C" = "#1A9850",
        "D" = "#4575B4",
        "E" = "#984EA3"
      ),
      labels = c(
        "A" = "Tropical",
        "B" = "Arid",
        "C" = "Temperate",
        "D" = "Cold",
        "E" = "Polar"
      ),
      name = "Climate zone"
    ) +
    
    ggplot2::labs(
      title = "GLORIA regions across global climatic zones",
      subtitle = "Köppen–Geiger classification and Last Glacial Maximum extent",
      caption = "Climate: Beck et al. (2018) | LGM extent: GLACIMONTIS"
    ) +
    
    ggplot2::theme_minimal() +
    
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(
        color = "grey85",
        linewidth = 0.2
      ),
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold")
    )
}


plot_glaciation_model <- function(model, results_model) {
  
  # Extract model data
  data <- model$model
  
  # Calculate observed proportions
  results <- data |>
    dplyr::group_by(biome) |>
    dplyr::summarise(
      n = dplyr::n(),
      observed = mean(LGM_glaciated),
      .groups = "drop"
    )
  
  # Predicted probabilities
  results$predicted <- predict(
    model,
    newdata = results,
    type = "response"
  )
  
  # Extract model coefficients
  coefficients <- as.data.frame(
    results_model$coefficients
  )
  
  coefficients$term <- rownames(coefficients)
  
  # Match coefficients to biome groups
  results <- results |>
    dplyr::mutate(
      term = paste0("biome", biome)
    ) |>
    dplyr::left_join(
      coefficients |>
        dplyr::select(
          term,
          Estimate,
          p_value = `Pr(>|z|)`
        ),
      by = "term"
    ) |>
    dplyr::mutate(
      label = dplyr::if_else(
        is.na(Estimate),
        paste0("Reference\nn = ", n),
        paste0(
          "Est. = ", round(Estimate, 2),
          "\np = ", format.pval(p_value, digits = 2),
          "\nn = ", n
        )
      )
    )
  
  # Biome colours
  biome_colors <- c(
    "A" = "#D73027",
    "B" = "#FDAE61",
    "C" = "#1A9850",
    "D" = "#4575B4",
    "E" = "#984EA3"
  )
  
  biome_labels <- c(
    "A" = "Tropical",
    "B" = "Arid",
    "C" = "Temperate",
    "D" = "Cold",
    "E" = "Polar"
  )
  
  # Plot
  ggplot2::ggplot(
    results,
    ggplot2::aes(x = biome, y = observed, fill = biome)
  ) +
    
    ggplot2::geom_col(
      alpha = 0.8,
      width = 0.7
    ) +
    
    # Predicted probabilities
    ggplot2::geom_point(
      ggplot2::aes(y = predicted),
      color = "black",
      size = 3
    ) +
    
    # Model estimates and p-values
    ggplot2::geom_text(
      ggplot2::aes(
        y = pmax(observed, predicted) + 0.05,
        label = label
      ),
      size = 3.2,
      lineheight = 1.1
    ) +
    
    ggplot2::scale_fill_manual(
      values = biome_colors
    ) +
    
    ggplot2::scale_x_discrete(
      labels = biome_labels
    ) +
    
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(),
      breaks = seq(0, 1, 0.2),
      limits = c(0, 1.25),
      oob = scales::oob_keep
    ) +
    
    ggplot2::labs(
      title = "LGM glaciation across climatic biomes",
      subtitle = paste0(
        "Logistic regression | Overall p = ",
        signif(results_model$p_value, 3),
        " | n = ",
        results_model$n
      ),
      x = "Climate zone",
      y = "Proportion of glaciated GLORIA regions",
      caption = paste0(
        "Bars = observed proportions | Black points = predicted probabilities\n",
        "Estimates are log-odds differences relative to Tropical (reference)"
      )
    ) +
    
    ggplot2::theme_bw() +
    
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(size = 11),
      plot.margin = ggplot2::margin(15, 15, 15, 15)
    )
}
  