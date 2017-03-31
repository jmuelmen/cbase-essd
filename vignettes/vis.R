## ---- vis-cbase ---------------------
df <- readRDS("~/r-packages/cloud-bases-2008.rds") %>%
    select(lon, lat, feature.qa.lowest.cloud, feature.above.surface,
           cloud.base.altitude, surface.elevation)

df <- df %>%
    plotutils::discretize(lon, 180) %>%
    plotutils::discretize(lat, 90) %>%
    group_by(lon, lat, feature.qa.lowest.cloud, feature.above.surface) %>%
    summarize(cloud.base.altitude = median(cloud.base.altitude),
              cloud.base.height = median(cloud.base.altitude - surface.elevation))

gc()

## ---- vis-cbase2 ---------------------
df %>%
    ## filter(feature.above.surface %in% c("cloud", "clear air"),
    ##        feature.qa.lowest.cloud == "high") %>%
    ggplot2::ggplot(ggplot2::aes(x = lon, y = lat, fill = cloud.base.height)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_distiller("Base (km AGL)", palette = "Spectral") +
    plotutils::geom_world_polygon() +
    plotutils::scale_x_geo(facet = TRUE) +
    plotutils::scale_y_geo() +
    ggplot2::facet_grid(feature.qa.lowest.cloud ~ feature.above.surface) +
    ggplot2::theme_bw()
