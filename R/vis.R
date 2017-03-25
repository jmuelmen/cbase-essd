#' Visualization of geographical distribution of cloud bases
#' 
#' @param df data.frame.  Result of \code{bases.cbase()}
#' @return NULL
#' @export
vis.geo <- function(df) {
    df %>%
        plotutils::discretize(lon, 180) %>%
        plotutils::discretize(lat, 90) %>%
        group_by(lon, lat, feature.qa.lowest.cloud, feature.above.surface) %>%
        summarize(cloud.base.altitude = median(cloud.base.altitude),
                  cloud.base.height = median(cloud.base.altitude - surface.elevation))  -> tmp

    tmp %>%
        filter(feature.above.surface %in% c("cloud", "clear air"),
               feature.qa.lowest.cloud == "high") %>%
        ggplot2::ggplot(ggplot2::aes(x = lon, y = lat, fill = cloud.base.height)) +
        ggplot2::geom_raster() +
        ggplot2::scale_fill_distiller("Base (km AGL)", palette = "Spectral",
                                      guide = ggplot2::guide_colorbar(## fill = ggplot2::guide_legend(##
                                          ## keywidth = 2, keyheight = 4,
                                          barwidth = 2, barheight = 16,
                                          title.position = "right",
                                          title.theme = ggplot2::element_text(angle = 270, vjust = 0.5, hjust = 0.5, size = 24))) +
        plotutils::geom_world_polygon() +
        plotutils::scale_x_geo(facet = TRUE) +
        plotutils::scale_y_geo() +
        ggplot2::facet_grid(feature.qa.lowest.cloud ~ feature.above.surface) +
        ggplot2::theme_bw(24)

    tmp %>%
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

    NULL
}
