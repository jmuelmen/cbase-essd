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
        summarize(cloud.base.altitude = median(cloud.base.altitude)) %>%
        ggplot2::ggplot(ggplot2::aes(x = lon, y = lat, fill = cloud.base.altitude)) +
        ggplot2::geom_raster() +
        plotutils::geom_world_polygon() +
        plotutils::scale_x_geo() +
        plotutils::scale_y_geo() +
        ggplot2::facet_grid(feature.qa.lowest.cloud ~ feature.above.surface) +
        ggplot2::theme_bw(24)

    NULL
}
