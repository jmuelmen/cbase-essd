#' Evaluation function (retrieval efficiency)
#' 
#' @param path Character.  Apply algorithm to all HDF files in this path
#' @return A data frame containing cloud bases
#' @export
eval.efficiency <- function() {
    
}

#' Evaluation function (METAR comparison)
#' 
#' @param df Data frame.  Combination of A-Train and METAR cloud base heights
#' @return A data frame containing cloud bases
#' @export
eval.metar <- function(df) {
    df %>%
        dplyr::mutate(ceilo = hgts.1 + elevation.m,
                      caliop = cloud.base.altitude,
                      caliop.local = caliop) %>%
        dplyr::mutate(region = factor(substr(station.icao, 1,1))) %>%
        filter(ceilo < 5000, hgts.1 < 3000) %>%
        dplyr::filter(region == "K") -> test
        
    df %>%
        dplyr::mutate(ceilo = hgts.1 + elevation.m,
                      caliop = cloud.base.altitude,
                      caliop.local = caliop) %>%
        dplyr::mutate(thickness = cloud.top.altitude - cloud.base.altitude) %>%
        dplyr::mutate(region = factor(substr(station.icao, 1,1))) %>%
        dplyr::filter(ceilo < 5000, hgts.1 < 3000, caliop < 5, lays == 1) %>%
        dplyr::filter(feature.qa.lowest.cloud == "high") %>%
        dplyr::filter(region == "K") %>%
        ## plotutils::discretize(dist, quantile(dist, seq(0, 1, 0.2)), as_factor = TRUE) %>%
        dplyr::filter(dist < 50) %>%
        plotutils::discretize(thickness, quantile(thickness, seq(0, 1, 0.2)), as_factor = TRUE) %>%
        ##     dummy = "",
        ##     dummy2 = "") %>%
        dplyr::group_by(covs.1, thickness) %>%
        regression_plot(title = "test")
        
        dplyr::mutate(lon = lon.caliop, lat = lat.caliop) %>%
        plotutils::discretize(lon, 180) %>%
        plotutils::discretize(lat, 90) %>%
        group_by(lon, lat, feature.qa.lowest.cloud, feature.above.surface) %>%
        summarize(cloud.base.altitude = median(cloud.base.altitude),
                  cloud.base.height = median(cloud.base.altitude - surface.elevation))  -> tmp

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
        ggplot2::theme_bw(24)

    
}

