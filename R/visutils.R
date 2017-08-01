#' Visualization of geographical distribution of cloud bases
#' 
#' @param df data.frame.  Result of \code{bases.cbase()}
#' @return NULL
#' @export
vis.geo <- function(df) {
    df %>%
        ## dplyr::mutate(lon = lon.caliop, lat = lat.caliop) %>%
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

#' Visualization of geographical distribution of cloud-base
#' uncertainties (quantiles)
#' 
#' @param df data.frame.  Result of \code{bases.cbase()}
#' @return NULL
#' @export
vis.quantiles <- function(df, base_size) {
    df %>%
        plotutils::discretize(lon, seq(-180, 180, 2)) %>%
        plotutils::discretize(lat, seq(-90, 90, 2)) %>%
        plyr::ddply(~ lon + lat + daynight, function(df) {
            quantile(df$pred.rmse, c(0.1, 0.25, 0.5, 0.75, 0.9))
        }) %>%
        tidyr::gather(quantile, rmse, dplyr::matches("%")) %>%
        ggplot2::ggplot(ggplot2::aes(x = lon, y = lat, fill = rmse)) +
        ggplot2::geom_raster() +
        plotutils::geom_world_polygon() +
        plotutils::scale_x_geo(facet = TRUE) +
        plotutils::scale_y_geo() +
        ggplot2::scale_fill_distiller(palette = "Spectral") +
        ggplot2::facet_grid(quantile ~ daynight) +
        ggplot2::theme_bw(base_size) + ggplot2::theme(legend.position = "bottom")
    
}

#' Visualization of geographical distribution of cloud-base
#' uncertainties (CDF)
#' 
#' @param df data.frame.  Result of \code{bases.cbase()}
#' @return NULL
#' @export
vis.cdf <- function(df, base_size) {
    doParallel::registerDoParallel(4)
    df %>%
        plotutils::discretize(lon, seq(-180, 180, 2)) %>%
        plotutils::discretize(lat, seq(-90, 90, 2)) %>%
        plyr::ddply(~ lon + lat + daynight, function(df) {
            cdf <- ecdf(df$pred.rmse)
            data.frame(rmse = seq(300,480, by = 60)) %>%
                dplyr::mutate(cdf = cdf(rmse))
        }, .parallel = TRUE) -> tmp

    tmp %>%
        plotutils::discretize(cdf, c(0,0.05,0.1,0.15,0.2,0.4,0.6,0.8,1)) %>%
        dplyr::mutate(cdf = factor(cdf)) %>%
        ggplot2::ggplot(ggplot2::aes(x = lon, y = lat, fill = cdf)) +
        ggplot2::geom_raster() +
        plotutils::geom_world_polygon() +
        plotutils::scale_x_geo(facet = TRUE) +
        plotutils::scale_y_geo() +
        ggplot2::scale_fill_brewer(palette = "Spectral", direction = -1) +
        ggplot2::facet_grid(daynight ~ rmse) +
        ggplot2::theme_bw(base_size) + ggplot2::theme(legend.position = "bottom")
}

#' Visualization of geographical distribution of cloud-base
#' uncertainties (CDF)
#' 
#' @param df data.frame.  Result of \code{bases.cbase()}
#' @return NULL
#' @export
vis.n <- function(df, base_size) {
    doParallel::registerDoParallel(4)
    df %>%
        plotutils::discretize(lon, seq(-180, 180, 2)) %>%
        plotutils::discretize(lat, seq(-90, 90, 2)) %>%
        dplyr::group_by(lon, lat, daynight) %>%
        dplyr::summarize(n = n()) %>%
        ggplot2::ggplot(ggplot2::aes(x = lon, y = lat, fill = n)) +
        ggplot2::geom_raster() +
        plotutils::geom_world_polygon() +
        plotutils::scale_x_geo(facet = TRUE) +
        plotutils::scale_y_geo() +
        ggplot2::scale_fill_distiller(palette = "Spectral", direction = -1) +
        ggplot2::facet_grid(. ~ daynight) +
        ggplot2::theme_bw(18) 

    df %>%
        plotutils::discretize(lon, seq(-180, 180, 5)) %>%
        plotutils::discretize(lat, seq(-90, 90, 5)) %>%
        plyr::ddply(~ lon + lat + daynight, function(df) {
            cdf <- ecdf(df$pred.rmse)
            data.frame(rmse = seq(320,480, by = 40)) %>%
                dplyr::mutate(cdf = cdf(rmse),
                              ncdf = nrow(df) * cdf)
        }, .parallel = TRUE) -> tmp

    tmp %>%
        plotutils::discretize(cdf, c(0,0.05,0.1,0.15,0.2,0.4,0.6,0.8,1)) %>%
        plotutils::discretize(ncdf, c(0,10,50,100,200,500,1000)) %>%
        dplyr::mutate(cdf = factor(cdf),
                      ncdf = factor(ncdf)) %>%
        ggplot2::ggplot(ggplot2::aes(x = lon, y = lat, fill = ncdf)) +
        ggplot2::geom_raster() +
        plotutils::geom_world_polygon() +
        plotutils::scale_x_geo(facet = TRUE) +
        plotutils::scale_y_geo() +
        ggplot2::coord_fixed(1) +
        ggplot2::scale_fill_brewer(palette = "Reds", direction = +1) +
        ggplot2::facet_grid(rmse ~ daynight) +
        ggplot2::theme_bw(base_size) + ggplot2::theme(legend.position = "bottom")
}
