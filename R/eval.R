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
        filter(ceilo < 5000, base < 3000) %>%
        dplyr::filter(region == "K") -> test
        
    df %>%
        dplyr::mutate(ceilo = hgts.1 + elevation.m,
                      caliop = cloud.base.altitude,
                      caliop.local = caliop) %>%
        dplyr::mutate(region = factor(substr(station.icao, 1,1))) %>%
        filter(ceilo < 5000, hgts.1 < 3000, caliop < 5, lays == 1) %>%
        dplyr::filter(region == "K") %>%
        ## dplyr::mutate(## dist = cut(dist, quantile(dist)),
        ##     dummy = "",
        ##     dummy2 = "") %>%
        dplyr::group_by(covs.1, feature.above.surface) %>%
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

#' add summary statistics (linear regression, correlation, RMS error) to a data frame or grouped data frame
#' @export
statistify <- function(df) {
    regress <- function(ceilo, caliop, type) {
        res <- try({
            sum <- summary(lm(ceilo ~ caliop))
            switch(type,
                   slope = sum$coefficients[2,1],
                   icpt = sum$coefficients[1,1],
                   rmse = sqrt(mean(sum$residuals^2)))
        })
        if (class(res) == "try-error")
            NA
        else res
    }
    dplyr::summarize(df,
              n = n(),
              cor = cor(ceilo, caliop, use = "na"),
              cor.loc = cor(ceilo, caliop.local, use = "na"),
              rmse = sqrt(mean((caliop * 1000 - ceilo) ^ 2, na.rm = TRUE)),
              bias = mean(caliop * 1000 - ceilo, na.rm = TRUE),
              slope = regress(ceilo, caliop, "slope"),
              icpt = regress(ceilo, caliop, "icpt"),
              rmse.fit = regress(ceilo, caliop, "rmse"))
}

#' make a ggplot the way we like them, based on a grouped data frame
#' @export
statistify.df <- function(df) {
    stats <- statistify(df)
    base.df <- dplyr::select(stats, -(cor:rmse))
    rbind(cbind(base.df, statistic = stats$n, statistic.name = "n"),
          cbind(base.df, statistic = stats$cor, statistic.name = "Correlation"),
          cbind(base.df, statistic = stats$cor.loc, statistic.name = "Local correlation"),
          cbind(base.df, statistic = stats$rmse, statistic.name = "RMSE"),
          cbind(base.df, statistic = stats$bias, statistic.name = "Bias"),
          cbind(base.df, statistic = stats$slope, statistic.name = "Slope"),
          cbind(base.df, statistic = stats$icpt, statistic.name = "Intercept"),
          cbind(base.df, statistic = stats$rmse.fit, statistic.name = "Fit RMSE"))
}

#' make a ggplot the way we like them, based on a grouped data frame
#' @export
regression_plot <- function(df, title, xlab = "CALIOP base (km)") {
    stats <- statistify(df)
    gg <- ggplot2::ggplot(df, ggplot2::aes(caliop, ceilo)) +
        ggplot2::geom_point(pch = ".") +
        ggplot2::stat_density2d(geom = "density2d") +
        ggplot2::stat_smooth(method = "lm", col = "red", fill = "red", formula = y ~ x) +
        ggplot2::stat_smooth(method = "loess", col = "blue", fill = "blue", formula = y ~ x) +
        ggplot2::geom_abline(intercept = 0, slope = 1000, lty = "dashed", col = "lightgrey") +
        ggplot2::coord_cartesian(ylim = c(-500,5500)) +
        ggplot2::facet_grid(dplyr::groups(df)) +
        ggplot2::labs(title = title, x = xlab, y = "Ceilometer base (m)") +
        ggplot2::geom_text(data = stats, ggplot2::aes(x = 5, y = 0,
                                    label = gsub("NA", "'NA'", sprintf("atop(atop(italic(n) == %.0f, italic(r)[loc] == %.2f~ italic(r)[fit] == %.2f), atop(italic(y) == %4.0f* x + %4.0f ~ fit ~ RMSE == %4.0f~m, RMSE == %4.0f~ m~bias == %4.0f~m))",
                                                    n, cor.loc, cor, slope, icpt, rmse.fit, rmse, bias))),
                  size = 4, vjust = 0, hjust = 1, parse = TRUE) +
        ggplot2::theme_bw(base_size = 20)
    ## print(gg)
    return(list(stats = stats, ggplot = gg))
}
