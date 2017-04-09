#' Add summary statistics (linear regression, correlation, RMS error)
#' to a data frame or grouped data frame
#' 
#'
#' @param df
#' @return 
#'
#' @export
#'
#' @examples
#' 
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
    df %>%
        ## dplyr::filter(df, caliop < 2) %>%
        dplyr::summarize(n = n(),
                         cor = cor(ceilo, caliop, use = "na"),
                         cor.loc = cor(ceilo, caliop.local, use = "na"),
                         rmse = sqrt(mean((caliop * 1000 - ceilo) ^ 2, na.rm = TRUE)),
                         bias = mean(caliop * 1000 - ceilo, na.rm = TRUE),
                         slope = regress(ceilo, caliop, "slope"),
                         icpt = regress(ceilo, caliop, "icpt"),
                         rmse.fit = regress(ceilo, caliop, "rmse"))
}

#' Add summary statistics (linear regression, correlation, RMS error)
#' to a data frame or grouped data frame
#' 
#'
#' @param df
#' @return 
#'
#' @export
#'
#' @examples
#' 
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

#' Make a ggplot the way we like them, based on a grouped data frame
#'
#' @param df
#' @return 
#'
#' @export
#'
#' @examples
#' 
regression_plot <- function(df, title, xlab = "50 km running mean CALIOP base (km)") {
    stats <- statistify(df)
    stats.n <- summarize(df, n = n())
    print(stats.n)
    gg <- ggplot2::ggplot(df, ggplot2::aes(caliop, ceilo)) +
        (if (max(stats.n$n) < 1e4)
             ggplot2::geom_point(pch = ".")
         else
             ggplot2::stat_bin2d()) +
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
        ggplot2::scale_fill_distiller(palette = "GnBu") +
        ggplot2::theme_bw()
    ## print(gg)
    return(list(stats = stats, ggplot = gg))
}
