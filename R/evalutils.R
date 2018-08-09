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
regression_plot <- function(df, title, xlab = "CALIOP cloud base height (km AGL)",
                            base_size = 11) {
    stats <- statistify(df)
    stats.n <- summarize(df, n = n())
    print(stats.n)
    
    ## df.prof <- df %>%
    ##     plotutils::discretize(caliop, 30) %>%
    ##     dplyr::group_by_(c(dplyr::groups(df), expression(caliop))) %>%
    ##     dplyr::summarize(mean = mean(ceilo),
    ##                      sd = sd(ceilo)) 
    ## str(df.prof)

    ## do not facet on dummy variables
    gr <- gsub("^dummy.*", ".", as.character(dplyr::groups(df)))
    ## gr <- replace(gr, as.character(gr) == "dummy", as.name("."))
    ## gr <- gr[!grepl("^dummy.*", as.character(gr))]
    gr <- as.formula(sprintf("%s ~ %s", gr[1], gr[2]))
    
    gg <- ggplot2::ggplot(df, ggplot2::aes(caliop, ceilo)) +
        (if (max(stats.n$n) < 1e3)
             ggplot2::geom_point(pch = ".")
         else
             ggplot2::stat_bin2d(geom = "raster", binwidth = c(0.1, 100))) +
        ggplot2::stat_density2d(geom = "density2d", col = "black") +
        ggplot2::stat_smooth(method = "lm", col = "red", fill = "red", formula = y ~ x) +
        ggplot2::stat_smooth(method = "auto", col = "blue", fill = "blue", formula = y ~ x) +
        ggplot2::geom_abline(intercept = 0, slope = 1000, lty = "dashed", col = "lightgrey") +
        ## ggplot2::coord_cartesian(ylim = c(-500,5500)) +
        ggplot2::coord_fixed(1e-3, xlim = c(0,3), ylim = c(0,3000), expand = FALSE) +
        (if (any(grepl("[A-Za-z0-9]", gr))) ggplot2::facet_grid(gr)) +
        ggplot2::labs(title = title, x = xlab, y = "Ceilometer base (m AGL)") +
        (if (0) {
            ggplot2::geom_text(data = stats,
                               ggplot2::aes(x = 3, y = 0,
                                            label = gsub("NA", "'NA'",
                                                         sprintf("$\\begin{aligned} n = %.0f\\\\r = %.2f\\\\ y = %4.0f x + %4.0f \\quad \\text{fit RMSE} = %4.0f\\ \\text{m}\\\\ \\text{RMSE} = %4.0f\\ \\text{m}\\quad \\text{bias} = %4.0f\\ \\text{m}\\end{aligned}$",
                                                                 ## sprintf(" n &= %.0f\\\\r &= %.2f\\\\ y = %4.0f x + %4.0f \\quad \\text{fit RMSE} = %4.0f\\ \\text{m}\\\\ \\text{RMSE} = %4.0f\\ \\text{m}\\quad \\text{bias} = %4.0f\\ \\text{m}",
                                                                 n, cor, slope, icpt, rmse.fit, rmse, bias))),
                               size = 4, vjust = 0, hjust = 1, parse = FALSE)
        }) +
        ggplot2::scale_fill_distiller("Count",
                                      palette = "GnBu",
                                      guide = ggplot2::guide_colorbar(direction = "horizontal",
                                                                      nrow = 1,
                                                                      keywidth = 2,
                                                                      barwidth = 10,
                                                                      title.vjust = 0.75,
                                                                      label.hjust = 0.5,
                                                                      label.position = "bottom")
                                      ) +
        ## ggplot2::scale_fill_distiller(palette = "GnBu") + ## , trans = "log10") +
        ## ggplot2::geom_pointrange(ggplot2::aes(x = caliop, y = mean,
        ##                                      ymin = mean - sd, ymax = mean + sd),
        ##                          df.prof) +
        ggplot2::theme_bw(base_size) + ggplot2::theme(legend.position = "bottom")
    ## print(gg)
    return(list(stats = stats, ggplot = gg))
}

#' @export
sanitize.numbers <- function (str, type, math.style.negative = FALSE, math.style.exponents = FALSE) {
    if (type == "latex") {
        result <- str
        if (math.style.negative) {
            for (i in 1:length(str)) {
                result[i] <- gsub("-", "$-$", result[i], fixed = TRUE)
            }
        }
        if (is.logical(math.style.exponents) && !math.style.exponents) {
        }
        else if (is.logical(math.style.exponents) && math.style.exponents || 
                 math.style.exponents == "$$") {
            for (i in 1:length(str)) {
                result[i] <- gsub("^\\$?(-?)\\$?([0-9.]+)[eE]\\$?(-?)\\+?\\$?0*(\\d+)$", 
                                  "$\\1\\2 \\\\times 10^{\\3\\4}$", result[i])
                result[i] <- gsub("^\\$?(-?)\\$?1 \\\\times 10", 
                                  "$\\110", result[i])
            }
        }
        else if (math.style.exponents == "ensuremath") {
            for (i in 1:length(str)) {
                result[i] <- gsub("([0-9.]+)[eE](-?)\\+?0*(\\d+)", 
                                  "\\\\ensuremath{\\1 \\\\times 10^{\\2\\3}}", 
                                  result[i])
            }
        }
        else if (math.style.exponents == "UTF8" || math.style.exponents == 
                 "UTF-8") {
            for (i in 1:length(str)) {
                if (all(grepl("^\\$?(-?)\\$?([0-9.]+)[eE]\\$?(-?)\\+?\\$?0*(\\d+)$", 
                              result[i]))) {
                    temp <- strsplit(result[i], "eE", result[i])
                    result[i] <- paste0(temp[1], "×10", chartr("-1234567890", 
                                                               "⁻¹²³⁴⁵⁴6⁴7⁴8⁴9⁰", temp[2]))
                }
            }
        }
        return(result)
    }
    else {
        return(str)
    }
}

#' Make an xtable from statistics of a grouped data frame
#'
#' @param df.stats List or data.frame.  Statistics summary from
#'     \code{statistify()} or list output from \code{regression_plot}
#'     (in which case the \code{stats} element is used)
#' @return An \code{xtable}
#'
#' @export
#'
#' @examples
#' 
regression_table <- function(df.stats) {
    if (class(df.stats) == "list")
        df.stats <- df.stats$stats

    ## add hlines between levels of the first grouping variable
    group.diffs <- df.stats %>%
        select(group1 = dplyr::matches(as.character(groups(df.stats)[[1]]))) %>%
        as.data.frame() %>% ## needed for ordered factor comparison
        mutate(group.diff = group1 != group1[2:(n() + 1)])
    hlines <- which(group.diffs$group.diff)
    
    df.stats %>%
        ungroup() %>%
        select(-starts_with("dummy")) %>%
        mutate("$n$" = n,
               "$r$" = cor,
               ## "$r_\\text{loc}$" = cor.loc,
               "RMSE (m)" = rmse,
               "bias (m)" = bias,
               fit = sanitize.numbers(sprintf("$\\hat{z} = %#.3G z %s %#.3G$ m",
                                              slope * 1e-3,
                                              ifelse(icpt < 0, "-", "+"),
                                              abs(icpt)),
                                      type = "latex",
                                      math.style.exponents = "ensuremath")## ,
               ## "RMSE(fit)" = rmse.fit
               ) %>%
        select(-(n:rmse.fit)) %>%
        xtable::xtable(.,
                       auto = TRUE,
                       digits = 3,
                       display = gsub("f", "G", xtable::xdisplay(.))) %>%
        print(., sanitize.colnames.function = identity,
              sanitize.text.function = identity,
              floating = FALSE,
              math.style.negative = TRUE,
              math.style.exponents = TRUE,
              include.rownames = FALSE,
              format.args = list(flag = "#"),
              hline.after = c(rep(-1, 2), 0, hlines, rep(nrow(.), 2)))
}
