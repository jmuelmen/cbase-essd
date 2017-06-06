## ---- eval-setup ---------------------
df <- readRDS("~/r-packages/cbm-all.rds") %>%
    factor.vfm() %>%
    dplyr::mutate(region = factor(substr(station.icao, 1,1))) %>%
    ## new way: AGL height
    dplyr::mutate(ceilo = hgts.1,
                  caliop = cloud.base.altitude - elevation.m * 1e-3,
                  caliop.local = caliop) %>%
    ## ## old way: MSL height
    ## dplyr::mutate(ceilo = hgts.1 + elevation.m,
    ##               caliop = cloud.base.altitude,
    ##               caliop.local = caliop) %>%
    dplyr::mutate(thickness = cloud.top.altitude - cloud.base.altitude) %>%
    mutate(dummy = "",
           dummy2 = "") %>%
    dplyr::filter(region == "K") %>%
    ## new way: AGL
    dplyr::filter(ceilo < 5000,
                  hgts.1 < 3000,
                  caliop > 0,
                  caliop + elevation.m * 1e-3 < 3)
    ## ## old way: MSL
    ## dplyr::filter(ceilo < 5000, hgts.1 < 3000, caliop < 3)
gc()

## ---- eval-qual ---------------------
res <- df %>%
    ## filter(lays == 1) %>%
    dplyr::group_by(dummy, feature.qa.lowest.cloud) %>%
    regression_plot(title = NULL, base_size = 14)
gc()
res$ggplot

## ---- eval-qual-tbl ---------------------
regression_table(res)

## ---- eval-dist ---------------------
res <- df %>%
    ## filter(lays == 1) %>%
    dplyr::filter(feature.qa.lowest.cloud == "high") %>%
    plotutils::discretize(dist, quantile(dist, seq(0, 1, 0.2)), as_factor = TRUE) %>%
    dplyr::group_by(dummy, dist) %>%
    regression_plot(title = "dist")
res$ggplot

## ---- eval-dist-tbl ---------------------
regression_table(res)

## ---- eval-deltat ---------------------
res <- df %>%
    ## filter(lays == 1) %>%
    dplyr::filter(feature.qa.lowest.cloud == "high") %>%
    dplyr::mutate(delta.t = as.numeric(datetime) - time) %>%
    plotutils::discretize(delta.t, quantile(delta.t, seq(0, 1, 0.2)), as_factor = TRUE) %>%
    dplyr::group_by(dummy, delta.t) %>%
    regression_plot(title = "$\\Delta t$")
res$ggplot

## ---- eval-deltat-tbl ---------------------
regression_table(res)

## ---- eval-mult ---------------------
res <- df %>%
    ## filter(lays == 1) %>%
    dplyr::filter(feature.qa.lowest.cloud == "high") %>%
    plotutils::discretize(resolution.out, quantile(resolution.out, seq(0, 1, 0.2)), as_factor = TRUE) %>%
    dplyr::group_by(dummy, resolution.out) %>%
    regression_plot(title = "$n_\\text{ret}$")
res$ggplot

## ---- eval-mult-tbl ---------------------
regression_table(res)

## ---- eval-thick ---------------------
res <- df %>%
    dplyr::filter(feature.qa.lowest.cloud == "high") %>%
    plotutils::discretize(thickness, quantile(thickness, seq(0, 1, 0.2)), as_factor = TRUE) %>%
    dplyr::group_by(dummy, thickness) %>%
    regression_plot(title = "thickness")
res$ggplot

## ---- eval-thick-tbl ---------------------
regression_table(res)

## ---- eval-phase ---------------------
res <- df %>%
    dplyr::filter(feature.qa.lowest.cloud == "high") %>%
    dplyr::group_by(dummy, phase.lowest.cloud) %>%
    regression_plot(title = "phase.lowest.cloud")
res$ggplot

## ---- eval-phase-tbl ---------------------
regression_table(res)

## ---- eval-feature ---------------------
res <- df %>%
    dplyr::filter(feature.qa.lowest.cloud == "high") %>%
    dplyr::group_by(dummy, feature.above.surface) %>%
    regression_plot(title = "feature.above.surface")
res$ggplot

## ---- eval-feature-tbl ---------------------
regression_table(res)

## ---- eval-averaging ---------------------
res <- df %>%
    dplyr::filter(feature.qa.lowest.cloud == "high") %>%
    dplyr::group_by(dummy, horizontal.averaging.lowest.cloud.min) %>%
    regression_plot(title = "horizontal averaging distance")
res$ggplot

## ---- eval-averaging-tbl ---------------------
regression_table(res)

## ---- eval-best-case-setup --------------------
df.best <- filter(df,
             dist < 40,
             feature.qa.lowest.cloud == "high",
             resolution.out > 400,
             thickness < 1,
             phase.lowest.cloud != "horizontally oriented ice",
             !(feature.above.surface %in% c("invalid", "no signal")),
             horizontal.averaging.lowest.cloud.min < "5 km")

## ---- eval-lays ---------------------
res <- df.best %>%
    dplyr::filter(feature.qa.lowest.cloud == "high") %>%
    dplyr::group_by(dummy, dummy2) %>%
    regression_plot(title = "lays")
res$ggplot

## ---- eval-lays-tbl ---------------------
regression_table(res)

## ---- eval-cov-thick ---------------------
res <- df %>%
    dplyr::filter(lays == 1) %>%
    dplyr::filter(feature.qa.lowest.cloud == "high") %>%
    plotutils::discretize(thickness, quantile(thickness, seq(0, 1, 0.2)), as_factor = TRUE) %>%
    dplyr::group_by(covs.1, thickness) %>%
    regression_plot(title = "covs.1:thickness")
res$ggplot

## ---- eval-cov-thick-tbl ---------------------
regression_table(res)

## ## ---- eval-min-qual-setup ---------------------
## df <- readRDS("~/r-packages/cbm-min_cbh.qual.rds") %>%
##     factor.vfm() %>%
##     dplyr::mutate(region = factor(substr(station.icao, 1,1))) %>%
##     dplyr::mutate(ceilo = hgts.1 + elevation.m,
##                   caliop = cloud.base.altitude,
##                   caliop.local = caliop) %>%
##     dplyr::mutate(thickness = cloud.top.altitude - cloud.base.altitude) %>%
##     mutate(dummy = "",
##            dummy2 = "") %>%
##     dplyr::filter(region == "K") %>%
##     dplyr::filter(dist < 50) %>%
##     dplyr::filter(ceilo < 5000, hgts.1 < 3000, caliop < 3)

## ## ---- eval-min-qual ---------------------
## res <- df %>%
##     ## filter(lays == 1) %>%
##     dplyr::group_by(## dummy, ##dummy2) %>%
##         lays,
##                     feature.qa.lowest.cloud) %>%
##     regression_plot(title = NULL)
## res$ggplot

## ## ---- eval-min-qual-tbl ---------------------
## regression_table(res)

## ## ---- eval-min-qual-cor ---------------------
## res <- df %>%
##     dplyr::mutate(caliop = caliop * 0.768 + 0.527) %>%
##     ## filter(lays == 1) %>%
##     dplyr::group_by(dummy, ##dummy2) %>%
##                     feature.qa.lowest.cloud) %>%
##     regression_plot(title = NULL)
## res$ggplot

## ## ---- eval-min-qual-cor-tbl ---------------------
## regression_table(res)

## ## ---- eval-quantiles-setup ---------------------
## df <- readRDS("~/r-packages/cbm-quantile_cbh.qual.rds") %>%
##     factor.vfm() %>%
##     dplyr::mutate(region = factor(substr(station.icao, 1,1))) %>%
##     dplyr::mutate(ceilo = hgts.1 + elevation.m,
##                   caliop = cloud.base.altitude,
##                   caliop.local = caliop) %>%
##     dplyr::mutate(thickness = cloud.top.altitude - cloud.base.altitude) %>%
##     mutate(dummy = "",
##            dummy2 = "") %>%
##     dplyr::filter(region == "K") %>%
##     dplyr::filter(dist < 50) %>%
##     dplyr::filter(ceilo < 5000, hgts.1 < 3000, caliop < 3)

## ## ---- eval-quantiles-qual ---------------------
## res <- df %>%
##     ## filter(lays == 1) %>%
##     dplyr::mutate(resolution.method = sprintf("\\verb+%s+", resolution.method)) %>%
##     dplyr::group_by(dummy, resolution.method) %>%
##     regression_plot(title = NULL)
## res$ggplot

## ## ---- eval-quantiles-qual-tbl ---------------------
## regression_table(res)

## ## ---- eval-min-qual-2007-setup ---------------------
## df <- readRDS("~/r-packages/cbm-min_cbh.qual-2007.rds") %>%
##     factor.vfm() %>%
##     dplyr::mutate(region = factor(substr(station.icao, 1,1))) %>%
##     dplyr::mutate(ceilo = hgts.1 + elevation.m,
##                   caliop = cloud.base.altitude,
##                   caliop.local = caliop) %>%
##     dplyr::mutate(thickness = cloud.top.altitude - cloud.base.altitude) %>%
##     mutate(dummy = "",
##            dummy2 = "") %>%
##     dplyr::filter(region == "K") %>%
##     dplyr::filter(dist < 50) %>%
##     dplyr::filter(ceilo < 5000, hgts.1 < 3000, caliop < 3)

## ## ---- eval-min-qual-2007 ---------------------
## res <- df %>%
##     ## filter(lays == 1) %>%
##     dplyr::group_by(dummy, ## dummy2) %>%
##         ## lays,
##                     feature.qa.lowest.cloud) %>%
##     regression_plot(title = NULL)
## res$ggplot

## ## ---- eval-min-qual-2007-tbl ---------------------
## regression_table(res)

## ## ---- eval-min-qual-2007-cor ---------------------
## res <- df %>%
##     dplyr::mutate(caliop = caliop * 0.768 + 0.527) %>%
##     ## filter(lays == 1) %>%
##     dplyr::group_by(dummy, ##dummy2) %>%
##                     feature.qa.lowest.cloud) %>%
##     regression_plot(title = NULL)
## res$ggplot

## ## ---- eval-min-qual-2007-cor-tbl ---------------------
## regression_table(res)

## ---- eval-2bgeoprof-setup --------------------
df.comp <- readRDS("~/r-packages/atrain-2bgeoprof-metar-comp-2008.rds")[,-c(10:11)]
df.2bgeoprof <- filter(df.comp, !is.na(base.1)) %>%
    ## AGL heights
    mutate(ceilo = base,
           caliop = (base.1 - elev) * 1e-3,
           caliop.local = caliop) %>%
    ## ## old: MSL heights
    ## mutate(ceilo = base + elev,
    ##        caliop = base.1 * 1e-3,
    ##        caliop.local = base.1 * 1e-3) %>%
    ## AGL heights
    dplyr::filter(ceilo < 5000,
                  base < 3000,
                  caliop > 0,
                  base.1 * 1e-3 < 3) %>%
    ## old: MSL heights
    ## filter(ceilo < 5000, base < 3000, caliop < 3) %>%
    mutate(region = factor(substr(station.icao, 1,1)),
           type = factor(type, levels = c("FEW", "SCT", "BKN", "OVC"), ordered = TRUE),
           flag.base = factor(flag.base, levels = 0:3, labels = c("None", "Radar", "Lidar", "Radar+Lidar")),
           station.dist.dec = cut(station.dist.km, breaks = c(0,20,40,70,100))) %>%
    mutate(dummy = "",
           dummy2 = "") %>%
    filter(!is.na(type), region == "K", flag.base %in% c("Radar", "Lidar"))

## ---- eval-2bgeoprof --------------------
res <- df.2bgeoprof %>%
    ## filter(lays == 1) %>%
    dplyr::group_by(dummy, ## dummy2) %>%
        ## lays,
                    flag.base) %>%
    regression_plot(title = NULL, xlab = "2B-GEOPROF-LIDAR cloud base height (km)", base_size = 14)
res$ggplot

## ---- eval-2bgeoprof-tbl --------------------
regression_table(res)

## ---- eval-tune-setup --------------------
val.self <- FALSE
if (val.self) {
    df.val <- df
} else {
    df.val <- readRDS("~/r-packages/cbm-all-2007.rds") %>%
        factor.vfm() %>%
        dplyr::mutate(region = factor(substr(station.icao, 1,1))) %>%
        ## AGL
        dplyr::mutate(ceilo = hgts.1,
                      caliop = cloud.base.altitude - elevation.m * 1e-3,
                      caliop.local = caliop) %>%
        ## ## old: MSL
        ## dplyr::mutate(ceilo = hgts.1 + elevation.m,
        ##               caliop = cloud.base.altitude,
        ##               caliop.local = caliop) %>%
        dplyr::mutate(thickness = cloud.top.altitude - cloud.base.altitude) %>%
        mutate(dummy = "",
               dummy2 = "") %>%
        dplyr::filter(region == "K") %>%
        ## ## old: MSL
        ## dplyr::filter(ceilo < 5000, hgts.1 < 3000, caliop < 3)
        ## new: AGL
        dplyr::filter(ceilo < 5000,
                      hgts.1 < 3000,
                      caliop > 0,
                      caliop + elevation.m * 1e-3 < 3)
}
gc()
val.svm <- TRUE
if (val.svm) {
    ## ddf <- tune.cbase.svm(df)
    library(e1071)
    ddf <- readRDS("~/r-packages/models.svm.rds")
} else {
    ddf <- tune.cbase.lm(df)
}
dddf <- correct.cbase.lm(df.val, ddf)
combo <- cbase.combine(dddf)
gc()

## ---- tune-test --------------------
test.cbase.lm(dddf) %>% mutate(ratio = rmse / pred.rmse) %>% ggplot(aes(x = ratio)) + geom_histogram()

## ---- combo-eval-pull --------------------
tikz_minus<- function(x) sub("^-", "$-$", format(x))
tikz_sanitize <- function(x) sanitize.numbers(x, "latex", TRUE, TRUE)

# change the default scales
scale_x_continuous <- function(..., labels=tikz_sanitize)
                        ggplot2::scale_x_continuous(..., labels=labels)

scale_y_continuous <- function(..., labels=tikz_sanitize)
    ggplot2::scale_y_continuous(..., labels=labels)

ggplot(combo, aes(x = ((pred.ceilo - ceilo) / pred.rmse))) +
    geom_histogram(aes(y = ..density..)) +
    labs(x = "$(z - \\hat{z}) / \\sigma$", y = "Density") +
    stat_function(fun = dnorm,
                  args = with(combo, list(mean = mean((ceilo - pred.ceilo) / pred.rmse),
                                          sd = sd((ceilo - pred.ceilo) / pred.rmse))),
                  col = "blue") +
    theme_bw()
## with(combo, mean((ceilo - pred.ceilo) / pred.rmse))
## with(combo, sd((ceilo - pred.ceilo) / pred.rmse))

## ---- combo-eval-rmse --------------------
tikz_minus<- function(x) sub("^-", "$-$", format(x))
tikz_sanitize <- function(x) sanitize.numbers(x, "latex", TRUE, TRUE)

# change the default scales
scale_x_continuous <- function(..., labels=tikz_sanitize)
                        ggplot2::scale_x_continuous(..., labels=labels)

scale_y_continuous <- function(..., labels=tikz_sanitize)
    ggplot2::scale_y_continuous(..., labels=labels)

ggplot(combo, aes(x = pred.rmse)) +
    geom_histogram(aes(y = ..density..)) +
    labs(x = "$\\sigma$ (m)",
         y = "Density (m$^{-1}$)") +
    theme_bw()

## ---- combo-eval-rmse-ecdf --------------------
ggplot(combo, aes(x = pred.rmse)) +
    stat_ecdf(geom = "geom_histogram") +
    ## geom_histogram(aes(y = ..density..)) +
    labs(x = "$\\sigma$ (m)",
         ## y = "Density (m$^{-1}$)") +
         y = "Cumulative probability") +
    theme_bw()

## ---- combo-plot --------------------
res <- combo %>%
    ungroup() %>%
    mutate(dummy = "", dummy2 = "") %>%
    ## select(-caliop) %>%
    mutate(caliop = pred.ceilo * 1e-3, caliop.local = caliop) %>%
    group_by(dummy, dummy2) %>%
    regression_plot(title = NULL, xlab = "CBASE cloud base height (km)", base_size = 14)
res$ggplot

## ---- combo-qq --------------------
ggplot(combo, aes(x = sort(pred.ceilo), y = sort(ceilo))) +
    geom_line() +
    geom_abline(intercept = 0, slope = 1, lty = "dashed") +
    theme_bw()

## ---- combo-plot-rmseclass --------------------
res <- combo %>%
    ungroup() %>%
    mutate(dummy = "", dummy2 = "") %>%
    plotutils::discretize(pred.rmse,
                          quantile(pred.rmse, seq(0, 1, 0.1)),
                          ## c(0,400,500,600,1000),
                          ## c(0,450,500,550,1000),
                          as_factor = TRUE) %>%
    ## select(-caliop) %>%
    mutate(caliop = pred.ceilo * 1e-3, caliop.local = caliop) %>%
    group_by(dummy, pred.rmse) %>%
    regression_plot(title = NULL, xlab = "CBASE cloud base height (km)")
## res$ggplot

## ---- combo-tbl-rmseclass --------------------
regression_table(res)

## ---- glorious-victory ---------------------
library(beepr)
beep(0)

