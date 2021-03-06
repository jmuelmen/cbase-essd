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
print(res$ggplot)
res$ggplot <- NULL
gc()

## ---- eval-qual-tbl ---------------------
regression_table(res)

## ---- eval-qual-after-other-cuts ---------------------
res <- df %>%
    dplyr::filter(phase.lowest.cloud == "water",
                  !(feature.above.surface %in% c("invalid", "no signal")),
                  horizontal.averaging.lowest.cloud.min < "5 km") %>%
    ## filter(lays == 1) %>%
    dplyr::group_by(dummy, feature.qa.lowest.cloud) %>%
    regression_plot(title = NULL, base_size = 14)
print(res$ggplot)
res$ggplot <- NULL
gc()

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
    regression_plot(title = NULL, xlab = "2B-GEOPROF-LIDAR cloud base height (km AGL)", base_size = 14)
res$ggplot

## ---- eval-2bgeoprof-tbl --------------------
res$stats %<>% rename("Base type" = flag.base)
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
combo <- cbase.combine.station(dddf)
gc()

## ---- eval-tune-setup-self --------------------
dddf.self <- correct.cbase.lm(df, ddf)
combo.self <- cbase.combine.station(dddf.self)
gc()

## ---- eval-asos-setup --------------------
df.asos <- df.val %>%
    filter(substr(station.icao, 1, 1) == "K") %>%
    group_by(station.icao, date, episode) %>%
    summarize(min.dist = min(dist)) %>%
    group_by(station.icao) %>%
    summarize(n = n(), min.dist = min(min.dist)) %>%
    left_join(stations.metar(), by = "station.icao") 

## ---- eval-asos --------------------
ggplot(df.asos, aes(x = lon, y = lat, col = min.dist, size = n)) +
    geom_point() +
    borders("state") +
    scale_size("$N$", trans = "identity", range = c(0.05, 2),
               guide = ggplot2::guide_legend(title.vjust = 0.5)) +
    scale_color_distiller("$\\min(D)$ (km)", palette = "Blues",
                          guide = ggplot2::guide_colorbar(title.vjust = 0.75)) +
    labs(x = NULL, y = NULL) +
    scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) +
    theme_void(12) +
    theme(legend.position = "bottom")
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
    filter(pred.ceilo > 0, pred.ceilo < 3e3) %>%
    mutate(caliop = pred.ceilo * 1e-3, caliop.local = caliop) %>%
    group_by(dummy, dummy2) %>%
    regression_plot(title = NULL, xlab = "CBASE cloud base height (km AGL)", base_size = 14)
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
    regression_plot(title = NULL, xlab = "CBASE cloud base height (km AGL)")
## res$ggplot

## ---- combo-tbl-qa --------------------
res$stats %<>% rename("QA flag" = feature.qa.lowest.cloud)
regression_table(res)

## ---- combo-tbl-rmseclass --------------------
res$stats %<>% rename("Predicted $\\sigma$ (m)" = pred.rmse)
regression_table(res)

## ---- combo-lays ---------------------
res <- combo %>%
    ungroup() %>%
    mutate(dummy = "", dummy2 = "") %>%
    dplyr::group_by(dummy, lays) %>%
    regression_plot(title = "lays")
res$ggplot

## ---- combo-lays-tbl ---------------------
regression_table(res)

## ---- 2bgeoprof-cbase-comparison ----------
df.2bgeoprof.cbase <- inner_join(combo.self,
                                 df.2bgeoprof %>% filter(flag.base == "Lidar"),
                                 by = c("station.icao", "date", "episode")) %>%
    ungroup()

res <- df.2bgeoprof.cbase %>%
    mutate(caliop = caliop,
           ceilo = pred.ceilo) %>%
    group_by(dummy, dummy2) %>%
    regression_plot(title = NULL,
                    ylab = "CBASE cloud base height (m AGL)",
                    xlab = "2B-GEOPROF-LIDAR lidar cloud base height (km AGL)",
                    include.lm = FALSE, base_size = 14)
res$ggplot

## ---- eval-svm-corr-setup --------------------
## combine cbases without SVM correction for later evaluation
dddfi.self <- correct.cbase.ident(df, ddf)
combo.selfi <- cbase.combine.station(dddfi.self %>%
                                     mutate(pred.ceilo.msl = pred.ceilo + surface.elevation * 1e3))

## ---- eval-svm-corr --------------------
svm.eval <- inner_join(combo.self, combo.selfi, by = c("station.icao", "date", "episode")) %>%
    ungroup() %>%
    mutate(pred.ceilo.cor = pred.ceilo.x,
           pred.ceilo.uncor = pred.ceilo.y)

res <- svm.eval %>%
    mutate(ceilo = pred.ceilo.cor,
           caliop = pred.ceilo.uncor * 1e-3,
           caliop.local = caliop) %>%
    mutate(dummy = "",
           dummy2 = "") %>%
    group_by(dummy, dummy2) %>%
    regression_plot(title = NULL,
                    xlab = "Uncorrected \\CBHc~(km AGL)",
                    ylab = "Corrected \\CBHc~(m AGL)",
                    include.lm = FALSE, base_size = 14)

res$ggplot

## ---- eval-uncertainty --------------------
dddf %>%
    select(dist, resolution.out, thickness, pred.rmse) %>%
    tidyr::gather(predictor, val, dist:thickness) %>%
    mutate(predictor = factor(predictor)) %>%
    mutate(predictor = revalue(predictor,
                               c("dist" = "$D$~(km)",
                                 "resolution.out" = "$n$",
                                 "thickness" = "$\\Delta z$~(km)"))) %>%
    mutate(val = revalue(val,
                         c("(1,1e+09]" = "$>1$",
                           "(400,1e+09]" = "$>400$"))) %>%
    ggplot(aes(y = pred.rmse, x = val)) +
    geom_violin(adjust = 4, trim = FALSE, fill = "grey92") +
    facet_grid(. ~ predictor, scales = "free_x") +
    labs(x = "Uncertainty predictor category",
         y = "Predicted uncertainty $\\sigmac(D, n, \\Delta z)$ (m)") +
    theme_bw(14) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

## ---- glorious-victory ---------------------
library(beepr)
beep(0)

