## ---- eval-setup ---------------------
df <- readRDS("~/r-packages/cbm-all.rds") %>%
    factor.vfm() %>%
    dplyr::mutate(region = factor(substr(station.icao, 1,1))) %>%
    dplyr::mutate(ceilo = hgts.1 + elevation.m,
                  caliop = cloud.base.altitude,
                  caliop.local = caliop) %>%
    dplyr::mutate(thickness = cloud.top.altitude - cloud.base.altitude) %>%
    mutate(dummy = "",
           dummy2 = "") %>%
    dplyr::filter(region == "K") %>%
    dplyr::filter(dist < 50) %>%
    dplyr::filter(ceilo < 3000, hgts.1 < 3000, caliop < 3) %>%
    group_by(station.icao, datetime, date, episode) ## %>%
    ## slice(1)
    
## ---- eval-qual ---------------------
res <- df %>%
    filter(lays == 1) %>%
    dplyr::group_by(dummy, feature.qa.lowest.cloud) %>%
    regression_plot(title = "feature.qa.lowest.cloud")
res$ggplot

## ---- eval-qual-tbl ---------------------
regression_table(res)

## ---- eval-mult ---------------------
res <- df %>%
    ## filter(lays == 1) %>%
    dplyr::filter(feature.qa.lowest.cloud == "high") %>%
    plotutils::discretize(resolution.out, quantile(resolution.out, seq(0, 1, 0.2)), as_factor = TRUE) %>%
    dplyr::group_by(lays, resolution.out) %>%
    regression_plot(title = "resolution.out")
res$ggplot

## ---- eval-mult-tbl ---------------------
regression_table(res)

## ---- eval-lays ---------------------
res <- df %>%
    dplyr::filter(feature.qa.lowest.cloud == "high") %>%
    dplyr::group_by(dummy, lays) %>%
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

## ---- eval-min-qual-setup ---------------------
df <- readRDS("~/r-packages/cbm-min_cbh.qual.rds") %>%
    factor.vfm() %>%
    dplyr::mutate(region = factor(substr(station.icao, 1,1))) %>%
    dplyr::mutate(ceilo = hgts.1 + elevation.m,
                  caliop = cloud.base.altitude,
                  caliop.local = caliop) %>%
    dplyr::mutate(thickness = cloud.top.altitude - cloud.base.altitude) %>%
    mutate(dummy = "",
           dummy2 = "") %>%
    dplyr::filter(region == "K") %>%
    dplyr::filter(dist < 50) %>%
    dplyr::filter(ceilo < 5000, hgts.1 < 3000, caliop < 3)

## ---- eval-min-qual ---------------------
res <- df %>%
    ## filter(lays == 1) %>%
    dplyr::group_by(## dummy, ##dummy2) %>%
        lays,
                    feature.qa.lowest.cloud) %>%
    regression_plot(title = NULL)
res$ggplot

## ---- eval-min-qual-tbl ---------------------
regression_table(res)

## ---- eval-min-qual-cor ---------------------
res <- df %>%
    dplyr::mutate(caliop = caliop * 0.768 + 0.527) %>%
    ## filter(lays == 1) %>%
    dplyr::group_by(dummy, ##dummy2) %>%
                    feature.qa.lowest.cloud) %>%
    regression_plot(title = NULL)
res$ggplot

## ---- eval-min-qual-cor-tbl ---------------------
regression_table(res)

## ---- eval-quantiles-setup ---------------------
df <- readRDS("~/r-packages/cbm-quantile_cbh.qual.rds") %>%
    factor.vfm() %>%
    dplyr::mutate(region = factor(substr(station.icao, 1,1))) %>%
    dplyr::mutate(ceilo = hgts.1 + elevation.m,
                  caliop = cloud.base.altitude,
                  caliop.local = caliop) %>%
    dplyr::mutate(thickness = cloud.top.altitude - cloud.base.altitude) %>%
    mutate(dummy = "",
           dummy2 = "") %>%
    dplyr::filter(region == "K") %>%
    dplyr::filter(dist < 50) %>%
    dplyr::filter(ceilo < 5000, hgts.1 < 3000, caliop < 3)

## ---- eval-quantiles-qual ---------------------
res <- df %>%
    ## filter(lays == 1) %>%
    dplyr::mutate(resolution.method = sprintf("\\verb+%s+", resolution.method)) %>%
    dplyr::group_by(dummy, resolution.method) %>%
    regression_plot(title = NULL)
res$ggplot

## ---- eval-quantiles-qual-tbl ---------------------
regression_table(res)

## ---- eval-min-qual-2007-setup ---------------------
df <- readRDS("~/r-packages/cbm-min_cbh.qual-2007.rds") %>%
    factor.vfm() %>%
    dplyr::mutate(region = factor(substr(station.icao, 1,1))) %>%
    dplyr::mutate(ceilo = hgts.1 + elevation.m,
                  caliop = cloud.base.altitude,
                  caliop.local = caliop) %>%
    dplyr::mutate(thickness = cloud.top.altitude - cloud.base.altitude) %>%
    mutate(dummy = "",
           dummy2 = "") %>%
    dplyr::filter(region == "K") %>%
    dplyr::filter(dist < 50) %>%
    dplyr::filter(ceilo < 5000, hgts.1 < 3000, caliop < 3)

## ---- eval-min-qual-2007 ---------------------
res <- df %>%
    ## filter(lays == 1) %>%
    dplyr::group_by(dummy, ## dummy2) %>%
        ## lays,
                    feature.qa.lowest.cloud) %>%
    regression_plot(title = NULL)
res$ggplot

## ---- eval-min-qual-2007-tbl ---------------------
regression_table(res)

## ---- eval-min-qual-2007-cor ---------------------
res <- df %>%
    dplyr::mutate(caliop = caliop * 0.768 + 0.527) %>%
    ## filter(lays == 1) %>%
    dplyr::group_by(dummy, ##dummy2) %>%
                    feature.qa.lowest.cloud) %>%
    regression_plot(title = NULL)
res$ggplot

## ---- eval-min-qual-2007-cor-tbl ---------------------
regression_table(res)

## ---- eval-2bgeoprof-setup --------------------
df.comp <- readRDS("~/r-packages/atrain-2bgeoprof-metar-comp-2008.rds")[,-c(10:11)]
df.2bgeoprof <- filter(df.comp, !is.na(base.1)) %>% 
    mutate(ceilo = base + elev, caliop = base.1 * 1e-3, caliop.local = base.1 * 1e-3) %>%
    filter(ceilo < 5000, base < 3000, caliop < 3) %>%
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
    regression_plot(title = NULL)
res$ggplot

## ---- eval-2bgeoprof-tbl --------------------
regression_table(res)

## ---- glorious-victory ---------------------
library(beepr)
beep(0)
