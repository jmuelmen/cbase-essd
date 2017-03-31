## ---- eval-cov-thick ---------------------
df <- readRDS("~/r-packages/cbm-min.rds") %>% factor.vfm()
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
