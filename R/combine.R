#' Combination of retrievals for comparison
#'
#' Combine evaluation dataset with retrievals to be evaluated
#' 
#' @param df.eval Data.frame.  Data frame to evaluate against.
#' @param df.ret Data.frame.  Data frame of retrievals to be evaluated.
#' @param max.dist Numeric.  Maximum distance between retrieval and evaluation points
#' @return A data frame containing information from both
#' @export
combine.cbase <- function(df.eval, df.ret, max.dist) {
    
}

#' Combine METAR with cloud base retrievals
#' 
#' @export
combine.cbase.metar <- function(metar = get.metar.2008(),
                                cloudbase = dbtools::db_spec("cloud-bases-2008.sqlite", "cloudbase"),
                                n.cores = 72) {

    ## set up worker processes
    cl <- snow::makeCluster(rep("localhost", n.cores), type = "SOCK", outfile = "snow.log")
    on.exit({
        snow::stopCluster(cl)
    })
    doSNOW::registerDoSNOW(cl)

    snow::clusterExport(cl, "cloudbase", environment())
    snow::clusterEvalQ(cl, {
        library(dplyr)
        db <- dplyr::src_sqlite(cloudbase$filename)
        df <- dplyr::tbl(db, cloudbase$table)
        NULL
    })

    ## find A-Train point closest to each METAR
    plyr::ddply(dplyr::slice(metar) %>% dplyr::mutate(datetime = as.numeric(datetime)),
                ~ station.icao + datetime + date + episode,
                function(x) {
                    df %>%
                        dplyr::filter(time > x$datetime - 3600, time < x$datetime + 3600) %>%
                        dplyr::collect() %>%
                        dplyr::mutate(dist = cbasetools::dist.gc(lon, x$lon, lat, x$lat)) %>%
                        dplyr::filter(dist == min(dist)) %>%
                        dplyr::filter(dist < 100) ## %>%
                        ## dplyr::bind_cols(., slice(x, rep(1, nrow(.))))
                    ## dplyr::summarize(n = n()) 
                },
                .progress = "text", .parallel = TRUE) -> ret

    ## extract METAR cloud-base information
    metar <- dplyr::bind_cols(metar, metar.to.cloud.heights(metar$metar, .parallel = TRUE))

    ## return combination of METARs and A-Train
    left_join(mutate(ret,
                     datetime = as.POSIXct(datetime, origin = "1970-01-01", tz = "UTC")) %>%
              rename(lon.caliop = lon,
                     lat.caliop = lat),
              rename(metar,
                     lon.metar = lon,
                     lat.metar = lat),
              by = c("station.icao",
                     "datetime",
                     "date", 
                     "episode"))
}
