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

    plyr::ddply(dplyr::slice(metar) %>% dplyr::mutate(datetime = as.numeric(datetime)),
                ~ station.icao + datetime + date + episode,
                function(x) {
                    df %>%
                        dplyr::filter(time > x$datetime - 3600, time < x$datetime + 3600) %>%
                        dplyr::collect() %>%
                        dplyr::filter(cbasetools::dist.gc(lon, x$lon, lat, x$lat) < 100) ## %>%
                    ## dplyr::summarize(n = n()) 
                },
                .progress = "text", .parallel = TRUE) 
}
    
