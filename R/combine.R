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

## in cases with multiple retrievals,
## different resolution methods are possible:
## * retrieval at minimum distance
## ## dplyr::filter(dist == min(dist)) %>%
## * retrieval at minimum cloud base height
## ## dplyr::filter(dist < 100)  %>%
## ## dplyr::filter(cloud.base.altitude == min(cloud.base.altitude))
## * retrieval at minimum cloud base height
##   subject to quality criteria
resolution.min_dist <- function(df) {
    dplyr::filter(df, dist == min(dist))
}

resolution.min_cbh <- function(df) {
    df %>%
        dplyr::filter(dist < 100)  %>%
        dplyr::slice(which.min(cloud.base.altitude))
}

resolution.quantile_cbh <- function(prob) {
    function(df) {
        df %>%
            dplyr::filter(dist < 100)  %>%
            dplyr::slice(which.min(abs(cloud.base.altitude -
                                       quantile(cloud.base.altitude, prob))))
    }
}

resolve <- function(df, resolution) {
    if (class(resolution) == "function") {
        ## apply, add name
        fun.name <- as.character(substitute(resolution))
        resolution(df) %>%
            mutate(resolution.method = fun.name)
    } else {
        ## get names from names attribute
        fun.names <- names(resolution)
        ## or from the parse tree
        sub <- as.character(substitute(resolution))[-1]
        if (is.null(fun.names))
            fun.names <- sub
        fun.names[fun.names == ""] <- sub[fun.names == ""]

        ## apply sequentially, add names
        ldply(seq_along(resolution), function(i) {
            fun <- resolution[[i]]
            fun.name <- fun.names[i]
            print(fun.name)
            fun(df) %>%
                mutate(resolution.method = fun.name)
        })
    }
}
    
#' Combine METAR with cloud base retrievals
#' 
#' @export
combine.cbase.metar <- function(metar = get.metar.2008(),
                                cloudbase = dbtools::db_spec("cloud-bases-2008.sqlite", "cloudbase"),
                                resolution = resolution.min_cbh,
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
                function(x, resolution) {
                    df %>%
                        dplyr::filter(time > x$datetime - 3600, time < x$datetime + 3600) %>%
                        dplyr::collect() %>%
                        dplyr::mutate(dist = cbasetools::dist.gc(lon, x$lon, lat, x$lat)) %>%
                        resolve(df, resolution)
                },
                .progress = "text", .parallel = TRUE, resolution = resolution) -> ret

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
