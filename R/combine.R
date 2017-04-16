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

#' Methods to resolve multiple retrievals at the same location
#' 
#' @param df Data frame containing multiple retrievals
#' @return A \code{data.frame} containing the resolution of the
#'     multiple retrievals (single retrieval)
#' @name resolution
NULL


#' @describeIn resolution Retrieval at minimum distance
#'
#' @export
resolution.min_dist <- function(df) {
    dplyr::filter(df, dist == min(dist))
}

#' @describeIn resolution Retrieval at minimum cloud base height
#'
#' @export
resolution.min_cbh <- function(df) {
    df %>%
        dplyr::filter(dist < 100)  %>%
        dplyr::slice(which.min(cloud.base.altitude))
}

#' @describeIn resolution Retrieval at minimum cloud base height subject to quality criteria
#'
#' @export
resolution.min_cbh.qual <- function(df) {
    df %>%
        dplyr::filter(dist < 100)  %>%
        dplyr::filter(feature.qa.lowest.cloud %in% c("medium", "high")) %>%
        dplyr::filter(cloud.top.altitude - cloud.base.altitude < 1) %>%
        dplyr::slice(which.min(cloud.base.altitude))
}

#' @describeIn resolution Retrieval at quantile of cloud base height
#'
#' @param prob Scalar numeric.  Quantile of cloud base altitude
#' @export
resolution.quantile_cbh.qual <- function(prob) {
    function(df) {
        df %>%
            dplyr::filter(dist < 100)  %>%
            dplyr::filter(feature.qa.lowest.cloud %in% c("medium", "high")) %>%
            dplyr::filter(cloud.top.altitude - cloud.base.altitude < 1) %>%
            dplyr::slice(which.min(abs(cloud.base.altitude -
                                       quantile(cloud.base.altitude, prob))))
    }
}

resolve <- function(df, resolution, resolution_names) {
    if (class(resolution) == "function") {
        resolution(df) %>%
            mutate(resolution.method = resolution_names)
    } else {
        ## apply sequentially, add names
        ldply(seq_along(resolution), function(i) {
            fun <- resolution[[i]]
            fun.name <- resolution_names[i]
            fun(df) %>%
                mutate(resolution.method = fun.name)
        })
    }
}

resolution.names <- function(resolution) {
    if (class(resolution) == "function") {
        fun.name <- as.character(substitute(resolution))
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
            fun.name <- fun.names[i]
        })
    }
}

#' Combine METAR with cloud base retrievals
#' 
#' @export
combine.cbase.metar <- function(eval = get.metar.2008(),
                                retrieval = dbtools::db_spec("cloud-bases-2008.sqlite", "cloudbase"),
                                resolution = resolution.min_cbh,
                                ncores = 72) {
    ## find names of resolution functions
    resolution_names <- resolution.names(resolution)
    print(resolution_names)
    return(resolution_names)
    
    ## set up worker processes
    cl <- snow::makeCluster(rep("localhost", ncores), type = "SOCK", outfile = "snow.log")
    on.exit({
        snow::stopCluster(cl)
    })
    doSNOW::registerDoSNOW(cl)

    snow::clusterExport(cl, "retrieval", environment())
    snow::clusterEvalQ(cl, {
        library(dplyr)
        db <- dplyr::src_sqlite(retrieval$filename)
        df <- dplyr::tbl(db, retrieval$table)
    })

    ## find A-Train point closest to each METAR
    plyr::ddply(dplyr::slice(eval,100) %>% dplyr::mutate(datetime = as.numeric(datetime)),
                ~ station.icao + datetime + date + episode,
                function(x, resolution, resolution_names) {
                    df %>%
                        dplyr::filter(time > x$datetime - 3600, time < x$datetime + 3600) %>%
                        dplyr::collect() %>%
                        dplyr::mutate(dist = cbasetools::dist.gc(lon, x$lon, lat, x$lat)) %>%
                        resolve(resolution, resolution_names)
                },
                resolution = resolution,
                resolution_names = resolution_names,
                .progress = "text", .parallel = TRUE) -> ret

    ## extract METAR cloud-base information
    eval <- dplyr::bind_cols(eval, metar.to.cloud.heights(eval$metar, .parallel = TRUE))

    ## return combination of METARs and A-Train
    left_join(mutate(ret,
                     datetime = as.POSIXct(datetime, origin = "1970-01-01", tz = "UTC")) %>%
              rename(lon.caliop = lon,
                     lat.caliop = lat),
              rename(eval,
                     lon.metar = lon,
                     lat.metar = lat),
              by = c("station.icao",
                     "datetime",
                     "date", 
                     "episode"))
}
