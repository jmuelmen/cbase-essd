#' CBASE accessor functions
#' @name CBASE_accessors
NULL

#' @describeIn CBASE_accessors Get CBASE cloud heights
#' @export
get.cbase <- function() {
    data(cbase, envir = environment())
    cbase
}

#' @describeIn CBASE_accessors Get CBASE cloud heights
#' @export
get.cbase.bitsies <- function() {
    cbase <- plyr::adply(c(40, 100), 1, function(dist) {
        lf.day <- list.files("~/cloud-bases", sprintf("CBASE-%d.*D.rds", dist),
                             full.names = TRUE)
        lf.night <- list.files("~/cloud-bases", sprintf("CBASE-%d.*N.rds", dist),
                               full.names = TRUE)
        doParallel::registerDoParallel(4)
        df.day <- plyr::adply(lf.day, 1, function(fname) {
            lf <- readRDS(fname)
        }, .parallel = TRUE, .inform = TRUE)
        df.night <- plyr::adply(lf.night, 1, function(fname) {
            lf <- readRDS(fname)
        }, .parallel = TRUE, .inform = TRUE)
        
        cbase <- dplyr::bind_rows(dplyr::mutate(df.day, daynight = "day"),
                                  dplyr::mutate(df.night, daynight = "night")) %>%
            dplyr::select(segment, time : n,
                          pred.ceilo, pred.ceilo.msl, pred.rmse, daynight) %>%
            dplyr::mutate(daynight = factor(daynight),
                          dist = as.integer(dist))
    }, .id = NULL)
    
    save(cbase, file = "~/r-packages/cbasetools/data/cbase.rda", compress = "xz")
    cbase
}
