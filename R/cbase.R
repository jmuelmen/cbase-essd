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
    lf.day <- list.files("~/cloud-bases", "CBASE-40.*D.rds", full.names = TRUE)
    lf.night <- list.files("~/cloud-bases", "CBASE-40.*N.rds", full.names = TRUE)

    doParallel::registerDoParallel(4)
    df.day <- plyr::adply(lf.day, 1, function(fname) {
        lf <- readRDS(fname)
    }, .parallel = TRUE, .inform = TRUE)
    df.night <- plyr::adply(lf.night, 1, function(fname) {
        lf <- readRDS(fname)
    }, .parallel = TRUE, .inform = TRUE)

    cbase <- dplyr::bind_rows(dplyr::mutate(df.day, daynight = "day"),
                              dplyr::mutate(df.night, daynight = "night")) %>%
        dplyr::select(segment, X1 : n, pred.ceilo, pred.rmse, daynight)
    
    save(cbase, file = "~/r-packages/cbasetools/data/cbase.rda")
    cbase
}
