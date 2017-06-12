#' Combination of local thin-cloud CALIOP bases into C-BASE product
#'
#' @param local.cbase Database containing the local cloud bases
#' @return A data frame containing information from both
#' @export
#' @describeIn C-BASE_gen Combine local
#' @export
bases.cbase.combine <-
    function(local.cbase = dbtools::db_spec("cloud-bases-2008-local.sqlite",
                                            "cloudbase"),
             ncores = 72) {
        db <- dplyr::src_sqlite(local.cbase$filename)
        df <- dplyr::tbl(db, local.cbase$table)
        
}

