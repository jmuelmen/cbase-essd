#' Save a data.frame to an SQL database
#'
#' @param dd data.frame.
#' @param fname character.  Output file name.
#' @param name character.  Table name.
#' @param indexes list.  List of variables to index.
#' @return \code{dplyr::tbl} version of the database table
#' @export
df.to.db <- function(dd, fname, name, indexes = list("lon", "lat")) {
    db <- dplyr::src_sqlite(fname, create = TRUE)
    dplyr::copy_to(db, dd, name,
            indexes = indexes, 
            temporary = FALSE)
    return(dplyr::tbl(db, name))
}
