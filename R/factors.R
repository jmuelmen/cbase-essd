#' CALIOP VFM factors
#' 
#' @return With the exception of \code{factor.vfm}, these functions
#'     return factor version of their character or integer vector
#'     argument.
#' @name CALIOP_VFM_factors
NULL

#' @describeIn CALIOP_VFM_factors Utility function for converting
#'     character or integer vectors to factor
#'
#' @param x Character or integer.  Vector to convert to factor
#' @param labels Character.  Factor labels
#' @param ordered Boolean.  Create ordered factor?
factor.relabel <- function(x, labels, ordered = FALSE) {
    if (is.character(x)) {
        factor(x, levels = labels, labels = labels, ordered = ordered)
    } else {
        factor(x, levels = 0 : (length(labels) - 1), labels = labels,
               ordered = ordered)
    }
}

#' @describeIn CALIOP_VFM_factors Convert feature type from string to factor
#' @export
factor.feature.type <- function(x) {
    labels <- c("invalid",
                "clear air",
                "cloud",
                "aerosol",
                "stratospheric feature",
                "surface",
                "subsurface",
                "no signal")
    factor.relabel(x, labels)
}

#' @describeIn CALIOP_VFM_factors Convert feature type from string to factor
#' @export
factor.qa <- function(x) {
    factor.relabel(x,
                   labels = c("none",
                              "low",
                              "medium",
                              "high"),
                   ordered = TRUE)
}

#' @describeIn CALIOP_VFM_factors Convert feature type from string to factor
#' @export
factor.ice.water.phase <- function(x) {
    factor.relabel(x,
                   labels = c("unknown",
                              "randomly oriented ice",
                              "water",
                              "horizontally oriented ice"))
}

#' @describeIn CALIOP_VFM_factors Convert feature type from string to factor
#' @export
factor.horizontal.averaging <- function(x) {
    factor.relabel(x,
                   labels = c("NA",
                              "1/3 km",
                              "1 km",
                              "5 km",
                              "20 km",
                              "80 km"),
                   ordered = TRUE)
}

#' @describeIn CALIOP_VFM_factors Convert CALIOP VFM strings to factors in a data.frame
#'
#' @param df Data.frame.  A data frame containing the variables
#'     \code{feature.qa.lowest.cloud} ... \code{feature.above.surface}
#'     as strings
#' @return \code{factor.vfm} returns a data.frame in which the string
#'     variables describing the CALIOP VFM have been replaced by
#'     (ordered, where appropriate) factors
#' @export
factor.vfm <- function(df) {
    mutate(df,
           feature.qa.lowest.cloud 	= factor.qa(feature.qa.lowest.cloud),
           horizontal.averaging.lowest.cloud.min = factor.horizontal.averaging(horizontal.averaging.lowest.cloud.min),
           horizontal.averaging.lowest.cloud.max = factor.horizontal.averaging(horizontal.averaging.lowest.cloud.max),
           phase.lowest.cloud 		= factor.ice.water.phase(phase.lowest.cloud),
           phase.qa.lowest.cloud 	= factor.qa(phase.qa.lowest.cloud),
           feature.above.surface 	= factor.feature.type	(feature.above.surface))
}
