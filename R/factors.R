#' CALIOP VFM factors
#' 
#' @return With the exception of \code{factor.vfm}, these functions
#'     return factor version of their character or integer vector
#'     argument.
#' @name CALIOP_VFM_factors
NULL

#' Utility function for converting character or integer vectors to
#'     factor
#'
#' @param x Character or integer.  Vector to convert to factor
#' @param labels Character.  Factor labels
#' @param ordered Boolean.  Create ordered factor?
factor.relabel <- function(x, labels, ordered = FALSE) {
    if (is.character(x)) {
        factor(x, levels = labels, labels = labels, ordered = ordered)
    } else if (is.factor(x)) {
        factor.relabel(as.character(x), labels, ordered)
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

#' @describeIn CALIOP_VFM_factors Convert feature type from string to factor
#' @export
factor.land.sea <- function(x) {
    factor.relabel(x,
                   labels = c(
                       "shallow ocean",
                       "land",
                       "coastlines",
                       "shallow inland water",
                       "intermittent water",
                       "deep inland water",
                       "continental ocean",
                       "deep ocean"),
                   ordered = FALSE)
}

#' @describeIn CALIOP_VFM_factors Convert feature type from string to factor
#' @export
factor.day.night <- function(x) {
    factor.relabel(x,
                   labels = c(
                       "day",
                       "night"),
                   ordered = FALSE)
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

#' C-BASE tuning factors
#' 
#' @return With the exception of \code{factor.vfm}, these functions
#'     return factor version of their character or integer vector
#'     argument.
#' @name C-BASE_factors
NULL

#' @describeIn C-BASE_factors Convert distance to factor
#' @export
factor.cbase.tuning.dist <- function(df) {
    plotutils::discretize(df, dist, c(0, 40, 60, 75, 88, 100), as_factor = "ordered")
}

#' @describeIn C-BASE_factors Convert retrieval multiplicity to factor
#' @export
factor.cbase.tuning.mult <- function(df) {
    plotutils::discretize(df, resolution.out, c(0, 175, 250, 325, 400, 1e9), as_factor = "ordered")
}

#' @describeIn C-BASE_factors Convert cloud thickness to factor
#' @export
factor.cbase.tuning.thick <- function(df) {
    plotutils::discretize(df, thickness, c(0, 0.25, 0.45, 0.625, 1, 1e9), as_factor = "ordered")
}
