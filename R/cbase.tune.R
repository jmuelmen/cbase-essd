#' CBASE tuning functions
#' 
#' @param df Data.frame.  Contains tuning data set
#' @return A list with two components, a list of tuned models for each
#'     data category and a data.frame containing the index into said list of models 
#' @name CBASE_tune
NULL

#' @describeIn CBASE_tune Tuning using linear models
#'
#' @param thresh Numeric.  If non-NULL, exclude retrieved base heights
#'     below \code{thresh} from the fit.
#' @export
tune.cbase.lm <- function(df, thresh = NULL) {
    models <- list()
    index <- 0

    df <- df %>%
        factor.cbase.tuning.dist() %>%
        factor.cbase.tuning.mult() %>%
        factor.cbase.tuning.thick() %>%
        dplyr::filter(feature.qa.lowest.cloud == "high",
                      phase.lowest.cloud == "water",
                      !(feature.above.surface %in% c("invalid", "no signal")),
                      horizontal.averaging.lowest.cloud.min < "5 km") %>%
        plyr::ddply(~ dist +
                        resolution.out +
                        feature.qa.lowest.cloud +
                        thickness +
                        phase.lowest.cloud +
                        feature.above.surface,
                    function(df) {
                        index <<- index + 1
                        models[[index]] <<- try({
                            lm(ceilo ~ caliop,
                               if (is.null(thresh)) {
                                   df
                               } else {
                                   dplyr::filter(df, caliop > thresh)
                               })
                        })
                        data.frame(index = index,
                                   rmse = sqrt(mean((df$ceilo - 1e3 * df$caliop)^2)),
                                   pred.rmse = if (class(models[[index]]) != "try-error") {
                                       sqrt(mean(models[[index]]$residuals^2))
                                   } else {
                                       NA
                                   })
                    }, .parallel = FALSE, .progress = "text")

    list(models = models, df = df)
}

#' @describeIn CBASE_tune Tuning using SVM
#'
#' @export
tune.cbase.svm <- function(df) {
    models <- list()
    index <- 0

    df <- df %>%
        factor.cbase.tuning.dist() %>%
        factor.cbase.tuning.mult() %>%
        factor.cbase.tuning.thick() %>%
        dplyr::filter(feature.qa.lowest.cloud == "high",
                      phase.lowest.cloud == "water",
                      !(feature.above.surface %in% c("invalid", "no signal")),
                      horizontal.averaging.lowest.cloud.min < "5 km") %>%
        plyr::ddply(~ dist +
                        resolution.out +
                        feature.qa.lowest.cloud +
                        thickness +
                        phase.lowest.cloud +
                        feature.above.surface,
                    function(df) {
                        index <<- index + 1
                        models[[index]] <<- e1071::svm(ceilo ~ caliop, df)
                        data.frame(index = index,
                                   rmse = sqrt(mean((df$ceilo - 1e3 * df$caliop)^2)),
                                   pred.rmse = sqrt(mean(models[[index]]$residuals^2)))
                    }, .parallel = FALSE, .progress = "text")

    list(models = models, df = df)
}

#' CBASE local cloud base correction functions
#' 
#' @param df Data.frame.  Contains local cloud bases to be corrected
#' @param correction List produced by the \code{\link{CBASE_tune}}
#'     functions
#' @return Data frame with corrected local cloud bases
#' @name CBASE_correct
NULL

#' @describeIn CBASE_correct Correct using linear model or inherited
#'     classes (including SVM)
#' @export
correct.cbase.lm <- function(df, correction) {
    models <- correction$models
    df.cor <- correction$df

    df <- df %>%
        factor.cbase.tuning.dist() %>%
        factor.cbase.tuning.mult() %>%
        factor.cbase.tuning.thick() %>%
        dplyr::filter(feature.qa.lowest.cloud == "high",
                      phase.lowest.cloud == "water",
                      !(feature.above.surface %in% c("invalid", "no signal")),
                      horizontal.averaging.lowest.cloud.min < "5 km") %>%
        dplyr::left_join(df.cor, by = c("dist",
                                        "resolution.out",
                                        "feature.qa.lowest.cloud",
                                        "thickness",
                                        "phase.lowest.cloud",
                                        "feature.above.surface")) %>%
        mutate(index.model = index) %>%
        mutate(index = 1 : nrow(.)) %>%
        filter(!is.na(index.model)) %>%
        plyr::ddply(~ index.model, function(x) {
            model <- models[[median(x$index.model)]]
            if (any(class(model) == "try-error")) {
                pred.ceilo <- NA
            } else {
                pred.ceilo <- predict(model, newdata = select(x, caliop))
            }
            dplyr::mutate(x, pred.ceilo = pred.ceilo)
        }, .parallel = FALSE)
}

#' @describeIn CBASE_correct "Identity" (i.e., no) correction
#' @export
correct.cbase.ident <- function(df, correction) {
    models <- correction$models
    df.cor <- correction$df

    df <- df %>%
        factor.cbase.tuning.dist() %>%
        factor.cbase.tuning.mult() %>%
        factor.cbase.tuning.thick() %>%
        dplyr::filter(feature.qa.lowest.cloud == "high",
                      phase.lowest.cloud == "water",
                      !(feature.above.surface %in% c("invalid", "no signal")),
                      horizontal.averaging.lowest.cloud.min < "5 km") %>%
        dplyr::left_join(df.cor, by = c("dist",
                                        "resolution.out",
                                        "feature.qa.lowest.cloud",
                                        "thickness",
                                        "phase.lowest.cloud",
                                        "feature.above.surface")) %>%
        mutate(index.model = index) %>%
        mutate(index = 1 : nrow(.)) %>%
        filter(!is.na(index.model)) %>%
        plyr::ddply(~ index.model, function(x) {
            dplyr::mutate(x, pred.ceilo = 1e3 * caliop, pred.rmse = rmse)
        }, .parallel = FALSE)
}

#' @export
test.cbase.lm <- function(df) {
    ret <- plyr::ddply(df,
                       ~ dist +
                           resolution.out +
                           feature.qa.lowest.cloud +
                           thickness +
                           phase.lowest.cloud +
                           feature.above.surface,
                       function(df) {
                           df %>%
                               dplyr::select(-rmse) %>%
                               dplyr::summarize(pred.rmse = median(pred.rmse),
                                                rmse = sqrt(mean((ceilo - pred.ceilo)^2)))
                       })
}

#' CBASE combination of local cloud bases into CBASE product
#' 
#' @param df.cor Data.frame.  Contains (corrected) local cloud bases
#' @return Data frame with combined cloud bases
#' @name CBASE_combine
NULL

#' @describeIn CBASE_combine Combination around a ground station
#'     overpass
#' @export
cbase.combine.station <- function(df.cor) {
    ## group by ground station overpass
    df.cor %>%
        dplyr::group_by(station.icao, datetime, date, episode) %>%
        cbase.combine()
}

#' @describeIn CBASE_combine Combination within a Calipso track
#'     segment
#' @export
cbase.combine.segment <- function(df.cor) {
    ## in general, ceilometer "true" values are not known
    if (!(any(names(df.cor) == "ceilo"))) {
        df.cor <- mutate(df.cor, ceilo = NA)
    }
    ## group by Calipso track segment
    df.cor %>%
        dplyr::group_by(segment, time, lon, lat) %>%
        cbase.combine()
}

#' @describeIn CBASE_combine Actual worker function
#' @export
cbase.combine <- function(df.cor) {
    dplyr::summarize(df.cor,
                     n = n(),
                     all = all(ceilo == mean(ceilo)),
                     sd.ceilo = sd(ceilo),
                     ceilo = mean(ceilo),
                     sd.pred.ceilo = sd(pred.ceilo),
                     mean.pred.ceilo = mean(pred.ceilo),
                     sd.pred.rmse = sd(pred.rmse),
                     best.ceilo = pred.ceilo[which.min(pred.rmse)],
                     pred.ceilo = sum(pred.ceilo / pred.rmse^2, na.rm = TRUE) /
                         sum(pred.rmse^-2, na.rm = TRUE),
                     pred.rmse.uncor = sqrt(1 / sum(pred.rmse^-2, na.rm = TRUE)),
                     pred.rmse = sqrt(mean(pred.rmse^2, na.rm = TRUE)))
}
