#' C-BASE tuning functions
#' 
#' @param df Data.frame.  Contains tuning data set
#' @return A list with two components, a list of tuned models for each
#'     data category and a data.frame containing the index into said list of models 
#' @name C-BASE_tune
NULL

#' @describeIn C-BASE_tune Tuning using linear models
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
                        models[[index]] <<- lm(ceilo ~ caliop,
                                               if (is.null(thresh))
                                                   df
                                               else
                                                   dplyr::filter(df, caliop > thresh)
                                               )
                        data.frame(index = index,
                                   rmse = sqrt(mean((df$ceilo - 1e3 * df$caliop)^2)),
                                   pred.rmse = sqrt(mean(models[[index]]$residuals^2)))
                    })

    list(models = models, df = df)
}

#' @describeIn C-BASE_tune Tuning using SVM
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
                        models[[index]] <<- e1071::lm(ceilo ~ caliop, df)
                        data.frame(index = index,
                                   rmse = sqrt(mean((df$ceilo - 1e3 * df$caliop)^2)),
                                   pred.rmse = sqrt(mean(models[[index]]$residuals^2)))
                    })

    list(models = models, df = df)
}

#' @export
correct.cbase.lm <- function(df, correction) {
    models <- correction$models
    df.cor <- correction$df

    doParallel::registerDoParallel(8)
    
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
            pred.ceilo <- predict(model, newdata = select(x, caliop))
            dplyr::mutate(x, pred.ceilo = pred.ceilo)
        }, .parallel = FALSE, .progress = "text")##   %>%
}

#' @export
correct.cbase.ident <- function(df, correction) {
    models <- correction$models
    df.cor <- correction$df

    doParallel::registerDoParallel(8)
    
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
        }, .parallel = FALSE, .progress = "text")##   %>%
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

#' @export
cbase.combine <- function(df.cor) {
    df.cor %>%
        dplyr::group_by(station.icao, datetime, date, episode) %>%
        dplyr::summarize(n = n(),
                         all = all(ceilo == mean(ceilo)),
                         sd.ceilo = sd(ceilo),
                         ceilo = mean(ceilo),
                         sd.pred.ceilo = sd(pred.ceilo),
                         mean.pred.ceilo = mean(pred.ceilo),
                         sd.pred.rmse = sd(pred.rmse),
                         best.ceilo = pred.ceilo[which.min(pred.rmse)],
                         pred.ceilo = sum(pred.ceilo / pred.rmse^2) / sum(pred.rmse^-2),
                         pred.rmse.uncor = sqrt(1 / sum(pred.rmse^-2)),
                         pred.rmse = sqrt(mean(pred.rmse^2)))
}
