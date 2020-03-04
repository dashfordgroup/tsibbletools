#' Add STL Decomposition Components
#'
#' @param tsbl tsibble
#' @param long return a long tsibble with only one measured variable
#'
#' @return tsibble with columns for each component of the decomposition, including the residual
#' @export
#'
#' @examples
add_decomposition <-
    function(tsbl, long = FALSE) {
        if (!"tbl_ts" %in% class(tsbl)) {
            rlang::abort("add_decomp_components only works on tsibbles")
        }
        obs_per_season <- frequency(tsbl)

        library(dplyr)
        tsbl <-
            tsbl %>%
            fabletools::model(feasts::STL(value, robust = TRUE)) %>%
            fabletools::components() %>%
            dplyr::select(-.model)
        component_names <- names(tsbl)[which(!names(tsbl) %in% c("index", "value", tsibble::key_vars(tsbl)))]
        names(tsbl)[which(names(tsbl) %in% component_names)] <- paste0(".", component_names)

        if (long) {
            pivot_tsbl_measures_longer(tsbl, key_name = "decomp_key") %>%
                clean_tsbl()
        } else {
            clean_tsbl(tsbl)
        }
    }


#' Add Series without Outliers
#'
#' @param tsbl tsibble
#' @param long return a long tsibble with only one measured variable
#'
#' @return tsibble with a column for cleaned values
#' @export
#'
#' @examples
remove_outliers <-
    function(tsbl, long = FALSE) {
        if (frequency(tsbl) %in% c(1, 4, 12)) {
            rlang::warn("removing outliers is only implemented for daily or faster data")
            tsbl
        } else {
            add_clean_values <-
                function(tsbl) {
                    xts_data <- tsbox::ts_xts(tsbl)
                    tsbl %>%
                        mutate(.clean = forecast::tsclean(xts_data) %>% as.double(),
                               .outlier = ifelse(xts_data == .clean, FALSE, TRUE) %>% as.logical())
                }

            tbl_nested <-
                tsbl %>%
                tsibble::group_by_key() %>%
                tidyr::nest()

            if ("furrr" %in% (.packages())) {
                tbl_nested <-
                    dplyr::mutate(tbl_nested, data = furrr::future_map(data, add_clean_values))
            } else {
                tbl_nested <-
                    dplyr::mutate(tbl_nested, data = purrr::map(data, add_clean_values))
            }

            tsbl_clean <-
                tbl_nested %>%
                tidyr::unnest(cols = c(data)) %>%
                tsibble::as_tsibble(key = tsibble::key_vars(tsbl), index = tsibble::index_var(tsbl))

            if (any(tsbl_clean$.outlier)) {
                message("No outliers found. Returning original tsibble.")
                clean_tsbl(tsbl)
            } else {
                if (long) {
                    tsbl_clean %>%
                        select(-.outlier) %>%
                        pivot_tsbl_measures_longer(key_name = "outlier_key") %>%
                        clean_tsbl()
                } else {
                    clean_tsbl(tsbl_clean)
                }
            }
        }
    }

#' Add Imputed Series
#'
#' @param tsbl tsibble
#' @param long return a long tsibble with only one measured variable
#' @param method method of imputation
#' @param value if method is "replace" then the value for replacement. if "ma" then the value of k.
#' @param Kalman_smooth use Kalman smoothing for StructTS or ARIMA
#' @param seasonal_splitting
#'
#' @return tsibble with a column for imputed values
#' @export
#'
#' @examples
impute_values <-
    function(
        tsbl,
        method = c("linear", "spline", "stine", "ma", "StructTS", "ARIMA", "locf", "nocb",
                   "mean", "median", "mode", "random", "replace",
                   "seasonal_interpolation", "seasonal_locf", "seasonal_mean", "seasonal_random",
                   "seasonal_kalman", "seasonal_ma"),
        long = FALSE,
        value = NULL,
        Kalman_smooth = FALSE,
        seasonal_splitting = FALSE) {

        method <- match.arg(method)

        add_inputed_value <-
            function(tsbl) {
                xts_data <- tsbox::ts_xts(tsbl)
                if (method == "linear") {
                    mutate(tsbl,
                           .imputed_value = imputeTS::na_interpolation(xts_data, option = "linear") %>% as.double(),
                           .imputed_method = "linear interpolation")
                } else if (method == "spline") {
                    mutate(tsbl,
                           .imputed_value = imputeTS::na_interpolation(xts_data, option = "spline") %>% as.double(),
                           .imputed_method = "spline interpolation")
                } else if (method == "stine") {
                    mutate(tsbl,
                           .imputed_value = imputeTS::na_interpolation(xts_data, option = "stine") %>% as.double(),
                           .imputed_method = "stine interpolation")
                } else if (method == "ma") {
                    mutate(tsbl,
                           .imputed_value = imputeTS::na_ma(xts_data, k = value) %>% as.double(),
                           .imputed_method = "ma")
                } else if (method == "structTS") {
                    mutate(tsbl,
                           .imputed_value = imputeTS::na_kalman(xts_data, model = "StructTS", smooth = Kalman_smooth) %>% as.double(),
                           .imputed_method = "StructTS")
                } else if (method == "ARIMA") {
                    mutate(tsbl,
                           .imputed_value = imputeTS::na_kalman(xts_data, model = "ARIMA", smooth = Kalman_smooth) %>% as.double(),
                           .imputed_method = "ARIMA")
                } else if (method == "locf") {
                    mutate(tsbl,
                           .imputed_value = imputeTS::na_locf(xts_data, na_remaining = "rev") %>% as.double(),
                           .imputed_method = "locf")
                } else if (method == "nocb") {
                    mutate(tsbl,
                           .imputed_value = imputeTS::na_locf(xts_data, option = "nocb", na_remaining = "rev") %>% as.double(),
                           .imputed_method = "nocb")
                } else if (method == "mean") {
                    mutate(tsbl,
                           .imputed_value = imputeTS::na_mean(xts_data) %>% as.double(),
                           .imputed_method = "mean")
                } else if (method == "median") {
                    mutate(tsbl,
                           .imputed_value = imputeTS::na_mean(xts_data, option = "median") %>% as.double(),
                           .imputed_method = "median")
                } else if (method == "mode") {
                    mutate(tsbl,
                           .imputed_value = imputeTS::na_mean(xts_data, option = "mode") %>% as.double(),
                           .imputed_method = "mode")
                } else if (method == "random") {
                    mutate(tsbl,
                           .imputed_value = imputeTS::na_random(xts_data) %>% as.double(),
                           .imputed_method = "random")
                } else if (method == "seasonal_interpolation") {
                    if (seasonal_splitting) {
                        mutate(tsbl,
                               .imputed_value = imputeTS::na_seadec(xts_data, algorithm = "interpolation") %>% as.double(),
                               .imputed_method = "seadec_interpolation")
                    } else {
                        mutate(tsbl,
                               .imputed_value = imputeTS::na_seasplit(xts_data, algorithm = "interpolation") %>% as.double(),
                               .imputed_method = "seasplit_interpolation")
                    }
                } else if (method == "seasonal_locf") {
                    if (seasonal_splitting) {
                        mutate(tsbl,
                               .imputed_value = imputeTS::na_seadec(xts_data, algorithm = "locf") %>% as.double(),
                               .imputed_method = "seasonal_decomp_locf")
                    } else {
                        mutate(tsbl,
                               .imputed_value = imputeTS::na_seasplit(xts_data, algorithm = "locf") %>% as.double(),
                               .imputed_method = "seasonal_split_locf")
                    }
                } else if (method == "seasonal_mean") {
                    if (seasonal_splitting) {
                        mutate(tsbl,
                               .imputed_value = imputeTS::na_seadec(xts_data, algorithm = "mean") %>% as.double(),
                               .imputed_method = "seasonal_decomp_mean")
                    } else {
                        mutate(tsbl,
                               .imputed_value = imputeTS::na_seasplit(xts_data, algorithm = "mean") %>% as.double(),
                               .imputed_method = "seasonal_split_mean")
                    }
                } else if (method == "seasonal_random") {
                    if (seasonal_splitting) {
                        mutate(tsbl,
                               .imputed_value = imputeTS::na_seadec(xts_data, algorithm = "random") %>% as.double(),
                               .imputed_method = "seasonal_decomp_random")
                    } else {
                        mutate(tsbl,
                               .imputed_value = imputeTS::na_seasplit(xts_data, algorithm = "random") %>% as.double(),
                               .imputed_method = "seasonal_split_random")
                    }
                } else if (method == "seasonal_kalman") {
                    if (seasonal_splitting) {
                        mutate(tsbl,
                               .imputed_value = imputeTS::na_seadec(xts_data, algorithm = "kalman") %>% as.double(),
                               .imputed_method = "seasonal_decomp_kalman")
                    } else {
                        mutate(tsbl,
                               .imputed_value = imputeTS::na_seasplit(xts_data, algorithm = "kalman") %>% as.double(),
                               .imputed_method = "seasonal_split_kalman")
                    }
                } else if (method == "seasonal_ma") {
                    if (seasonal_splitting) {
                        mutate(tsbl,
                               .imputed_value = imputeTS::na_seadec(xts_data, algorithm = "ma") %>% as.double(),
                               .imputed_method = "seasonal_decomp_ma")
                    } else {
                        mutate(tsbl,
                               .imputed_value = imputeTS::na_seasplit(xts_data, algorithm = "ma") %>% as.double(),
                               .imputed_method = "seasonal_split_ma")
                    }
                } else if (method == "replace") {
                    dplyr::mutate(tsbl,
                           .imputed_value = imputeTS::na_replace(xts_data, value) %>% as.double(),
                           .imputed_method = "replace")
                }
            }

        tbl_nested <-
            tsbl %>%
            tsibble::group_by_key() %>%
            tidyr::nest(data = everything())

        if ("furrr" %in% (.packages())) {
            tbl_nested <-
                dplyr::mutate(tbl_nested, data = furrr::future_map(data, add_inputed_value))
        } else {
            tbl_nested <-
                dplyr::mutate(tbl_nested, data = purrr::map(data, add_inputed_value))
        }

        tsbl_imputed <-
            tbl_nested %>%
            tidyr::unnest(cols = c(data)) %>%
            tsibble::as_tsibble(key = tsibble::key_vars(tsbl), index = tsibble::index_var(tsbl))

        if (any(tsbl_imputed$value != tsbl_imputed$.imputed_value)) {
            message("No values imputed. Returning original tsibble.")
            clean_tsbl(tsbl)
        } else {
            if (long) {
                tsbl_imputed %>%
                    select(-.imputed_method) %>%
                    pivot_tsbl_measures_longer(key_name = "imputed_key") %>%
                    clean_tsbl()
            } else {
                clean_tsbl(tsbl_imputed)
            }
        }
    }
