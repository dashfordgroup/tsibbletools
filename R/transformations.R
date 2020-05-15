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
    function(tsbl, .vars, ..., long = FALSE) {
        if (!"tbl_ts" %in% class(tsbl)) {
            rlang::abort("add_decomp_components only works on tsibbles")
        }

        indx_var <- tsibble::index_var(tsbl)
        key_var <- tsibble::key_vars(tsbl)

        obs_per_season <- frequency(tsbl)
        .var_list <- list(.vars, ...)


        mod_sel <- function(tsbl, .var, label){
            tsbl <- fabletools::components(fabletools::model(tsbl, feasts::STL({{.var}}, robust = TRUE)))
            component_names <- names(tsbl)[which(!names(tsbl) %in% c(tsibble::index_var(tsbl), label, tsibble::key_vars(tsbl)))]
            names(tsbl)[which(names(tsbl) %in% component_names)] <- paste0(".", component_names, '_', label)
            tsbl
        }

        res_lst <- list()
        for(i in 1:length(.var_list)){
            sym_item <- sym(.var_list[[i]])
            res_lst[[i]] <- tibble::as_tibble(mod_sel(tsbl,!!sym_item, .var_list[[i]]))
        }

        res_tbl <- res_lst[[1]]
        for(i in 2:length(res_lst)){
            res_tbl <- left_join(res_tbl, res_lst[[i]])
        }

        res_tsbl <- tsibble::as_tsibble(res_tbl,index = tsibble::index_var(tsbl), key = tsibble::key_vars(tsbl))

        if (long) {
            clean_tsbl(pivot_tsbl_measures_longer(res_tsbl, key_name = "decomp_key"))
        } else {
            clean_tsbl(res_tsbl)
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
                    dplyr::mutate(
                        tsbl,
                        .clean = as.double(forecast::tsclean(xts_data)),
                        .outlier = as.logical(ifelse(xts_data == .clean, FALSE, TRUE)))

                }

            tbl_nested <- tidyr::nest(tsibble::group_by_key(tsbl))

            if (rlang::is_attached("package::furrr")) {
                tbl_nested <-
                    dplyr::mutate(tbl_nested, data = furrr::future_map(data, add_clean_values))
            } else {
                tbl_nested <-
                    dplyr::mutate(tbl_nested, data = purrr::map(data, add_clean_values))
            }

            tsbl_clean <-
                tsibble::as_tsibble(
                    tidyr::unnest(
                        tbl_nested,
                        cols = c(data)),
                    key = tsibble::key_vars(tsbl),
                    index = tsibble::index_var(tsbl))

            if (any(tsbl_clean$.outlier)) {
                message("No outliers found. Returning original tsibble.")
                clean_tsbl(tsbl)
            } else {
                if (long) {
                    clean_tsbl(
                        pivot_tsbl_measures_longer(
                            select(tsbl_clean, -.outlier),
                            key_name = "outlier_key"))
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
                    dplyr::mutate(tsbl,
                           .imputed_value = as.double(imputeTS::na_interpolation(xts_data, option = "linear")),
                           .imputed_method = "linear interpolation")
                } else if (method == "spline") {
                    dplyr::mutate(tsbl,
                           .imputed_value = as.double(imputeTS::na_interpolation(xts_data, option = "spline")),
                           .imputed_method = "spline interpolation")
                } else if (method == "stine") {
                    dplyr::mutate(tsbl,
                           .imputed_value = as.double(imputeTS::na_interpolation(xts_data, option = "stine")),
                           .imputed_method = "stine interpolation")
                } else if (method == "ma") {
                    dplyr::mutate(tsbl,
                           .imputed_value = as.double(imputeTS::na_ma(xts_data, k = value)),
                           .imputed_method = "ma")
                } else if (method == "structTS") {
                    dplyr::mutate(tsbl,
                           .imputed_value = as.double(imputeTS::na_kalman(xts_data, model = "StructTS", smooth = Kalman_smooth)),
                           .imputed_method = "StructTS")
                } else if (method == "ARIMA") {
                    dplyr::mutate(tsbl,
                           .imputed_value = as.double(imputeTS::na_kalman(xts_data, model = "ARIMA", smooth = Kalman_smooth)),
                           .imputed_method = "ARIMA")
                } else if (method == "locf") {
                    dplyr::mutate(tsbl,
                           .imputed_value = as.double(imputeTS::na_locf(xts_data, na_remaining = "rev")),
                           .imputed_method = "locf")
                } else if (method == "nocb") {
                    dplyr::mutate(tsbl,
                           .imputed_value = as.double(imputeTS::na_locf(xts_data, option = "nocb", na_remaining = "rev")),
                           .imputed_method = "nocb")
                } else if (method == "mean") {
                    dplyr::mutate(tsbl,
                           .imputed_value = as.double(imputeTS::na_mean(xts_data)),
                           .imputed_method = "mean")
                } else if (method == "median") {
                    dplyr::mutate(tsbl,
                           .imputed_value = as.double(imputeTS::na_mean(xts_data, option = "median")),
                           .imputed_method = "median")
                } else if (method == "mode") {
                    dplyr::mutate(tsbl,
                           .imputed_value = as.double(imputeTS::na_mean(xts_data, option = "mode")),
                           .imputed_method = "mode")
                } else if (method == "random") {
                    dplyr::mutate(tsbl,
                           .imputed_value = as.double(imputeTS::na_random(xts_data)),
                           .imputed_method = "random")
                } else if (method == "seasonal_interpolation") {
                    if (seasonal_splitting) {
                        dplyr::mutate(tsbl,
                               .imputed_value = as.double(imputeTS::na_seadec(xts_data, algorithm = "interpolation")),
                               .imputed_method = "seadec_interpolation")
                    } else {
                        dplyr::mutate(tsbl,
                               .imputed_value = as.double(imputeTS::na_seasplit(xts_data, algorithm = "interpolation")),
                               .imputed_method = "seasplit_interpolation")
                    }
                } else if (method == "seasonal_locf") {
                    if (seasonal_splitting) {
                        dplyr::mutate(tsbl,
                               .imputed_value = as.double(imputeTS::na_seadec(xts_data, algorithm = "locf")),
                               .imputed_method = "seasonal_decomp_locf")
                    } else {
                        dplyr::mutate(tsbl,
                               .imputed_value = as.double(imputeTS::na_seasplit(xts_data, algorithm = "locf")),
                               .imputed_method = "seasonal_split_locf")
                    }
                } else if (method == "seasonal_mean") {
                    if (seasonal_splitting) {
                        dplyr::mutate(tsbl,
                               .imputed_value = as.double(imputeTS::na_seadec(xts_data, algorithm = "mean")),
                               .imputed_method = "seasonal_decomp_mean")
                    } else {
                        dplyr::mutate(tsbl,
                               .imputed_value = as.double(imputeTS::na_seasplit(xts_data, algorithm = "mean")),
                               .imputed_method = "seasonal_split_mean")
                    }
                } else if (method == "seasonal_random") {
                    if (seasonal_splitting) {
                        dplyr::mutate(tsbl,
                               .imputed_value = as.double(imputeTS::na_seadec(xts_data, algorithm = "random")),
                               .imputed_method = "seasonal_decomp_random")
                    } else {
                        dplyr::mutate(tsbl,
                               .imputed_value = as.double(imputeTS::na_seasplit(xts_data, algorithm = "random")),
                               .imputed_method = "seasonal_split_random")
                    }
                } else if (method == "seasonal_kalman") {
                    if (seasonal_splitting) {
                        dplyr::mutate(tsbl,
                               .imputed_value = as.double(imputeTS::na_seadec(xts_data, algorithm = "kalman")),
                               .imputed_method = "seasonal_decomp_kalman")
                    } else {
                        dplyr::mutate(tsbl,
                               .imputed_value = as.double(imputeTS::na_seasplit(xts_data, algorithm = "kalman")),
                               .imputed_method = "seasonal_split_kalman")
                    }
                } else if (method == "seasonal_ma") {
                    if (seasonal_splitting) {
                        dplyr::mutate(tsbl,
                               .imputed_value = as.double(imputeTS::na_seadec(xts_data, algorithm = "ma")),
                               .imputed_method = "seasonal_decomp_ma")
                    } else {
                        dplyr::mutate(tsbl,
                               .imputed_value = as.double(imputeTS::na_seasplit(xts_data, algorithm = "ma")),
                               .imputed_method = "seasonal_split_ma")
                    }
                } else if (method == "replace") {
                    dplyr::mutate(tsbl,
                           .imputed_value = as.double(imputeTS::na_replace(xts_data, value)),
                           .imputed_method = "replace")
                }
            }

        tbl_nested <-
            tidyr::nest(
                tsibble::group_by_key(tsbl),
                data = everything())

        if ("furrr" %in% (.packages())) {
            tbl_nested <-
                dplyr::mutate(tbl_nested, data = furrr::future_map(data, add_inputed_value))
        } else {
            tbl_nested <-
                dplyr::mutate(tbl_nested, data = purrr::map(data, add_inputed_value))
        }

        tsbl_imputed <-
            tsibble::as_tsibble(
                tidyr::unnest(
                    tbl_nested,
                    cols = c(data)),
                key = tsibble::key_vars(tsbl),
                index = tsibble::index_var(tsbl))

        if (any(tsbl_imputed$value != tsbl_imputed$.imputed_value)) {
            message("No values imputed. Returning original tsibble.")
            clean_tsbl(tsbl)
        } else {
            if (long) {
                clean_tsbl(
                    pivot_tsbl_measures_longer(
                        select(
                            tsbl_imputed,
                            -.imputed_method),
                        key_name = "imputed_key"))
            } else {
                clean_tsbl(tsbl_imputed)
            }
        }
    }
