#' Change tsibble frequency
#'
#' @param tsbl tsibble
#' @param freq frequency to apply to tsibble
#' @param .f summary function
#'
#' @return tsibble
#' @importFrom dplyr %>%
#' @importFrom rlang :=
#' @importFrom rlang !!
#' @export
#'
#' @examples
change_frequency <-
    function(
        tsbl,
        freq = c("daily", "weekly", "monthly", "quarterly", "yearly"),
        rename_index = TRUE,
        .f = sum
    ) {
        library(rlang)
        freq <- match.arg(freq)

        if (!tsibble::is_tsibble(tsbl)) {
            stop("input must be a tsibble (of class 'tbl_ts')")
        }
        if (length(tsibble::index_var(tsbl)) > 1) {
            stop("change_frequency only works with tsibbles with one index variable")
        }

        if (!rename_index) {
            index_name <- tsibble::index_var(tsbl)
        }

        colnames(tsbl)[which(colnames(tsbl) == tsibble::index_var(tsbl))] <- "index"
        tsbl <- tsibble::update_tsibble(tsbl, index = index)

        if (freq == "daily") {
            tsbl <- tsibble::index_by(tsbl, Date = ~ lubridate::date(.))
        } else if (freq == "weekly") {
            tsbl <- tsibble::index_by(tsbl, Week = ~ tsibble::yearweek(.))
        } else if (freq == "monthly") {
            tsbl <- tsibble::index_by(tsbl, Month = ~ tsibble::yearmonth(.))
        } else if (freq == "quarterly") {
            tsbl <- tsibble::index_by(tsbl, Quarter = ~ tsibble::yearquarter(.))
        } else if (freq == "yearly") {
            tsbl <- tsibble::index_by(tsbl, Year = ~ lubridate::year(.))
        }

        tsbl <-
            tsbl %>%
            tsibble::update_tsibble(index = tsibble::index2_var(tsbl)) %>%
            dplyr::select(-index) %>%
            tsibble::group_by_key() %>%
            dplyr::summarise_all(dplyr::across(tsibble::measured_vars(.)), {{ .f }}) %>%
            tsibble::as_tsibble(
                key = tsibble::key_vars(tsbl),
                index = tsibble::index2_var(tsbl)
            ) %>%
            dplyr::ungroup()

        if (!rename_index) {
            tsbl <- dplyr::rename(tsbl, !!(index_name) := tsibble::index(tsbl))
        }

        return(tsbl)
    }


#' Correct the frequency of a tsibble
#'
#' @param tsbl tsibble
#'
#' @return tsibble
#' @export
#'
#' @examples
correct_frequency <-
    function(tsbl) {
        if (!tsibble::is_tsibble(tsbl)) stop("input must be a tsibble (of class 'tbl_ts')")
        if (frequency(tsbl) != 7) stop("correct_frequency currently supports tsibbles reporting daily frequency")

        tally_test <- function(tsbl) {
            all(dplyr::pull((dplyr::tally(tsbl)), n) == 1)
            # tsbl %>%
            #     dplyr::tally() %>%
            #     dplyr::pull(n) %>%
            #     all(. == 1)
        }

        grouping_keys <-
        tsbl <- tsibble::group_by_key(tsbl)

        if (tally_test(tsibble::index_by(tsbl, year = ~ lubridate::year(.)))) {
            tsbl <- change_frequency(tsbl, freq = "year")
        } else if (tally_test(tsibble::index_by(tsbl, year_qtr = ~ tsibble::yearquarter(.)))) {
            tsbl <- change_frequency(tsbl, freq = "quarterly")
        } else if (tally_test(tsibble::index_by(tsbl, year_month = ~ tsibble::yearmonth(.)))) {
            tsbl <- change_frequency(tsbl, freq = "monthly")
        } else if (tally_test(tsibble::index_by(tsbl, year_week = ~ tsibble::yearweek(.)))) {
            tsbl <- change_frequency(tsbl, freq = "weekly")
        }

        dplyr::ungroup(tsbl)
    }


#' Ungroup and fix frequency of tsibble
#'
#' @param tsbl tsibble object
#'
#' @return ungrouped tsibble object of correct frequency
#' @export
#'
#' @examples
clean_tsbl <-
    function(tsbl) {
        if (!tsibble::is_tsibble(tsbl)) stop("input must be a tsibble (of class 'tbl_ts')")
        dplyr::ungroup(correct_frequency(tsbl))
    }
