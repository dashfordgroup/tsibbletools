#' Change tsibble frequency
#'
#' @param tsbl tsibble
#' @param freq frequency to apply to tsibble
#' @param .f summary function
#'
#' @return tsibble
#' @export
#'
#' @examples
change_frequency <-
    function(
        tsbl,
        freq = c("daily", "weekly", "monthly", "quarterly", "yearly"),
        .f = sum
    ) {
        freq <- match.arg(freq)

        if (!tsibble::is_tsibble(tsbl)) stop("input must be a tsibble (of class 'tbl_ts')")

        index_name <- tsibble::index_var(tsbl)

        if (freq == "daily") {
            tsbl <- tsibble::index_by(tsbl, index_new = ~ lubridate::date(.))
        } else if (freq == "weekly") {
            tsbl <- tsibble::index_by(tsbl, index_new = ~ tsibble::yearweek(.))
        } else if (freq == "monthly") {
            tsbl <- tsibble::index_by(tsbl, index_new = ~ tsibble::yearmonth(.))
        } else if (freq == "quarterly") {
            tsbl <- tsibble::index_by(tsbl, index_new = ~ tsibble::yearquarter(.))
        } else if (freq == "yearly") {
            tsbl <- tsibble::index_by(tsbl, index_new = ~ lubridate::year(.))
        }

        dplyr::rename(dplyr::summarise_all(tsbl, {{ .f }}), !!(index_name) := index_new)

        # tsbl %>%
        #     dplyr::summarise_all({{ .f }}) %>%
        #     dplyr::rename(!!(index_name) := index_new)
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

        ungroup(tsbl)
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
        ungroup(fix_tsibble_frequency_automatically(tsbl))
    }
