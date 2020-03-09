library(dplyr)
devtools::load_all(".")

test_that("change_frequency changes daily to weekly, monthly, quarterly, annual", {
    expect_equal(
        change_frequency(
            tibble::tibble(
                index = seq.Date(from = as.Date("2000-01-01"), by = 7, length.out = 10),
                value = 1:10
            ) %>%
                tsibble::as_tsibble(index = index),
            "weekly"
        ),
        as_tsibble(

        )
    )
    expect_equal(change_frequency(tsbl_dailyBut), tsbl_weekly)
    expect_equal(change_frequency(tsbl_daily_butWeekly), tsbl_weekly)
    expect_equal(change_frequency(tsbl_daily_butWeekly), tsbl_weekly)
})
