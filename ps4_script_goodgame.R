install.packages("BVAR")
library(tidyverse)
library(fpp3)
library(BVAR)

liquor_raw <- read_csv(
  "data/MRTSSM4453USN.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    MRTSSM4453USN = col_double()
  )
)

liquor <- liquor_raw |>
  transmute(Month = yearmonth(observation_date), sales = MRTSSM4453USN) |>
  as_tsibble(index = Month) |>
  filter(Month <= yearmonth("2019 Dec"))

has_gaps(liquor)

autoplot(liquor, sales) +
  labs(
    title = "Monthly US liquor sales",
    y = "Millions of dollars",
    x = ""
  )

gg_season(liquor, log(sales)) +
  labs(
    title = "Seasonal plot: Monthly US liquor sales",
    y = "Log of millions of dollars",
    x = ""
  )

## Monthly seasonal liquor sales in the US have a mostly linear upward trend with a strong seasonal pattern. This seasonal pattern has a strong peak in December after slow growth all year.
## The seasonal swings do grow with the level of the series which suggests that logs may be useful to account for this.

liquor_fit <- liquor |>
  model(arima = ARIMA(log(sales)),
        ets   = ETS(log(sales)))

liquor_fit |> select(arima) |> report()
liquor_fit |> select(ets) |> report()

## ARIMA: ARIMA(1,0,2)(2,1,1)[12] with drift, fit to log(sales).
## Non-seasonal part (1,0,2): one AR term, zero ordinary (first) differences, and two MA terms.
## Seasonal part (2,1,1)[12]: two seasonal AR terms, one seasonal difference at lag 12, and one seasonal MA term.
## The series was seasonally differenced once (D = 1), meaning each month’s log sales has the same month from the previous year subtracted: log(sales)ₜ − log(sales)ₜ₋₁₂. That is approximately the year-over-year growth rate in sales. Because the log was taken first, the differencing removes the repeating annual seasonal pattern and leaves a roughly stationary series of annual growth rates.





























