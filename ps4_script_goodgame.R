install.packages("BVAR")
library(tidyverse)
library(fpp3)
library(BVAR)

##PART 2

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

liquor_fit |> select(arima) |> gg_tsresiduals()

liquor_fit |>
  select(arima) |>
  augment() |>
  features(.innov, ljung_box, lag = 24, dof = 6)

## The results of the Ljung-Box test tell us to reject the null hypothesis, meaning that our residuals are autocorrelated and there is structure left in our data we need to capture.

## 2.3 Horse Race

liquor_cv <- liquor |>
  stretch_tsibble(.init = 120, .step = 1) |>
  filter(.id < max(.id))

liquor_cv_fit <- readRDS("results/liquor_cv_fit.rds")
#liquor_cv_fit <- liquor_cv |>
  #model(snaive = SNAIVE(sales),
        #ets    = ETS(log(sales)),
        #arima  = ARIMA(log(sales)))

saveRDS(liquor_cv_fit, "results/liquor_cv_fit.rds")

## There will be 216 forecast origins.

## 2.3b
liquor_cv_fc <- liquor_cv_fit |>
  forecast(h = 1)

liquor_cv_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE))

## Our ARIMA model has both the lowest RMSE and MAE, meaning the metrics agree.

## 2.4
liquor_errs <- liquor_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(liquor) |> select(Month, actual = sales),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e)

forecast::dm.test(liquor_errs$arima, liquor_errs$ets, h = 1, power = 2)

## -2.9062, Forecast horizon = 1, Loss function power = 2, p-value = 0.004042
## Given these results, we reject the null hypothesis that the two models have equal predictive accuracy. The ARIMA model is significantly more accurate than the ETS model.

liquor_errs <- liquor_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(liquor) |> select(Month, actual = sales),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e)

forecast::dm.test(liquor_errs$arima, liquor_errs$snaive, h = 1, power = 2)

## -10.755, Forecast horizon = 1, Loss function power = 2, p-value < 2.2e-16
## Given these results, we reject the null hypothesis that the two models have equal predictive accuracy. The ARIMA model is significantly more accurate than the SNAIVE model.

## If I was the liquor store chain, I would choose the ARIMA model to forecast sales. It has the lowest RMSE and MAE, and the Diebold-Mariano tests show that it is significantly more accurate than both the ETS and SNAIVE models.

## Part 3

liquor_combo_fc <- liquor_cv_fit |>
  mutate(combo = (arima + ets) / 2) |>
  forecast(h = 1)

liquor_combo_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE))

## Combo got second place in both metrics, behind our ARIMA model. It is better than the ETS and SNAIVE models, but not as good as ARIMA.

cor(liquor_errs$arima, liquor_errs$ets)

## The ARIMA and ETS forecast errors are highly correlated (0.879), so the two models tend to miss in the same direction at the same times and averaging them cancels very little error. Because the DM test also showed ARIMA to be significantly more accurate than ETS, the equal-weight combination puts half its weight on the weaker model, and its accuracy (RMSE 85.2) lands between ARIMA (81.4) and ETS (94.4) without beating ARIMA. Combining helps most when errors are weakly correlated and the models are similarly accurate, and neither condition holds here.















