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

## Part 4

qd_raw <- read_csv("data/fredqd_2026-09.csv", show_col_types = FALSE) |>
  slice(-(1:2))

growth <- function(x) 100 * difference(log(x))

qd <- qd_raw |>
  mutate(date = as.Date(sasdate, "%m/%d/%Y")) |>
  transmute(date,
            gdp    = growth(GDPC1),     # real GDP
            cons   = growth(PCECC96),   # real consumption
            inv    = growth(GPDIC1),    # real investment
            emp    = growth(PAYEMS),    # payroll employment
            infl   = growth(GDPCTPI),   # GDP price index
            unrate = difference(UNRATE),     # unemployment rate
            ffr    = difference(FEDFUNDS),   # federal funds rate
            gs10   = difference(GS10)) |>    # 10-year Treasury yield
  filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))

y <- qd |> select(-date) |> as.matrix()

nrow(y)
head(y)

## 4.1c

N <- nrow(y)
origins <- 120:(N - 1)
actual <- y[121:N, ]

## 120 forecasts, first forecast is 1990 Q1, last forecast is 2019 Q4.

## 4.2
var_ols_fc <- function(z, p) {
  e <- embed(z, p + 1)            # columns: y_t, then its lags 1 to p
  M <- ncol(z)                    # number of variables
  B <- coef(lm(e[, 1:M] ~ e[, -(1:M)]))
  x <- c(1, t(z[nrow(z):(nrow(z) - p + 1), ]))   # constant, latest p lags
  x %*% B
}

fc_var1 <- NULL
fc_var4 <- NULL
for (T in origins) {
  fc_var1 <- rbind(fc_var1, var_ols_fc(y[1:T, ], 1))
  fc_var4 <- rbind(fc_var4, var_ols_fc(y[1:T, ], 4))
}

rmse_by_var <- function(f) sqrt(colMeans((actual - f)^2))

rmse_by_var(fc_var1)
rmse_by_var(fc_var4)

sum(rmse_by_var(fc_var1) < rmse_by_var(fc_var4))
## The VAR 1 model forecasts better for almost every variable, except for inflation where it was slightly worse. 
## Although the VAR(4) nests the VAR(1), it estimates many more coefficients (33 vs. 9 per equation) from a limited sample, so parameter estimation error adds variance that hurts out-of-sample forecasts, even though in-sample fit is better.

## 4.3
# 4.3a
bvar_cv <- function(lambda) {
  pr <- bv_priors(hyper = "lambda",
                  mn = bv_mn(lambda = bv_lambda(mode = lambda, sd = 0.4,
                                                min = 0.999 * lambda,
                                                max = 1.001 * lambda),
                             b = 0))
  fc <- NULL
  for (T in origins) {
    fit_T <- bvar(y[1:T, ], lags = 4, priors = pr,
                  n_draw = 2000, n_burn = 1000, verbose = FALSE)
    x <- c(1, t(y[T:(T - 3), ]))         # constant and 4 lags of each variable
    fc <- rbind(fc, x %*% coef(fit_T))   # forecast of y_{T+1}
  }
  fc
}

set.seed(422)
fc_bvar_03 <- bvar_cv(0.3)
saveRDS(fc_bvar_03, "results/fc_bvar_03.rds")

# 4.3b
set.seed(422)
fc_bvar_005 <- bvar_cv(0.05)

var_results <- tibble(
  Variable = colnames(y),
  `VAR(1)` = rmse_by_var(fc_var1),
  `VAR(4)` = rmse_by_var(fc_var4),
  `BVAR(4), lambda = 0.3` = rmse_by_var(fc_bvar_03),
  `BVAR(4), lambda = 0.05` = rmse_by_var(fc_bvar_005)
)

saveRDS(var_results, "results/var_results.rds")

var_results |> knitr::kable(digits = 3)

readRDS("results/var_results.rds")

## The BVAR is better for all 8 variables. Since the two models have the same variables and lags, the only difference is how they are estimated. The BVAR uses a prior to shrink the coefficients toward zero, which reduces variance in the estimates and improves out-of-sample forecasts. 
## VAR(1) is slightly ahead. The BVAR wins on investment (2.385 vs. 2.493) and inflation (0.197 vs. 0.208). It essentially ties on gs10 (0.359 vs. 0.361). VAR(1) wins on GDP, consumption, employment, unemployment and the fed funds rate. So it's roughly 3 variables for the BVAR (one a tie) and 5 for VAR(1).
## With λ = 0.05 the prior is much tighter, so the coefficients are shrunk heavily toward the prior and the model largely ignores the data. The winner's RMSE would not be a fair estimate of future accuracy. If I choose the model, and λ, because it has the lowest RMSE in this TS-CV, then I have selected on the same errors I would report.


# 4.3c

e_var4 <- actual[, "gdp"] - fc_var4[, 1]
e_bvar <- actual[, "gdp"] - fc_bvar_03[, 1]

forecast::dm.test(e_var4, e_bvar, h = 1, power = 2)

e_var4_ffr <- actual[, "ffr"] - fc_var4[, 7]
e_bvar_ffr <- actual[, "ffr"] - fc_bvar_03[, 7]

forecast::dm.test(e_var4_ffr, e_bvar_ffr, h = 1, power = 2)

# For GDP growth, the DM test rejects equal accuracy of the VAR(4) and the BVAR(4, λ = 0.3) (DM = 2.14, p = 0.035), and the positive statistic indicates the BVAR has the lower squared-error loss. The same holds for the federal funds rate (DM = 2.00, p = 0.048). This supports my result in 4.3b that shrinkage improves on the OLS VAR(4), so the lower RMSEs are not just sampling noise. However, the evidence is not overwhelming: both p-values are close to 0.05








