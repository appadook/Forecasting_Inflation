# Your Names Goes Here
# Course Project 

# load packages
library(tidyverse)
library(fpp3)
library(GGally)

################## Data Preparation ##################
# read data
CPI <- read_csv("CPI.csv") |> 
  mutate(DATE = yearmonth(DATE)) |> 
  rename(CPI = "CPIAUCNS")
CPI

Wage_Growth <- read_csv("Wage Growth.csv") |> 
  mutate(DATE = yearmonth(DATE)) |> 
  rename(Wage_Growth = "FRBATLWGTUMHWGO")
Wage_Growth


PPI <- read_csv("PPI Data.csv") |> 
  mutate(DATE = yearmonth(DATE)) |> 
  rename(PPI = "PPIACO")
PPI

Oil_Prices <- read_csv("Oil Prices....csv") |> 
  mutate(DATE = yearmonth(DATE)) |> 
  rename(Oil_Prices = "PCU333132333132")

Exchange_Rates <- read_csv("RBUSBIS.csv") |> 
  mutate(DATE = yearmonth(DATE)) |> 
  rename(Exchange_Rates = "RBUSBIS")

Unemp_Rate <- read_csv("UNRATENSA.csv") |> 
  mutate(DATE = yearmonth(DATE)) |> 
  rename(Unemp_Rate = "UNRATENSA")


data <- CPI |> 
  left_join(Wage_Growth, by = "DATE") |> 
  left_join(PPI, by = "DATE") |>
  left_join(Oil_Prices, by = "DATE") |>
  left_join(Exchange_Rates, by = "DATE") |> 
  left_join(Unemp_Rate, by = "DATE")

data

# converting data set to percentage form
data <- data |> 
  arrange(DATE) |> 
  mutate(CPI = (CPI/lag(CPI) - 1) * 100,
         Wage_Growth = (Wage_Growth/lag(Wage_Growth) - 1) * 100,
         PPI = (PPI/lag(PPI) - 1) * 100,
         Oil_Prices = (Oil_Prices/lag(Oil_Prices) - 1) * 100,
         Exchange_Rates = (Exchange_Rates/lag(Exchange_Rates) - 1) * 100,
         Unemp_Rate = (Unemp_Rate/lag(Unemp_Rate) - 1) *100) 

# remove rows that turns NA because of conversion
data <- data |> 
  filter(!is.na(CPI))
data

# obtain a long version as well
data_long <- data  |> 
  pivot_longer(
    cols = -DATE,
    names_to = "Series",
    values_to = "Values")

#Adding Dummy Variables
data <- data |> 
  mutate(
    dummy_2008Q1 = if_else(DATE == yearmonth("2008 Q1"), 1, 0),
    dummy_2008Q2 = if_else(DATE == yearmonth("2008 Q2"), 1, 0),
    dummy_2008Q3 = if_else(DATE == yearmonth("2008 Q3"), 1, 0),
    dummy_2008Q4 = if_else(DATE == yearmonth("2008 Q4"), 1, 0))
data

# converting to tsibble and obtaining tsibble long version
data_ts <- data |> 
  as_tsibble(index = DATE)
data_ts


data_long_ts <- data_long |> 
  as_tsibble(index = DATE, key = Series)
data_long_ts

# write data sets in the current working directory
write_csv(data, "data2.csv")
write_csv(data_long, "data_long2.csv")

########## Time Series Characteristics ##########
data_long |> 
  ggplot(aes(x = DATE, y = Values, color = Series)) +
  geom_line(show.legend = FALSE) +
  facet_grid(Series ~., scales = "free_y")

# KPSS test results
data_long_ts |> 
  features(Values, features = list(unitroot_kpss))

# Seasonality
data_long_ts |> 
  gg_subseries(Values)

# autocorrelation
data_long_ts |> 
  ACF(Values, lag_max = 12) |> 
  autoplot()

# Decomposition of Dependent Variable
dcmp <- data_ts|>
  model(stl = STL(CPI))

components(dcmp) |>
  autoplot()


## Correlations 
lag_n <- 12
CPI_Wagegrowth <- data_ts |> 
  CCF(CPI, Wage_Growth, lag_max = lag_n) |> 
  rename(lag = lag, Wage_Growth = ccf)
CPI_OilPrices <- data_ts |> 
  CCF(CPI, Oil_Prices, lag_max = lag_n) |> 
  rename(lag = lag, Oil_Prices = ccf)
CPI_PPI <- data_ts |> 
  CCF(CPI, PPI, lag_max = lag_n) |> 
  rename(lag = lag, PPI = ccf)
CPI_ExchangeRates <- data_ts |> 
  CCF(CPI, Exchange_Rates, lag_max = lag_n) |> 
  rename(lag = lag, Exchange_Rates = ccf)
CPI_UnempRates <- data_ts |> 
  CCF(CPI, Unemp_Rate, lag_max = lag_n) |> 
  rename(lag = lag, Unemp_Rate = ccf)


corrs <- CPI_Wagegrowth |> 
  left_join(CPI_OilPrices, by = "lag") |>
  left_join(CPI_PPI, by = "lag") |>
  left_join(CPI_ExchangeRates, by = "lag") |>
  left_join(CPI_UnempRates, by = "lag") |> 
  as_tibble() |> 
  filter(row_number() <= 1+lag_n) |> 
  arrange(desc(lag))
corrs


########## Forecasting Methods ######### 

## specify training and test sets
train <- data_ts |>
  filter(year(DATE) <= 2021)
test <- data_ts |>
  filter(year(DATE) > 2021)

autoplot(train, CPI) +
  autolayer(test, CPI, color= "red")


#### Exploring TSLM-D models

fit_lm <- train |> 
  model(
    lm_1 = TSLM(CPI ~ season() + lag(Oil_Prices,2) + lag(PPI,1) + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4),
    lm_2 = TSLM(CPI ~ season() + lag(Oil_Prices,2) + lag(PPI,1) + lag(Oil_Prices,3) + lag(PPI, 2)+ dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4),
    lm_3 = TSLM(CPI ~ season() + lag(Oil_Prices, 1) + lag(PPI, 3)+ dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4),
    lm_4 = TSLM(CPI ~ season() + lag(Oil_Prices,1) + lag(Oil_Prices, 2) + lag(PPI, 1) + lag(PPI, 2)+ dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4)
  )

fit_lm
coefficients(fit_lm)
glance(fit_lm) |> 
  arrange(AICc) |> 
  select(.model, AICc)

fc_model1 <- fit_lm |> 
  forecast(test)

accuracy(fc_model1, test)

# residuals
fit_lm |> 
  select(lm_2) |> 
  gg_tsresiduals(lag = 12)


#### Exploring TSLM-D-ARIMA models
fit_lm_arima <- train |>
  model(
    lm_arima_1 = ARIMA(CPI ~ season() + lag(Oil_Prices,2) + lag(PPI,1)+ dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4),
    lm_arima_2 = ARIMA(CPI ~ season() + lag(Oil_Prices,2) + lag(PPI,1) 
                       + pdq(0:2, 0, 0:2) + PDQ(0:1, 0, 0) + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4)
  )

fit_lm_arima |> 
  pivot_longer(everything())

coefficients(fit_lm_arima)

glance(fit_lm_arima) |> 
  arrange(AICc) |> 
  select(.model, AICc)

fit_lm_arima |> 
  select(lm_arima_2) |> 
  report()

#### ARIMA models
fit_arima <- train |> model(
  auto = ARIMA(CPI ~ dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4),  
  auto_s = ARIMA(CPI ~ dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4, stepwise = FALSE),  
  arima_p = ARIMA(CPI ~ season() + 0 + pdq(0,0,0) + PDQ(0:3,1,0:3) + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4),
  arima002001 = ARIMA(CPI ~ season() + 0 + pdq(0,0,2) + PDQ(0,0,1) + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4),
  arima202202 = ARIMA(CPI ~ season() + 0 + pdq(2,0,0) + PDQ(0,0,1) + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4))  

# tabulate the proposed specifications
fit_arima |> 
  pivot_longer(everything(), 
               names_to = "Model name",
               values_to = "Orders")

# Using AICs to pick the winning among ARIMA
glance(fit_arima) |> 
  arrange(AICc) |> 
  select(.model, AICc)


### Comparing three models on the test set
fit_train <- train |>
  model(
    lm_1       = TSLM(CPI ~ season() + 0 + lag(Oil_Prices,2) + lag(PPI,1) + lag(Oil_Prices,3) + lag(PPI, 2)+ dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4),
    lm_arima_2 = ARIMA(CPI ~ season() + 0 + lag(Oil_Prices,2) + lag(PPI,1) 
                       + pdq(0:2, 0, 0:2) + PDQ(0:1, 0, 0)+ dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4),
    auto       = ARIMA(CPI ~ season() + 0 + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4)
  )

fc <- fit_train |> 
  forecast(test)

accuracy(fc, test) |> 
  arrange(RMSE)

############# Out-of-sample forecasts #############
# Estimate the winner model using the full sample
fit_full <- data_ts |>
  model(
    lm_1 = ARIMA(CPI ~ season() + 0 + lag(Oil_Prices,2) + lag(PPI,1) + lag(Oil_Prices,3) + lag(PPI, 2) 
                 + pdq(0, 0, 0) + PDQ(0, 0, 0)+ dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4))

# Residual diagnostics
fit_full |> 
  select(lm_1) |> 
  gg_tsresiduals()


# generate future values for predictors
Oil_Prices_fit <- data_ts |>
  model(
    ARIMA(Oil_Prices)
  )
Oil_Prices_forecast <- Oil_Prices_fit |>
  forecast(h = "12 months")
Oil_Prices_forecast


PPI_fit <- data_ts |>
  model(
    ARIMA(PPI)
  )
PPI_forecast <- PPI_fit |>
  forecast(h = "12 months")
PPI_forecast

Oil_Prices_Forecast_values <- Oil_Prices_forecast$.mean |> as.vector()
PPI_Forecast_values <- PPI_forecast$.mean |> as.vector()


data_future <- new_data(data_ts, 12) |> 
  mutate(
    Oil_Prices = Oil_Prices_Forecast_values, 
    PPI = PPI_Forecast_values,
    dummy_2008Q1 = 0, 
    dummy_2008Q2 = 0,
    dummy_2008Q3 = 0,
    dummy_2008Q4 = 0,
    dummy_2020Q1 = 0,  
    dummy_2020Q2 = 0
  )
data_future


# Forecasting next 12 months:
fc_full <- fit_full |> 
  forecast(new_data = data_future)

fc_full

fc_full |> 
  autoplot(data_ts)





