# Your Names Goes Here
# Course Project 

# load packages
library(tidyverse)
library(fpp3)
library(GGally)

################## Data Preparation ##################
# read data
Int_Rates <- read_csv("Interest Rates.csv") |> 
  mutate(DATE = yearquarter(DATE)) |> 
  rename(Int_Rates = "BOGZ1FL072052006Q")


RNFI <- read_csv("Real Nonresidential Fixed Investment.csv") |> 
  mutate(DATE = yearquarter(DATE)) |>
  rename(RNFI = "ND000336Q") |> 
  group_by(DATE) |>
  summarise(RNFI = mean(RNFI, na.rm = TRUE)) 


Tech_Inv <- read_csv("Tech Inv.csv") |> 
  mutate(DATE = yearquarter(DATE)) |> 
  rename(Tech_Inv = "UITITI") |> 
  group_by(DATE) |>
  summarise(Tech_Inv = mean(Tech_Inv, na.rm = TRUE)) 

RealEstate_MV <- read_csv("HNOREMV.csv") |> 
  mutate(DATE = yearquarter(DATE)) |> 
  rename(Real_Estate_MV = "HNOREMV") |>
  mutate(Real_Estate_MV = as.numeric(Real_Estate_MV)) |> 
  filter(year(DATE) > 1960)


data <- RNFI |> 
  left_join(Tech_Inv, by = "DATE") |> 
  left_join(Int_Rates, by = "DATE") |> 
  left_join(RealEstate_MV, by = "DATE")

data

# converting data set to percentage form

data <- data |> 
  arrange(DATE) |> 
  mutate(RNFI = 100*(RNFI/lag(RNFI) - 1),
         Int_Rates = 100*(Int_Rates/lag(Int_Rates) - 1),
         Tech_Inv = 100*(Tech_Inv/lag(Tech_Inv) - 1),
         Real_Estate_MV = 100*(Real_Estate_MV/lag(Real_Estate_MV) - 1)
         )
data
# remove rows that turns NA because of conversion
data <- data |> 
  filter(!is.na(RNFI))
data

# obtain a long version as well
data_long <- data  |> 
  pivot_longer(
    cols = -DATE,
    names_to = "Series",
    values_to = "Values")

data <- data |> 
  mutate(
    dummy_2008Q1 = if_else(DATE == yearquarter("2008 Q1"), 1, 0),
    dummy_2008Q2 = if_else(DATE == yearquarter("2008 Q2"), 1, 0),
    dummy_2008Q3 = if_else(DATE == yearquarter("2008 Q3"), 1, 0),
    dummy_2008Q4 = if_else(DATE == yearquarter("2008 Q4"), 1, 0),
    dummy_2020Q1 = if_else(DATE == yearquarter("2020 Q1"), 1, 0),
    dummy_2020Q2 = if_else(DATE == yearquarter("2020 Q2"), 1, 0)
  )
data

# converting to tsibble and obtaining tsibble long version
data_ts <- data |> 
  as_tsibble(index = DATE)

data_long_ts <- data_long |> 
  as_tsibble(index = DATE, key = Series)
data_long_ts


# write data sets in the current working directory

write_csv(data, "data3.csv")
write_csv(data_long, "data_long3.csv")

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
  model(stl = STL(RNFI))

components(dcmp) |>
  autoplot()

## Correlations 
lag_n <- 12
RNFI_IntRates <- data_ts |> 
  CCF(RNFI, Int_Rates, lag_max = lag_n) |> 
  rename(lag = lag, Int_Rates = ccf)
RNFI_TechInv <- data_ts |> 
  CCF(RNFI, Tech_Inv, lag_max = lag_n) |> 
  rename(lag = lag, Tech_Inv = ccf)
RNFI_RealEstateMV <- data_ts |> 
  CCF(RNFI, Real_Estate_MV, lag_max = lag_n) |> 
  rename(lag = lag, Real_Estate_MV = ccf)

corrs <- RNFI_IntRates |> 
  left_join(RNFI_TechInv, by = "lag") |>
  left_join(RNFI_RealEstateMV, by = "lag") |> 
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

autoplot(train, RNFI) +
  autolayer(test, RNFI, color= "red")


#### Exploring TSLM-D models

fit_lm <- train |>
  model(
    lm_1 = TSLM(RNFI ~ season() + lag(Tech_Inv,1) + lag(Real_Estate_MV,1) + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4 + dummy_2020Q1 + dummy_2020Q2),
    lm_2 = TSLM(RNFI ~ season() + lag(Tech_Inv,5) + lag(Real_Estate_MV,2) + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4 + dummy_2020Q1 + dummy_2020Q2),
    lm_3 = TSLM(RNFI ~ season() + lag(Tech_Inv,1) + lag(Tech_Inv,5) + lag(Real_Estate_MV,1) + lag(Real_Estate_MV, 5) + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4 + dummy_2020Q1 + dummy_2020Q2),
    lm_4 = TSLM(RNFI ~ season() + lag(Tech_Inv,1) + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4 + dummy_2020Q1 + dummy_2020Q2),
    lm_5 = TSLM(RNFI ~ season() + lag(Tech_Inv, 1) + lag(Tech_Inv,5) + lag(Tech_Inv,9)+ lag(Real_Estate_MV,1) + lag(Real_Estate_MV, 2) + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4 + dummy_2020Q1 + dummy_2020Q2))
  

fit_lm
coefficients(fit_lm)
glance(fit_lm) |> 
  arrange(AICc) |> 
  select(.model, AICc)

# residuals
fit_lm |> 
  select(lm_5) |> 
  gg_tsresiduals(lag = 12)


#### Exploring TSLM-D-ARIMA models
fit_lm_arima <- train |>
  model(
    lm_arima_1 = ARIMA(RNFI ~ season() + 0 + lag(Tech_Inv,1) + lag(Real_Estate_MV,1) + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4 + dummy_2020Q1 + dummy_2020Q2),
    lm_arima_2 = ARIMA(RNFI ~ season() + 0 + lag(Tech_Inv,5) + lag(Real_Estate_MV,2) + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4 + dummy_2020Q1 + dummy_2020Q2),
    lm_arima_3 = ARIMA(RNFI ~ season() + 0 + lag(Tech_Inv,5) + lag(Real_Estate_MV,2) 
                       + pdq(1, 0, 3) + PDQ(2, 0, 0)+ dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4 + dummy_2020Q1 + dummy_2020Q2),
    lm_arima_4 = ARIMA(RNFI ~ season() + 0 + lag(Tech_Inv,1) + lag(Real_Estate_MV,1)
                       + pdq(1, 0, 2) + PDQ(2, 0, 1)+ dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4 + dummy_2020Q1 + dummy_2020Q2))
  

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
  auto = ARIMA(RNFI ~ dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4   + dummy_2020Q1 + dummy_2020Q2),  
  auto_s = ARIMA(RNFI ~ dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4   + dummy_2020Q1 + dummy_2020Q2, stepwise = FALSE),  
  arima_p = ARIMA(RNFI ~ season() + 0 + pdq(1,0,1) + PDQ(0,0,0)
                  + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4   + dummy_2020Q1 + dummy_2020Q2),
  arima002001 = ARIMA(RNFI ~ season() + 0 + pdq(1,0,2) + PDQ(1,0,1)
                      + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4   + dummy_2020Q1 + dummy_2020Q2),
  arima202202 = ARIMA(RNFI ~ season() + 0 + pdq(1,0,0) + PDQ(1,0,1)
                      + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4   + dummy_2020Q1 + dummy_2020Q2))

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
    lm_1       = TSLM(RNFI ~ season() + 0 + lag(Tech_Inv,1) + lag(Real_Estate_MV,1) + lag(Tech_Inv, 5) + lag(Real_Estate_MV, 5) 
                      + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4 + dummy_2020Q1 + dummy_2020Q2),
    lm_arima_1 = ARIMA(RNFI ~ season() + 0 + lag(Tech_Inv,1) + lag(Real_Estate_MV,1) + lag(Tech_Inv, 5)
                       + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4  + dummy_2020Q1 + dummy_2020Q2),
    auto       = ARIMA(RNFI ~ season() + 0 +  
                       dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4  + dummy_2020Q1 + dummy_2020Q2)
  )

fc <- fit_train |> 
  forecast(test)

accuracy(fc, test) |> 
  arrange(RMSE)

############# Out-of-sample forecasts #############
# Estimate the winner model using the full sample
fit_full <- data_ts |>
  model(
    lm_arima_1 = ARIMA(RNFI ~ season()  + lag(Tech_Inv,1) + lag(Real_Estate_MV,1) + lag(Tech_Inv, 5) + 
                   pdq(0,0,0) + PDQ(0,0,0) 
                 + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4   + dummy_2020Q1 + dummy_2020Q2)
  )

# Residual diagnostics
fit_full |> 
  select(lm_arima_1) |> 
  gg_tsresiduals()


# generate future values for predictors
RealEstate_MV_fit <- data_ts |>
  model(
    ARIMA(Real_Estate_MV)
  )
RealEstate_MV_forecast <- RealEstate_MV_fit |>
  forecast(h = 4)
RealEstate_MV_forecast

TechInv_fit <- data_ts |>
  model(
    ARIMA(Tech_Inv)
  )
TechInv_forecast <- TechInv_fit |>
  forecast(h = 4)
TechInv_forecast


RealEstate_MV_forecast_values <- RealEstate_MV_forecast$.mean |>  as.vector()
TechInv_forecast_values <- TechInv_forecast$.mean |> as.vector()


data_future <- new_data(data_ts, 4) |> 
  mutate(
    Real_Estate_MV = RealEstate_MV_forecast_values, 
    Tech_Inv = TechInv_forecast_values,
    dummy_2008Q1 = 0, 
    dummy_2008Q2 = 0,
    dummy_2008Q3 = 0,
    dummy_2008Q4 = 0,
    dummy_2020Q1 = 0,  
    dummy_2020Q2 = 0
  )
data_future

# Forecasting next 4 quarters:
fc_full <- fit_full |> 
  forecast(new_data = data_future)

fc_full

fc_full |> 
  autoplot(data_ts)


