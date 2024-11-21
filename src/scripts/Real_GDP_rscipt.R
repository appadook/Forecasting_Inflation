#### Real GDP R-Script file #######

# load packages
library(tidyverse)
library(fpp3)
library(GGally)
library(forecast)

################## Data Preparation ##################
# read data
real_gdp_data <- read_csv("ND000334Q.csv") |> 
  mutate(DATE = yearquarter(DATE)) |> 
  rename(real_gdp = "ND000334Q")

FEDFUNDS <- read_csv("FEDFUNDS.csv") |> 
  mutate(DATE = yearquarter(DATE)) |> 
  group_by(DATE) |> 
  summarise(FEDFUNDS = mean(FEDFUNDS, na.rm = TRUE)) |> 
  rename(IR = "FEDFUNDS")

sentiment <- read_csv("UMCSENT.csv") |> 
  mutate(DATE = yearquarter(DATE)) |> 
  rename(sent = "UMCSENT") |> 
  filter(year(DATE) > 1980) |> 
  group_by(DATE) |> 
  mutate(sent = as.numeric(sent)) |> 
  summarise(sent = mean(sent, na.rm = TRUE)) 
  

Inv <- read_csv("NA000335Q.csv") |> 
  mutate(DATE = yearquarter(DATE)) |> 
  rename(investment = "NA000335Q")

PCE<- read_csv("PCE.csv") |> 
  mutate(DATE = yearquarter(DATE)) |> 
  rename(pce = "PCE") |> 
  mutate(pce = as.numeric(pce))

gov_spend <- read_csv("NA000283Q.csv") |> 
  mutate(DATE = yearquarter(DATE)) |> 
  rename(govt_exp = "NA000283Q") 


data <- real_gdp_data |> 
  left_join(FEDFUNDS, by = "DATE") |> 
  left_join(Inv, by = "DATE") |> 
  left_join(PCE, by = "DATE") |> 
  left_join(gov_spend, by = "DATE" ) |> 
  left_join(sentiment,by = "DATE" )

data

# converting data set to percentage form
data <- data |> 
  arrange(DATE) |> 
  mutate(real_gdp = (real_gdp/lag(real_gdp) - 1) * 100,
         IR = (IR/lag(IR) - 1) * 100,
         investment = (investment/lag(investment) - 1) * 100,
         pce = (pce/lag(pce)-1) *100,
         govt_exp = (govt_exp/lag(govt_exp)-1)*100,
         sent = (sent/lag(sent)-1)*100
         ) 

# remove rows that turns NA because of conversion
data <- data |> 
  filter(!is.na(real_gdp))

# obtain a long version as well
data_long <- data  |> 
  pivot_longer(
    cols = -DATE,
    names_to = "Series",
    values_to = "Values")


## adding dummy variables for 2008 Q1 and 2020 Q1

data <- data %>%
  mutate(
    dummy_2008Q1 = if_else(DATE == yearquarter("2008 Q1"), 1, 0),
    dummy_2008Q2 = if_else(DATE == yearquarter("2008 Q2"), 1, 0),
    dummy_2008Q3 = if_else(DATE == yearquarter("2008 Q3"), 1, 0),
    dummy_2008Q4 = if_else(DATE == yearquarter("2008 Q4"), 1, 0),
    dummy_2020Q1 = if_else(DATE == yearquarter("2020 Q1"), 1, 0),
    dummy_2020Q2 = if_else(DATE == yearquarter("2020 Q2"), 1, 0)
  )

# converting to tsibble and obtaining tsibble long version
data_ts <- data |> 
  as_tsibble(index = DATE)


data_long_ts <- data_long |> 
  as_tsibble(index = DATE, key = Series)

# write data sets in the current working directory
write_csv(data, "data1.csv")
write_csv(data_long, "data_long1.csv")

########## Time Series Characteristics ##########
data_long |> 
  ggplot(aes(x = DATE, y = Values, color = Series)) +
  geom_line(show.legend = FALSE) +
  ggtitle("Time Series Plot of Real GDP and predictors") +
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
  model(stl = STL(real_gdp))

components(dcmp) |>
  autoplot()


## Correlations with Real GDP and it's predictors
lag_n <- 12
real_gdp_ir <- data_ts |> 
  CCF(real_gdp, IR, lag_max = lag_n) |> 
  rename(lag = lag, FED_rate = ccf)
real_gdp_inv <- data_ts |> 
  CCF(real_gdp, investment, lag_max = lag_n) |> 
  rename(lag = lag, investment = ccf)
real_gdp_pce <- data_ts |> 
  CCF(real_gdp, pce, lag_max = lag_n) |> 
  rename(lag = lag, PCE = ccf)
real_gdp_govt_exp <- data_ts |> 
  CCF(real_gdp, govt_exp, lag_max = lag_n) |> 
  rename(lag = lag, govt_exp = ccf)
real_gdp_sent <- data_ts |> 
  CCF(real_gdp, sent, lag_max = lag_n) |> 
  rename(lag = lag, sent = ccf)

corrs <- real_gdp_ir |> 
  left_join(real_gdp_pce, by = "lag") |> 
  left_join(real_gdp_inv, by = "lag") |> 
  left_join(real_gdp_govt_exp, by = "lag") |> 
  left_join(real_gdp_sent, by = "lag") |> 
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

autoplot(train, real_gdp) +
  autolayer(test, real_gdp, color= "red")


#### Exploring TSLM-D models

fit_lm <- train |>
  model(
    lm_1 = TSLM(real_gdp ~ season()  + lag(govt_exp,6) + lag(investment,5) + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4 + dummy_2020Q1 + dummy_2020Q2),
    lm_2 = TSLM(real_gdp ~ season()  + lag(govt_exp,6) + lag(investment,5) + lag(pce,7) + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4  + dummy_2020Q1 + dummy_2020Q2),
    lm_3 = TSLM(real_gdp ~ season()  + lag(govt_exp,7) + lag(investment,5) + lag(govt_exp,6) + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4  + dummy_2020Q1 + dummy_2020Q2),
    lm_4 = TSLM(real_gdp ~ season()  + lag(govt_exp,6) + lag(investment,5) + lag(pce, 7) + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4  + dummy_2020Q1 + dummy_2020Q2),
    lm_5 = TSLM(real_gdp ~ season()  + lag(govt_exp,1) + lag(investment,1) + lag(pce, 2) + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4  + dummy_2020Q1 + dummy_2020Q2),
    lm_6 = TSLM(real_gdp ~ season()  + lag(govt_exp,1) + lag(investment,1) + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4  + dummy_2020Q1 + dummy_2020Q2),
    lm_7 = TSLM(real_gdp ~ season()  + lag(govt_exp,1) + lag(investment,1) + lag(pce,3) + lag(sent, 2) + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4  + dummy_2020Q1 + dummy_2020Q2)
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
  select(lm_6) |> 
  gg_tsresiduals(lag = 12)


#### Exploring TSLM-D-ARIMA models
fit_lm_arima <- train |>
  model(
    lm_arima_1 = ARIMA(real_gdp ~ season()  + lag(govt_exp,7) + lag(investment,5) + lag(govt_exp,6)
                       + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4  + dummy_2020Q1 + dummy_2020Q2),
    lm_arima_2 = ARIMA(real_gdp ~ 0 + season() + pdq(1,0,2) + PDQ(2,0,1) + lag(govt_exp,7) + 
                         lag(investment, 5) + lag(govt_exp, 6)
                       + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4  + dummy_2020Q1 + dummy_2020Q2),
    lm_arima_3 = ARIMA(real_gdp ~ 0 + season() + pdq(1,0,3) + PDQ(2,0,0) + lag(govt_exp,7) + 
                         lag(investment, 5) + lag(govt_exp, 6)
                       + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4  + dummy_2020Q1 + dummy_2020Q2),
    lm_arima_4 = ARIMA(real_gdp ~ season()  + lag(govt_exp,1) + lag(investment,1) + lag(pce, 2)
                       + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4  + dummy_2020Q1 + dummy_2020Q2),
    lm_arima_5 = ARIMA(real_gdp ~ season() + 0 + lag(govt_exp,1) + lag(investment,1) +
                        pdq(0:1,0,0:2) + PDQ(0:2,0,0:2)
                       + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4  + dummy_2020Q1 + dummy_2020Q2)
  )

fit_lm_arima |> 
  pivot_longer(everything())

coefficients(fit_lm_arima)

glance(fit_lm_arima) |> 
  arrange(AICc) |> 
  select(.model, AICc)

fit_lm_arima |> 
  select(lm_arima_1) |> 
  report()

#### ARIMA models
fit_arima <- train |> model(
  auto = ARIMA(real_gdp ~ dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4   + dummy_2020Q1 + dummy_2020Q2),  
  auto_s = ARIMA(real_gdp ~ dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4   + dummy_2020Q1 + dummy_2020Q2, stepwise = FALSE),  
  arima_p = ARIMA(real_gdp ~ season() + 0 + pdq(1,0,1) + PDQ(0,0,0)
                  + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4   + dummy_2020Q1 + dummy_2020Q2),
  arima002001 = ARIMA(real_gdp ~ season() + 0 + pdq(1,0,2) + PDQ(1,0,1)
                      + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4   + dummy_2020Q1 + dummy_2020Q2),
  arima202202 = ARIMA(real_gdp ~ season() + 0 + pdq(1,0,0) + PDQ(1,0,1)
                      + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4   + dummy_2020Q1 + dummy_2020Q2),
  arima111 = ARIMA(real_gdp ~ season() + 0 + pdq(1,1,1)
                   + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4   + dummy_2020Q1 + dummy_2020Q2),
  arima010 = ARIMA(real_gdp ~ season() + 0 + pdq(0,1,0)
                   + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4   + dummy_2020Q1 + dummy_2020Q2),
  arima_seasonal = ARIMA(real_gdp ~ season() + 0 + PDQ(1,0,1)
                         + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4   + dummy_2020Q1 + dummy_2020Q2),
  arima_complex = ARIMA(real_gdp ~ season() + 0 + pdq(2,0,2) + PDQ(2,0,2)
                        + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4   + dummy_2020Q1 + dummy_2020Q2),
  arima_stepwise_off = ARIMA(real_gdp ~ dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4   + dummy_2020Q1 + dummy_2020Q2, stepwise = FALSE, approximation = FALSE),
  arima_non_seasonal = ARIMA(real_gdp ~ season() + 0 + pdq(0,0,2)
                             + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4   + dummy_2020Q1 + dummy_2020Q2),
  arima_seasonal_adjust = ARIMA(real_gdp ~ season() + 0 + pdq(2,0,2) + PDQ(1,0,1)
                                + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4   + dummy_2020Q1 + dummy_2020Q2)
  )  

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
    lm_1       = TSLM(real_gdp ~ season()  + lag(govt_exp,1) + lag(investment,1) + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4 + dummy_2020Q1 + dummy_2020Q2),
    lm_arima_1 = ARIMA(real_gdp ~ season()  + lag(govt_exp,1) + lag(investment,1) + lag(pce, 2)
                       + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4  + dummy_2020Q1 + dummy_2020Q2),
    auto       = ARIMA(real_gdp ~ season() + 
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
    lm_1 = ARIMA(real_gdp ~ season()  + lag(govt_exp,1) + lag(investment,1) + 
                   pdq(0,0,0) + PDQ(0,0,0) 
                 + dummy_2008Q1 + dummy_2008Q2 + dummy_2008Q3 + dummy_2008Q4   + dummy_2020Q1 + dummy_2020Q2)
  )

# Residual diagnostics
fit_full |> 
  select(lm_1) |> 
  gg_tsresiduals()


# generate future values for predictors
gov_exp_fit <- data_ts |>
  model(
    ARIMA(govt_exp)
  )
gov_exp_forecast <- gov_exp_fit |>
  forecast(h = 4)
gov_exp_forecast

inv_fit <- data_ts |>
  model(
    ARIMA(investment)
  )
inv_forecast <- inv_fit |>
  forecast(h = 4)
inv_forecast

gov_exp_forecast_values <- gov_exp_forecast$.mean %>% as.vector()
inv_forecast_values <- inv_forecast$.mean %>% as.vector()


data_future <- new_data(data_ts, 4) |> 
  mutate(
    govt_exp = gov_exp_forecast_values, 
    investment = inv_forecast_values,
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
  autoplot(data_ts, level = NULL)

