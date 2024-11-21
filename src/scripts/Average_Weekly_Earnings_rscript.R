# load packages
library(tidyverse)
library(fpp3)
library(GGally)

################## Data Preparation ##################
# read data
avg_w_earnings <- read_csv("CEU0500000011.csv") |> 
  rename(avg_weekly_earnings = "CEU0500000011") |> 
  mutate(DATE = yearmonth(DATE)) 


cpi <- read_csv("CPIAUCNS.csv") |> 
  mutate(DATE = yearmonth(DATE)) |> 
  group_by(DATE) |> 
  rename(CPI_index = "CPIAUCNS")

unemp_rate <- read_csv("UNRATENSA.csv") |> 
  mutate(DATE = yearmonth(DATE)) |> 
  rename(unemp_rate = "UNRATENSA") 

emp_rate <- read_csv("UNRATENSA.csv") |> 
  mutate(DATE = yearmonth(DATE)) |> 
  rename(unemp_rate = "UNRATENSA") |> 
  mutate(emp_rate = 1 - unemp_rate) |> 
  select(-unemp_rate)

industrial_index <- read_csv("IPB50001N.csv") |> 
  mutate(DATE = yearmonth(DATE)) |> 
  rename(index = "IPB50001N") 

## data is not monthly! ####
productivity <- read_csv("PRS85006092.csv") |> 
  mutate(DATE = yearmonth(DATE)) |> 
  rename(Output_per_hour = "PRS85006092") 


data <- avg_w_earnings |> 
  left_join(cpi, by = "DATE") |> 
  left_join(unemp_rate, by = "DATE") |> 
  left_join(emp_rate, by = "DATE") %>% 
  left_join(industrial_index,by = "DATE" )

data

# converting data set to percentage form
data <- data |> 
  arrange(DATE) |> 
  mutate(avg_weekly_earnings = (avg_weekly_earnings/lag(avg_weekly_earnings) - 1) * 100,
         CPI_index = (CPI_index/lag(CPI_index) - 1) * 100,
         unemp_rate = (unemp_rate/lag(unemp_rate) - 1) * 100,
         emp_rate = (emp_rate/lag(emp_rate) - 1) * 100,
         index = (index/lag(index)-1)*100
  ) 

# remove rows that turns NA because of conversion
data <- data |> 
  filter(!is.na(avg_weekly_earnings))

# obtain a long version as well
data_long <- data  |> 
  pivot_longer(
    cols = -DATE,
    names_to = "Series",
    values_to = "Values")

# converting to tsibble and obtaining tsibble long version
data_ts <- data |> 
  as_tsibble(index = DATE)

data_long_ts <- data_long |> 
  as_tsibble(index = DATE, key = Series)

# write data sets in the current working directory
write_csv(data, "data4.csv")
write_csv(data_long, "data_long4.csv")

########## Time Series Characteristics ##########
data_long |> 
  ggplot(aes(x = DATE, y = Values, color = Series)) +
  geom_line(show.legend = FALSE) +
  ggtitle("Time Series Plot of Avg Weekly Earnings and predictors") +
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
  model(stl = STL(avg_weekly_earnings))

components(dcmp) |>
  autoplot()


## Correlations with income and sentiment
lag_n <- 24
awe_cpi <- data_ts |> 
  CCF(avg_weekly_earnings, CPI_index, lag_max = lag_n) |> 
  rename(lag = lag, CPI = ccf)
awe_unemp <- data_ts |> 
  CCF(avg_weekly_earnings, unemp_rate, lag_max = lag_n) |> 
  rename(lag = lag, Unemp_rate = ccf)
awe_emp <- data_ts |> 
  CCF(avg_weekly_earnings, emp_rate, lag_max = lag_n) |> 
  rename(lag = lag, Emp_rate = ccf)
awe_ind_index <- data_ts |> 
  CCF(avg_weekly_earnings, index, lag_max = lag_n) |> 
  rename(lag = lag, Ind_index = ccf)


corrs <- awe_cpi |> 
  left_join(awe_unemp, by = "lag") |> 
  left_join(awe_emp, by = "lag") |> 
  left_join(awe_ind_index, by = "lag") |> 
  as_tibble() |> 
  filter(row_number() <= 1+lag_n) |> 
  arrange(desc(lag))
corrs

## START WORKING FROM HERE TO MAKE CHANGES #####

########## Forecasting Methods ######### 

## specify training and test sets
train <- data_ts |>
  filter(year(DATE) <= 2021)
test <- data_ts |>
  filter(year(DATE) > 2021)

autoplot(train, avg_weekly_earnings) +
  autolayer(test, avg_weekly_earnings, color= "red")


#### Exploring TSLM-D models

fit_lm <- train |>
  model(
    lm_1 = TSLM(avg_weekly_earnings ~ season() + lag(CPI_index,5) + lag(emp_rate,4) + lag(unemp_rate,4)),
    lm_2 = TSLM(avg_weekly_earnings ~ season() + lag(CPI_index,5) + lag(CPI_index,6) + lag(unemp_rate, 4) + lag(emp_rate,4) + lag(emp_rate,7)),
    lm_3 = TSLM(avg_weekly_earnings ~ season() + lag(CPI_index,5)),
    lm_4 = TSLM(avg_weekly_earnings ~ season() + lag(index, 9) + lag(CPI_index,5) + lag(CPI_index,2)),
    lm_5 = TSLM(avg_weekly_earnings ~ season() + lag(unemp_rate, 4)),
    lm_6 = TSLM(avg_weekly_earnings ~ season() + lag(index, 9) + lag(CPI_index,5))
  )

fit_lm
coefficients(fit_lm)
glance(fit_lm) |> 
  arrange(AICc) |> 
  select(.model, AICc)

# residuals
fit_lm |> 
  select(lm_1) |> 
  gg_tsresiduals(lag = 12)


fc_model1 <- fit_lm |>   
  forecast(test)

accuracy(fc_model1, test)


#### Exploring TSLM-D-ARIMA models
fit_lm_arima <- train |>
  model(
    lm_arima_1 = ARIMA(avg_weekly_earnings ~ season() + 0 + lag(CPI_index,5) + lag(emp_rate,4) + lag(unemp_rate,4) +
                         pdq(0:3,0,0:2) + PDQ(0:1,0,0:1)),
    lm_arima_2 = ARIMA(avg_weekly_earnings ~ season() + 0 + lag(CPI_index,5) + lag(emp_rate,4) + lag(unemp_rate,4), 
                       stepwise = FALSE),
    lm_arima_3 = ARIMA(avg_weekly_earnings ~ season() + 0 + lag(CPI_index,5) + lag(emp_rate,4) + lag(unemp_rate,4) +
                         pdq(2:3,0,0:1) + PDQ(0:2,0,0:1)),
    lm_arima_4 = ARIMA(avg_weekly_earnings ~ season() + lag(index, 9) + lag(CPI_index,5))
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
  auto = ARIMA(avg_weekly_earnings ~ season() + 0 ),  
  auto_s = ARIMA(avg_weekly_earnings ~ season() + 0 , stepwise = FALSE), 
  arima_p = ARIMA(avg_weekly_earnings ~ season() + 0 + pdq(0,0,0) + PDQ(0:3,0,0:2)),
  arima1 = ARIMA(avg_weekly_earnings ~ season() + 0 + pdq(0:3,0,0:1) + PDQ(0:1,0,0:1)),
  arima2 = ARIMA(avg_weekly_earnings ~ season() + 0 + pdq(0:2,0,0:2) + PDQ(0:1,0,0:1)),
  arima3 = ARIMA(avg_weekly_earnings ~ season() + 0  +
                       pdq(2:3,0,0:1) + PDQ(0:2,0,0:1))
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
    lm_1     =  TSLM(avg_weekly_earnings ~ season() + lag(index, 9) + lag(CPI_index,5)),
    lm_arima_1 = ARIMA(avg_weekly_earnings ~ season() + lag(index, 9) + lag(CPI_index,5)),
    auto_s = ARIMA(avg_weekly_earnings ~ season() + 0 , stepwise = FALSE)
  )

fc <- fit_train |> 
  forecast(test)

accuracy(fc, test) |> 
  arrange(RMSE)

############# Out-of-sample forecasts #############
# Estimate the winner model using the full sample
fit_full <- data_ts |>
  model(
    auto_s = ARIMA(avg_weekly_earnings ~ season() + 0 , stepwise = FALSE)
  )

# Residual diagnostics
fit_full |> 
  select(auto_s) |> 
  gg_tsresiduals()


# Forecasting next 12 months:
fc_full <- fit_full |> 
  forecast(h = 12)

fc_full

fc_full |> 
  autoplot(data_ts, level = NULL)
