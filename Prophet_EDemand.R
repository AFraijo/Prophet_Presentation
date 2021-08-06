library(tidyverse)
library(readr)
library(lubridate)
library(tsibble)
set.seed(8675309)

##bring in data and mutate it
data <- tsibbledata::vic_elec
data <- data %>% select(Time, Demand) %>% as_tibble()
data <- data %>% mutate(ds = cut.POSIXt(Time, "day"))
Elect_dem <- data %>% select(ds, Demand) %>% group_by(ds) %>% summarise(y = sum(Demand))
Elect_dem <- Elect_dem %>% ungroup()
Elect_dem <- Elect_dem %>% mutate(ds = ymd(as.character(ds)))
rm(data)


##Initial Plot
Elect_dem %>% as_tsibble(index = ds) %>%
  fabletools::autoplot() +
  labs(title = "Hourly Electricity Demand In Victoria, Australia",
       x = "Date-Time",
       y = "Daily Demand (kW)") +
  theme_bw()

##Auto Prophet
library(prophet)

Elect_auto <- prophet(Elect_dem) ##automatically disables daily seasonality, because data is daily
future <- make_future_dataframe(Elect_auto, periods = 31, freq = 'day') ##predict january, and freq set for daily
Elect_auto_fcast <- predict(Elect_auto, future)
prophet_plot_components(Elect_auto,Elect_auto_fcast) ##Cannot force it to theme_bw()
plot(Elect_auto,Elect_auto_fcast, xlabel = "Date-Time", ylabel = "Daily Demand (kW)") +
  labs(title = "Hourly Electricity Demand In Victoria, Australia",
       subtitle = "With Auto-generated Forecast") +
  theme_bw()
##if we want to see precise numbers we can pull them from the prediction
Elect_auto_fcast %>% select(ds, yhat, yhat_lower, yhat_upper) %>% tail(n=31) %>% knitr::kable()

##Be more verbose with the prophet call
Elect_1 <- prophet(growth = "linear", yearly.seasonality = 'auto', weekly.seasonality = 'auto', daily.seasonality = FALSE)
##Add holidays
Elect_1 <- add_country_holidays(Elect_1, country_name = "AU") ##data is from australia

Elect_1 <- fit.prophet(Elect_1, Elect_dem)
Elect_1_fcast <- predict(Elect_1, future)
prophet_plot_components(Elect_1,Elect_1_fcast) 
plot(Elect_1,Elect_1_fcast, xlabel = "Date-Time", ylabel = "Daily Demand (kW)") +
  labs(title = "Hourly Electricity Demand In Victoria, Australia",
       subtitle = "With Auto-generated Forecast") +
  theme_bw()
##if we want to see precise numbers we can pull them from the prediction
Elect_1_fcast %>% select(ds, yhat, yhat_lower, yhat_upper) %>% tail(n=31) %>% knitr::kable()

##Change seasonality mode
Elect_2 <- prophet(growth = "linear", yearly.seasonality = 'auto', weekly.seasonality = 'auto', daily.seasonality = FALSE,
                   seasonality.mode = 'multiplicative')
Elect_2 <- add_country_holidays(Elect_2, country_name = "AU") ##data is from australia

Elect_2 <- fit.prophet(Elect_2, Elect_dem)
Elect_2_fcast <- predict(Elect_2, future)
prophet_plot_components(Elect_2,Elect_2_fcast) 
plot(Elect_2,Elect_2_fcast, xlabel = "Date-Time", ylabel = "Daily Demand (kW)") +
  labs(title = "Hourly Electricity Demand In Victoria, Australia",
       subtitle = "With Auto-generated Forecast") +
  theme_bw()
##if we want to see precise numbers we can pull them from the prediction
Elect_2_fcast %>% select(ds, yhat, yhat_lower, yhat_upper) %>% tail(n=31) %>% knitr::kable()

##Try mcmc
Elect_3 <- prophet(growth = "linear", yearly.seasonality = 'auto', weekly.seasonality = 'auto', daily.seasonality = FALSE,
                   seasonality.mode = 'multiplicative', mcmc.samples = 1000)
Elect_3 <- add_country_holidays(Elect_3, country_name = "AU") ##data is from australia

Elect_3 <- fit.prophet(Elect_3, Elect_dem)
Elect_3_fcast <- predict(Elect_3, future)
prophet_plot_components(Elect_3,Elect_3_fcast) 
plot(Elect_3,Elect_3_fcast, xlabel = "Date-Time", ylabel = "Daily Demand (kW)") +
  labs(title = "Hourly Electricity Demand In Victoria, Australia",
       subtitle = "With Auto-generated Forecast") +
  theme_bw()
##if we want to see precise numbers we can pull them from the prediction
Elect_3_fcast %>% select(ds, yhat, yhat_lower, yhat_upper) %>% tail(n=31) %>% knitr::kable()