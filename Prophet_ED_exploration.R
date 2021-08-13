library(tidyverse)
library(readr)
library(lubridate)
library(tsibble)
set.seed(8675309)

## Bring in data and mutate it
data <- tsibbledata::vic_elec

data %>%
  fabletools::autoplot(Demand) +
  geom_smooth()+
  labs(title = "Hourly Electricity Demand In Victoria, Australia",
       x = "Date-Time",
       y = "Hourly Demand (kW)") +
  theme_bw()


library(prophet)
## Create dataframe with appropriate variables 
Elect_dem <- data %>% 
  rename(ds = Time, y = Demand)
## Create Sample and Test Tibbles
Test <- Elect_dem[52441:52608,]
Elect_dem <- Elect_dem[1:52440,]
## Initialize model and add regressor for Temperature
Elect_auto <- prophet() 
Elect_auto <- add_regressor(Elect_auto, 'Temperature')

## Fit Model 
Elect_auto <- fit.prophet(Elect_auto, Elect_dem)
Elect_auto_fcast <- predict(Elect_auto, Test)
prophet_plot_components(Elect_auto,Elect_auto_fcast) ##Cannot force it to theme_bw()
plot(Elect_auto,Elect_auto_fcast, xlabel = "Date-Time", ylabel = "Daily Demand (kW)") +
  labs(title = "Hourly Electricity Demand In Victoria, Australia",
       subtitle = "With Auto-generated Forecast") +
  theme_bw()

## Add test valus to dataframe for easy plotting
Elect_auto_fcast <- Elect_auto_fcast %>% 
  mutate(Demand = Test$y)
## Create plot. This actually catches the shape really well, the magnitude just seems to be off
ggplot(Elect_auto_fcast, aes(ds, Demand)) +
  geom_point() +
  geom_line(aes(y=yhat), color = "red") +
  geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper), color = "orange", alpha = 0.3) + 
  labs(title = "Half-Hourly Electricity Demand In Victoria, Australia",
       subtitle = "With Forecast and 80% Uncertainty Interval",
       xlabel = "Date-Time",
       ylabel = "Daily Demand (kW)" ) +
  theme_bw()

## Can look directly at the varaibles
Elect_auto_fcast %>% select(ds, Demand, yhat, yhat_lower, yhat_upper) %>% knitr::kable()
