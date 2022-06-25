# 08 02 Prophet

library(fpp3)
library(fable)

#install.packages("fable.prophet")
library(fable.prophet)
cement <- aus_production %>%
  filter(year(Quarter) >= 1988)
train <- cement %>%
  filter(year(Quarter) <= 2007)
fit <- train %>%
  model(
    arima = ARIMA(Cement),
    ets = ETS(Cement),
    prophet = prophet(Cement ~ season(period = 4, order = 2,
                                      type = "multiplicative"))
  )

fc <- fit %>% forecast(h = "2 years 6 months")
fc %>% autoplot(cement)

fc %>% accuracy(cement)

### Half hour electricity demand
# Set up data
elec <- vic_elec %>%
  mutate(
    DOW = wday(Date, label = TRUE),
    Working_Day = !Holiday & !(DOW %in% c("Sat", "Sun")),
    Cooling = pmax(Temperature, 18)
  )

elec_newdata <- new_data(elec, 2*48) %>%
  mutate(
    Temperature = tail(elec$Temperature, 2 * 48),
    Date = lubridate::as_date(Time),
    DOW = wday(Date, label = TRUE),
    Working_Day = (Date != "2015-01-01") &
      !(DOW %in% c("Sat", "Sun")),
    Cooling = pmax(Temperature, 18)
  )

###The following fit will take several minutes
fit <- elec %>%
  model(
    prophet(Demand ~ Temperature + Cooling + Working_Day +
              season(period = "day", order = 10) +
              season(period = "week", order = 5) +
              season(period = "year", order = 3))
  )

fit %>%
  components() %>%
  autoplot()

fit %>% gg_tsresiduals()

#Forecast
fc <- fit %>%
  forecast(new_data = elec_newdata)

fc %>%
  autoplot(elec %>% tail(10 * 48)) +
  labs(x = "Date", y = "Demand (MWh)")

fit %>% gg_tsresiduals()
