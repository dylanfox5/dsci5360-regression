##### 08 01 Dynamic Regression

#######   Introduction to ARIMA with fable

#install.packages('fpp3')
#install.packages('fable')
library(fpp3)
library(fable)

#EDA US Personal Income
us_change %>%
  pivot_longer(c(Consumption, Income),
               names_to = "var", values_to = "value") %>%
  ggplot(aes(x = Quarter, y = value)) +
  geom_line() +
  facet_grid(vars(var), scales = "free_y") +
  labs(title = "US consumption and personal income",
       y = "Quarterly % change")

#Model
fit <- us_change %>%
  model(ARIMA(Consumption ~ Income))
report(fit)

#residuals
bind_rows(
  `Regression residuals` =
    as_tibble(residuals(fit, type = "regression")),
  `ARIMA residuals` =
    as_tibble(residuals(fit, type = "innovation")),
  .id = "type"
) %>%
  mutate(
    type = factor(type, levels=c(
      "Regression residuals", "ARIMA residuals"))
  ) %>%
  ggplot(aes(x = Quarter, y = .resid)) +
  geom_line() +
  facet_grid(vars(type))

#innovations
fit %>% gg_tsresiduals()
augment(fit) %>%
  features(.innov, ljung_box, dof = 5, lag = 8)

### Forecasting
us_change_future <- new_data(us_change, 8) %>%
  mutate(Income = mean(us_change$Income))
forecast(fit, new_data = us_change_future) %>%
  autoplot(us_change) +
  labs(y = "Percentage change")

#########################################################################
# Electricity and Temperature

vic_elec_daily <- vic_elec %>%
  filter(year(Time) == 2014) %>%
  index_by(Date = date(Time)) %>%
  summarise(
    Demand = sum(Demand) / 1e3,
    Temperature = max(Temperature),
    Holiday = any(Holiday)
  ) %>%
  mutate(Day_Type = case_when(
    Holiday ~ "Holiday",
    wday(Date) %in% 2:6 ~ "Weekday",
    TRUE ~ "Weekend"
  ))

vic_elec_daily %>%
  ggplot(aes(x = Temperature, y = Demand, colour = Day_Type)) +
  geom_point() +
  labs(y = "Electricity demand (GW)",
       x = "Maximum daily temperature")

vic_elec_daily %>%
  pivot_longer(c(Demand, Temperature)) %>%
  ggplot(aes(x = Date, y = value)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") + ylab("")

fit <- vic_elec_daily %>%
  model(ARIMA(Demand ~ Temperature + I(Temperature^2) +
                (Day_Type == "Weekday")))
fit %>% gg_tsresiduals()

vic_elec_future <- new_data(vic_elec_daily, 14) %>%
  mutate(
    Temperature = 26,
    Holiday = c(TRUE, rep(FALSE, 13)),
    Day_Type = case_when(
      Holiday ~ "Holiday",
      wday(Date) %in% 2:6 ~ "Weekday",
      TRUE ~ "Weekend"
    )
  )
forecast(fit, vic_elec_future) %>%
  autoplot(vic_elec_daily) +
  labs(title="Daily electricity demand: Victoria",
       y="GW")

#############################################################################
# Dynamic Harmonic Regression
library(astsa)
x1 = 2*cos(2*pi*1:100*6/100) + 3*sin(2*pi*1:100*6/100)
x2 = 4*cos(2*pi*1:100*10/100) + 5*sin(2*pi*1:100*10/100)
x3 = 6*cos(2*pi*1:100*40/100) + 7*sin(2*pi*1:100*40/100)
x = x1 + x2 + x3
par(mfrow=c(2,2))
tsplot(x1, ylim=c(-10,10), main=expression(omega==6/100~~~A^2==13))
tsplot(x2, ylim=c(-10,10), main=expression(omega==10/100~~~A^2==41))
tsplot(x3, ylim=c(-10,10), main=expression(omega==40/100~~~A^2==85))
tsplot(x, ylim=c(-16,16), main="sum")
par(mfrow=c(1,1))

# Expenditures
aus_cafe <- aus_retail %>%
  filter(
    Industry == "Cafes, restaurants and takeaway food services",
    year(Month) %in% 2004:2018
  ) %>%
  summarise(Turnover = sum(Turnover))

fit <- model(aus_cafe,
             `K = 1` = ARIMA(log(Turnover) ~ fourier(K=1) + PDQ(0,0,0)),
             `K = 2` = ARIMA(log(Turnover) ~ fourier(K=2) + PDQ(0,0,0)),
             `K = 3` = ARIMA(log(Turnover) ~ fourier(K=3) + PDQ(0,0,0)),
             `K = 4` = ARIMA(log(Turnover) ~ fourier(K=4) + PDQ(0,0,0)),
             `K = 5` = ARIMA(log(Turnover) ~ fourier(K=5) + PDQ(0,0,0)),
             `K = 6` = ARIMA(log(Turnover) ~ fourier(K=6) + PDQ(0,0,0))
)

fit %>%
  forecast(h = "2 years") %>%
  autoplot(aus_cafe, level = 95) +
  facet_wrap(vars(.model), ncol = 2) +
  guides(colour = FALSE, fill = FALSE, level = FALSE) +
  geom_label(
    aes(x = yearmonth("2007 Jan"), y = 4250,
        label = paste0("AICc = ", format(AICc))),
    data = glance(fit)
  ) +
  labs(title= "Total monthly eating-out expenditure",
       y="$ billions")

###### US Gasoline Production

gas_dhr <- us_gasoline %>%
  model(dhr = ARIMA(Barrels ~ PDQ(0, 0, 0) + fourier(K = 6)))

gas_dhr %>%
  forecast(h = "2 years") %>%
  autoplot(us_gasoline) +
  labs(y = "Millions of barrels per day",
       title = "Weekly US gasoline production")

summary(gas_dhr)

######### Lagged Predictors
# EDA Insurance Advertising

insurance %>%
  pivot_longer(Quotes:TVadverts) %>%
  ggplot(aes(x = Month, y = value)) +
  geom_line() +
  facet_grid(vars(name), scales = "free_y") +
  labs(y = "", title = "Insurance advertising and quotations")

# Model
fit <- insurance %>%
  # Restrict data so models use same fitting period
  mutate(Quotes = c(NA, NA, NA, Quotes[4:40])) %>%
  # Estimate models
  model(
    lag0 = ARIMA(Quotes ~ pdq(d = 0) + TVadverts),
    lag1 = ARIMA(Quotes ~ pdq(d = 0) +
                   TVadverts + lag(TVadverts)),
    lag2 = ARIMA(Quotes ~ pdq(d = 0) +
                   TVadverts + lag(TVadverts) +
                   lag(TVadverts, 2)),
    lag3 = ARIMA(Quotes ~ pdq(d = 0) +
                   TVadverts + lag(TVadverts) +
                   lag(TVadverts, 2) + lag(TVadverts, 3))
  )

glance(fit)

### Best model lag1

fit_best <- insurance %>%
  model(ARIMA(Quotes ~ pdq(d = 0) +
                TVadverts + lag(TVadverts)))
report(fit_best)

### Forecast

insurance_future <- new_data(insurance, 20) %>%
  mutate(TVadverts = 8)
fit_best %>%
  forecast(insurance_future) %>%
  autoplot(insurance) +
  labs(
    y = "Quotes",
    title = "Forecast quotes with future advertising set to 8"
  )