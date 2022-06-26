### dsci 5360 week 8 HW

library(tidyverse)
library(fpp3)
library(fable)
library(gghighlight)
library(ggdendro)

## problem 1

# a
lh <- as_tibble(LakeHuron)

# b
T <- length(LakeHuron)
x1 <- seq(T)
x2 <- pmax(0, x1-52)
t <- seq(T+30)

fit <- Arima(LakeHuron, xreg=cbind(x1,x2), order=c(1,0,1))

# c
forecast <- forecast(fit, xreg=cbind(max(x1)+seq(30), max(x2)+seq(30)))
b0 <- coef(fit)["intercept"]
b1 <- coef(fit)["x1"]
b2 <- coef(fit)["x2"]
trend <- ts(b0 + b1*t + b2*pmax(0,t-52),
            start=start(LakeHuron))

plot(forecast)
lines(trend, col='red')
# i think the linear trend is realistic

## problem 2

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
  model(ARIMA(Demand ~ Temperature + I(Temperature^2) + I(Temperature^3) +
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

## problem 3

# a
gas_dhr4 <- us_gasoline %>%
  model(dhr = ARIMA(Barrels ~ PDQ(0, 0, 0) + fourier(K = 4)))
gas_dhr6 <- us_gasoline %>%
  model(dhr = ARIMA(Barrels ~ PDQ(0, 0, 0) + fourier(K = 6)))
gas_dhr8 <- us_gasoline %>%
  model(dhr = ARIMA(Barrels ~ PDQ(0, 0, 0) + fourier(K = 8)))

gas_dhr4 %>%
  forecast(h = "2 years") %>%
  autoplot(us_gasoline) +
  labs(y = "Millions of barrels per day",
       title = "Weekly US gasoline production")
dev.new()
gas_dhr6 %>%
  forecast(h = "2 years") %>%
  autoplot(us_gasoline) +
  labs(y = "Millions of barrels per day",
       title = "Weekly US gasoline production")
dev.new()
gas_dhr8 %>%
  forecast(h = "2 years") %>%
  autoplot(us_gasoline) +
  labs(y = "Millions of barrels per day",
       title = "Weekly US gasoline production")


## problem 4
crime <- read.csv("/Users/dylanfox/OneDrive - Graceland University/Graduate Work/dsci5360-regression_time_series/week08/ucr_crime_1975_2015.csv")

# a
violent_per_100k <- crime %>%
  select(violent_per_100k, year, department_name) %>%
  drop_na() 

spread_homs_per_100k <- violent_per_100k %>%
  spread(department_name, violent_per_100k)  %>%
  glimpse()

violent <- t(spread_homs_per_100k[-1])
violent_dist <- dist(violent, method="euclidean")  
fit <- hclust(violent_dist, method="ward.D")

plot(fit, family="Arial", cex=0.7)
rect.hclust(fit, k=4, border="red")

plot(fit, family="Arial", cex=0.7)
rect.hclust(fit, k=5, border="red")

plot(fit, family="Arial", cex=0.7)
rect.hclust(fit, k=6, border="red")

# 6 clusters looks to be too many, 5 might be better than 4

# b
clustered_data <- cutree(fit, k=4)
clustered_data_tidy <- as.data.frame(as.table(clustered_data)) %>% glimpse()
colnames(clustered_data_tidy) <- c("department_name","cluster")
clustered_data_tidy$department_name <- as.character(clustered_data_tidy$department_name)

joined_clusters <- crime %>%
  inner_join(clustered_data_tidy, by = "department_name") %>%
  glimpse()

table(clustered_data_tidy$cluster)

cluster2 <- joined_clusters %>% filter(cluster == "2") 

ggplot(cluster2, aes(year, violent_per_100k)) +
  geom_line(color="grey") +
  theme_minimal() +
  ylab("violent crimes per 100K") + xlab("") +
  geom_smooth(method="auto",color="red", se=F, size=0.5) +
  facet_wrap(~department_name)

# the crime trend in this cluster contains many horizontal trends. meaning that the crime trend is consistent and rather lower when compared
# to other clusters

# c
clustered_data <- cutree(fit, k=6)
clustered_data_tidy <- as.data.frame(as.table(clustered_data)) %>% glimpse()
colnames(clustered_data_tidy) <- c("department_name","cluster")
clustered_data_tidy$department_name <- as.character(clustered_data_tidy$department_name)

joined_clusters <- crime %>%
  inner_join(clustered_data_tidy, by = "department_name") %>%
  glimpse()

table(clustered_data_tidy$cluster)

cluster3 <- joined_clusters %>% filter(cluster == "3") 

ggplot(cluster3, aes(year, violent_per_100k)) +
  geom_line(color="grey") +
  theme_minimal() +
  ylab("violent crimes per 100K") + xlab("") +
  geom_smooth(method="auto",color="red", se=F, size=0.5) +
  facet_wrap(~department_name)

cluster1 <- joined_clusters %>% filter(cluster == "1") 

ggplot(cluster1, aes(year, violent_per_100k)) +
  geom_line(color="grey") +
  theme_minimal() +
  ylab("violent crimes per 100K") + xlab("") +
  geom_smooth(method="auto",color="red", se=F, size=0.5) +
  facet_wrap(~department_name)
