# 08 03 Clustering and Time Series

#install.packages('gghighlight')
#install.packages('ggdendro')

library(tidyverse)
library(gghighlight)
library(ggdendro)
crime <- read_csv("ucr_crime_1975_2015.csv")

#EDA

head(crime)

ggplot(crime, aes(year, violent_per_100k, color=department_name)) +
  geom_line(stat="identity") +
  ylab("violent crimes per 100K") +
  gghighlight(max(violent_per_100k) > 3800,
              max_highlight = 4,
              use_direct_label = TRUE) +
  theme_minimal() +
  theme(legend.position = 'none')

# Data Cleaning to get the file we need for clustering.  Must remove all NA's
violent_per_100k <- crime %>%
  select(violent_per_100k, year, department_name) %>%
  drop_na() 

spread_homs_per_100k <- violent_per_100k %>%
  spread(department_name, violent_per_100k)  %>%
  glimpse()

# Clustering Distances and fit
violent <- t(spread_homs_per_100k[-1])
violent_dist <- dist(violent, method="euclidean")  
fit <- hclust(violent_dist, method="ward.D")

# Visualization
plot(fit, family="Arial", cex=0.7)
rect.hclust(fit, k=4, border="red")

library(ggdendro)
ggdendrogram(fit, rotate = TRUE, theme_dendro = FALSE) +
  theme_minimal() + xlab("") + ylab("")

#Merging clusters with k = 4
clustered_data <- cutree(fit, k=4)
clustered_data_tidy <- as.data.frame(as.table(clustered_data)) %>% glimpse()
colnames(clustered_data_tidy) <- c("department_name","cluster")
clustered_data_tidy$department_name <- as.character(clustered_data_tidy$department_name)

joined_clusters <- ucr_crime_1975_2015 %>%
  inner_join(clustered_data_tidy, by = "department_name") %>%
  glimpse()

table(clustered_data_tidy$cluster)

# Visualizing

#  3 is the highest violent crime rate

cluster3 <- joined_clusters %>% filter(cluster == "2") 

ggplot(cluster3, aes(year, violent_per_100k)) +
  geom_line(color="grey") +
  theme_minimal() +
  ylab("violent crimes per 100K") + xlab("") +
  geom_smooth(method="auto",color="red", se=F, size=0.5) +
  facet_wrap(~department_name)

clus3year2015 <- cluster3 %>%
  filter(year == "2015") %>%
  summarize(avg = mean(na.omit(violent_per_100k))) %>%
  glimpse()
