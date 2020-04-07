
library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(viridis)
library(viridisLite)
library(ggpubr)

#### Lodi Tmax and Tmin weather records ####

lodi_weather_records_1985_2015<- read.csv("data/records_weather_lodi_1985_2015.csv")

str(lodi_weather_records_1985_2015)


lodi_weather_records_1985_2015$DATE <- as.Date(lodi_weather_records_1985_2015$DATE)
lodi_weather_records_1985_2015$DATE <-data.frame(date = lodi_weather_records_1985_2015$DATE,
                 year = as.numeric(format(lodi_weather_records_1985_2015$DATE, format = "%Y")),
                 month = as.numeric(format(lodi_weather_records_1985_2015$DATE, format = "%m")),
                 day = as.numeric(format(lodi_weather_records_1985_2015$DATE, format = "%d")))


lodi_weather_records_1985_2015_tmax_JJAS <-lodi_weather_records_1985_2015 %>%
  filter(DATE$month %in% (6:9)) %>%
  mutate(month = DATE$month) %>%
  mutate(year = DATE$year) %>%
  mutate (day = DATE$day) %>%
  mutate(date =DATE$date) %>%
  select(STATION, NAME, year, month, day, date, TMAX, TMIN) %>%
  filter(!is.na(TMAX))

str(lodi_weather_records_1985_2015_tmax_JJAS)
  

lodi_weather_records_1985_2015_tmax_JJAS_avg <- lodi_weather_records_1985_2015_tmax_JJAS %>%
  group_by(year) %>%
  summarise(tmax_avg_year = mean(TMAX))

str(lodi_weather_records_1985_2015_tmax_JJAS_avg)

lodi_weather_records_1985_2015_tmax_JJAS_avg_count_n <- lodi_weather_records_1985_2015_tmax_JJAS %>%
  group_by(year) %>%
  tally()

lodi_weather_records_1985_2015_tmax_JJAS_avg <- ggplot(lodi_weather_records_1985_2015_tmax_JJAS_avg, aes(year,tmax_avg_year)) +
  geom_point(alpha =0.6, size=2,stat = "identity") + 
  theme_classic() +
  scale_x_continuous(breaks=seq(1985, 2014,4), limit = c(1985, 2014))+
  geom_smooth(method = "lm", se =FALSE, color = "grey22", linetype ="solid", formula = y ~ x) +
  geom_line()

write.csv(lodi_weather_records_1985_2015_tmax_JJAS_avg_count_n,"data_output/lodi_weather_records_1985_2014_tmax_JJAS_avg_count_n.csv")

ggsave(lodi_weather_records_1985_2015_tmax_JJAS_avg, filename = "figures/lodi_weather_records_1985_2015_tmax_JJAS_avg.pdf", device = cairo_pdf, 
       width = 7, height = 6)
