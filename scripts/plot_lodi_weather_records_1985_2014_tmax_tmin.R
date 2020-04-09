
library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(viridis)
library(viridisLite)
library(ggpubr)

#### Lodi Tmax and Tmin weather records LODI station USC00045032####


### JJAS avg Tmax ####

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
  scale_x_continuous(breaks=seq(1984, 2014,5), limit = c(1984, 2014))+
  geom_smooth(method = "lm", se =FALSE, color = "grey22", linetype ="solid", formula = y ~ x) +
  geom_line() +
  scale_y_continuous(breaks = seq (30,33,0.5), limit = c(30,33)) +
  ylab("Average Tmax JJAS (ºC)") +
  xlab("Year") +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) 


write.csv(lodi_weather_records_1985_2015_tmax_JJAS_avg_count_n,"data_output/lodi_weather_records_1985_2014_tmax_JJAS_avg_count_n.csv")

ggsave(lodi_weather_records_1985_2015_tmax_JJAS_avg, filename = "figures/lodi_weather_records_1985_2015_tmax_JJAS_avg.pdf", device = cairo_pdf, 
       width = 7, height = 5)


#### JJAS Average T min  #####

lodi_weather_records_1985_2015_tmin_JJAS <-lodi_weather_records_1985_2015 %>%
filter(DATE$month %in% (6:9)) %>%
  mutate(month = DATE$month) %>%
  mutate(year = DATE$year) %>%
  mutate (day = DATE$day) %>%
  mutate(date =DATE$date) %>%
  select(STATION, NAME, year, month, day, date, TMAX, TMIN) %>%
  filter(!is.na(TMIN))

str(lodi_weather_records_1985_2015_tmin_JJAS)


lodi_weather_records_1985_2015_tmin_JJAS_avg <- lodi_weather_records_1985_2015_tmin_JJAS %>%
  group_by(year) %>%
  summarise(tmin_avg_year = mean(TMIN))

str(lodi_weather_records_1985_2015_tmin_JJAS_avg)

lodi_weather_records_1985_2015_tmin_JJAS_avg_count_n <- lodi_weather_records_1985_2015_tmin_JJAS %>%
  group_by(year) %>%
  tally()

lodi_weather_records_1985_2015_tmin_JJAS_avg <- ggplot(lodi_weather_records_1985_2015_tmin_JJAS_avg, aes(year,tmin_avg_year)) +
  geom_point(alpha =0.6, size=2,stat = "identity") + 
  theme_classic() +
  scale_x_continuous(breaks=seq(1984, 2014,5), limit = c(1984, 2014))+
  geom_smooth(method = "lm", se =FALSE, color = "grey22", linetype ="solid", formula = y ~ x) +
  geom_line() +
  scale_y_continuous(breaks = seq (10,15,1), limit = c(10,15)) +
  ylab("Average Tmin JJAS (ºC)") +
  xlab("Year") +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) 

write.csv(lodi_weather_records_1985_2015_tmin_JJAS_avg_count_n,"data_output/lodi_weather_records_1985_2014_tmin_JJAS_avg_count_n.csv")

ggsave(lodi_weather_records_1985_2015_tmin_JJAS_avg, filename = "figures/lodi_weather_records_1985_2015_tmin_JJAS_avg.pdf", device = cairo_pdf, 
       width = 7, height = 5)


#### Weather data from Stockton airport station ####

#### JJAS tmax avg ####

stockton_weather_records_1989_2019<- read.csv("data/stockton_fire_station_and_airport_weather_data_1985_2020.csv")

str(stockton_weather_records_1989_2019)


stockton_weather_records_1989_2019$DATE <- as.Date(stockton_weather_records_1989_2019$DATE)
stockton_weather_records_1989_2019$DATE <-data.frame(date = stockton_weather_records_1989_2019$DATE,
                                                 year = as.numeric(format(stockton_weather_records_1989_2019$DATE, format = "%Y")),
                                                 month = as.numeric(format(stockton_weather_records_1989_2019$DATE, format = "%m")),
                                                 day = as.numeric(format(stockton_weather_records_1989_2019$DATE, format = "%d")))


stockton_weather_records_1989_2019_tmax_JJAS <-stockton_weather_records_1989_2019 %>%
  filter(DATE$month %in% (6:9)) %>%
  mutate(month = DATE$month) %>%
  mutate(year = DATE$year) %>%
  mutate (day = DATE$day) %>%
  mutate(date =DATE$date) %>%
  select(STATION, NAME, year, month, day, date, TMAX, TMIN) %>%
  filter(!is.na(TMAX))%>%
  filter(NAME == "STOCKTON METROPOLITAN AIRPORT, CA US") %>%
  filter(year >=1989)

str(stockton_weather_records_1989_2019_tmax_JJAS)


stockton_weather_records_1989_2019_tmax_JJAS_avg <- stockton_weather_records_1989_2019_tmax_JJAS %>%
  group_by(year) %>%
  summarise(tmax_avg_year = mean(TMAX))

str(stockton_weather_records_1989_2019_tmax_JJAS_avg)

stockton_weather_records_1989_2019_tmax_JJAS_avg_count_n <- stockton_weather_records_1989_2019_tmax_JJAS %>%
  group_by(year) %>%
  tally()

stockton_weather_records_1989_2019_tmax_JJAS_avg <- ggplot(stockton_weather_records_1989_2019_tmax_JJAS_avg, aes(year,tmax_avg_year)) +
  geom_point(alpha =0.6, size=2,stat = "identity") + 
  theme_classic() +
  scale_x_continuous(breaks=seq(1989, 2019,5), limit = c(1989, 2019))+
  geom_smooth(method = "lm", se =FALSE, color = "grey22", linetype ="solid", formula = y ~ x) +
  geom_line() +
  scale_y_continuous(breaks = seq (31,35.5,0.5), limit = c(31,35.5))

write.csv(stockton_weather_records_1989_2019_tmax_JJAS_avg_count_n,"data_output/lodi_weather_records_1985_2014_tmax_JJAS_avg_count_n.csv")

ggsave(stockton_weather_records_1989_2019_tmax_JJAS_avg, filename = "figures/stockton_weather_records_1989_2019_tmax_JJAS_avg.pdf", device = cairo_pdf, 
       width = 7, height = 6)



####JJAS tmin avg ####


stockton_weather_records_1989_2019_tmin_JJAS <-stockton_weather_records_1989_2019 %>%
  filter(DATE$month %in% (6:9)) %>%
  mutate(month = DATE$month) %>%
  mutate(year = DATE$year) %>%
  mutate (day = DATE$day) %>%
  mutate(date =DATE$date) %>%
  select(STATION, NAME, year, month, day, date, TMAX, TMIN) %>%
  filter(!is.na(TMIN))%>%
  filter(NAME == "STOCKTON METROPOLITAN AIRPORT, CA US") %>%
  filter(year >=1989)

str(stockton_weather_records_1989_2019_tmin_JJAS)


stockton_weather_records_1989_2019_tmin_JJAS_avg <- stockton_weather_records_1989_2019_tmin_JJAS %>%
  group_by(year) %>%
  summarise(tmin_avg_year = mean(TMIN))

str(stockton_weather_records_1989_2019_tmin_JJAS_avg)

stockton_weather_records_1989_2019_tmin_JJAS_avg_count_n <- stockton_weather_records_1989_2019_tmin_JJAS %>%
  group_by(year) %>%
  tally()

stockton_weather_records_1989_2019_tmin_JJAS_avg <- ggplot(stockton_weather_records_1989_2019_tmin_JJAS_avg, aes(year,tmin_avg_year)) +
  geom_point(alpha =0.6, size=2,stat = "identity") + 
  theme_classic() +
  scale_x_continuous(breaks=seq(1989, 2019,5), limit = c(1989, 2019))+
  geom_smooth(method = "lm", se =FALSE, color = "grey22", linetype ="solid", formula = y ~ x) +
  geom_line() +
  scale_y_continuous(breaks = seq (13,16.5,0.5), limit = c(13,16.5))

write.csv(stockton_weather_records_1989_2019_tmin_JJAS_avg_count_n,"data_output/lodi_weather_records_1985_2014_tmin_JJAS_avg_count_n.csv")

ggsave(stockton_weather_records_1989_2019_tmin_JJAS_avg, filename = "figures/stockton_weather_records_1989_2019_tmin_JJAS_avg.pdf", device = cairo_pdf, 
       width = 7, height = 6)

