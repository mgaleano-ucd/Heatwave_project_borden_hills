# New column of datetime for diurnals 2019 
library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)
diurnals_borden_hills_2019 <-read.csv("data_output/diurnals_2019_old_and_new_blocks_cleaned_no_NAs.csv", header = TRUE)

diurnals_2019_leaf_temp_irt_vs_licor  <- diurnals_borden_hills_2019 %>%
  mutate(time = hhmmss) %>%
  select(-hhmmss) %>%
  filter(!is.na(time)) %>%
  filter(!is.na(leaf_temp_C)) %>%
  filter(!leaf_temp_C == "-") %>%
  select(date, time, day, BH_Vine, BH_Block, pixel_number, Tleaf, leaf_temp_C)


diurnals_2019_leaf_temp_irt_vs_licor$time<- format(strptime(diurnals_2019_leaf_temp_irt_vs_licor$time,"%H:%M:%S"), format = "%H:%M", tz = "America_Los_Angeles")

diurnals_2019_leaf_temp_irt_vs_licor$datetime <- paste(diurnals_2019_leaf_temp_irt_vs_licor$date, " ", diurnals_2019_leaf_temp_irt_vs_licor$time, sep = "")

glimpse(diurnals_2019_leaf_temp_irt_vs_licor) 

diurnals_2019_leaf_temp_irt_vs_licor$datetime <- mdy_hm(diurnals_2019_leaf_temp_irt_vs_licor$datetime, tz = "UTC")

str(diurnals_2019_leaf_temp_irt_vs_licor)

tz(diurnals_2019_leaf_temp_irt_vs_licor$datetime)

tz(diurnals_2019_leaf_temp_irt_vs_licor$time)

diurnals_2019_leaf_temp_irt_vs_licor$leaf_temp_C<- format(diurnals_2019_leaf_temp_irt_vs_licor$leaf_temp_C)

diurnals_2019_leaf_temp_irt_vs_licor$leaf_temp_C<-as.numeric(diurnals_2019_leaf_temp_irt_vs_licor$leaf_temp_C)

str(diurnals_2019_leaf_temp_irt_vs_licor$datetime)

str(diurnals_2019_leaf_temp_irt_vs_licor$leaf_temp_C)

write.csv(diurnals_2019_leaf_temp_irt_vs_licor,"data_output/diurnals_2019_leaf_temp_irt_vs_licor.csv")


leafT_irt_vs_licor2019<- ggscatter(diurnals_2019_leaf_temp_irt_vs_licor, x = "Tleaf", y = "leaf_temp_C", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "LI-COR Temperature (ºC)", ylab = "IRT Temperature (ºC)") +
  scale_y_continuous(breaks=seq(15,50,5), limits = c (14,50)) +
  scale_x_continuous(breaks=seq(15,50,5), limits = c (14,50)) +
  theme_classic() +
  geom_point(alpha = 0.1, color = "pink" ) +
  stat_regline_equation(label.y = 45)

cor(diurnals_2019_leaf_temp_irt_vs_licor$Tleaf, diurnals_2019_leaf_temp_irt_vs_licor$leaf_temp_C, method = "pearson")



ggsave(leafT_irt_vs_licor2019 , filename = "figures/leafT_irt_vs_licor2019.pdf", device = cairo_pdf, width = 10, height = 8)




