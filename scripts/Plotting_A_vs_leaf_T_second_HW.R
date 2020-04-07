library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)

diurnals_borden_hills_2019 <-read.csv("data_output/diurnals_2019_old_and_new_blocks_cleaned_no_NAs.csv", header = TRUE)

diurnals_borden_hills_2019$round<-format(diurnals_borden_hills_2019$round)
diurnals_borden_hills_2019$round<-as.numeric(as.factor(diurnals_borden_hills_2019$round))

str(diurnals_borden_hills_2019$round)


str(diurnals_borden_hills_2019)

diurnals_2019_A_vs_leafT <- diurnals_borden_hills_2019 %>%
  mutate(time = hhmmss) %>%
  select(-hhmmss) %>%
  filter(!is.na(time)) %>%
  filter(!is.na(leaf_temp_C)) %>%
  filter(!leaf_temp_C == "-")


diurnals_2019_A_vs_leafT$time<- format(strptime(diurnals_2019_A_vs_leafT$time,"%H:%M:%S"), format = "%H:%M", tz = "America_Los_Angeles")

diurnals_2019_A_vs_leafT$datetime <- paste(diurnals_2019_A_vs_leafT$date, " ", diurnals_2019_A_vs_leafT$time, sep = "")

glimpse(diurnals_2019_A_vs_leafT) 

diurnals_2019_A_vs_leafT$datetime <- mdy_hm(diurnals_2019_A_vs_leafT$datetime, tz = "UTC")

str(diurnals_2019_A_vs_leafT)

tz(diurnals_2019_A_vs_leafT$datetime)

tz(diurnals_2019_A_vs_leafT$time)

diurnals_2019_A_vs_leafT$leaf_temp_C <- format(diurnals_2019_A_vs_leafT $leaf_temp_C)
diurnals_2019_A_vs_leafT$leaf_temp_C<-as.numeric(diurnals_2019_A_vs_leafT$leaf_temp_C)

str(diurnals_2019_A_vs_leafT$datetime)

str(diurnals_2019_A_vs_leafT$leaf_temp_C)
####Plotting  A vs IRT leaf T FISRT HW round 2-5 ####

diurnals_2019_A_vs_leafT_second_HW<- diurnals_2019_A_vs_leafT %>%
  filter(!is.na(leaf_temp_C)) %>%
  select(datetime, day, leaf_temp_C, A, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block, BH_Vine) %>%
  filter(day < 233) %>%
  filter(day > 214)

write.csv(diurnals_2019_A_vs_leafT_second_HW,"data_output/diurnals_2019_A_vs_leafT_second_HW")

str(diurnals_2019_A_vs_leafT_second_HW$leaf_temp_C)

se <- function(x) sqrt(var(x)/length(x))


diurnals_2019_A_vs_leafT_second_HW$treatment<-format(diurnals_2019_A_vs_leafT_second_HW$treatment)
as.character(diurnals_2019_A_vs_leafT_second_HW$treatment)

diurnals_2019_A_vs_leafT_second_HW$treatment<- reorder(diurnals_2019_A_vs_leafT_second_HW$treatment, diurnals_2019_A_vs_leafT_second_HW$leaf_temp_C)

str(diurnals_2019_A_vs_leafT_second_HW$treatment)

diurnals_2019_A_vs_leafT_second_HW$Rep<-format(diurnals_2019_A_vs_leafT_second_HW$Rep)
as.character(diurnals_2019_A_vs_leafT_second_HW$Rep)

diurnals_2019_A_vs_leafT_second_HW$treatment<-format(diurnals_2019_A_vs_leafT_second_HW$treatment)
as.character(diurnals_2019_A_vs_leafT_second_HW$treatment)

str(diurnals_2019_A_vs_leafT_second_HW$treatment)



## Plot second HW RESPONSE
A_vs_leafT_IRT_second_HW <-ggplot(diurnals_2019_A_vs_leafT_second_HW, aes(leaf_temp_C, A, group = treatment, color = treatment)) +
  geom_point(alpha =0.6, size = 1.7 ) + 
  stat_smooth(aes(y = A, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.3) +
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab ("Net photosynthesis (mol m⁻² s⁻¹)") +
  ggtitle( " A vs leaf temperature second HW response (R 2-5)") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("IRT Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=14, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14,face = "bold", family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(15,40,5), limits = c (13,45)) +
  scale_y_continuous(breaks=seq(0,30,5), limits = c (0,30))

ggsave(A_vs_leafT_IRT_second_HW, filename = "figures/A_vs_leafT_IRT_second_HW.pdf", device = cairo_pdf, 
       width = 8, height = 6)
####Plotting  A vs IRT leaf T sECOND HW round 3-4 ####

diurnals_2019_A_vs_leafT_second_HW_R_3_4<- diurnals_2019_A_vs_leafT %>%
  filter(!is.na(leaf_temp_C)) %>%
  select(datetime, day, leaf_temp_C, A, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block, BH_Vine) %>%
  filter(day < 233) %>%
  filter(day > 214) %>%
  filter(round > 2 ) %>%
  filter(round < 5)


str(diurnals_2019_A_vs_leafT_second_HW_R_3_4$leaf_temp_C)

diurnals_2019_A_vs_leafT_second_HW_R_3_4$treatment<-format(diurnals_2019_A_vs_leafT_second_HW_R_3_4$treatment)
as.character(diurnals_2019_A_vs_leafT_second_HW_R_3_4$treatment)

diurnals_2019_A_vs_leafT_second_HW_R_3_4$treatment<- reorder(diurnals_2019_A_vs_leafT_second_HW_R_3_4$treatment, diurnals_2019_A_vs_leafT_second_HW_R_3_4$leaf_temp_C)

str(diurnals_2019_A_vs_leafT_second_HW_R_3_4$treatment)

diurnals_2019_A_vs_leafT_second_HW_R_3_4$Rep<-format(diurnals_2019_A_vs_leafT_second_HW_R_3_4$Rep)
as.character(diurnals_2019_A_vs_leafT_second_HW_R_3_4$Rep)

str(diurnals_2019_A_vs_leafT_second_HW_R_3_4$treatment)

diurnals_2019_A_vs_leafT_second_HW_R_3_4$treatment<-format(diurnals_2019_A_vs_leafT_second_HW_R_3_4$treatment)
as.character(diurnals_2019_A_vs_leafT_second_HW_R_3_4$treatment)

## Plot second HW RESPONSE
A_vs_leafT_IRT_second_HW_r_3_4 <-ggplot(diurnals_2019_A_vs_leafT_second_HW_R_3_4, aes(leaf_temp_C, A, group = treatment, color = treatment)) +
  geom_point(alpha =0.6, size = 1.7 ) + 
  stat_smooth(aes(y = A, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.3) +
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab ("Net photosynthesis (mol m⁻² s⁻¹)") +
  ggtitle( " A vs leaf temperature second HW response (R3&4)") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("IRT Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=14, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14,face = "bold", family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(20,40,5), limits = c (20,45)) +
  scale_y_continuous(breaks=seq(0,30,5), limits = c (0,30))

ggsave(A_vs_leafT_IRT_second_HW_r_3_4, filename = "figures/A_vs_leafT_IRT_second_HW_r_3_4.pdf", device = cairo_pdf, 
       width = 8, height = 6)

####Plotting  A vs LICOR leaf T SECOND HW round 2-5 ####

str(diurnals_borden_hills_2019$round)

diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5<- diurnals_borden_hills_2019 %>%
  filter(!is.na(Tleaf)) %>%
  select( day, Tleaf, A, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block, BH_Vine) %>%
  filter(day < 233) %>%
  filter(day > 214) %>%
  filter(round > 1)

str(diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5$Tleaf)


diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5$treatment<-format(diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5$treatment)
as.character(diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5$treatment)

diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5$treatment<- reorder(diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5$treatment, diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5$Tleaf)

str(diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5$treatment)

diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5$Rep<-format(diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5$Rep)
as.character(diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5$Rep)

diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5$treatment<-format(diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5$treatment)
as.character(diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5$treatment)

str(diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5$treatment)


## Plot second HW RESPONSE
A_vs_leafT_licor_second_HW_r_2_5 <-ggplot(diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5, aes(Tleaf, A, group = treatment, color = treatment)) +
  geom_point(alpha =0.5, size = 1.7 ) + 
  stat_smooth(aes(y = A, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.3) +
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab ("Net photosynthesis (mol m⁻² s⁻¹)") +
  ggtitle( " A vs leaf temperature second HW response (R 2-5)") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("LI-COR Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=14, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14,face = "bold", family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(20,50,5), limits = c (20,50)) +
  scale_y_continuous(breaks=seq(0,30,5), limits = c (-1,30))

ggsave(A_vs_leafT_licor_second_HW_r_2_5, filename = "figures/A_vs_leafT_licor_second_HW_r_2_5.pdf", device = cairo_pdf, 
       width = 8, height = 6)


####Plotting  A vs LICOR leaf T FISRT HW round 3-4 ####

diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4<- diurnals_borden_hills_2019 %>%
  filter(!is.na(Tleaf)) %>%
  select( day, Tleaf, A, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block, BH_Vine) %>%
  filter(day < 233) %>%
  filter(day > 214) %>%
  filter(round > 2) %>%
  filter(round < 5)

str(diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4$Tleaf)


diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4$treatment<-format(diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4$treatment)
as.character(diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4$treatment)

diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4$treatment<- reorder(diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4$treatment, diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4$Tleaf)

str(diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4$treatment)

diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4$Rep<-format(diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4$Rep)
as.character(diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4$Rep)

diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4$treatment<-format(diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4$treatment)
as.character(diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4$treatment)

str(diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4$treatment)


## Plot second HW RESPONSE
A_vs_leafT_licor_second_HW_r_3_4 <-ggplot(diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4, aes(Tleaf, A, group = treatment, color = treatment)) +
  geom_point(alpha =0.4, size = 1.7 ) + 
  stat_smooth(aes(y = A, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.6) +
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab ("Net photosynthesis (mol m⁻² s⁻¹)") +
  ggtitle( " A vs leaf temperature second HW response (R3&4)") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("LI-COR Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=14, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14,face = "bold", family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(25,50,5), limits = c (25,50)) +
  scale_y_continuous(breaks=seq(0,30,5), limits = c (0,30))

ggsave(A_vs_leafT_licor_second_HW_r_3_4, filename = "figures/A_vs_leafT_licor_second_HW_r_3_4.pdf", device = cairo_pdf, 
       width = 8, height = 6)
#### pLOTTING WITH AVERAGE temp LICOR and A per vine !!!! ####


diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5<- diurnals_borden_hills_2019 %>%
  filter(!is.na(Tleaf)) %>%
  select( day, Tleaf, A, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block, BH_Vine) %>%
  filter(day < 233) %>%
  filter(day > 214) %>%
  filter(round > 1)

str(diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5$Tleaf)


diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5$treatment<-format(diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5$treatment)
as.character(diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5$treatment)

diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5$treatment<- reorder(diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5$treatment, diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5$Tleaf)

str(diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5$treatment)

diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5$Rep<-format(diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5$Rep)
as.character(diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5$Rep)

diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5$treatment<-format(diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5$treatment)
as.character(diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5$treatment)

str(diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5$treatment)


diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5_avg <-diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5 %>%
  group_by(treatment,pixel_number,BH_Vine, BH_Block, day, round) %>%
  summarize(avg_A = mean(A), avg_Tleaf = mean(Tleaf))

## Plot second HW RESPONSE

A_vs_leafT_licor_second_HW_r_2_5_avg <-ggplot(diurnals_2019_A_vs_leafT_licor_second_HW_r_2_5_avg, aes(avg_Tleaf, avg_A, group = treatment, color = treatment)) +
  geom_point(alpha =0.5, size = 1.7 ) + 
  stat_smooth(aes(y = avg_A, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.3) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~")),
    formula = y ~ x + I(x^2), size = 3.5
  ) +
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab ("Net photosynthesis (A)") +
  ggtitle( " A vs leaf temperature second HW response (R 2-5/vine)") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("LI-COR Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=14, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14,face = "bold", family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(20,50,5), limits = c (20,50)) +
  scale_y_continuous(breaks=seq(0,30,5), limits = c (-1,30)) 

ggsave(A_vs_leafT_licor_second_HW_r_2_5_avg, filename = "figures/A_vs_leafT_licor_second_HW_r_2_5_avg.pdf", device = cairo_pdf, 
       width = 8, height = 6)
