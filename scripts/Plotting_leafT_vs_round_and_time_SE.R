# New column of datetime for diurnals 2019 
library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)

diurnals_borden_hills_2019 <-read.csv("data_output/diurnals_2019_old_and_new_blocks_cleaned_no_NAs.csv", header = TRUE)

str(diurnals_borden_hills_2019)

diurnals_2019_leaf_temp_C_vs_round <- diurnals_borden_hills_2019 %>%
  mutate(time = hhmmss) %>%
  select(-hhmmss) %>%
  filter(!is.na(time)) %>%
  filter(!is.na(leaf_temp_C)) %>%
  filter(!leaf_temp_C == "-")


diurnals_2019_leaf_temp_C_vs_round$time<- format(strptime(diurnals_2019_leaf_temp_C_vs_round$time,"%H:%M:%S"), format = "%H:%M", tz = "America_Los_Angeles")

diurnals_2019_leaf_temp_C_vs_round$datetime <- paste(diurnals_2019_leaf_temp_C_vs_round$date, " ", diurnals_2019_leaf_temp_C_vs_round$time, sep = "")

glimpse(diurnals_2019_leaf_temp_C_vs_round) 

diurnals_2019_leaf_temp_C_vs_round$datetime <- mdy_hm(diurnals_2019_leaf_temp_C_vs_round$datetime, tz = "UTC")


diurnals_2019_leaf_temp_C_vs_round$round<-format(diurnals_2019_leaf_temp_C_vs_round$round)
diurnals_2019_leaf_temp_C_vs_round$round<-as.numeric(as.factor(diurnals_2019_leaf_temp_C_vs_round$round))

str(diurnals_2019_leaf_temp_C_vs_round$round)

str(diurnals_2019_leaf_temp_C_vs_round)

tz(diurnals_2019_leaf_temp_C_vs_round$datetime)

tz(diurnals_2019_leaf_temp_C_vs_round$time)

diurnals_2019_leaf_temp_C_vs_round$leaf_temp_C <- format(diurnals_2019_leaf_temp_C_vs_round $leaf_temp_C)
diurnals_2019_leaf_temp_C_vs_round$leaf_temp_C<-as.numeric(diurnals_2019_leaf_temp_C_vs_round$leaf_temp_C)

str(diurnals_2019_leaf_temp_C_vs_round$datetime)

str(diurnals_2019_leaf_temp_C_vs_round$leaf_temp_C)


####Plotting lwp vs round and A vs time diurnals Jul 25 (right before first heatwave) ####

diurnals_2019_leaf_temp_C_vs_round_jul_25<- diurnals_2019_leaf_temp_C_vs_round %>%
  filter(!is.na(leaf_temp_C)) %>%
  select(datetime, day, leaf_temp_C, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block) %>%
  filter(day == "206")


str(diurnals_2019_leaf_temp_C_vs_round_jul_25$leaf_temp_C)
se <- function(x) sqrt(var(x)/length(x))


diurnals_2019_leaf_temp_C_vs_round_jul_25_avg_se <-diurnals_2019_leaf_temp_C_vs_round_jul_25 %>%
  group_by(treatment, round) %>%
  summarise(avg_leaf_temp_C = mean(leaf_temp_C), sev = se(leaf_temp_C))

write.csv(diurnals_2019_leaf_temp_C_vs_round_jul_25_avg_se, "data_output/diurnals_2019_leaf_temp_C_vs_round_jul_25_avg_se")

diurnals_2019_leaf_temp_C_vs_round_jul_25_avg_se$treatment<- reorder(diurnals_2019_leaf_temp_C_vs_round_jul_25_avg_se$treatment, diurnals_2019_leaf_temp_C_vs_round_jul_25_avg_se$round)

diurnals_2019_leaf_temp_C_vs_round_jul_25$treatment<- reorder(diurnals_2019_leaf_temp_C_vs_round_jul_25$treatment, diurnals_2019_leaf_temp_C_vs_round_jul_25$round)



diurnals_2019_leaf_temp_C_vs_round_jul_25$Rep<-format(diurnals_2019_leaf_temp_C_vs_round_jul_25$Rep)
as.character(diurnals_2019_leaf_temp_C_vs_round_jul_25$Rep)


#Plot JUL 25
library(wesanderson)
library(ggsci)
library(extrafont)

pd <- position_dodge(0.2)


leaf_temp_Cjul25_round_avg_se<-ggplot(diurnals_2019_leaf_temp_C_vs_round_jul_25_avg_se, aes(round, avg_leaf_temp_C, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg_leaf_temp_C-sev, ymax=avg_leaf_temp_C+sev), width=.2, position=pd, stat = "identity") +
  geom_line(alpha =1, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha =1, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
ylab ("IRT leaf temperature (ºC)") +
  ggtitle( "July 25 2019")+
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Round") +
  theme(axis.title.y = element_text(size=16, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(10,45,10), limits = c (10,45))


ggsave(leaf_temp_Cjul25_round_avg_se, filename = "figures/leaf_temp_Cjul25_round_avg_se.pdf", device = cairo_pdf, 
       width = 8, height = 6)


####Plotting Avs round diurnals Jul 28 (during first heatwave) ####

diurnals_2019_leaf_temp_C_vs_round_jul_28<- diurnals_2019_leaf_temp_C_vs_round %>%
  filter(!is.na(leaf_temp_C)) %>%
  select(datetime, day, leaf_temp_C, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block) %>%
  filter(day == "209")

diurnals_2019_leaf_temp_C_vs_round_jul_28_avg_se <-diurnals_2019_leaf_temp_C_vs_round_jul_28 %>%
  group_by(treatment, round) %>%
  summarise(avg_leaf_temp_C = mean(leaf_temp_C), sev = se(leaf_temp_C))


diurnals_2019_leaf_temp_C_vs_round_jul_28_avg_se$treatment<- reorder(diurnals_2019_leaf_temp_C_vs_round_jul_28_avg_se$treatment, diurnals_2019_leaf_temp_C_vs_round_jul_28_avg_se$round)

diurnals_2019_leaf_temp_C_vs_round_jul_28$treatment<- reorder(diurnals_2019_leaf_temp_C_vs_round_jul_28$treatment, diurnals_2019_leaf_temp_C_vs_round_jul_28$round)


diurnals_2019_leaf_temp_C_vs_round_jul_28$Rep<-format(diurnals_2019_leaf_temp_C_vs_round_jul_28$Rep)
as.character(diurnals_2019_leaf_temp_C_vs_round_jul_28$Rep)


#Plot JUL 28


pd <- position_dodge(0.2)


leaf_temp_Cjul28_round_avg_se<-ggplot(diurnals_2019_leaf_temp_C_vs_round_jul_28_avg_se, aes(round, avg_leaf_temp_C, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg_leaf_temp_C-sev, ymax=avg_leaf_temp_C+sev), width=.2, position=pd, stat = "identity") +
  geom_line(alpha =0.5, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha =0.3, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab (expression(paste ("leaf temperature" , (C))))+
  ggtitle( "leaf temperature vs round diurnal Jul 28 2019")+
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Round") +
  theme(axis.title.y = element_text(size=16, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(10 ,45, 10), limits = c (10,45))


ggsave(leaf_temp_Cjul28_round_avg_se, filename = "figures/leaf_temp_Cjul28_round_avg_se.pdf", device = cairo_pdf, width = 8, height = 6)


#### Plotting leaf_temp_Cvs round  diurnals Aug 1 (after first heatwave) ####

diurnals_2019_leaf_temp_C_vs_round_aug_1<- diurnals_2019_leaf_temp_C_vs_round %>%
  filter(!is.na(leaf_temp_C)) %>%
  select(datetime, day, leaf_temp_C, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block) %>%
  filter(day == "213") 

diurnals_2019_leaf_temp_C_vs_round_aug_1_avg_se <-diurnals_2019_leaf_temp_C_vs_round_aug_1 %>%
  group_by(treatment, round) %>%
  summarise(avg_leaf_temp_C = mean(leaf_temp_C), sev = se(leaf_temp_C))


diurnals_2019_leaf_temp_C_vs_round_aug_1_avg_se$treatment<- reorder(diurnals_2019_leaf_temp_C_vs_round_aug_1_avg_se$treatment, diurnals_2019_leaf_temp_C_vs_round_aug_1_avg_se$round)

diurnals_2019_leaf_temp_C_vs_round_aug_1$treatment<- reorder(diurnals_2019_leaf_temp_C_vs_round_aug_1$treatment, diurnals_2019_leaf_temp_C_vs_round_aug_1$round)


diurnals_2019_leaf_temp_C_vs_round_aug_1$Rep<-format(diurnals_2019_leaf_temp_C_vs_round_aug_1$Rep)
as.character(diurnals_2019_leaf_temp_C_vs_round_aug_1$Rep)


#Plot Aug 1

leaf_temp_Caug1_round_avg_se<-ggplot(diurnals_2019_leaf_temp_C_vs_round_aug_1_avg_se, aes(round, avg_leaf_temp_C, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg_leaf_temp_C-sev, ymax=avg_leaf_temp_C+sev), width=.2, position=pd, stat = "identity") +
  geom_line(alpha =0.5, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha =0.3, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab (expression(paste ("leaf temperature" , (C))))+
  ggtitle( "leaf temperature vs round diurnal Aug 1 2019")+
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Round") +
  theme(axis.title.y = element_text(size=16, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0) +
  scale_y_continuous(breaks=seq(10 ,45, 10), limits = c (10,45))


ggsave(leaf_temp_Caug1_round_avg_se, filename = "figures/leaf_temp_Caug1_round_avg_se.pdf", device = cairo_pdf, 
       width = 8, height = 6)


#### Plotting A vs round diurnals Aug 15 (before second heatwave) ####

diurnals_2019_leaf_temp_C_vs_round_aug_15<- diurnals_2019_leaf_temp_C_vs_round %>%
  filter(!is.na(leaf_temp_C)) %>%
  select(datetime, day, leaf_temp_C, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block) %>%
  filter(day == "227")

diurnals_2019_leaf_temp_C_vs_round_aug_15_avg_se <-diurnals_2019_leaf_temp_C_vs_round_aug_15 %>%
  group_by(treatment, round) %>%
  summarise(avg_leaf_temp_C = mean(leaf_temp_C), sev = se(leaf_temp_C))


diurnals_2019_leaf_temp_C_vs_round_aug_15_avg_se$treatment<- reorder(diurnals_2019_leaf_temp_C_vs_round_aug_15_avg_se$treatment, diurnals_2019_leaf_temp_C_vs_round_aug_15_avg_se$round)

diurnals_2019_leaf_temp_C_vs_round_aug_15$treatment<- reorder(diurnals_2019_leaf_temp_C_vs_round_aug_15$treatment, diurnals_2019_leaf_temp_C_vs_round_aug_15$round)


diurnals_2019_leaf_temp_C_vs_round_aug_15$Rep<-format(diurnals_2019_leaf_temp_C_vs_round_aug_15$Rep)
as.character(diurnals_2019_leaf_temp_C_vs_round_aug_15$Rep)


#Plot Aug 1

leaf_temp_Caug15_round_avg_se<-ggplot(diurnals_2019_leaf_temp_C_vs_round_aug_15_avg_se, aes(round, avg_leaf_temp_C, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg_leaf_temp_C-sev, ymax=avg_leaf_temp_C+sev), width=.2, position=pd, stat = "identity") +
  geom_line(alpha =0.5, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha =0.3, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab (expression(paste ("leaf temperature" , (C))))+
  ggtitle( "leaf temperature vs round diurnal Aug 15 2019")+
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Round") +
  theme(axis.title.y = element_text(size=16, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(10 ,45, 10), limits = c (10,45))


ggsave(leaf_temp_Caug15_round_avg_se, filename = "figures/leaf_temp_Caug15_round_avg_se.pdf", device = cairo_pdf, 
       width = 8, height = 6)

######## Plotting A vs round  diurnals Aug 20 (during second heatwave) ####

pd <-position_dodge(0.2)

diurnals_2019_leaf_temp_C_vs_round_aug_20<- diurnals_2019_leaf_temp_C_vs_round %>%
  filter(!is.na(leaf_temp_C)) %>%
  select(datetime, day, leaf_temp_C, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block) %>%
  filter(day == "232") 


diurnals_2019_leaf_temp_C_vs_round_aug_20_avg_se <-diurnals_2019_leaf_temp_C_vs_round_aug_20 %>%
  group_by(treatment, round) %>%
  summarise(avg_leaf_temp_C = mean(leaf_temp_C), sev = se(leaf_temp_C))


diurnals_2019_leaf_temp_C_vs_round_aug_20_avg_se$treatment<- reorder(diurnals_2019_leaf_temp_C_vs_round_aug_20_avg_se$treatment, diurnals_2019_leaf_temp_C_vs_round_aug_20_avg_se$round)

diurnals_2019_leaf_temp_C_vs_round_aug_20$treatment<- reorder(diurnals_2019_leaf_temp_C_vs_round_aug_20$treatment, diurnals_2019_leaf_temp_C_vs_round_aug_20$round)


diurnals_2019_leaf_temp_C_vs_round_aug_20$Rep<-format(diurnals_2019_leaf_temp_C_vs_round_aug_20$Rep)
as.character(diurnals_2019_leaf_temp_C_vs_round_aug_20$Rep)


#Plot Aug 20
pd<- position_dodge(0.2)

leaf_temp_Caug20_round_avg_se<-ggplot(diurnals_2019_leaf_temp_C_vs_round_aug_20_avg_se, aes(round, avg_leaf_temp_C, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg_leaf_temp_C-sev, ymax=avg_leaf_temp_C+sev), width=.2, position=pd, stat = "identity") +
  geom_line(alpha =0.5, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha =0.3, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab (expression(paste ("leaf temperature" , (C))))+
  ggtitle( "leaf temperature vs round diurnal Aug 20 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Round") +
  theme(axis.title.y = element_text(size=16, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(10 ,45, 10), limits = c (10,45))



ggsave(leaf_temp_Caug20_round_avg_se, filename = "figures/leaf_temp_Caug20_round_avg_se.pdf", device = cairo_pdf, 
       width = 8, height = 6)

######## Plotting A vs round  diurnals sep 5 (after second heatwave) ####


diurnals_2019_leaf_temp_C_vs_round_sep_5<- diurnals_2019_leaf_temp_C_vs_round %>%
  filter(!is.na(leaf_temp_C)) %>%
  select(datetime, day, leaf_temp_C, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block) %>%
  filter(day == "248")

diurnals_2019_leaf_temp_C_vs_round_sep_5_avg_se <-diurnals_2019_leaf_temp_C_vs_round_sep_5 %>%
  group_by(treatment, round) %>%
  summarise(avg_leaf_temp_C = mean(leaf_temp_C), sev = se(leaf_temp_C))


diurnals_2019_leaf_temp_C_vs_round_sep_5_avg_se$treatment<- reorder(diurnals_2019_leaf_temp_C_vs_round_sep_5_avg_se$treatment, diurnals_2019_leaf_temp_C_vs_round_sep_5_avg_se$round)

diurnals_2019_leaf_temp_C_vs_round_sep_5$treatment<- reorder(diurnals_2019_leaf_temp_C_vs_round_sep_5$treatment, diurnals_2019_leaf_temp_C_vs_round_sep_5$round)


diurnals_2019_leaf_temp_C_vs_round_sep_5$Rep<-format(diurnals_2019_leaf_temp_C_vs_round_sep_5$Rep)
as.character(diurnals_2019_leaf_temp_C_vs_round_sep_5$Rep)


#Plot Sep 5

leaf_temp_Csep5_round<-ggplot(diurnals_2019_leaf_temp_C_vs_round_sep_5_avg_se, aes(round, avg_leaf_temp_C, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg_leaf_temp_C-sev, ymax=avg_leaf_temp_C+sev), width=.2, position=pd, stat = "identity") +
  geom_line(alpha =0.5, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha =0.3, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  geom_point(data = diurnals_2019_leaf_temp_C_vs_round_sep_5, mapping = 
               aes(x = round, y = leaf_temp_C, group =treatment, color = treatment, shape = Rep), position =pd) +
  scale_shape_manual(values = c (0, 2, 8), name = "Replication", labels = c("Rep 1", "Rep 2", "Rep 3")) +
  ylab(label = "Stomatal conductance (leaf_temp_C)") +
  ggtitle( " leaf_temp_C vs round diurnal Sep 5 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Round") +
  theme(axis.title.y = element_text(size=16, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-10000,12000,3000), limits = c (-10000,12000))


ggsave(leaf_temp_Csep5_round, filename = "figures/leaf_temp_Csep5_round.pdf", device = cairo_pdf, 
       width = 8, height = 6)


#### Putting A vs round together in one frame

#Heatwaves with new blocks

library(cowplot)

panel_plot_leaf_temp_C_round_2019_avg_se <- plot_grid (leaf_temp_Cjul25_round_avg_se, leaf_temp_Cjul28_round_avg_se, leaf_temp_Caug1_round_avg_se, leaf_temp_Caug15_round_avg_se, leaf_temp_Caug20_round_avg_se, labels=c("Pre-heatwave", "Heatwave", "Post-heatwave","Heatwave", "Post-heatwave", ncol=3, nrow = 2), vjust = 5.8, hjust = -1.5, label_size = 10)

ggsave(panel_plot_leaf_temp_C_round_2019_avg_se, filename = "figures/panel_plot_leaf_temp_C_round_2019_avg_se.pdf", device = cairo_pdf, width = 18, height = 10)



####Leaf temperature vs time not rounds jul 25) #### 

diurnals_2019_leaf_temp_C_vs_time_Jul25<- diurnals_2019_leaf_temp_C_vs_round %>%
  filter(!is.na(leaf_temp_C)) %>%
  select(datetime, day, leaf_temp_C, pixel_number, round, treatment, Rep) %>%
  filter(day == "206") 

diurnals_2019_leaf_temp_C_vs_time_Jul25$interval <-cut(diurnals_2019_leaf_temp_C_vs_time_Jul25$datetime, breaks= "125 min", labels = c ( "07-25-2019 8:30","07-25-2019 11:00", "07-25-2019 13:00", "07-25-2019 17:00", "07-25-2019 17:00"))

diurnals_2019_leaf_temp_C_vs_time_Jul25$interval <- format((diurnals_2019_leaf_temp_C_vs_time_Jul25$interval))

diurnals_2019_leaf_temp_C_vs_time_Jul25$interval<- mdy_hm(as.character(diurnals_2019_leaf_temp_C_vs_time_Jul25$interval))


str(diurnals_2019_leaf_temp_C_vs_time_Jul25$interval)

diurnals_2019_leaf_temp_C_vs_time_Jul25_avg_se <-diurnals_2019_leaf_temp_C_vs_time_Jul25 %>%
  group_by(interval, treatment, round) %>%
  summarise(avg_leaf_temp_C = mean(leaf_temp_C), sev = se(leaf_temp_C))


diurnals_2019_leaf_temp_C_vs_time_Jul25 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2019_leaf_temp_C_vs_time_Jul25$Rep<- format(diurnals_2019_leaf_temp_C_vs_time_Jul25$Rep)
as.character(diurnals_2019_leaf_temp_C_vs_time_Jul25$Rep)
str(diurnals_2019_leaf_temp_C_vs_time_Jul25$Rep)


diurnals_2019_leaf_temp_C_vs_time_Jul25$treatment<- reorder(diurnals_2019_leaf_temp_C_vs_time_Jul25$treatment, diurnals_2019_leaf_temp_C_vs_time_Jul25$datetime) 

diurnals_2019_leaf_temp_C_vs_time_Jul25_avg_se$treatment<- reorder(diurnals_2019_leaf_temp_C_vs_time_Jul25_avg_se$treatment, diurnals_2019_leaf_temp_C_vs_time_Jul25_avg_se$interval)

str(diurnals_2019_leaf_temp_C_vs_time_Jul25_avg_se$interval)
tz(diurnals_2019_leaf_temp_C_vs_time_Jul25_avg_se$interval)
tz(diurnals_2019_leaf_temp_C_vs_time_Jul25$datetime)

# Plot Jul 25 vs time  

pd<- position_dodge(1400)


leaf_temp_Cjul25_time_avg_se<-ggplot(diurnals_2019_leaf_temp_C_vs_time_Jul25_avg_se, aes(interval, avg_leaf_temp_C, group = treatment, color = treatment)) + 
  geom_errorbar(alpha = 1, aes(ymin=avg_leaf_temp_C-sev, ymax=avg_leaf_temp_C+sev), width = 1800, position=pd, stat = "identity") +
  geom_line(alpha =1, size =1, position=pd, linetype = "solid", stat = "identity") +
  geom_point(alpha =1, position=pd, size=2,stat = "identity") + 
  theme_classic()+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab ("IRT leaf temperature (ºC)")+
  ggtitle( "July 25 2019")+
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(18 ,42, 3), limits = c (18 ,42)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  annotate("text", x = as.POSIXct("07-25-2019 13:00:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = 41, label = "*", size = 6)


ggsave(leaf_temp_Cjul25_time_avg_se, filename = "figures/leaf_temp_Cjul25_time_avg_se.pdf", device = cairo_pdf, width = 8, height = 6)

#### Leaf water potentials vs time not rounds wit no data points jul 28 ####

diurnals_2019_leaf_temp_C_vs_time_Jul28<- diurnals_2019_leaf_temp_C_vs_round %>%
  filter(!is.na(leaf_temp_C)) %>%
  select(datetime, day, leaf_temp_C, pixel_number, round, treatment, Rep) %>%
  filter(day == "209") 

diurnals_2019_leaf_temp_C_vs_time_Jul28$interval <-cut(diurnals_2019_leaf_temp_C_vs_time_Jul28$datetime, breaks= "140 min", labels = c ( "07-28-2019 9:00","07-28-2019 11:30", "07-28-2019 14:00", "07-28-2019 17:00","07-28-2019 17:00"))


diurnals_2019_leaf_temp_C_vs_time_Jul28$interval <- format((diurnals_2019_leaf_temp_C_vs_time_Jul28$interval))

diurnals_2019_leaf_temp_C_vs_time_Jul28$interval<- mdy_hm(as.character(diurnals_2019_leaf_temp_C_vs_time_Jul28$interval))


str(diurnals_2019_leaf_temp_C_vs_time_Jul28$interval)
diurnals_2019_leaf_temp_C_vs_time_Jul28_avg_se <-diurnals_2019_leaf_temp_C_vs_time_Jul28 %>%
  group_by(interval, treatment, round) %>%
  summarise(avg_leaf_temp_C = mean(leaf_temp_C), sev = se(leaf_temp_C))

diurnals_2019_leaf_temp_C_vs_time_Jul28 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2019_leaf_temp_C_vs_time_Jul28$Rep<- format(diurnals_2019_leaf_temp_C_vs_time_Jul28$Rep)
as.character(diurnals_2019_leaf_temp_C_vs_time_Jul28$Rep)
str(diurnals_2019_leaf_temp_C_vs_time_Jul28$Rep)


diurnals_2019_leaf_temp_C_vs_time_Jul28$treatment<- reorder(diurnals_2019_leaf_temp_C_vs_time_Jul28$treatment, diurnals_2019_leaf_temp_C_vs_time_Jul28$datetime) 

diurnals_2019_leaf_temp_C_vs_time_Jul28_avg_se$treatment<- reorder(diurnals_2019_leaf_temp_C_vs_time_Jul28_avg_se$treatment, diurnals_2019_leaf_temp_C_vs_time_Jul28_avg_se$interval)

str(diurnals_2019_leaf_temp_C_vs_time_Jul28_avg_se$interval)
tz(diurnals_2019_leaf_temp_C_vs_time_Jul28_avg_se$interval)
tz(diurnals_2019_leaf_temp_C_vs_time_Jul28$datetime)


# Plot Jul 28 vs time  

pd<- position_dodge(1400)


leaf_temp_Cjul28_time_avg_se<-ggplot(diurnals_2019_leaf_temp_C_vs_time_Jul28_avg_se, aes(interval, avg_leaf_temp_C, group = treatment, color = treatment)) + 
  geom_errorbar(alpha = 1, aes(ymin=avg_leaf_temp_C-sev, ymax=avg_leaf_temp_C+sev), width = 1800, position=pd, stat = "identity") +
  geom_line(alpha =1, size =1, position=pd, linetype = "solid", stat = "identity") +
  geom_point(alpha =1, position=pd, size=2,stat = "identity") + 
  theme_classic()+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab ("IRT leaf temperature (ºC)") +
  ggtitle( "July 28 2019")+
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(18 ,42, 3), limits = c (18 ,42)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  annotate("text", x = as.POSIXct("07-28-2019 14:00:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = 41, label = "*", size = 6) +
  annotate("text", x = as.POSIXct("07-28-2019 17:00:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = 41, label = "*", size = 6)

ggsave(leaf_temp_Cjul28_time_avg_se, filename = "figures/leaf_temp_Cjul28_time_avg_se.pdf", device = cairo_pdf, width = 8, height = 6)


#### A vs time not rounds aug1 no all datapoints ####

diurnals_2019_leaf_temp_C_vs_time_aug1<- diurnals_2019_leaf_temp_C_vs_round %>%
  filter(!is.na(leaf_temp_C)) %>%
  select(datetime, day, leaf_temp_C, pixel_number, round, treatment, Rep) %>%
  filter(day == "213")

diurnals_2019_leaf_temp_C_vs_time_aug1$interval <-cut(diurnals_2019_leaf_temp_C_vs_time_aug1$datetime, breaks= "140 min", labels = c ("08-01-2019 8:30","08-01-2019 10:40", "08-01-2019 13:30", "08-01-2019 17:00", "08-01-2019 17:00"))


diurnals_2019_leaf_temp_C_vs_time_aug1$interval <- format((diurnals_2019_leaf_temp_C_vs_time_aug1$interval))

diurnals_2019_leaf_temp_C_vs_time_aug1$interval<- mdy_hm(as.character(diurnals_2019_leaf_temp_C_vs_time_aug1$interval))


str(diurnals_2019_leaf_temp_C_vs_time_aug1$interval)
diurnals_2019_leaf_temp_C_vs_time_aug1_avg_se <-diurnals_2019_leaf_temp_C_vs_time_aug1 %>%
  group_by(interval, treatment, round) %>%
  summarise(avg_leaf_temp_C = mean(leaf_temp_C), sev = se(leaf_temp_C))

diurnals_2019_leaf_temp_C_vs_time_aug1 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2019_leaf_temp_C_vs_time_aug1$Rep<- format(diurnals_2019_leaf_temp_C_vs_time_aug1$Rep)
as.character(diurnals_2019_leaf_temp_C_vs_time_aug1$Rep)
str(diurnals_2019_leaf_temp_C_vs_time_aug1$Rep)


diurnals_2019_leaf_temp_C_vs_time_aug1$treatment<- reorder(diurnals_2019_leaf_temp_C_vs_time_aug1$treatment, diurnals_2019_leaf_temp_C_vs_time_aug1$datetime) 

diurnals_2019_leaf_temp_C_vs_time_aug1_avg_se$treatment<- reorder(diurnals_2019_leaf_temp_C_vs_time_aug1_avg_se$treatment, diurnals_2019_leaf_temp_C_vs_time_aug1_avg_se$interval)

str(diurnals_2019_leaf_temp_C_vs_time_aug1_avg_se$interval)
tz(diurnals_2019_leaf_temp_C_vs_time_aug1_avg_se$interval)
tz(diurnals_2019_leaf_temp_C_vs_time_aug1$datetime)


# Plot aug 1 vs time  

pd<- position_dodge(1400)


leaf_temp_Caug1_time_avg_se<-ggplot(diurnals_2019_leaf_temp_C_vs_time_aug1_avg_se, aes(interval, avg_leaf_temp_C, group = treatment, color = treatment)) + 
  geom_errorbar(alpha = 1, aes(ymin=avg_leaf_temp_C-sev, ymax=avg_leaf_temp_C+sev), width = 1800, position=pd, stat = "identity") +
  geom_line(alpha =1, size =1, position=pd, linetype = "solid", stat = "identity") +
  geom_point(alpha =1, position=pd, size=2,stat = "identity") + 
  theme_classic()+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab ("IRT leaf temperature (ºC)") +
  ggtitle( "Aug 1 2019")+
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(18 ,42, 3), limits = c (18 ,42)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  annotate("text", x = as.POSIXct("08-01-2019 17:00:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = 41, label = "*", size = 6) 

ggsave(leaf_temp_Caug1_time_avg_se, filename = "figures/leaf_temp_Caug1_time_avg_se.pdf", device = cairo_pdf, width = 8, height = 6)

#### Leaf water potentials vs time not rounds aug15 ####

diurnals_2019_leaf_temp_C_vs_time_aug15<- diurnals_2019_leaf_temp_C_vs_round %>%
  filter(!is.na(leaf_temp_C)) %>%
  select(datetime, day, leaf_temp_C, pixel_number, round, treatment, Rep) %>%
  filter(day == "227")

diurnals_2019_leaf_temp_C_vs_time_aug15$interval <-cut(diurnals_2019_leaf_temp_C_vs_time_aug15$datetime, breaks= "140 min", labels = c ( "08-15-2019 9:00","08-15-2019 11:15", "08-15-2019 13:30", "08-15-2019 17:00", "08-15-2019 17:00"))



diurnals_2019_leaf_temp_C_vs_time_aug15$interval <- format((diurnals_2019_leaf_temp_C_vs_time_aug15$interval))

diurnals_2019_leaf_temp_C_vs_time_aug15$interval<- mdy_hm(as.character(diurnals_2019_leaf_temp_C_vs_time_aug15$interval))


str(diurnals_2019_leaf_temp_C_vs_time_aug15$interval)
diurnals_2019_leaf_temp_C_vs_time_aug15_avg_se <-diurnals_2019_leaf_temp_C_vs_time_aug15 %>%
  group_by(interval, treatment, round) %>%
  summarise(avg_leaf_temp_C = mean(leaf_temp_C), sev = se(leaf_temp_C))


diurnals_2019_leaf_temp_C_vs_time_aug15 %>%
  group_by(interval, treatment, round) %>%
  tally()


diurnals_2019_leaf_temp_C_vs_time_aug15$Rep<- format(diurnals_2019_leaf_temp_C_vs_time_aug15$Rep)
as.character(diurnals_2019_leaf_temp_C_vs_time_aug15$Rep)
str(diurnals_2019_leaf_temp_C_vs_time_aug15$Rep)


diurnals_2019_leaf_temp_C_vs_time_aug15$treatment<- reorder(diurnals_2019_leaf_temp_C_vs_time_aug15$treatment, diurnals_2019_leaf_temp_C_vs_time_aug15$datetime) 

diurnals_2019_leaf_temp_C_vs_time_aug15_avg_se$treatment<- reorder(diurnals_2019_leaf_temp_C_vs_time_aug15_avg_se$treatment, diurnals_2019_leaf_temp_C_vs_time_aug15_avg_se$interval)

str(diurnals_2019_leaf_temp_C_vs_time_aug15_avg_se$interval)
tz(diurnals_2019_leaf_temp_C_vs_time_aug15_avg_se$interval)
tz(diurnals_2019_leaf_temp_C_vs_time_aug15$datetime)


# Plot aug 1 vs time  

pd<- position_dodge(1400)


leaf_temp_Caug15_time_avg_se<-ggplot(diurnals_2019_leaf_temp_C_vs_time_aug15_avg_se, aes(interval, avg_leaf_temp_C, group = treatment, color = treatment)) + 
  geom_errorbar(alpha = 1, aes(ymin=avg_leaf_temp_C-sev, ymax=avg_leaf_temp_C+sev), width = 1800, position=pd, stat = "identity") +
  geom_line(alpha =1, size =1, position=pd, linetype = "solid", stat = "identity") +
  geom_point(alpha =1, position=pd, size=2,stat = "identity") + 
  theme_classic()+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab ("IRT leaf temperature (ºC)") +
  ggtitle( "August 15 2019")+
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(18 ,42, 3), limits = c (18 ,42)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  annotate("text", x = as.POSIXct("08-15-2019 11:05:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = 41, label = "*", size = 6) +
  annotate("text", x = as.POSIXct("08-15-2019 13:25:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = 41, label = "*", size = 6) +
  annotate("text", x = as.POSIXct("08-15-2019 17:00:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = 41, label = "*", size = 6)

ggsave(leaf_temp_Caug15_time_avg_se, filename = "figures/leaf_temp_Caug15_time_avg_se.pdf", device = cairo_pdf, width = 8, height = 6)

#### A vs time not rounds aug20 ####

diurnals_2019_leaf_temp_C_vs_time_aug20<- diurnals_2019_leaf_temp_C_vs_round %>%
  filter(!is.na(leaf_temp_C)) %>%
  select(datetime, day, leaf_temp_C, pixel_number, round, treatment, Rep) %>%
  filter(day == "232")

diurnals_2019_leaf_temp_C_vs_time_aug20$interval <-cut(diurnals_2019_leaf_temp_C_vs_time_aug20$datetime, breaks= "130 min", labels = c ( "08-20-2019 9:00","08-20-2019 11:15", "08-20-2019 13:30", "08-20-2019 17:00", "08-20-2019 17:00"))



diurnals_2019_leaf_temp_C_vs_time_aug20$interval <- format((diurnals_2019_leaf_temp_C_vs_time_aug20$interval))

diurnals_2019_leaf_temp_C_vs_time_aug20$interval<- mdy_hm(as.character(diurnals_2019_leaf_temp_C_vs_time_aug20$interval))


str(diurnals_2019_leaf_temp_C_vs_time_aug20$interval)
diurnals_2019_leaf_temp_C_vs_time_aug20_avg_se <-diurnals_2019_leaf_temp_C_vs_time_aug20 %>%
  group_by(interval, treatment, round) %>%
  summarise(avg_leaf_temp_C = mean(leaf_temp_C), sev = se(leaf_temp_C))

diurnals_2019_leaf_temp_C_vs_time_aug20 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2019_leaf_temp_C_vs_time_aug20$Rep<- format(diurnals_2019_leaf_temp_C_vs_time_aug20$Rep)
as.character(diurnals_2019_leaf_temp_C_vs_time_aug20$Rep)
str(diurnals_2019_leaf_temp_C_vs_time_aug20$Rep)


diurnals_2019_leaf_temp_C_vs_time_aug20$treatment<- reorder(diurnals_2019_leaf_temp_C_vs_time_aug20$treatment, diurnals_2019_leaf_temp_C_vs_time_aug20$datetime) 

diurnals_2019_leaf_temp_C_vs_time_aug20_avg_se$treatment<- reorder(diurnals_2019_leaf_temp_C_vs_time_aug20_avg_se$treatment, diurnals_2019_leaf_temp_C_vs_time_aug20_avg_se$interval)

str(diurnals_2019_leaf_temp_C_vs_time_aug20_avg_se$interval)
tz(diurnals_2019_leaf_temp_C_vs_time_aug20_avg_se$interval)
tz(diurnals_2019_leaf_temp_C_vs_time_aug20$datetime)


# Plot aug 20 vs time  

pd<- position_dodge(1400)


leaf_temp_Caug20_time_avg_se<-ggplot(diurnals_2019_leaf_temp_C_vs_time_aug20_avg_se, aes(interval, avg_leaf_temp_C, group = treatment, color = treatment)) + 
  geom_errorbar(alpha = 1, aes(ymin=avg_leaf_temp_C-sev, ymax=avg_leaf_temp_C+sev), width = 1800, position=pd, stat = "identity") +
  geom_line(alpha =1, size =1, position=pd, linetype = "solid", stat = "identity") +
  geom_point(alpha =1, position=pd, size=2,stat = "identity") + 
  theme_classic()+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab ("IRT leaf temperature (ºC)") +
  ggtitle( "August 20 2019")+
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(18 ,42, 3), limits = c (18 ,42)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M")

ggsave(leaf_temp_Caug20_time_avg_se, filename = "figures/leaf_temp_Caug20_time_avg_se.pdf", device = cairo_pdf, width = 8, height = 6)



#### Putting lwp vs round together in one frame with TIME ####

#Heatwaves with new blocks

library(cowplot)

panel_plot_leaf_temp_C_time_2019_avg_se_ok <- plot_grid (leaf_temp_Cjul25_time_avg_se, leaf_temp_Cjul28_time_avg_se, leaf_temp_Caug1_time_avg_se, leaf_temp_Caug15_time_avg_se, leaf_temp_Caug20_time_avg_se, labels=c("Pre-heatwave", "Heatwave", "Post-heatwave","Heatwave", "Post-heatwave", ncol=3, nrow = 2), vjust = 4, hjust = -1.5, label_size = 12)

ggsave(panel_plot_leaf_temp_C_time_2019_avg_se_ok , filename = "figures/panel_plot_leaf_temp_C_time_2019_avg_se_ok.pdf", device = cairo_pdf, width = 15, height = 8)
