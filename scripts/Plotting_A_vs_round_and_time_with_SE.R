
##### Re grafique sobre tablas del penultimo script. Si hay que hacer de nuevo ver!!!
# New column of datetime for diurnals 2019 
library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)

diurnals_borden_hills_2019 <-read.csv("data_output/diurnals_2019_old_and_new_blocks_cleaned_no_NAs.csv", header = TRUE)

str(diurnals_borden_hills_2019)

diurnals_2019_F_vs_round <- diurnals_borden_hills_2019 %>%
  mutate(time = hhmmss) %>%
  select(-hhmmss) %>%
  filter(!is.na(time))

se <- function(x) sqrt(var(x)/length(x))

se1<- function (x) sd (x)/sqrt(length(x))

diurnals_2019_F_vs_round$time<- format(strptime(diurnals_2019_F_vs_round$time,"%H:%M:%S"), format = "%H:%M", tz = "America_Los_Angeles")

diurnals_2019_F_vs_round$datetime <- paste(diurnals_2019_F_vs_round$date, " ", diurnals_2019_F_vs_round$time, sep = "")

glimpse(diurnals_2019_F_vs_round) 

diurnals_2019_F_vs_round$datetime <- mdy_hm(diurnals_2019_F_vs_round$datetime, tz = "UTC")


diurnals_2019_F_vs_round$round<-format(diurnals_2019_F_vs_round$round)
diurnals_2019_F_vs_round$round<-as.numeric(as.factor(diurnals_2019_F_vs_round$round))

str(diurnals_2019_F_vs_round$round)
str(diurnals_2019_F_vs_round)

tz(diurnals_2019_F_vs_round$datetime)

tz(diurnals_2019_F_vs_round$time)

str(diurnals_2019_F_vs_round$datetime)

####Plotting lwp vs round and A vs time diurnals Jul 25 (right before first heatwave) ####

diurnals_2019_F_vs_round_jul_25<- diurnals_2019_F_vs_round %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block) %>%
  filter(day == "206")

write.csv(diurnals_2019_F_vs_round_jul_25,"data_output/diurnals_2019_A_vs_round_jul_25")

diurnals_2019_F_vs_round_jul_25_avg_se <-diurnals_2019_F_vs_round_jul_25 %>%
  group_by(treatment, round) %>%
  summarise(avg_A = mean(A), sev = se(A))

diurnals_2019_F_vs_round_jul_25 %>%
  group_by(treatment, round) %>%
  tally()


diurnals_2019_F_vs_round_jul_25_avg_se$treatment<- reorder(diurnals_2019_F_vs_round_jul_25_avg_se$treatment, diurnals_2019_F_vs_round_jul_25_avg_se$round)

diurnals_2019_F_vs_round_jul_25$treatment<- reorder(diurnals_2019_F_vs_round_jul_25$treatment, diurnals_2019_F_vs_round_jul_25$round)


diurnals_2019_F_vs_round_jul_25$Rep<-format(diurnals_2019_F_vs_round_jul_25$Rep)
as.character(diurnals_2019_F_vs_round_jul_25$Rep)

write.csv(diurnals_2019_F_vs_round_jul_25_avg_se,"data_output/diurnals_2019_F_vs_round_jul_25_avg_se.csv")
#Plot JUL 25
library(wesanderson)
library(ggsci)
library(extrafont)

pd <- position_dodge(0.2)


Ajul25_round_avg_se<-ggplot(diurnals_2019_F_vs_round_jul_25_avg_se, aes(round, avg_A, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg_A-sev, ymax=avg_A+sev), width=.2, position=pd, stat = "identity") +
  geom_line(alpha =0.5, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha =0.3, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab(label = "Net photosynthesis (A)") +
  ggtitle( " A vs round diurnal Jul 25 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Round") +
  theme(axis.title.y = element_text(size=16, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2,28,5))


ggsave(Ajul25_round_avg_se, filename = "figures/Ajul25_round_avg_se.pdf", device = cairo_pdf, 
       width = 8, height = 6)


####Plotting Avs round diurnals Jul 28 (during first heatwave) ####

diurnals_2019_F_vs_round_jul_28<- diurnals_2019_F_vs_round %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block) %>%
  filter(day == "209")

diurnals_2019_F_vs_round_jul_28_avg_se <-diurnals_2019_F_vs_round_jul_28 %>%
  group_by(treatment, round) %>%
  summarise(avg_A = mean(A), sev = se(A))

write.csv(diurnals_2019_F_vs_round_jul_28_avg_se, "data_output/diurnals_2019_A_vs_round_jul_28_avg_se")

diurnals_2019_F_vs_round_jul_28_avg_se$treatment<- reorder(diurnals_2019_F_vs_round_jul_28_avg_se$treatment, diurnals_2019_F_vs_round_jul_28_avg_se$round)

diurnals_2019_F_vs_round_jul_28$treatment<- reorder(diurnals_2019_F_vs_round_jul_28$treatment, diurnals_2019_F_vs_round_jul_28$round)


diurnals_2019_F_vs_round_jul_28$Rep<-format(diurnals_2019_F_vs_round_jul_28$Rep)
as.character(diurnals_2019_F_vs_round_jul_28$Rep)


#Plot JUL 28


pd <- position_dodge(0.2)


Ajul28_round_avg_se<-ggplot(diurnals_2019_F_vs_round_jul_28_avg_se, aes(round, avg_A, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg_A-sev, ymax=avg_A+sev), width=.2, position=pd, stat = "identity") +
  geom_line(alpha =0.5, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha =0.3, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab(label = "Net photosynthesis (A)") +
  ggtitle( " A vs round diurnal Jul 28 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Round") +
  theme(axis.title.y = element_text(size=16, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2,28,5))


ggsave(Ajul28_round_avg_se, filename = "figures/Ajul28_round_avg_se.pdf", device = cairo_pdf, 
       width = 8, height = 6)


#### Plotting A vs round  diurnals Aug 1 (after first heatwave) ####

diurnals_2019_F_vs_round_aug_1<- diurnals_2019_F_vs_round %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block) %>%
  filter(day == "213")

diurnals_2019_F_vs_round_aug_1_avg_se <-diurnals_2019_F_vs_round_aug_1 %>%
  group_by(treatment, round) %>%
  summarise(avg_A = mean(A), sev = se(A))

write.csv(diurnals_2019_F_vs_round_aug_1_avg_se, "data_output/diurnals_2019_A_vs_round_aug_1_avg_se")

diurnals_2019_F_vs_round_aug_1_avg_se$treatment<- reorder(diurnals_2019_F_vs_round_aug_1_avg_se$treatment, diurnals_2019_F_vs_round_aug_1_avg_se$round)

diurnals_2019_F_vs_round_aug_1$treatment<- reorder(diurnals_2019_F_vs_round_aug_1$treatment, diurnals_2019_F_vs_round_aug_1$round)


diurnals_2019_F_vs_round_aug_1$Rep<-format(diurnals_2019_F_vs_round_aug_1$Rep)
as.character(diurnals_2019_F_vs_round_aug_1$Rep)


#Plot Aug 1

Aaug1_round_avg_se<-ggplot(diurnals_2019_F_vs_round_aug_1_avg_se, aes(round, avg_A, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg_A-sev, ymax=avg_A+sev), width=.2, position=pd, stat = "identity") +
  geom_line(alpha =0.5, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha =0.3, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab(label = "Net photosynthesis (A)") +
  ggtitle( " A vs round diurnal Aug 1 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Round") +
  theme(axis.title.y = element_text(size=16, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2,28,5))


ggsave(Aaug1_round_avg_se, filename = "figures/Aaug1_round_avg_se.pdf", device = cairo_pdf, 
       width = 8, height = 6)


#### Plotting A vs round diurnals Aug 15 (before second heatwave) ####

diurnals_2019_F_vs_round_aug_15<- diurnals_2019_F_vs_round %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block) %>%
  filter(day == "227")

diurnals_2019_F_vs_round_aug_15_avg_se <-diurnals_2019_F_vs_round_aug_15 %>%
  group_by(treatment, round) %>%
  summarise(avg_A = mean(A), sev = se(A))

write.csv(diurnals_2019_F_vs_round_aug_15_avg_se, "data_output/diurnals_2019_A_vs_round_aug_15_avg_se")

diurnals_2019_F_vs_round_aug_15_avg_se$treatment<- reorder(diurnals_2019_F_vs_round_aug_15_avg_se$treatment, diurnals_2019_F_vs_round_aug_15_avg_se$round)

diurnals_2019_F_vs_round_aug_15$treatment<- reorder(diurnals_2019_F_vs_round_aug_15$treatment, diurnals_2019_F_vs_round_aug_15$round)


diurnals_2019_F_vs_round_aug_15$Rep<-format(diurnals_2019_F_vs_round_aug_15$Rep)
as.character(diurnals_2019_F_vs_round_aug_15$Rep)


#Plot Aug 15

Aaug15_round_avg_se<-ggplot(diurnals_2019_F_vs_round_aug_15_avg_se, aes(round, avg_A, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg_A-sev, ymax=avg_A+sev), width=.2, position=pd, stat = "identity") +
  geom_line(alpha =0.5, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha =0.3, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab(label = "Net photosynthesis (A)") +
  ggtitle( " A vs round diurnal Aug 15 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Round") +
  theme(axis.title.y = element_text(size=16, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2,28,5))


ggsave(Aaug15_round_avg_se, filename = "figures/Aaug15_round_avg_se.pdf", device = cairo_pdf, 
       width = 8, height = 6)

######## Plotting A vs round  diurnals Aug 20 (during second heatwave) ####


diurnals_2019_F_vs_round_aug_20<- diurnals_2019_F_vs_round %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block) %>%
  filter(day == "232")

diurnals_2019_F_vs_round_aug_20_avg_se <-diurnals_2019_F_vs_round_aug_20 %>%
  group_by(treatment, round) %>%
  summarise(avg_A = mean(A), sev = se(A))

write.csv(diurnals_2019_F_vs_round_aug_20_avg_se, "data_output/diurnals_2019_A_vs_round_aug_20_avg_se")

diurnals_2019_F_vs_round_aug_20_avg_se$treatment<- reorder(diurnals_2019_F_vs_round_aug_20_avg_se$treatment, diurnals_2019_F_vs_round_aug_20_avg_se$round)

diurnals_2019_F_vs_round_aug_20$treatment<- reorder(diurnals_2019_F_vs_round_aug_20$treatment, diurnals_2019_F_vs_round_aug_20$round)


diurnals_2019_F_vs_round_aug_20$Rep<-format(diurnals_2019_F_vs_round_aug_20$Rep)
as.character(diurnals_2019_F_vs_round_aug_20$Rep)


#Plot Aug 20

Aaug20_round_avg_se<-ggplot(diurnals_2019_F_vs_round_aug_20_avg_se, aes(round, avg_A, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg_A-sev, ymax=avg_A+sev), width=.2, position=pd, stat = "identity") +
  geom_line(alpha =0.5, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha =0.3, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab(label = "Net photosynthesis (A)") +
  ggtitle(" A vs round diurnal Aug 20 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Round") +
  theme(axis.title.y = element_text(size=16, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2,28,5))


ggsave(Aaug20_round_avg_se, filename = "figures/Aaug20_round_avg_se.pdf", device = cairo_pdf, 
       width = 8, height = 6)

######## Plotting A vs round  diurnals sep 5 (after second heatwave) ####


diurnals_2019_F_vs_round_sep_5<- diurnals_2019_F_vs_round %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block) %>%
  filter(day == "248")

diurnals_2019_F_vs_round_sep_5_avg_se <-diurnals_2019_F_vs_round_sep_5 %>%
  group_by(treatment, round) %>%
  summarise(avg_A = mean(A), sev = se(A))

write.csv(diurnals_2019_F_vs_round_sep_5_avg_se, "data_output/diurnals_2019_A_vs_round_sep_5_avg_se")

diurnals_2019_F_vs_round_sep_5_avg_se$treatment<- reorder(diurnals_2019_F_vs_round_sep_5_avg_se$treatment, diurnals_2019_F_vs_round_sep_5_avg_se$round)

diurnals_2019_F_vs_round_sep_5$treatment<- reorder(diurnals_2019_F_vs_round_sep_5$treatment, diurnals_2019_F_vs_round_sep_5$round)


diurnals_2019_F_vs_round_sep_5$Rep<-format(diurnals_2019_F_vs_round_sep_5$Rep)
as.character(diurnals_2019_F_vs_round_sep_5$Rep)


#Plot Sep 5

Asep5_round_avg_se<-ggplot(diurnals_2019_F_vs_round_sep_5_avg_se, aes(round, avg_A, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg_A-sev, ymax=avg_A+sev), width=.2, position=pd, stat = "identity") +
  geom_line(alpha =0.5, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha =0.3, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab(label = "Net photosynthesis (A)") +
  ggtitle( " A vs round diurnal Sep 5 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Round") +
  theme(axis.title.y = element_text(size=16, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2,28,5))


ggsave(Asep5_round_avg_se, filename = "figures/Asep5_round_avg_se.pdf", device = cairo_pdf, 
       width = 8, height = 6)

####Plotting A vs round for first diurnals before block ID change - Baseline ####
diurnals_2019_F_vs_round_jul_12<- diurnals_2019_F_vs_round %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block) %>%
  filter(day == "193") 

diurnals_2019_F_vs_round_jul_12_avg_se <-diurnals_2019_F_vs_round_jul_12 %>%
  group_by(treatment, round) %>%
  summarise(avg_A = mean(A), sev = se(A))


diurnals_2019_F_vs_round_jul_12_avg_se$treatment<- reorder(diurnals_2019_F_vs_round_jul_12_avg_se$treatment, diurnals_2019_F_vs_round_jul_12_avg_se$round)

diurnals_2019_F_vs_round_jul_12$treatment<- reorder(diurnals_2019_F_vs_round_jul_12$treatment, diurnals_2019_F_vs_round_jul_12$round)


diurnals_2019_F_vs_round_jul_12$Rep<-format(diurnals_2019_F_vs_round_jul_1$Rep)
as.character(diurnals_2019_F_vs_round_jul_12$Rep)


#Plot Jul 12

Ajul12_round_avg_se<-ggplot(diurnals_2019_F_vs_round_jul_12_avg_se, aes(round, avg_A, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg_A-sev, ymax=avg_A+sev), width=.2, position=pd, stat = "identity") +
  geom_line(alpha =0.5, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha =0.3, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab(label = "Net photosynthesis (A)") +
  ggtitle( " A vs round diurnal Jul 1 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Round") +
  theme(axis.title.y = element_text(size=16, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2,28,5))



ggsave(Ajul12_round_avg_se, filename = "figures/Ajul12_round_avg_se.pdf", device = cairo_pdf, 
       width = 8, height = 6)

#### Plotting A vs round Jul 1

diurnals_2019_F_vs_round_jul_1<- diurnals_2019_F_vs_round %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block) %>%
  filter(day == "182") 

diurnals_2019_F_vs_round_jul_1_avg_se <-diurnals_2019_F_vs_round_jul_1 %>%
  group_by(treatment, round) %>%
  summarise(avg_A = mean(A), sev = se(A))

write.csv(diurnals_2019_F_vs_round_jul_1_avg_se, "data_output/diurnals_2019_F_vs_round_jul_1_avg_se")

diurnals_2019_F_vs_round_jul_1_avg_se$treatment<- reorder(diurnals_2019_F_vs_round_jul_1_avg_se$treatment, diurnals_2019_F_vs_round_jul_1_avg_se$round)

diurnals_2019_F_vs_round_jul_1$treatment<- reorder(diurnals_2019_F_vs_round_jul_1$treatment, diurnals_2019_F_vs_round_jul_1$round)


diurnals_2019_F_vs_round_jul_1$Rep<-format(diurnals_2019_F_vs_round_jul_1$Rep)
as.character(diurnals_2019_F_vs_round_jul_1$Rep)


#Plot Jul 1

Ajul1_round_avg_se<-ggplot(diurnals_2019_F_vs_round_jul_1_avg_se, aes(round, avg_A, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg_A-sev, ymax=avg_A+sev), width=.2, position=pd, stat = "identity") +
  geom_line(alpha =0.5, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha =0.3, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab(label = "Net photosynthesis (A)") +
  ggtitle( " A vs round diurnal Jul 1 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Round") +
  theme(axis.title.y = element_text(size=16, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2, 28, 5))


ggsave(Ajul1_round_avg_se, filename = "figures/Ajul1_round_avg_se.pdf", device = cairo_pdf, 
       width = 8, height = 6)



#### Putting A vs round together in one frame

#Heatwaves with new blocks

library(cowplot)

panel_plot_A_round_2019_avg_se <- plot_grid (Ajul25_round_avg_se, Ajul28_round_avg_se, Aaug1_round_avg_se, Aaug15_round_avg_se, Aaug20_round_avg_se, Asep5_round_avg_se, labels=c("Pre-heatwave", "Heatwave", "Post-heatwave","Heatwave", "Post-heatwave", "Post-heatwave", ncol=3, nrow = 2), vjust = 5.8, hjust = -1.5, label_size = 10)

ggsave(panel_plot_A_round_2019_avg_se  , filename = "figures/ppanel_plot_A_round_2019_avg_se.pdf", device = cairo_pdf, width = 14, height = 8)


# Everyhting all diurnals and blocks

panel_plot_A_round_2019_w_baseline_avg_se<- plot_grid (Ajul1_round_avg_se,Ajul12_round_avg_se, Ajul25_round_avg_se, Ajul28_round_avg_se, Aaug1_round_avg_se, Aaug15_round_avg_se, Aaug20_round_avg_se, Asep5_round_avg_se, labels=c("Baseline","Baseline","Pre-heatwave", "Heatwave", "Post-heatwave","Heatwave", "Post-heatwave", "Post-heatwave", ncol=3, nrow = 3), vjust = 4.2, hjust = -1.5, label_size = 10)

ggsave(panel_plot_A_round_2019_w_baseline_avg_se , filename = "figures/panel_plot_A_round_2019_w_baseline_avg_se.pdf", device = cairo_pdf, width = 14, height = 12)

#### Net photosynthesis vs time not rounds jul 25 and no data points #### 

diurnals_2019_A_vs_time_Jul25<- diurnals_2019_F_vs_round %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, pixel_number, round, treatment, Rep) %>%
  filter(day == "206") 

diurnals_2019_A_vs_time_Jul25$interval <-cut(diurnals_2019_A_vs_time_Jul25$datetime, breaks= "135 min", labels = c ("07-25-2019 6:00", "07-25-2019 8:30","07-25-2019 11:00", "07-25-2019 13:00", "07-25-2019 17:00", "07-25-2019 17:00"))

diurnals_2019_A_vs_time_Jul25$interval <- format((diurnals_2019_A_vs_time_Jul25$interval))

diurnals_2019_A_vs_time_Jul25$interval<- mdy_hm(as.character(diurnals_2019_A_vs_time_Jul25$interval))


str(diurnals_2019_A_vs_time_Jul25$interval)
diurnals_2019_A_vs_time_Jul25_avg_se <-diurnals_2019_A_vs_time_Jul25 %>%
  group_by(interval, treatment, round) %>%
  summarise(avg_A = mean(A), sev = se(A))

diurnals_2019_A_vs_time_Jul25 %>%
  group_by(interval, treatment, round) %>%
  tally()
  
diurnals_2019_A_vs_time_Jul25$Rep<- format(diurnals_2019_A_vs_time_Jul25$Rep)
as.character(diurnals_2019_A_vs_time_Jul25$Rep)
str(diurnals_2019_A_vs_time_Jul25$Rep)


diurnals_2019_A_vs_time_Jul25$treatment<- reorder(diurnals_2019_A_vs_time_Jul25$treatment, diurnals_2019_A_vs_time_Jul25$datetime) 

diurnals_2019_A_vs_time_Jul25_avg_se$treatment<- reorder(diurnals_2019_A_vs_time_Jul25_avg_se$treatment, diurnals_2019_A_vs_time_Jul25_avg_se$interval)

str(diurnals_2019_A_vs_time_Jul25_avg_se$interval)
tz(diurnals_2019_A_vs_time_Jul25_avg_se$interval)
tz(diurnals_2019_A_vs_time_Jul25$datetime)


# Plot Jul 25 vs time  

pd<- position_dodge(1400)


Ajul25_time_avg_se<-ggplot(diurnals_2019_A_vs_time_Jul25_avg_se, aes(interval, avg_A, group = treatment, color = treatment)) + 
  geom_errorbar(alpha = 0.9, aes(ymin=avg_A-sev, ymax=avg_A+sev), width = 1800, position=pd, stat = "identity") +
  geom_line(alpha =0.7, size =1, position=pd, linetype = "solid", stat = "identity") +
  geom_point(alpha =0.6, position=pd, size=2,stat = "identity") + 
  theme_classic()+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab(label = "Net photosynthesis (µmol m⁻² s⁻¹)") +
  ggtitle("July 25 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=16, family = "serif")) +
  theme(axis.title.x = element_text(size=16, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2,20,3), limits = c (-2,20)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  annotate("text", x = as.POSIXct("07-25-2019 11:13:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = 19, label = "*", size = 6) +
  annotate("text", x = as.POSIXct("07-25-2019 13:00:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = 19, label = "*", size = 6) +
  annotate("text", x = as.POSIXct("07-25-2019 17:00:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = 19, label = "*", size = 6) +
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.5, linetype ="dashed")


ggsave(Ajul25_time_avg_se, filename = "figures/Ajul25_time_avg_se.pdf", device = cairo_pdf, width = 8, height = 6 )

#### Leaf water potentials vs time not rounds wit no data points jul 28 ####

diurnals_2019_A_vs_time_Jul28<- diurnals_2019_F_vs_round %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, pixel_number, round, treatment, Rep) %>%
  filter(day == "209") 

diurnals_2019_A_vs_time_Jul28$interval <-cut(diurnals_2019_A_vs_time_Jul28$datetime, breaks= "150 min", labels = c ("07-28-2019 6:00", "07-28-2019 9:00","07-28-2019 11:30", "07-28-2019 14:00", "07-28-2019 17:00", "07-28-2019 17:00"))


diurnals_2019_A_vs_time_Jul28$interval <- format((diurnals_2019_A_vs_time_Jul28$interval))

diurnals_2019_A_vs_time_Jul28$interval<- mdy_hm(as.character(diurnals_2019_A_vs_time_Jul28$interval))


str(diurnals_2019_A_vs_time_Jul28$interval)
diurnals_2019_A_vs_time_Jul28_avg_se <-diurnals_2019_A_vs_time_Jul28 %>%
  group_by(interval, treatment, round) %>%
  summarise(avg_A = mean(A), sev = se(A))

diurnals_2019_A_vs_time_Jul28 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2019_A_vs_time_Jul28$Rep<- format(diurnals_2019_A_vs_time_Jul28$Rep)
as.character(diurnals_2019_A_vs_time_Jul28$Rep)
str(diurnals_2019_A_vs_time_Jul28$Rep)


diurnals_2019_A_vs_time_Jul28$treatment<- reorder(diurnals_2019_A_vs_time_Jul28$treatment, diurnals_2019_A_vs_time_Jul28$datetime) 

diurnals_2019_A_vs_time_Jul28_avg_se$treatment<- reorder(diurnals_2019_A_vs_time_Jul28_avg_se$treatment, diurnals_2019_A_vs_time_Jul28_avg_se$interval)

str(diurnals_2019_A_vs_time_Jul28_avg_se$interval)
tz(diurnals_2019_A_vs_time_Jul28_avg_se$interval)
tz(diurnals_2019_A_vs_time_Jul28$datetime)


# Plot Jul 28 vs time  

pd<- position_dodge(1400)


Ajul28_time_avg_se<-ggplot(diurnals_2019_A_vs_time_Jul28_avg_se, aes(interval, avg_A, group = treatment, color = treatment)) + 
  geom_errorbar(alpha = 0.9, aes(ymin=avg_A-sev, ymax=avg_A+sev), width = 1800, position=pd, stat = "identity") +
  geom_line(alpha =0.7, size =1, position=pd, linetype = "solid", stat = "identity") +
  geom_point(alpha =0.6, position=pd, size=2,stat = "identity") + 
  theme_classic()+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab(label = "Net photosynthesis (µmol m⁻² s⁻¹)") +
  ggtitle("July 28 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=16, family = "serif")) +
  theme(axis.title.x = element_text(size=16, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2,20,3), limits = c (-2,20)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  annotate("text", x = as.POSIXct("07-28-2019 11:30:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = 19, label = "*", size = 6) +
  annotate("text", x = as.POSIXct("07-28-2019 14:15:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = 19, label = "*", size = 6) +
  annotate( "text", x = as.POSIXct("07-28-2019 17:05:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = 19, label = "*", size = 6) +
geom_hline(yintercept = 0, color = "darkgrey", size = 0.5, linetype ="dashed")

ggsave(Ajul28_time_avg_se, filename = "figures/Ajul28_time_avg_se.pdf", device = cairo_pdf, width = 8, height = 6)


#### A vs time not rounds aug1 no all datapoints ####

diurnals_2019_A_vs_time_aug1<- diurnals_2019_F_vs_round %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, pixel_number, round, treatment, Rep) %>%
  filter(day == "213") 

diurnals_2019_A_vs_time_aug1$interval <-cut(diurnals_2019_A_vs_time_aug1$datetime, breaks= "143 min", labels = c ("08-01-2019 5:30", "08-01-2019 8:30","08-01-2019 10:40", "08-01-2019 13:30", "08-01-2019 17:00", "08-01-2019 17:00"))


diurnals_2019_A_vs_time_aug1$interval <- format((diurnals_2019_A_vs_time_aug1$interval))

diurnals_2019_A_vs_time_aug1$interval<- mdy_hm(as.character(diurnals_2019_A_vs_time_aug1$interval))


str(diurnals_2019_A_vs_time_aug1$interval)
diurnals_2019_A_vs_time_aug1_avg_se <-diurnals_2019_A_vs_time_aug1 %>%
  group_by(interval, treatment, round) %>%
  summarise(avg_A = mean(A), sev = se(A))

diurnals_2019_A_vs_time_aug1 %>%
  group_by(treatment, round) %>%
  tally()

diurnals_2019_A_vs_time_aug1$Rep<- format(diurnals_2019_A_vs_time_aug1$Rep)
as.character(diurnals_2019_A_vs_time_aug1$Rep)
str(diurnals_2019_A_vs_time_aug1$Rep)


diurnals_2019_A_vs_time_aug1$treatment<- reorder(diurnals_2019_A_vs_time_aug1$treatment, diurnals_2019_A_vs_time_aug1$datetime) 

diurnals_2019_A_vs_time_aug1_avg_se$treatment<- reorder(diurnals_2019_A_vs_time_aug1_avg_se$treatment, diurnals_2019_A_vs_time_aug1_avg_se$interval)

str(diurnals_2019_A_vs_time_aug1_avg_se$interval)
tz(diurnals_2019_A_vs_time_aug1_avg_se$interval)
tz(diurnals_2019_A_vs_time_aug1$datetime)


# Plot aug 1 vs time  

pd<- position_dodge(1400)


Aaug1_time_avg_se<-ggplot(diurnals_2019_A_vs_time_aug1_avg_se, aes(interval, avg_A, group = treatment, color = treatment)) + 
  geom_errorbar(alpha = 0.9, aes(ymin=avg_A-sev, ymax=avg_A+sev), width = 1800, position=pd, stat = "identity") +
  geom_line(alpha =0.7, size =1, position=pd, linetype = "solid", stat = "identity") +
  geom_point(alpha =0.6, position=pd, size=2,stat = "identity") + 
  theme_classic()+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab(label = "Net photosynthesis (µmol m⁻² s⁻¹)") +
  ggtitle("August 1 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=16, family = "serif")) +
  theme(axis.title.x = element_text(size=16, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2,20,3), limits = c (-2,20)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  annotate( "text", x = as.POSIXct("08-01-2019 13:30:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = 19.5, label = "*", size = 6) +
  annotate( "text", x = as.POSIXct("08-01-2019 17:05:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = 19.5, label = "*", size = 6)+
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.5, linetype ="dashed")
 


ggsave(Aaug1_time_avg_se, filename = "figures/Aaug1_time_avg_se.pdf", device = cairo_pdf, width = 8, height = 6)

#### A vs time not rounds aug15 ####

diurnals_2019_A_vs_time_aug15<- diurnals_2019_F_vs_round %>%
  filter(!is.na(A))%>%
  select(datetime, day, A, pixel_number, round, treatment, Rep) %>%
  filter(day == "227") 


diurnals_2019_A_vs_time_aug15$interval <-cut(diurnals_2019_A_vs_time_aug15$datetime, breaks= "139 min", labels = c ("08-15-2019 6:00", "08-15-2019 9:00","08-15-2019 11:15", "08-15-2019 13:30", "08-15-2019 17:00", "08-15-2019 17:00"))


diurnals_2019_A_vs_time_aug15$interval <- format((diurnals_2019_A_vs_time_aug15$interval))

diurnals_2019_A_vs_time_aug15$interval<- mdy_hm(as.character(diurnals_2019_A_vs_time_aug15$interval))


str(diurnals_2019_A_vs_time_aug15$interval)
diurnals_2019_A_vs_time_aug15_avg_se <-diurnals_2019_A_vs_time_aug15 %>%
  group_by(interval, treatment, round) %>%
  summarise(avg_A = mean(A), sev = se(A))

diurnals_2019_A_vs_time_aug15 %>%
  group_by(interval, treatment, round) %>%
tally()

diurnals_2019_A_vs_time_aug15$treatment<- reorder(diurnals_2019_A_vs_time_aug15$treatment, diurnals_2019_A_vs_time_aug15$datetime) 

diurnals_2019_A_vs_time_aug15_avg_se$treatment<- reorder(diurnals_2019_A_vs_time_aug15_avg_se$treatment, diurnals_2019_A_vs_time_aug15_avg_se$interval)

str(diurnals_2019_A_vs_time_aug15_avg_se$interval)
tz(diurnals_2019_A_vs_time_aug15_avg_se$interval)

tz(diurnals_2019_A_vs_time_aug15$datetime)
diurnals_2019_A_vs_time_aug15$Rep<- format(diurnals_2019_A_vs_time_aug15$Rep)
as.character(diurnals_2019_A_vs_time_aug15$Rep)
str(diurnals_2019_A_vs_time_aug15$Rep)


# Plot aug 1 vs time  

pd<- position_dodge(1400)


Aaug15_time_avg_se<-ggplot(diurnals_2019_A_vs_time_aug15_avg_se, aes(interval, avg_A, group = treatment, color = treatment)) + 
  geom_errorbar(alpha = 0.9, aes(ymin=avg_A-sev, ymax=avg_A+sev), width = 1800, position=pd, stat = "identity") +
  geom_line(alpha =0.7, size =1, position=pd, linetype = "solid", stat = "identity") +
  geom_point(alpha =0.6, position=pd, size=2,stat = "identity") + 
  theme_classic()+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab(label = "Net photosynthesis (µmol m⁻² s⁻¹)") +
  ggtitle("August 15 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=16, family = "serif")) +
  theme(axis.title.x = element_text(size=16, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2,20,3), limits = c (-2,20)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  annotate("text", x = as.POSIXct("08-15-2019 11:25:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = 19, label = "*", size = 6) +
  annotate("text", x = as.POSIXct("08-15-2019 13:40:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = 19, label = "*", size = 6) +
  annotate( "text", x = as.POSIXct("08-15-2019 17:15:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = 19, label = "*", size = 6)+
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.5, linetype ="dashed")

ggsave(Aaug15_time_avg_se, filename = "figures/Aaug15_time_avg_se.pdf", device = cairo_pdf, width = 8, height = 6)

#### A vs time not rounds aug20 ####

diurnals_2019_A_vs_time_aug20<- diurnals_2019_F_vs_round %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, pixel_number, round, treatment, Rep) %>%
  filter(day == "232") 

diurnals_2019_A_vs_time_aug20$interval <-cut(diurnals_2019_A_vs_time_aug20$datetime, breaks= "147 min", labels = c ("08-20-2019 6:00", "08-20-2019 9:00","08-20-2019 11:15", "08-20-2019 13:30", "08-20-2019 17:00", "08-20-2019 17:00"))



diurnals_2019_A_vs_time_aug20$interval <- format((diurnals_2019_A_vs_time_aug20$interval))

diurnals_2019_A_vs_time_aug20$interval<- mdy_hm(as.character(diurnals_2019_A_vs_time_aug20$interval))


str(diurnals_2019_A_vs_time_aug20$interval)
diurnals_2019_A_vs_time_aug20_avg_se <-diurnals_2019_A_vs_time_aug20 %>%
  group_by(interval, treatment, round) %>%
  summarise(avg_A = mean(A), sev = se(A))

diurnals_2019_A_vs_time_aug20 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2019_A_vs_time_aug20$Rep<- format(diurnals_2019_A_vs_time_aug20$Rep)
as.character(diurnals_2019_A_vs_time_aug20$Rep)
str(diurnals_2019_A_vs_time_aug20$Rep)


diurnals_2019_A_vs_time_aug20$treatment<- reorder(diurnals_2019_A_vs_time_aug20$treatment, diurnals_2019_A_vs_time_aug20$datetime) 

diurnals_2019_A_vs_time_aug20_avg_se$treatment<- reorder(diurnals_2019_A_vs_time_aug20_avg_se$treatment, diurnals_2019_A_vs_time_aug20_avg_se$interval)

str(diurnals_2019_A_vs_time_aug20_avg_se$interval)
tz(diurnals_2019_A_vs_time_aug20_avg_se$interval)
tz(diurnals_2019_A_vs_time_aug20$datetime)





# Plot aug 20 vs time  

pd<- position_dodge(1400)


Aaug20_timeavg_se<-ggplot(diurnals_2019_A_vs_time_aug20_avg_se, aes(interval, avg_A, group =treatment, color = treatment)) + 
  geom_errorbar(alpha = 0.9, aes(ymin=avg_A-sev, ymax=avg_A+sev), width = 1800, position=pd, stat = "identity") +
  geom_line(alpha =0.7, size =1, position=pd, linetype = "solid", stat = "identity") +
  geom_point(alpha =0.6, position=pd, size=2,stat = "identity") + 
  theme_classic()+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab(label = "Net photosynthesis (µmol m⁻² s⁻¹)") +
  ggtitle(" August 20 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=16, family = "serif")) +
  theme(axis.title.x = element_text(size=16, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2,20,3), limits = c (-2,20)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  annotate("text", x = as.POSIXct("08-20-2019 13:40:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = 19.6, label = "*", size = 6) +
  annotate("text", x = as.POSIXct("08-20-2019 17:15:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = 19.6, label = "*", size = 6)+
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.5, linetype ="dashed")

ggsave(Aaug20_timeavg_se, filename = "figures/Aaug20_timeavg_se.pdf", device = cairo_pdf, width = 8, height = 6)


#### A vs time not rounds sep5 ####

diurnals_2019_A_vs_time_sep5<- diurnals_2019_F_vs_round %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, pixel_number, round, treatment, Rep) %>%
  filter(day == "248") 

diurnals_2019_A_vs_time_sep5$interval <-cut(diurnals_2019_A_vs_time_sep5$datetime, breaks= "145 min", labels = c ("09-05-2019 6:00", "09-05-2019 9:00","09-05-2019 11:15", "09-05-2019 13:30", "09-05-2019 17:00"))


diurnals_2019_A_vs_time_sep5$interval <- format((diurnals_2019_A_vs_time_sep5$interval))

diurnals_2019_A_vs_time_sep5$interval<- mdy_hm(as.character(diurnals_2019_A_vs_time_sep5$interval))


str(diurnals_2019_A_vs_time_sep5$interval)
diurnals_2019_A_vs_time_sep5_avg_se <-diurnals_2019_A_vs_time_sep5 %>%
  group_by(interval, treatment, round) %>%
  summarise(avg_A = mean(A), sev = se(A))


diurnals_2019_A_vs_time_sep5 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2019_A_vs_time_sep5$Rep<- format(diurnals_2019_A_vs_time_sep5$Rep)
as.character(diurnals_2019_A_vs_time_sep5$Rep)
str(diurnals_2019_A_vs_time_sep5$Rep)


diurnals_2019_A_vs_time_sep5$treatment<- reorder(diurnals_2019_A_vs_time_sep5$treatment, diurnals_2019_A_vs_time_sep5$datetime) 

diurnals_2019_A_vs_time_sep5_avg_se$treatment<- reorder(diurnals_2019_A_vs_time_sep5_avg_se$treatment, diurnals_2019_A_vs_time_sep5_avg_se$interval)

str(diurnals_2019_A_vs_time_sep5_avg_se$interval)
tz(diurnals_2019_A_vs_time_sep5_avg_se$interval)
tz(diurnals_2019_A_vs_time_sep5$datetime)


# Plot sep 5 vs time  

pd<- position_dodge(1400)


Asep5_time_avg_se<-ggplot(diurnals_2019_A_vs_time_sep5_avg_se, aes(interval, avg_A, group = treatment, color = treatment)) + 
  geom_errorbar(alpha = 0.9, aes(ymin=avg_A-sev, ymax=avg_A+sev), width = 1800, position=pd, stat = "identity") +
  geom_line(alpha =0.7, size =1, position=pd, linetype = "solid", stat = "identity") +
  geom_point(alpha =0.6, position=pd, size=2,stat = "identity") + 
  theme_classic()+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab(label = "Net photosynthesis (µmol m⁻² s⁻¹)") +
  ggtitle("September 5 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=16, family = "serif")) +
  theme(axis.title.x = element_text(size=16, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2,20,3), limits = c (-2,20)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  annotate("text", x = as.POSIXct("09-05-2019 09:00:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = 19, label = "*", size = 6) +
  annotate("text", x = as.POSIXct("09-05-2019 17:05:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = 19, label = "*", size = 6) +
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.5, linetype ="dashed")

ggsave(Asep5_time_avg_se, filename = "figures/Asep5_time_avg_se.pdf", device = cairo_pdf, width = 8, height = 6)

#### A vs time not rounds no all data points Jul 1####

diurnals_2019_A_vs_time_jul1<- diurnals_2019_F_vs_round %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, gsw, pixel_number, round, treatment, Rep) %>%
  filter(day == "182") %>%
  mutate(interval = case_when(
    round == 1 ~ "7/1/2019  7:15",
    round == 2 ~ "7/1/2019  11:00",
    round == 3 ~ "7/1/2019  14:00", 
    round == 4 ~ "7/1/2019  17:15",
    round == 5 ~ "7/1/2019  19:10"
  ))

diurnals_2019_A_vs_time_jul1$interval <- format((diurnals_2019_A_vs_time_jul1$interval))
diurnals_2019_A_vs_time_jul1$interval<- as.factor(diurnals_2019_A_vs_time_jul1$interval)

str(diurnals_2019_A_vs_time_jul1$interval)

diurnals_2019_A_vs_time_jul1$interval<- mdy_hm(diurnals_2019_A_vs_time_jul1$interval)

str(diurnals_2019_A_vs_time_jul1$interval)




diurnals_2019_A_vs_time_jul1_avg_se <-diurnals_2019_A_vs_time_jul1 %>%
  group_by(interval, treatment, round) %>%
  summarise(avg_A = mean(A), sev = se(A))

diurnals_2019_A_vs_time_jul1 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2019_A_vs_time_jul1$Rep<- format(diurnals_2019_A_vs_time_jul1$Rep)
as.character(diurnals_2019_A_vs_time_jul1$Rep)
str(diurnals_2019_A_vs_time_jul1$Rep)


diurnals_2019_A_vs_time_jul1$treatment<- reorder(diurnals_2019_A_vs_time_jul1$treatment, diurnals_2019_A_vs_time_jul1$datetime) 

diurnals_2019_A_vs_time_jul1_avg_se$treatment<- reorder(diurnals_2019_A_vs_time_jul1_avg_se$treatment, diurnals_2019_A_vs_time_jul1_avg_se$interval)

str(diurnals_2019_A_vs_time_jul1_avg_se$interval)
tz(diurnals_2019_A_vs_time_jul1_avg_se$interval)
tz(diurnals_2019_A_vs_time_jul1$datetime)

#Plot Jul 1

position_dodge(1400)

Ajul1_time_avg_se<-ggplot(diurnals_2019_A_vs_time_jul1_avg_se, aes(interval, avg_A, group = treatment, color = treatment)) + 
  geom_errorbar(alpha =0.9,aes(ymin=avg_A-sev, ymax=avg_A+sev), width=1800, position=pd, stat = "identity") +
  geom_line(alpha =0.7, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha =0.6, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab(label = "Net photosynthesis (µmol m⁻² s⁻¹)") +
  ggtitle("July 1 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=16, family = "serif")) +
  theme(axis.title.x = element_text(size=16, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2,20,3), limits = c (-2,20)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  annotate("text", x = as.POSIXct("07-01-2019 11:40:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = 19, label = "*", size = 6) +
  annotate("text", x = as.POSIXct("07-01-2019 15:00:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = 19, label = "*", size = 6) +
  annotate("text", x = as.POSIXct("07-01-2019 17:50:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = 19, label = "*", size = 6)+
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.5, linetype ="dashed")

ggsave(Ajul1_time_avg_se, filename = "figures/Ajul1_time_avg_se.pdf", device = cairo_pdf, width = 8, height = 6)

#### Leaf water potentials vs time not rounds Jul 12####

diurnals_2019_A_vs_time_jul12<- diurnals_2019_F_vs_round %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, gsw, Fv.Fm, pixel_number, round, treatment, Rep) %>%
  filter(day == "193")  %>%
  mutate(interval = case_when(
    round == 1 ~ "7/12/2019  6:20",
    round == 2 ~ "7/12/2019  9:00",
    round == 3 ~ "7/12/2019  11:45", 
    round == 4 ~ "7/12/2019  14:00",
    round == 5 ~ "7/12/2019  17:00", 
    round == 6 ~ "7/12/2019  19:30", 
  ))

diurnals_2019_A_vs_time_jul12$interval <- format((diurnals_2019_A_vs_time_jul12$interval))
diurnals_2019_A_vs_time_jul12$interval<- as.factor(diurnals_2019_A_vs_time_jul12$interval)

str(diurnals_2019_A_vs_time_jul12$interval)

diurnals_2019_A_vs_time_jul12$interval<- mdy_hm(diurnals_2019_A_vs_time_jul12$interval)

str(diurnals_2019_A_vs_time_jul12$interval)



diurnals_2019_A_vs_time_jul12_avg_se <-diurnals_2019_A_vs_time_jul12 %>%
  group_by(interval, treatment, round) %>%
  summarise(avg_A = mean(A), sev = se(A))

diurnals_2019_A_vs_time_jul12 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2019_A_vs_time_jul12$Rep<- format(diurnals_2019_A_vs_time_jul12$Rep)
as.character(diurnals_2019_A_vs_time_jul12$Rep)
str(diurnals_2019_A_vs_time_jul12$Rep)


diurnals_2019_A_vs_time_jul12$treatment<- reorder(diurnals_2019_A_vs_time_jul12$treatment, diurnals_2019_A_vs_time_jul12$datetime) 

diurnals_2019_A_vs_time_jul12_avg_se$treatment<- reorder(diurnals_2019_A_vs_time_jul12_avg_se$treatment, diurnals_2019_A_vs_time_jul12_avg_se$interval)

str(diurnals_2019_A_vs_time_jul12_avg_se$interval)
tz(diurnals_2019_A_vs_time_jul12_avg_se$interval)
tz(diurnals_2019_A_vs_time_jul12$datetime)



# Plot jul12 vs time  

pd<- position_dodge(1400)


Ajul12_time_avg_se<-ggplot(diurnals_2019_A_vs_time_jul12_avg_se, aes(interval, avg_A, group = treatment, color = treatment)) + 
  geom_errorbar(alpha =0.9,aes(ymin=avg_A-sev, ymax=avg_A+sev), width=1800, position=pd, stat = "identity") +
  geom_line(alpha =0.7, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha =0.6, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab(label = "Net photosynthesis (µmol m⁻² s⁻¹)") +
  ggtitle("July 12 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=16, family = "serif")) +
  theme(axis.title.x = element_text(size=16, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2,20,3), limits = c (-2,20)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  annotate("text", x = as.POSIXct("07-12-2019 11:45:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = 19, label = "*", size = 6) +
  annotate("text", x = as.POSIXct("07-12-2019 14:05:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = 19, label = "*", size = 6)+
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.5, linetype ="dashed")


ggsave(Ajul12_time_avg_se, filename = "figures/Ajul12_time_avg_se.pdf", device = cairo_pdf, width = 8, height = 6)



#### Putting A vs time together in one frame with TIME ####

#Heatwaves with new blocks

library(cowplot)

panel_plot_A_time_2019_avg_se_ok <- plot_grid (Ajul25_time_avg_se, Ajul28_time_avg_se, Aaug1_time_avg_se, Aaug15_time_avg_se, Aaug20_timeavg_se, Asep5_time_avg_se, labels=c("Pre-heatwave", "Heatwave", "Post-heatwave","Heatwave", "Post-heatwave", "Recovery", ncol=3, nrow = 2), vjust = 3.7, hjust = -1.5, label_size = 12)

ggsave(panel_plot_A_time_2019_avg_se_ok , filename = "figures/panel_plot_A_time_2019_avg_se_ok.pdf", device = cairo_pdf, width = 15, height = 8)


# Everyhting all diurnals and blocks

panel_plot_A_time_2019_w_baseline_avg_se_ok <- plot_grid (Ajul1_time_avg_se,Ajul12_time_avg_se, Ajul25_time_avg_se, Ajul28_time_avg_se, Aaug1_time_avg_se, Aaug15_time_avg_se, labels=c("Baseline","Baseline","Pre-heatwave", "Heatwave", "Post-heatwave","Heatwave", "Post-heatwave", "Post-heatwave", ncol=3, nrow = 2), vjust = 3.7, hjust = -1.5, label_size = 12)

ggsave(panel_plot_A_time_2019_w_baseline_avg_se_ok , filename = "figures/panel_plot_A_time_2019_w_baseline_avg_se_ok.pdf", device = cairo_pdf, width = 15, height = 8)