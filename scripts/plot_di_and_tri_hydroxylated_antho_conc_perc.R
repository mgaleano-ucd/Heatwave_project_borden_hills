
#### Graph monomeric antho harvest tri and di hydroxylated ####
library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(viridis)
library(viridisLite)
library(ggplot2)

monomeric_antho_concentration_perc_tri_di_hydroxylated_harvest<- read.csv("data/di_tri_hydroxylated_antho_conc_and_perc_harvest.csv", header = TRUE)

##### Concentration at Harvest####

monomeric_antho_concentration_tri_di_hydroxylated_harvest<- monomeric_antho_concentration_perc_tri_di_hydroxylated_harvest %>%
  mutate(treatment = ï..treatment) %>%
  select(-ï..treatment)

monomeric_antho_concentration_tri_di_hydroxylated_harvest$treatment<-format(monomeric_antho_concentration_tri_di_hydroxylated_harvest$treatment)
monomeric_antho_concentration_tri_di_hydroxylated_harvest$treatment<- as.character(monomeric_antho_concentration_tri_di_hydroxylated_harvest$treatment)

monomeric_antho_concentration_tri_di_hydroxylated_harvest$Antho_type<- format(monomeric_antho_concentration_tri_di_hydroxylated_harvest$Antho_type)
monomeric_antho_concentration_tri_di_hydroxylated_harvest$Antho_type<- as.character(monomeric_antho_concentration_tri_di_hydroxylated_harvest$Antho_type)



str(monomeric_antho_concentration_tri_di_hydroxylated_harvest)


se <- function(x) sqrt(var(x)/length(x))

monomeric_antho_concentration_tri_di_hydroxylated_harvest_avg_se <-monomeric_antho_concentration_tri_di_hydroxylated_harvest%>%
  group_by(Antho_type, treatment) %>%
  summarise(avg = mean(concetration), sev = se(concetration))

pd <- position_dodge(0.95)



monomeric_antho_concentration_tri_di_hydroxylated_harvest<-ggplot(monomeric_antho_concentration_tri_di_hydroxylated_harvest_avg_se, aes(Antho_type,avg, fill =treatment)) + 
  geom_col( position = "dodge", color = "gray36", alpha =0.8) +
  geom_errorbar(aes(ymin=avg-sev, ymax=avg+sev, color = treatment), width= 0.6, position = pd, stat = "identity", size = 0.8) +
  theme_classic()+
  scale_color_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+ 
  ylab(expression("Concentration (mg/berry)")) +
  xlab("Monomeric anthocyanins") +
  theme(axis.title.y = element_text(size=13, family = "serif")) +
  theme(axis.title.x = element_text(size=13, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") 

ggsave(monomeric_antho_concentration_tri_di_hydroxylated_harvest, filename = "figures/monomeric_antho_concentration_tri_di_hydroxylated_harvest.pdf", device = cairo_pdf, 
       width = 9, height = 6)

####Boxplot####

pd2 <- position_dodge(0.75)
monomeric_antho_concentration_tri_di_hydroxylated_harvest$Antho_short_name <- format(monomeric_antho_concentration_tri_di_hydroxylated_harvest$Antho_short_name)
monomeric_antho_concentration_tri_di_hydroxylated_harvest$Antho_short_name<- as.character(monomeric_antho_concentration_tri_di_hydroxylated_harvest$Antho_short_name)

monomeric_antho_concentration_tri_di_hydroxylated_harvest_boxplot<-ggplot(monomeric_antho_concentration_tri_di_hydroxylated_harvest, aes(Antho_type, concetration))+
  geom_boxplot(alpha =0.7, aes(fill=treatment))+
  geom_point(alpha = 0.9, position = pd2, aes(color = treatment, group =treatment))+
  theme_classic()+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))



##### plotting percentage

monomeric_antho_perc_tri_di_hydroxylated_harvest<- monomeric_antho_concentration_perc_tri_di_hydroxylated_harvest %>%
  mutate(treatment = ï..treatment) %>%
  select(-ï..treatment)

monomeric_antho_perc_tri_di_hydroxylated_harvest$treatment<-format(monomeric_antho_perc_tri_di_hydroxylated_harvest$treatment)
monomeric_antho_perc_tri_di_hydroxylated_harvest$treatment<- as.character(monomeric_antho_perc_tri_di_hydroxylated_harvest$treatment)

monomeric_antho_perc_tri_di_hydroxylated_harvest$Antho_type<- format(monomeric_antho_perc_tri_di_hydroxylated_harvest$Antho_type)
monomeric_antho_perc_tri_di_hydroxylated_harvest$Antho_type<- as.character(monomeric_antho_perc_tri_di_hydroxylated_harvest$Antho_type)



str(monomeric_antho_perc_tri_di_hydroxylated_harvest)


se <- function(x) sqrt(var(x)/length(x))

monomeric_antho_perc_tri_di_hydroxylated_harvest_avg_se <-monomeric_antho_perc_tri_di_hydroxylated_harvest%>%
  group_by(Antho_type, treatment) %>%
  summarise(avg = mean(percentage), sev = se(percentage)) %>%
  filter(!Antho_type == "tri_hydroxylated")

pd <- position_dodge(0.95)

monomeric_antho_perc_di_hydroxylated_harvest<-ggplot(monomeric_antho_perc_tri_di_hydroxylated_harvest_avg_se, aes(Antho_type,avg, fill =treatment)) + 
  geom_col( position = "dodge", color = "gray36", alpha =0.8) +
  geom_errorbar(aes(ymin=avg-sev, ymax=avg+sev, color = treatment), width= 0.6, position = pd, stat = "identity", size = 0.8) +
  theme_classic()+
  scale_color_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+ 
  ylab(expression("Anthocyanin percentage (%)")) +
  xlab("Monomeric anthocyanins") +
  theme(axis.title.y = element_text(size=13, family = "serif")) +
  theme(axis.title.x = element_text(size=13, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") 

ggsave(monomeric_antho_perc_di_hydroxylated_harvest, filename = "figures/monomeric_antho_perc_di_hydroxylated_harvest.pdf", device = cairo_pdf, 
       width = 7, height = 6)

monomeric_antho_perc_tri_di_hydroxylated_harvest_avg_se <-monomeric_antho_perc_tri_di_hydroxylated_harvest%>%
  group_by(Antho_type, treatment) %>%
  summarise(avg = mean(percentage), sev = se(percentage)) %>%
  filter(Antho_type == "tri_hydroxylated")

pd <- position_dodge(0.95)

monomeric_antho_perc_tri_hydroxylated_harvest<-ggplot(monomeric_antho_perc_tri_di_hydroxylated_harvest_avg_se, aes(Antho_type,avg, fill =treatment)) + 
  geom_col( position = "dodge", color = "gray36", alpha =0.8) +
  geom_errorbar(aes(ymin=avg-sev, ymax=avg+sev, color = treatment), width= 0.6, position = pd, stat = "identity", size = 0.8) +
  theme_classic()+
  scale_color_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+ 
  ylab(expression("Anthocyanin percentage (%)")) +
  xlab("Monomeric anthocyanins") +
  theme(axis.title.y = element_text(size=13, family = "serif")) +
  theme(axis.title.x = element_text(size=13, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  scale_y_continuous(breaks=seq(0,100,20), limits = c (0,100))

ggsave(monomeric_antho_perc_tri_hydroxylated_harvest, filename = "figures/monomeric_antho_perc_tri_hydroxylated_harvest.pdf", device = cairo_pdf, 
       width = 7, height = 6)


monomeric_antho_perc_tri_hydroxylated_harvest<- monomeric_antho_concentration_perc_tri_di_hydroxylated_harvest %>%
  mutate(treatment = ï..treatment) %>%
  select(-ï..treatment) %>%
  filter(!Antho_type == "tri_hydroxylated")

monomeric_antho_perc_tri_hydroxylated_harvest$treatment<-format(monomeric_antho_perc_tri_hydroxylated_harvest$treatment)
monomeric_antho_perc_tri_hydroxylated_harvest$treatment<- as.character(monomeric_antho_perc_tri_hydroxylated_harvest$treatment)

pd2 <- position_dodge(0.75)

monomeric_antho_concentration_tri_hydroxylated_harvest_boxplot<-ggplot(monomeric_antho_perc_tri_hydroxylated_harvest, aes(Antho_type, concetration))+
  geom_boxplot(alpha =0.7, aes(fill=treatment))+
  geom_point(alpha = 0.9, position = pd2, aes(color = treatment, group =treatment))+
  theme_classic()+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))


##### First point #####

monomeric_antho_concentration_perc_tri_di_hydroxylated_first_point<- read.csv("data/di_tri_hydroxylatd_antho_conc_and_perc_first_point.csv", header = TRUE)

monomeric_antho_perc_tri_di_hydroxylated_first_point<- monomeric_antho_concentration_perc_tri_di_hydroxylated_first_point %>%
  mutate(treatment = ï..treatment) %>%
  select(-ï..treatment)

monomeric_antho_perc_tri_di_hydroxylated_first_point$treatment<-format(monomeric_antho_perc_tri_di_hydroxylated_first_point$treatment)
monomeric_antho_perc_tri_di_hydroxylated_first_point$treatment<- as.character(monomeric_antho_perc_tri_di_hydroxylated_first_point$treatment)

monomeric_antho_perc_tri_di_hydroxylated_first_point$Antho_type<- format(monomeric_antho_perc_tri_di_hydroxylated_first_point$Antho_type)
monomeric_antho_perc_tri_di_hydroxylated_first_point$Antho_type<- as.character(monomeric_antho_perc_tri_di_hydroxylated_first_point$Antho_type)



str(monomeric_antho_perc_tri_di_hydroxylated_first_point)


se <- function(x) sqrt(var(x)/length(x))

monomeric_antho_perc_tri_di_hydroxylated_first_point_avg_se <-monomeric_antho_perc_tri_di_hydroxylated_first_point%>%
  group_by(Antho_type, treatment) %>%
  summarise(avg = mean(percentage), sev = se(percentage)) %>%
  filter(!Antho_type == "tri_hydroxylated")

pd <- position_dodge(0.95)



monomeric_antho_perc_di_hydroxylated_first_point<-ggplot(monomeric_antho_perc_tri_di_hydroxylated_first_point_avg_se, aes(Antho_type,avg, fill =treatment)) + 
  geom_col( position = "dodge", color = "gray36", alpha =0.8) +
  geom_errorbar(aes(ymin=avg-sev, ymax=avg+sev, color = treatment), width= 0.6, position = pd, stat = "identity", size = 0.8) +
  theme_classic()+
  scale_color_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+ 
  ylab(expression("Anthocyanin percentage (%)")) +
  xlab("Monomeric anthocyanins") +
  theme(axis.title.y = element_text(size=13, family = "serif")) +
  theme(axis.title.x = element_text(size=13, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") 

ggsave(monomeric_antho_perc_di_hydroxylated_first_point, filename = "figures/monomeric_antho_perc_di_hydroxylated_first_point.pdf", device = cairo_pdf, 
       width = 7, height = 6)

monomeric_antho_perc_tri_di_hydroxylated_first_point_avg_se <-monomeric_antho_perc_tri_di_hydroxylated_first_point%>%
  group_by(Antho_type, treatment) %>%
  summarise(avg = mean(percentage), sev = se(percentage)) %>%
  filter(Antho_type == "tri_hydroxylated")

pd <- position_dodge(0.95)

monomeric_antho_perc_tri_hydroxylated_first_point<-ggplot(monomeric_antho_perc_tri_di_hydroxylated_first_point_avg_se, aes(Antho_type,avg, fill =treatment)) + 
  geom_col( position = "dodge", color = "gray36", alpha =0.8) +
  geom_errorbar(aes(ymin=avg-sev, ymax=avg+sev, color = treatment), width= 0.6, position = pd, stat = "identity", size = 0.8) +
  theme_classic()+
  scale_color_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+ 
  ylab(expression("Anthocyanin percentage (%)")) +
  xlab("Monomeric anthocyanins") +
  theme(axis.title.y = element_text(size=13, family = "serif")) +
  theme(axis.title.x = element_text(size=13, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  scale_y_continuous(breaks=seq(0,100,20), limits = c (0,100))

ggsave(monomeric_antho_perc_tri_hydroxylated_first_point, filename = "figures/monomeric_antho_perc_tri_hydroxylated_first_point.pdf", device = cairo_pdf, 
       width = 7, height = 6)