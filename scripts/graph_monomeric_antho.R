
#### Graph monomeric antho harvest ####
library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(viridis)
library(viridisLite)
library(ggplot2)
monomeric_antho_concentration<- read.csv("data/monomeric_antho_concentration_2019.csv", header = TRUE)


##### Concentration at Harvest####

monomeric_antho_no_leak_2019_concentration_harvest <- monomeric_antho_concentration%>%
  filter(Set == "Harvest") %>%
  filter(!ï..Sample == "B1R2") %>%
  mutate(sample = ï..Sample) %>%
  select(-ï..Sample)

monomeric_antho_no_leak_2019_concentration_harvest$treatment<- format(monomeric_antho_no_leak_2019_concentration_harvest$treatment)
monomeric_antho_no_leak_2019_concentration_harvest$treatment<- as.character(monomeric_antho_no_leak_2019_concentration_harvest$treatment)

monomeric_antho_no_leak_2019_concentration_harvest$Antho_type<- format(monomeric_antho_no_leak_2019_concentration_harvest$Antho_type)
monomeric_antho_no_leak_2019_concentration_harvest$Antho_type<- as.character(monomeric_antho_no_leak_2019_concentration_harvest$Antho_type)



str(monomeric_antho_no_leak_2019_concentration_harvest)


se <- function(x) sqrt(var(x)/length(x))

monomeric_antho_no_leak_2019_concentration_harvest_avg_se <-monomeric_antho_no_leak_2019_concentration_harvest%>%
  group_by(Antho_short_name, treatment) %>%
  summarise(avg = mean(Concentration_mg_berry), sev = se(Concentration_mg_berry))

pd <- position_dodge(0.95)

monomeric_antho_no_leak_2019_concentration_harvest_avg_se$Antho_short_name<-factor(monomeric_antho_no_leak_2019_concentration_harvest_avg_se$Antho_short_name, levels = c("D3G","C3G","Pet3G", "Peo3G", "M3G","D3Gac","C3Gac","Pet3Gac", "Peo3Gac", "M3Gac","Peo3Gpc", "M3Gpc"))

monomeric_antho_no_leak_2019_concentration_harvest<-ggplot(monomeric_antho_no_leak_2019_concentration_harvest_avg_se, aes(Antho_short_name,avg, fill =treatment)) + 
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
  theme(legend.position = "right") +
  scale_y_continuous(breaks=seq(0,0.21,0.03), limits = c (0,0.23))

ggsave(monomeric_antho_no_leak_2019_concentration_harvest, filename = "figures/monomeric_antho_no_leak_2019_concentration_harvest.pdf", device = cairo_pdf, 
       width = 11, height = 6)

####Boxplot####

monomeric_antho_no_leak_2019_concentration_harvest$Antho_short_name <- format(monomeric_antho_no_leak_2019_concentration_harvest$Antho_short_name)
monomeric_antho_no_leak_2019_concentration_harvest$Antho_short_name<- as.character(monomeric_antho_no_leak_2019_concentration_harvest$Antho_short_name)

monomeric_antho_no_leak_2019_concentration_harvest_boxplot<-ggplot(monomeric_antho_no_leak_2019_concentration_harvest, aes(Antho_short_name, Concentration_mg_berry))+
  geom_boxplot(alpha =0.7, aes(fill=treatment))+
  geom_point(alpha = 0.9, position = pd2, aes(color = treatment, group =treatment))+
  theme_classic()+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))
  
##### Concentration First sampling point####

monomeric_antho_no_leak_2019_concentration_first_point <- monomeric_antho_concentration%>%
  filter(Set == "1st point") %>%
  filter(!ï..Sample == "B1R2") %>%
  mutate(sample = ï..Sample) %>%
  select(-ï..Sample)

write.csv(monomeric_antho_no_leak_2019_concentration_first_point, "data_output/monomeric_antho_no_leak_2019_concentration_first_point.csv")

monomeric_antho_no_leak_2019_concentration_first_point$treatment<- format(monomeric_antho_no_leak_2019_concentration_first_point$treatment)
monomeric_antho_no_leak_2019_concentration_first_point$treatment<- as.character(monomeric_antho_no_leak_2019_concentration_first_point$treatment)

str(monomeric_antho_no_leak_2019_concentration_first_point)


se <- function(x) sqrt(var(x)/length(x))

monomeric_antho_no_leak_2019_concentration_first_point_avg_se <-monomeric_antho_no_leak_2019_concentration_first_point%>%
  group_by(Antho_short_name, treatment) %>%
  summarise(avg = mean(Concentration_mg_berry), sev = se(Concentration_mg_berry))

pd <- position_dodge(0.95)

monomeric_antho_no_leak_2019_concentration_first_point_avg_se$Antho_short_name<-factor(monomeric_antho_no_leak_2019_concentration_first_point_avg_se$Antho_short_name, levels = c("D3G","C3G","Pet3G", "Peo3G", "M3G","D3Gac","C3Gac","Pet3Gac", "Peo3Gac", "M3Gac","Peo3Gpc", "M3Gpc"))

monomeric_antho_no_leak_2019_concentration_first_point<-ggplot(monomeric_antho_no_leak_2019_concentration_first_point_avg_se, aes(Antho_short_name,avg, fill =treatment)) + 
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
  theme(legend.position = "right") +
  scale_y_continuous(breaks=seq(0,0.21,0.03), limits = c (0,0.23))

ggsave(monomeric_antho_no_leak_2019_concentration_first_point, filename = "figures/monomeric_antho_no_leak_2019_concentration_first_point.pdf", device = cairo_pdf, 
       width = 11, height = 6)


##### Percentage at Harvest ####

monomeric_antho_percentage<- read.csv("data/monomeric_antho_percentage_2019_harvest_first_point.csv", header = TRUE)

monomeric_antho_no_leak_2019_percentage_harvest <- monomeric_antho_percentage%>%
  filter(Set == "Harvest") %>%
  filter(!ï..Sample == "B1R2") %>%
  mutate(sample = ï..Sample) %>%
  select(-ï..Sample)

monomeric_antho_no_leak_2019_percentage_harvest$treatment<- format(monomeric_antho_no_leak_2019_percentage_harvest$treatment)
monomeric_antho_no_leak_2019_percentage_harvest$treatment<- as.character(monomeric_antho_no_leak_2019_percentage_harvest$treatment)

str(monomeric_antho_no_leak_2019_percentage_harvest)


se <- function(x) sqrt(var(x)/length(x))

monomeric_antho_no_leak_2019_percentage_harvest_avg_se <-monomeric_antho_no_leak_2019_percentage_harvest%>%
  group_by(Antho_short_name, treatment) %>%
  summarise(avg = mean(percentage_antho), sev = se(percentage_antho))


pd <- position_dodge(0.95)

monomeric_antho_no_leak_2019_percentage_harvest_avg_se$Antho_short_name<-factor(monomeric_antho_no_leak_2019_percentage_harvest_avg_se$Antho_short_name, levels = c("D3G","C3G","Pet3G", "Peo3G", "M3G","D3Gac","C3Gac","Pet3Gac", "Peo3Gac", "M3Gac","Peo3Gpc", "M3Gpc"))

monomeric_antho_no_leak_2019_percentage_harvest<-ggplot(monomeric_antho_no_leak_2019_percentage_harvest_avg_se, aes(Antho_short_name,avg, fill =treatment)) + 
  geom_col( position = "dodge", color = "gray36", alpha =0.8) +
  geom_errorbar(aes(ymin=avg-sev, ymax=avg+sev, color = treatment), width= 0.6, position = pd, stat = "identity", size = 0.8) +
  theme_classic()+
  scale_color_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+ 
  ylab(expression(" Anthocyanin percentage (%)")) +
  xlab("Monomeric anthocyanins") +
  theme(axis.title.y = element_text(size=13, family = "serif")) +
  theme(axis.title.x = element_text(size=13, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  scale_y_continuous(breaks=seq(0,40,5), limits = c (0,42))

ggsave(monomeric_antho_no_leak_2019_percentage_harvest, filename = "figures/monomeric_antho_no_leak_2019_percentage_harvest.pdf", device = cairo_pdf, 
       width = 11, height = 6)




##### Percentage first point ####

monomeric_antho_no_leak_2019_percentage_first_point <- monomeric_antho_percentage%>%
  filter(Set == "1st point") %>%
  filter(!ï..Sample == "B1R2") %>%
  mutate(sample = ï..Sample) %>%
  select(-ï..Sample)

monomeric_antho_no_leak_2019_percentage_first_point$treatment<- format(monomeric_antho_no_leak_2019_percentage_first_point$treatment)
monomeric_antho_no_leak_2019_percentage_first_point$treatment<- as.character(monomeric_antho_no_leak_2019_percentage_first_point$treatment)

str(monomeric_antho_no_leak_2019_percentage_first_point)



monomeric_antho_no_leak_2019_percentage_first_point_avg_se <-monomeric_antho_no_leak_2019_percentage_first_point%>%
  group_by(Antho_short_name, treatment) %>%
  summarise(avg = mean(percentage_antho), sev = sep(percentage_antho))

pd <- position_dodge(0.95)

monomeric_antho_no_leak_2019_percentage_first_point_avg_se$Antho_short_name<-factor(monomeric_antho_no_leak_2019_percentage_first_point_avg_se$Antho_short_name, levels = c("D3G","C3G","Pet3G", "Peo3G", "M3G","D3Gac","C3Gac","Pet3Gac", "Peo3Gac", "M3Gac","Peo3Gpc", "M3Gpc"))

monomeric_antho_no_leak_2019_percentage_first_point<-ggplot(monomeric_antho_no_leak_2019_percentage_first_point_avg_se, aes(Antho_short_name,avg, fill =treatment)) + 
  geom_col( position = "dodge", color = "gray36", alpha =0.8) +
  geom_errorbar(aes(ymin=avg-sev, ymax=avg+sev, color = treatment), width= 0.6, position = pd, stat = "identity", size = 0.8) +
  theme_classic()+
  scale_color_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+ 
  ylab(expression(" Anthocyanin percentage (%)")) +
  xlab("Monomeric anthocyanins") +
  theme(axis.title.y = element_text(size=13, family = "serif")) +
  theme(axis.title.x = element_text(size=13, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  scale_y_continuous(breaks=seq(0,35,5), limits = c (0,35))

ggsave(monomeric_antho_no_leak_2019_percentage_first_point, filename = "figures/monomeric_antho_no_leak_2019_percentage_first_point.pdf", device = cairo_pdf, 
       width = 11, height = 6)

