
#### Graph monomeric antho harvest ####
library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(viridis)
library(viridisLite)
library(ggplot2)
library(agricolae)
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

monomeric_antho_percentage<-monomeric_antho_percentage%>%
  mutate(main_antho = case_when(
    Antho_short_name == "D3G" ~ "Delphinidins",
    Antho_short_name == "C3G" ~ "Cyanidins",
    Antho_short_name == "C3Gac" ~ "Cyanidins",
    Antho_short_name == "D3Gac" ~ "Delphinidins",
    Antho_short_name == "M3G" ~ "Malvidins",
    Antho_short_name == "M3Gac" ~ "Malvidins",
    Antho_short_name == "M3Gpc" ~ "Malvidins",
    Antho_short_name == "Peo3G" ~ "Peonidins",
    Antho_short_name == "Peo3Gac" ~ "Peonidins",
    Antho_short_name == "Peo3Gpc" ~ "Peonidins",
    Antho_short_name == "Pet3G" ~ "Petunidins",
    Antho_short_name == "Pet3Gac" ~ "Petunidins",
  ))

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


##### Percentage at Harvest of 5 main types of anthocyanins####

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_add_total <- monomeric_antho_percentage%>%
  filter(Set == "Harvest") %>%
  filter(!ï..Sample == "B1R2") %>%
  mutate(sample = ï..Sample) %>%
  select(-ï..Sample) %>%
  group_by(treatment, Rep, sample)%>%
  mutate(total_antho_mg_berry = sum(Concentration_mg_berry))


monomeric_antho_no_leak_2019_percentage_harvest_main_antho<- monomeric_antho_no_leak_2019_percentage_harvest_main_antho_add_total%>%
  group_by(treatment, Rep, sample, main_antho) %>%
  summarise(percent_main_antho = sum((Concentration_mg_berry/total_antho_mg_berry)*100))


monomeric_antho_no_leak_2019_percentage_harvest_main_antho$treatment<- format(monomeric_antho_no_leak_2019_percentage_harvest_main_antho$treatment)
monomeric_antho_no_leak_2019_percentage_harvest_main_antho$treatment<- as.character(monomeric_antho_no_leak_2019_percentage_harvest_main_antho$treatment)

str(monomeric_antho_no_leak_2019_percentage_harvest_main_antho)

se <- function(x) sqrt(var(x)/length(x))

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_avg_se <-monomeric_antho_no_leak_2019_percentage_harvest_main_antho%>%
  group_by(main_antho, treatment) %>%
  summarise(avg = mean(percent_main_antho), sev = se(percent_main_antho))

monomeric_antho_no_leak_2019_percentage_harvest_main_antho%>%
  group_by(main_antho, treatment) %>%
  tally()

pd <- position_dodge(0.95)

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_avg_se<-ggplot(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_avg_se, aes(main_antho,avg, fill =treatment)) + 
  geom_col( position = "dodge", color = "gray36", alpha =0.8) +
  geom_errorbar(aes(ymin=avg-sev, ymax=avg+sev, color = treatment), width= 0.6, position = pd, stat = "identity", size = 0.8) +
  theme_classic()+
  scale_color_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+ 
  ylab(expression(" Anthocyanin percentage (%)")) +
  xlab("Primary anthocyanins") +
  theme(axis.title.y = element_text(size=13, family = "serif")) +
  theme(axis.title.x = element_text(size=13, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  scale_y_continuous(breaks=seq(0,75,10), limits = c (0,75))


ggsave(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_avg_se, filename = "figures/monomeric_antho_no_leak_2019_percentage_harvest_main_antho_avg_se.pdf", device = cairo_pdf, 
       width = 9, height = 6)

### ANOVA harvest point 

###Delphinidins

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova <-monomeric_antho_no_leak_2019_percentage_first_point_main_antho %>%
 select(main_antho, treatment, percent_main_antho) %>%
  filter(main_antho == "Delphinidins") 

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
    group_by(treatment)%>%
  tally()
  

is.factor(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment<- format(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)
monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment<- as.factor(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)

is.factor(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)


str(monomeric_antho_no_leak_2019_percentage_first_point_main_antho)

ggplot(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova, aes (treatment,percent_main_antho, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (percent_main_antho~treatment, monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova )
summary (anova_mono_antho)

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist$percent_main_antho)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist$percent_main_antho)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist$percent_main_antho)

####Cyanidins 

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova <-monomeric_antho_no_leak_2019_percentage_first_point_main_antho %>%
  filter(main_antho == "Cyanidins") 

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  group_by(treatment)%>%
  tally()


is.factor(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment<- format(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)
monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment<- as.factor(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)

is.factor(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)


str(monomeric_antho_no_leak_2019_percentage_first_point_main_antho)

ggplot(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova, aes (treatment,percent_main_antho, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (percent_main_antho~treatment, monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova )
summary (anova_mono_antho)

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist$percent_main_antho)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist$percent_main_antho)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist$percent_main_antho)

####Malvidins

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova <-monomeric_antho_no_leak_2019_percentage_first_point_main_antho %>%
  filter(main_antho == "Malvidins") 

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  group_by(treatment)%>%
  tally()


is.factor(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment<- format(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)
monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment<- as.factor(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)

is.factor(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)


str(monomeric_antho_no_leak_2019_percentage_first_point_main_antho)

ggplot(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova, aes (treatment,percent_main_antho, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (percent_main_antho~treatment, monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova )
summary (anova_mono_antho)

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist$percent_main_antho)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist$percent_main_antho)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist$percent_main_antho)


##### Petunidins 

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova <-monomeric_antho_no_leak_2019_percentage_first_point_main_antho %>%
  filter(main_antho == "Petunidins") 

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  group_by(treatment)%>%
  tally()


is.factor(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment<- format(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)
monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment<- as.factor(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)

is.factor(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)


str(monomeric_antho_no_leak_2019_percentage_first_point_main_antho)

ggplot(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova, aes (treatment,percent_main_antho, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (percent_main_antho~treatment, monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova )
summary (anova_mono_antho)

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist$percent_main_antho)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist$percent_main_antho)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist$percent_main_antho)

#### Peonidins 

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova <-monomeric_antho_no_leak_2019_percentage_first_point_main_antho %>%
  filter(main_antho == "Peonidins") 

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  group_by(treatment)%>%
  tally()


is.factor(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment<- format(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)
monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment<- as.factor(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)

is.factor(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)


str(monomeric_antho_no_leak_2019_percentage_first_point_main_antho)

ggplot(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova, aes (treatment,percent_main_antho, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (percent_main_antho~treatment, monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova )
summary (anova_mono_antho)

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist$percent_main_antho)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist$percent_main_antho)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist$percent_main_antho)



##### Percentage at first sampling point of 5 main types of anthocyanins####


monomeric_antho_no_leak_2019_percentage_first_point_main_antho_add_total <- monomeric_antho_percentage%>%
  filter(Set == "1st point") %>%
  filter(!ï..Sample == "B1R2") %>%
  mutate(sample = ï..Sample) %>%
  select(-ï..Sample) %>%
  group_by(treatment, Rep, sample)%>%
  mutate(total_antho_mg_berry = sum(Concentration_mg_berry))


monomeric_antho_no_leak_2019_percentage_first_point_main_antho<- monomeric_antho_no_leak_2019_percentage_first_point_main_antho_add_total%>%
  group_by(treatment, Rep, sample, main_antho) %>%
  summarise(percent_main_antho = sum((Concentration_mg_berry/total_antho_mg_berry)*100))


monomeric_antho_no_leak_2019_percentage_first_point_main_antho$treatment<- format(monomeric_antho_no_leak_2019_percentage_first_point_main_antho$treatment)
monomeric_antho_no_leak_2019_percentage_first_point_main_antho$treatment<- as.character(monomeric_antho_no_leak_2019_percentage_first_point_main_antho$treatment)

str(monomeric_antho_no_leak_2019_percentage_first_point_main_antho)

se <- function(x) sqrt(var(x)/length(x))

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_avg_se <-monomeric_antho_no_leak_2019_percentage_first_point_main_antho%>%
  group_by(main_antho, treatment) %>%
  summarise(avg = mean(percent_main_antho), sev = se(percent_main_antho))

monomeric_antho_no_leak_2019_percentage_first_point_main_antho%>%
  group_by(main_antho, treatment) %>%
  tally()

pd <- position_dodge(0.95)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_avg_se<-ggplot(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_avg_se, aes(main_antho,avg, fill =treatment)) + 
  geom_col( position = "dodge", color = "gray36", alpha =0.8) +
  geom_errorbar(aes(ymin=avg-sev, ymax=avg+sev, color = treatment), width= 0.6, position = pd, stat = "identity", size = 0.8) +
  theme_classic()+
  scale_color_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+ 
  ylab(expression(" Anthocyanin percentage (%)")) +
  xlab("Primary anthocyanins") +
  theme(axis.title.y = element_text(size=13, family = "serif")) +
  theme(axis.title.x = element_text(size=13, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  scale_y_continuous(breaks=seq(0,75,10), limits = c (0,75))


ggsave(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_avg_se, filename = "figures/monomeric_antho_no_leak_2019_percentage_first_point_main_antho_avg_se.pdf", device = cairo_pdf, 
       width = 9, height = 6)



### ANOVA first point 

###Delphinidins

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova <-monomeric_antho_no_leak_2019_percentage_first_point_main_antho %>%
  filter(main_antho == "Delphinidins") 

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  group_by(treatment)%>%
  tally()


is.factor(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment<- format(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)
monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment<- as.factor(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)

is.factor(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)


str(monomeric_antho_no_leak_2019_percentage_first_point_main_antho)

ggplot(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova, aes (treatment,percent_main_antho, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (percent_main_antho~treatment, monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova )
summary (anova_mono_antho)

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist$percent_main_antho)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist$percent_main_antho)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist$percent_main_antho)

####Cyanidins 

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova <-monomeric_antho_no_leak_2019_percentage_first_point_main_antho %>%
  filter(main_antho == "Cyanidins") 

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  group_by(treatment)%>%
  tally()


is.factor(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment<- format(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)
monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment<- as.factor(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)

is.factor(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)


str(monomeric_antho_no_leak_2019_percentage_first_point_main_antho)

ggplot(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova, aes (treatment,percent_main_antho, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (percent_main_antho~treatment, monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova )
summary (anova_mono_antho)

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist$percent_main_antho)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist$percent_main_antho)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist$percent_main_antho)

####Malvidins

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova <-monomeric_antho_no_leak_2019_percentage_first_point_main_antho %>%
  filter(main_antho == "Malvidins") 

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  group_by(treatment)%>%
  tally()


is.factor(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment<- format(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)
monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment<- as.factor(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)

is.factor(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)


str(monomeric_antho_no_leak_2019_percentage_first_point_main_antho)

ggplot(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova, aes (treatment,percent_main_antho, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (percent_main_antho~treatment, monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova )
summary (anova_mono_antho)

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist$percent_main_antho)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist$percent_main_antho)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist$percent_main_antho)


##### Petunidins 

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova <-monomeric_antho_no_leak_2019_percentage_first_point_main_antho %>%
  filter(main_antho == "Petunidins") 

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  group_by(treatment)%>%
  tally()


is.factor(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment<- format(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)
monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment<- as.factor(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)

is.factor(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)


str(monomeric_antho_no_leak_2019_percentage_first_point_main_antho)

ggplot(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova, aes (treatment,percent_main_antho, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (percent_main_antho~treatment, monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova )
summary (anova_mono_antho)

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist$percent_main_antho)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist$percent_main_antho)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist$percent_main_antho)

#### Peonidins 

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova <-monomeric_antho_no_leak_2019_percentage_first_point_main_antho %>%
  filter(main_antho == "Peonidins") 

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  group_by(treatment)%>%
  tally()


is.factor(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment<- format(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)
monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment<- as.factor(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)

is.factor(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova$treatment)


str(monomeric_antho_no_leak_2019_percentage_first_point_main_antho)

ggplot(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova, aes (treatment,percent_main_antho, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (percent_main_antho~treatment, monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova )
summary (anova_mono_antho)

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist$percent_main_antho)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist$percent_main_antho)

monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_anova_hist$percent_main_antho)