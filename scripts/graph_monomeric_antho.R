
#### Graph monomeric antho harvest ####
library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(viridis)
library(viridisLite)
library(ggplot2)
library(agricolae)

main_monomeric_antho_concentration_percentage_2019<- read.csv("data_output/monomeric_antho_concentration_2019.csv", header = TRUE)

##### Concentration at Harvest####

monomeric_antho_no_leak_2019_concentration_harvest <- main_monomeric_antho_concentration_percentage_2019%>%
  filter(Set == "Harvest") %>%
  filter(!ï..Sample == "B1R2") 

monomeric_antho_no_leak_2019_concentration_harvest$treatment<- format(monomeric_antho_no_leak_2019_concentration_harvest$treatment)
monomeric_antho_no_leak_2019_concentration_harvest$treatment<- as.character(monomeric_antho_no_leak_2019_concentration_harvest$treatment)

monomeric_antho_no_leak_2019_concentration_harvest$main_antho<- format(monomeric_antho_no_leak_2019_concentration_harvest$main_antho)
monomeric_antho_no_leak_2019_concentration_harvest$main_antho<- as.character(monomeric_antho_no_leak_2019_concentration_harvest$main_antho)



str(monomeric_antho_no_leak_2019_concentration_harvest)


se <- function(x) sqrt(var(x)/length(x))

monomeric_antho_no_leak_2019_concentration_harvest%>%
  group_by(short_name_antho, treatment) %>%
  tally()

monomeric_antho_no_leak_2019_concentration_harvest_avg_se <-monomeric_antho_no_leak_2019_concentration_harvest%>%
  group_by(short_name_antho, treatment) %>%
  summarise(avg = mean(antho_mg_berry), sev = se(antho_mg_berry))

pd <- position_dodge(0.95)

monomeric_antho_no_leak_2019_concentration_harvest_avg_se$short_name_antho<-factor(monomeric_antho_no_leak_2019_concentration_harvest_avg_se$short_name_antho, levels = c("D3G","C3G","Pet3G", "Peo3G", "M3G","D3Gac","C3Gac","Pet3Gac", "Peo3Gac", "M3Gac","Peo3Gpc", "M3Gpc"))

monomeric_antho_no_leak_2019_concentration_harvest<-ggplot(monomeric_antho_no_leak_2019_concentration_harvest_avg_se, aes(short_name_antho,avg, fill =treatment)) + 
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

ggsave(monomeric_antho_no_leak_2019_concentration_harvest, filename = "figures/monomeric_antho_no_leak_2019_concentration_harvest_redo.pdf", device = cairo_pdf, 
       width = 11, height = 6)

####Boxplot####

monomeric_antho_no_leak_2019_concentration_harvest$short_name_antho <- format(monomeric_antho_no_leak_2019_concentration_harvest$short_name_antho)
monomeric_antho_no_leak_2019_concentration_harvest$short_name_antho<- as.character(monomeric_antho_no_leak_2019_concentration_harvest$short_name_antho)

monomeric_antho_no_leak_2019_concentration_harvest_boxplot<-ggplot(monomeric_antho_no_leak_2019_concentration_harvest, aes(short_name_antho, Concentration_mg_berry))+
  geom_boxplot(alpha =0.7, aes(fill=treatment))+
  geom_point(alpha = 0.9, position = pd2, aes(color = treatment, group =treatment))+
  theme_classic()+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))
  
##### Concentration First sampling point####

monomeric_antho_no_leak_2019_concentration_first_point <- main_monomeric_antho_concentration_percentage_2019%>%
  filter(Set == "1st point") %>%
  filter(!ï..Sample == "B1R2") 


monomeric_antho_no_leak_2019_concentration_first_point$treatment<- format(monomeric_antho_no_leak_2019_concentration_first_point$treatment)
monomeric_antho_no_leak_2019_concentration_first_point$treatment<- as.character(monomeric_antho_no_leak_2019_concentration_first_point$treatment)

str(monomeric_antho_no_leak_2019_concentration_first_point)


se <- function(x) sqrt(var(x)/length(x))

monomeric_antho_no_leak_2019_concentration_first_point%>%
  group_by(short_name_antho, treatment) %>%
  tally()

monomeric_antho_no_leak_2019_concentration_first_point_avg_se <-monomeric_antho_no_leak_2019_concentration_first_point%>%
  group_by(short_name_antho, treatment) %>%
  summarise(avg = mean(antho_mg_berry), sev = se(antho_mg_berry))

pd <- position_dodge(0.95)

monomeric_antho_no_leak_2019_concentration_first_point_avg_se$short_name_antho<-factor(monomeric_antho_no_leak_2019_concentration_first_point_avg_se$short_name_antho, levels = c("D3G","C3G","Pet3G", "Peo3G", "M3G","D3Gac","C3Gac","Pet3Gac", "Peo3Gac", "M3Gac","Peo3Gpc", "M3Gpc"))

monomeric_antho_no_leak_2019_concentration_first_point<-ggplot(monomeric_antho_no_leak_2019_concentration_first_point_avg_se, aes(short_name_antho,avg, fill =treatment)) + 
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

ggsave(monomeric_antho_no_leak_2019_concentration_first_point, filename = "figures/monomeric_antho_no_leak_2019_concentration_first_point_redo.pdf", device = cairo_pdf, 
       width = 11, height = 6)


##### Percentage at Harvest ####

monomeric_antho_no_leak_2019_percentage_harvest <-main_monomeric_antho_concentration_percentage_2019%>%
  filter(Set == "Harvest") %>%
  filter(!ï..Sample == "B1R2") %>%
  mutate(percentage_antho = ((antho_mg_berry/total_antho_mg_berry)*100))

monomeric_antho_no_leak_2019_percentage_harvest$treatment<- format(monomeric_antho_no_leak_2019_percentage_harvest$treatment)
monomeric_antho_no_leak_2019_percentage_harvest$treatment<- as.character(monomeric_antho_no_leak_2019_percentage_harvest$treatment)

str(monomeric_antho_no_leak_2019_percentage_harvest)


se <- function(x) sqrt(var(x)/length(x))

monomeric_antho_no_leak_2019_percentage_harvest%>%
  group_by(short_name_antho, treatment) %>%
  tally()

monomeric_antho_no_leak_2019_percentage_harvest_avg_se <-monomeric_antho_no_leak_2019_percentage_harvest%>%
  group_by(short_name_antho, treatment) %>%
  summarise(avg = mean(percentage_antho), sev = se(percentage_antho))


pd <- position_dodge(0.95)

monomeric_antho_no_leak_2019_percentage_harvest_avg_se$short_name_antho<-factor(monomeric_antho_no_leak_2019_percentage_harvest_avg_se$short_name_antho, levels = c("D3G","C3G","Pet3G", "Peo3G", "M3G","D3Gac","C3Gac","Pet3Gac", "Peo3Gac", "M3Gac","Peo3Gpc", "M3Gpc"))

monomeric_antho_no_leak_2019_percentage_harvest<-ggplot(monomeric_antho_no_leak_2019_percentage_harvest_avg_se, aes(short_name_antho,avg, fill =treatment)) + 
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


ggsave(monomeric_antho_no_leak_2019_percentage_harvest, filename = "figures/monomeric_antho_no_leak_2019_percentage_harvest_redo.pdf", device = cairo_pdf, 
       width = 11, height = 6)




##### Percentage first point ####

monomeric_antho_no_leak_2019_percentage_first_point <- main_monomeric_antho_concentration_percentage_2019%>%
  filter(Set == "1st point") %>%
  filter(!ï..Sample == "B1R2") %>%
  mutate(percentage_antho = ((antho_mg_berry/total_antho_mg_berry)*100))

monomeric_antho_no_leak_2019_percentage_first_point$treatment<- format(monomeric_antho_no_leak_2019_percentage_first_point$treatment)
monomeric_antho_no_leak_2019_percentage_first_point$treatment<- as.character(monomeric_antho_no_leak_2019_percentage_first_point$treatment)

str(monomeric_antho_no_leak_2019_percentage_first_point)



monomeric_antho_no_leak_2019_percentage_first_point_avg_se <-monomeric_antho_no_leak_2019_percentage_first_point%>%
  group_by(short_name_antho, treatment) %>%
  summarise(avg = mean(percentage_antho), sev = se(percentage_antho))

pd <- position_dodge(0.95)

monomeric_antho_no_leak_2019_percentage_first_point_avg_se$short_name_antho<-factor(monomeric_antho_no_leak_2019_percentage_first_point_avg_se$short_name_antho, levels = c("D3G","C3G","Pet3G", "Peo3G", "M3G","D3Gac","C3Gac","Pet3Gac", "Peo3Gac", "M3Gac","Peo3Gpc", "M3Gpc"))

monomeric_antho_no_leak_2019_percentage_first_point<-ggplot(monomeric_antho_no_leak_2019_percentage_first_point_avg_se, aes(short_name_antho,avg, fill =treatment)) + 
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

ggsave(monomeric_antho_no_leak_2019_percentage_first_point, filename = "figures/monomeric_antho_no_leak_2019_percentage_first_point_redo.pdf", device = cairo_pdf, 
       width = 11, height = 6)


##### Percentage at Harvest of 5 main types of anthocyanins####

monomeric_antho_no_leak_2019_percentage_harvest_main_antho <- main_monomeric_antho_concentration_percentage_2019%>%
  filter(Set == "Harvest") %>%
  filter(!ï..Sample == "B1R2") %>%
  group_by(treatment, Rep, Block_id,main_antho)%>%
  summarise(percent_main_antho = sum((antho_mg_berry)/total_antho_mg_berry)*100)


monomeric_antho_no_leak_2019_percentage_harvest_main_antho$treatment<- format(monomeric_antho_no_leak_2019_percentage_harvest_main_antho$treatment)
monomeric_antho_no_leak_2019_percentage_harvest_main_antho$treatment<- as.character(monomeric_antho_no_leak_2019_percentage_harvest_main_antho$treatment)

str(monomeric_antho_no_leak_2019_percentage_harvest_main_antho)

se <- function(x) sqrt(var(x)/length(x))

monomeric_antho_no_leak_2019_percentage_harvest_main_antho%>%
  group_by(main_antho, treatment) %>%
  tally()



monomeric_antho_no_leak_2019_percentage_harvest_main_antho_avg_se <-monomeric_antho_no_leak_2019_percentage_harvest_main_antho%>%
  group_by(main_antho, treatment) %>%
  summarise(avg = mean(percent_main_antho), sev = se(percent_main_antho))


monomeric_antho_no_leak_2019_percentage_harvest_main_antho_avg_se$main_antho<-factor(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_avg_se$main_antho, levels = c("Cyanidins","Peonidins","Malvidins", "Petunidins", "Delphinidins"))

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
  scale_y_continuous(breaks=seq(0,75,10), limits = c (0,75))+
  geom_vline(xintercept = 2.5, color = "darkgrey", size = 0.5, linetype ="dashed")


ggsave(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_avg_se, filename = "figures/monomeric_antho_no_leak_2019_percentage_harvest_main_antho_avg_se_redo.pdf", device = cairo_pdf, 
       width = 9, height = 6)

### ANOVA harvest point 

###Delphinidins

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova <-monomeric_antho_no_leak_2019_percentage_harvest_main_antho %>%
 select(main_antho, treatment, percent_main_antho) %>%
  filter(main_antho == "Delphinidins") 

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova %>%
    group_by(treatment)%>%
  tally()
  

is.factor(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova$treatment)

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova$treatment<- format(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova$treatment)
monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova$treatment<- as.factor(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova$treatment)

is.factor(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova$treatment)

ggplot(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova, aes (treatment,percent_main_antho, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (percent_main_antho~treatment, monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova )
summary (anova_mono_antho)

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova_hist$percent_main_antho)

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova_hist$percent_main_antho)

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova_hist$percent_main_antho)

####Cyanidins 

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova <-monomeric_antho_no_leak_2019_percentage_harvest_main_antho %>%
  select(main_antho, treatment, percent_main_antho) %>%
  filter(main_antho == "Cyanidins") 

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova %>%
  group_by(treatment)%>%
  tally()


is.factor(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova$treatment)

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova$treatment<- format(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova$treatment)
monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova$treatment<- as.factor(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova$treatment)

is.factor(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova$treatment)


str(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova)

ggplot(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova, aes (treatment,percent_main_antho, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (percent_main_antho~treatment, monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova )
summary (anova_mono_antho)

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova_hist$percent_main_antho)

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova_hist$percent_main_antho)

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova_hist$percent_main_antho)

####Malvidins

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova <-monomeric_antho_no_leak_2019_percentage_harvest_main_antho %>%
  filter(main_antho == "Malvidins") 

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova %>%
  group_by(treatment)%>%
  tally()


is.factor(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova$treatment)

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova$treatment<- format(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova$treatment)
monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova$treatment<- as.factor(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova$treatment)

is.factor(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova$treatment)


str(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova)

ggplot(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova, aes (treatment,percent_main_antho, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (percent_main_antho~treatment, monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova )
summary (anova_mono_antho)

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova_hist$percent_main_antho)

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova_hist$percent_main_antho)

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova_hist$percent_main_antho)


##### Petunidins 

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova <-monomeric_antho_no_leak_2019_percentage_harvest_main_antho %>%
  filter(main_antho == "Petunidins") 

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova %>%
  group_by(treatment)%>%
  tally()


is.factor(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova$treatment)

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova$treatment<- format(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova$treatment)
monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova$treatment<- as.factor(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova$treatment)

is.factor(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova$treatment)


str(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova)

ggplot(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova, aes (treatment,percent_main_antho, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (percent_main_antho~treatment, monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova )
summary (anova_mono_antho)

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova_hist$percent_main_antho)

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova_hist$percent_main_antho)

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova_hist$percent_main_antho)

#### Peonidins 

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova <-monomeric_antho_no_leak_2019_percentage_harvest_main_antho %>%
  filter(main_antho == "Peonidins") 

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova %>%
  group_by(treatment)%>%
  tally()


is.factor(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova$treatment)

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova$treatment<- format(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova$treatment)
monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova$treatment<- as.factor(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova$treatment)

is.factor(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova$treatment)


str(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova)

ggplot(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova, aes (treatment,percent_main_antho, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (percent_main_antho~treatment, monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova )
summary (anova_mono_antho)

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova_hist$percent_main_antho)

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova_hist$percent_main_antho)

monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova_hist<-monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_percentage_harvest_main_antho_anova_hist$percent_main_antho)



##### Percentage at first sampling point of 5 main types of anthocyanins####


monomeric_antho_no_leak_2019_percentage_first_point_main_antho <-main_monomeric_antho_concentration_percentage_2019%>%
  filter(Set == "1st point") %>%
  filter(!ï..Sample == "B1R2") %>%
  group_by(treatment, Rep, Block_id,main_antho)%>%
  summarise(percent_main_antho = sum((antho_mg_berry)/total_antho_mg_berry)*100)


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


ggsave(monomeric_antho_no_leak_2019_percentage_first_point_main_antho_avg_se, filename = "figures/monomeric_antho_no_leak_2019_percentage_first_point_main_antho_avg_se_redo.pdf", device = cairo_pdf, 
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