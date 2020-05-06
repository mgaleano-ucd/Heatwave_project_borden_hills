
#### Graph monomeric antho harvest tri and di hydroxylated ####
library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(viridis)
library(viridisLite)
library(ggplot2)
library(ggpubr)

#### Standard curve for malvidin-3-glucoside ####

std_curve_M3G_2019_HPLC<- read.csv("data/standard_curve_M3G_hplc.csv", header = TRUE)

std_curve_M3G_2019_HPLC<-std_curve_M3G_2019_HPLC%>%
mutate(conc_mg_L = ï..conc_mg_L)%>%
  select(conc_mg_L, mAU)

std_curve_M3G_2019_HPLC%>%
  group_by(conc_mg_L)%>%
  tally()

std_curve_M3G_2019_HPLC_plot <-std_curve_M3G_2019_HPLC%>%
  ggplot(aes(conc_mg_L, mAU)) +
  geom_point(alpha =1) +
  geom_smooth(method = "lm", se =FALSE, formula = y ~ x,color="#A9A9A9") +
  stat_regline_equation(aes(label =  paste(stat(eq.label), stat(rr.label), sep = "~~~~")),formula = y ~ x,size = 3.5, label.y.npc = 1) +
  theme_classic()+
  ylab ("Milli-Absorbance units (mAU)") +
  xlab("malvidin-3-glucoside (mg/L)") +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  scale_y_continuous(breaks=seq(0,6000,1000), limits = c (0,6000)) +
  scale_x_continuous (breaks=seq(0,1000,200), limits = c (0,1000))

ggsave(std_curve_M3G_2019_HPLC_plot , filename = "figures/std_curve_M3G_2019_HPLC_plot.pdf", device = cairo_pdf, width = 10, height = 8)



monomeric_antho_concentration_2019<- read.csv("data/raw_data_monomeric_anthocyanins_2019.csv", header =TRUE)


monomeric_antho_concentration_2019<-monomeric_antho_concentration_2019%>%
  mutate(dilution_factor = (Extract_final_vol/Extract_ini_vol))%>%
  mutate(concetration_mg_l = ((mAU+8.3917)/5.483)*dilution_factor)%>%
  mutate(antho_mg_berry = ((concetration_mg_l*Extract_ini_vol)/(1000*Berry_numb)))%>%
  mutate(antho_mg_g_berry_weight =((concetration_mg_l*Extract_ini_vol)/(1000*Berry_weight)))%>%
  mutate(antho_mg_g_Skin = ((concetration_mg_l*Extract_ini_vol)/(1000*Skin_weight_aft))) %>%
  mutate(Block_id = ï..Sample)%>%
  mutate( treatment = case_when(
    Block_id == "B1R2" ~ 1,
    Block_id == "B1R3" ~ 1,
    Block_id == "B1R4" ~ 1, 
    Block_id == "B2R1" ~ 2,
    Block_id == "B2R2" ~ 2,
    Block_id == "B2R3" ~ 2,
    Block_id == "B3R1" ~ 3,
    Block_id == "B3R2" ~ 3,
    Block_id == "B3R3" ~ 3,
  )) %>%
  mutate(main_antho = case_when(
    short_name_antho == "D3G" ~ "Delphinidins",
    short_name_antho == "C3G" ~ "Cyanidins",
    short_name_antho == "C3Gac" ~ "Cyanidins",
    short_name_antho == "D3Gac" ~ "Delphinidins",
    short_name_antho == "M3G" ~ "Malvidins",
    short_name_antho == "M3Gac" ~ "Malvidins",
    short_name_antho == "M3Gpc" ~ "Malvidins",
    short_name_antho == "Peo3G" ~ "Peonidins",
    short_name_antho == "Peo3Gac" ~ "Peonidins",
    short_name_antho == "Peo3Gpc" ~ "Peonidins",
    short_name_antho == "Pet3G" ~ "Petunidins",
    short_name_antho == "Pet3Gac" ~ "Petunidins",
  )) %>%
  mutate(hydroxylation =case_when(
    short_name_antho == "D3G" ~ "trihydroxylated",
    short_name_antho == "C3G" ~ "dihydroxylated",
    short_name_antho == "C3Gac" ~ "dihydroxylated",
    short_name_antho == "D3Gac" ~ "trihydroxylated",
    short_name_antho == "M3G" ~ "trihydroxylated",
    short_name_antho == "M3Gac" ~ "trihydroxylated",
    short_name_antho == "M3Gpc" ~ "trihydroxylated",
    short_name_antho == "Peo3G" ~ "dihydroxylated",
    short_name_antho == "Peo3Gac" ~ "dihydroxylated",
    short_name_antho == "Peo3Gpc" ~ "dihydroxylated",
    short_name_antho == "Pet3G" ~ "trihydroxylated",
    short_name_antho == "Pet3Gac" ~ "trihydroxylated"
  )) %>%
  group_by(Block_id, Rep, Set,treatment)%>%
  mutate(total_antho_mg_berry = sum(antho_mg_berry))


write.csv(monomeric_antho_concentration_2019,"data_output/monomeric_antho_concentration_2019.csv")

monomeric_antho_concentration_perc_tri_di_hydroxylated<-monomeric_antho_concentration_2019%>%
  group_by(hydroxylation, Block_id, Rep, Set,treatment)%>%
  summarise(conc_hydro = sum(antho_mg_berry))



##### Concentration at Harvest####

monomeric_antho_concentration_tri_di_hydroxylated_harvest<- monomeric_antho_concentration_perc_tri_di_hydroxylated %>%
  filter(Set == "Harvest") %>%
  filter(!Block_id == "B1R2") 

monomeric_antho_concentration_tri_di_hydroxylated_harvest$treatment<-format(monomeric_antho_concentration_tri_di_hydroxylated_harvest$treatment)
monomeric_antho_concentration_tri_di_hydroxylated_harvest$treatment<- as.character(monomeric_antho_concentration_tri_di_hydroxylated_harvest$treatment)

monomeric_antho_concentration_tri_di_hydroxylated_harvest$hydroxylation<- format(monomeric_antho_concentration_tri_di_hydroxylated_harvest$hydroxylation)
monomeric_antho_concentration_tri_di_hydroxylated_harvest$hydroxylation<- as.character(monomeric_antho_concentration_tri_di_hydroxylated_harvest$hydroxylation)


str(monomeric_antho_concentration_tri_di_hydroxylated_harvest)

monomeric_antho_concentration_tri_di_hydroxylated_harvest%>%
  group_by(hydroxylation, treatment) %>%
  tally()


se <- function(x) sqrt(var(x)/length(x))

monomeric_antho_concentration_tri_di_hydroxylated_harvest_avg_se <-monomeric_antho_concentration_tri_di_hydroxylated_harvest%>%
  group_by(hydroxylation, treatment) %>%
  summarise(avg = mean(conc_hydro), sev = se(conc_hydro))

pd <- position_dodge(0.95)



monomeric_antho_concentration_tri_di_hydroxylated_harvest<-ggplot(monomeric_antho_concentration_tri_di_hydroxylated_harvest_avg_se, aes(hydroxylation,avg, fill =treatment)) + 
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

ggsave(monomeric_antho_concentration_tri_di_hydroxylated_harvest, filename = "figures/monomeric_antho_concentration_tri_di_hydroxylated_harvest_redo.pdf", device = cairo_pdf, 
       width = 9, height = 6)

##### plotting percentage

monomeric_antho_percentage_tri_di_hydroxylated<-monomeric_antho_concentration_2019%>%
  group_by(hydroxylation, Block_id, Rep, Set,treatment)%>%
  summarise(percentage_hydro = sum((antho_mg_berry)/total_antho_mg_berry)*100)


monomeric_antho_percentage_tri_di_hydroxylated<- monomeric_antho_percentage_tri_di_hydroxylated %>%
  filter(Set == "Harvest") %>%
  filter(!Block_id == "B1R2") 

monomeric_antho_percentage_tri_di_hydroxylated$treatment<-format(monomeric_antho_percentage_tri_di_hydroxylated$treatment)
monomeric_antho_percentage_tri_di_hydroxylated$treatment<- as.character(monomeric_antho_percentage_tri_di_hydroxylated$treatment)

monomeric_antho_percentage_tri_di_hydroxylated$hydroxylation<- format(monomeric_antho_percentage_tri_di_hydroxylated$hydroxylation)
monomeric_antho_percentage_tri_di_hydroxylated$hydroxylation<- as.character(monomeric_antho_percentage_tri_di_hydroxylated$hydroxylation)



str(monomeric_antho_percentage_tri_di_hydroxylated)

monomeric_antho_percentage_tri_di_hydroxylated%>%
group_by(hydroxylation, treatment) %>%
  tally()

se <- function(x) sqrt(var(x)/length(x))

monomeric_antho_percentage_tri_di_hydroxylated_avg_se <-monomeric_antho_percentage_tri_di_hydroxylated%>%
  group_by(hydroxylation, treatment) %>%
  summarise(avg = mean(percentage_hydro), sev = se(percentage_hydro)) %>%
  filter(!hydroxylation == "trihydroxylated")

write.csv(monomeric_antho_percentage_tri_di_hydroxylated,"data_output/monomeric_antho_percentage_tri_di_hydroxylated_harvest.csv")


pd <- position_dodge(0.95)

monomeric_antho_perc_di_hydroxylated_harvest<-ggplot(monomeric_antho_percentage_tri_di_hydroxylated_avg_se, aes(hydroxylation,avg, fill =treatment)) + 
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
  theme(legend.position = "right")+
  scale_y_continuous(breaks=seq(0,90,10), limits = c (0,90))

ggsave(monomeric_antho_perc_di_hydroxylated_harvest, filename = "figures/monomeric_antho_perc_di_hydroxylated_harvest_redo.pdf", device = cairo_pdf, 
       width = 7, height = 6)

monomeric_antho_percentage_tri_di_hydroxylated_avg_se <-monomeric_antho_percentage_tri_di_hydroxylated%>%
  group_by(hydroxylation, treatment) %>%
  summarise(avg = mean(percentage_hydro), sev = se(percentage_hydro)) %>%
  filter(hydroxylation == "trihydroxylated")

pd <- position_dodge(0.95)

monomeric_antho_perc_tri_hydroxylated_harvest<-ggplot(monomeric_antho_percentage_tri_di_hydroxylated_avg_se, aes(hydroxylation,avg, fill =treatment)) + 
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
  scale_y_continuous(breaks=seq(0,90,10), limits = c (0,90))

ggsave(monomeric_antho_perc_tri_hydroxylated_harvest, filename = "figures/monomeric_antho_perc_tri_hydroxylated_harvest_redo.pdf", device = cairo_pdf, 
       width = 7, height = 6)

#### ANOVA TRI AND DI HYDROXYLATED HARVEST POINT####


##### TRIHYDROXYLATED #####

monomeric_antho_percentage_tri_di_hydroxylated<-read.csv("data_output/monomeric_antho_percentage_tri_di_hydroxylated_harvest.csv")

monomeric_antho_percentage_tri_di_hydroxylated_anova <-monomeric_antho_percentage_tri_di_hydroxylated %>%
  filter(hydroxylation == "trihydroxylated") 

monomeric_antho_percentage_tri_di_hydroxylated_anova %>%
  group_by(treatment)%>%
  tally()


is.factor(monomeric_antho_percentage_tri_di_hydroxylated_anova$treatment)

monomeric_antho_percentage_tri_di_hydroxylated_anova$treatment<- format(monomeric_antho_percentage_tri_di_hydroxylated_anova$treatment)
monomeric_antho_percentage_tri_di_hydroxylated_anova$treatment<- as.factor(monomeric_antho_percentage_tri_di_hydroxylated_anova$treatment)

is.factor(monomeric_antho_percentage_tri_di_hydroxylated_anova$treatment)

ggplot(monomeric_antho_percentage_tri_di_hydroxylated_anova, aes (treatment,percentage_hydro, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (percentage_hydro~treatment, monomeric_antho_percentage_tri_di_hydroxylated_anova )
summary (anova_mono_antho)

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_percentage_tri_di_hydroxylated_anova_hist<-monomeric_antho_percentage_tri_di_hydroxylated_anova %>%
  filter(treatment == 1)
hist(monomeric_antho_percentage_tri_di_hydroxylated_anova_hist$percentage_hydro)

monomeric_antho_percentage_tri_di_hydroxylated_anova_hist<-monomeric_antho_percentage_tri_di_hydroxylated_anova %>%
  filter(treatment == 2)
hist(monomeric_antho_percentage_tri_di_hydroxylated_anova_hist$percentage_hydro)

monomeric_antho_percentage_tri_di_hydroxylated_anova_hist<-monomeric_antho_percentage_tri_di_hydroxylated_anova %>%
  filter(treatment == 3)
hist(monomeric_antho_percentage_tri_di_hydroxylated_anova_hist$percentage_hydro)


##### DIHYDROXYLATED #####

monomeric_antho_percentage_tri_di_hydroxylated_anova <-monomeric_antho_percentage_tri_di_hydroxylated %>%
  filter(!hydroxylation == "trihydroxylated") 


monomeric_antho_percentage_tri_di_hydroxylated_anova %>%
  group_by(treatment)%>%
  tally()


is.factor(monomeric_antho_percentage_tri_di_hydroxylated_anova$treatment)

monomeric_antho_percentage_tri_di_hydroxylated_anova$treatment<- format(monomeric_antho_percentage_tri_di_hydroxylated_anova$treatment)
monomeric_antho_percentage_tri_di_hydroxylated_anova$treatment<- as.factor(monomeric_antho_percentage_tri_di_hydroxylated_anova$treatment)

is.factor(monomeric_antho_percentage_tri_di_hydroxylated_anova$treatment)

ggplot(monomeric_antho_percentage_tri_di_hydroxylated_anova, aes (treatment,percentage_hydro, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (percentage_hydro~treatment, monomeric_antho_percentage_tri_di_hydroxylated_anova )
summary (anova_mono_antho)

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_percentage_tri_di_hydroxylated_anova_hist<-monomeric_antho_percentage_tri_di_hydroxylated_anova %>%
  filter(treatment == 1)
hist(monomeric_antho_percentage_tri_di_hydroxylated_anova_hist$percentage_hydro)

monomeric_antho_percentage_tri_di_hydroxylated_anova_hist<-monomeric_antho_percentage_tri_di_hydroxylated_anova %>%
  filter(treatment == 2)
hist(monomeric_antho_percentage_tri_di_hydroxylated_anova_hist$percentage_hydro)

monomeric_antho_percentage_tri_di_hydroxylated_anova_hist<-monomeric_antho_percentage_tri_di_hydroxylated_anova %>%
  filter(treatment == 3)
hist(monomeric_antho_percentage_tri_di_hydroxylated_anova_hist$percentage_hydro)


##### First point #####

monomeric_antho_percentage_tri_di_hydroxylated<-monomeric_antho_concentration_2019%>%
  group_by(hydroxylation, Block_id, Rep, Set,treatment)%>%
  summarise(percentage_hydro = sum((antho_mg_berry)/total_antho_mg_berry)*100)

monomeric_antho_perc_tri_di_hydroxylated_first_point<-monomeric_antho_percentage_tri_di_hydroxylated %>%
  filter(Set == "1st point") %>%
  filter(!Block_id == "B1R2") 

monomeric_antho_perc_tri_di_hydroxylated_first_point$treatment<-format(monomeric_antho_perc_tri_di_hydroxylated_first_point$treatment)
monomeric_antho_perc_tri_di_hydroxylated_first_point$treatment<- as.character(monomeric_antho_perc_tri_di_hydroxylated_first_point$treatment)

monomeric_antho_perc_tri_di_hydroxylated_first_point$hydroxylation<- format(monomeric_antho_perc_tri_di_hydroxylated_first_point$hydroxylation)
monomeric_antho_perc_tri_di_hydroxylated_first_point$hydroxylation<- as.character(monomeric_antho_perc_tri_di_hydroxylated_first_point$hydroxylation)


str(monomeric_antho_perc_tri_di_hydroxylated_first_point)


se <- function(x) sqrt(var(x)/length(x))

monomeric_antho_perc_tri_di_hydroxylated_first_point%>%
  group_by(hydroxylation, treatment) %>%
  tally()


monomeric_antho_perc_tri_di_hydroxylated_first_point_avg_se <-monomeric_antho_perc_tri_di_hydroxylated_first_point%>%
  group_by(hydroxylation, treatment) %>%
  summarise(avg = mean(percentage_hydro), sev = se(percentage_hydro)) %>%
  filter(!hydroxylation == "trihydroxylated")

pd <- position_dodge(0.95)

write.csv(monomeric_antho_perc_tri_di_hydroxylated_first_point,"data_output/monomeric_antho_perc_tri_di_hydroxylated_first_point.csv")

monomeric_antho_perc_di_hydroxylated_first_point<-ggplot(monomeric_antho_perc_tri_di_hydroxylated_first_point_avg_se, aes(hydroxylation,avg, fill =treatment)) + 
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
  theme(legend.position = "right")  +
  scale_y_continuous(breaks=seq(0,90,10), limits = c (0,90))

ggsave(monomeric_antho_perc_di_hydroxylated_first_point, filename = "figures/monomeric_antho_perc_di_hydroxylated_first_point_redo.pdf", device = cairo_pdf, 
       width = 7, height = 6)

monomeric_antho_perc_tri_di_hydroxylated_first_point_avg_se <-monomeric_antho_perc_tri_di_hydroxylated_first_point%>%
  group_by(hydroxylation, treatment) %>%
  summarise(avg = mean(percentage_hydro), sev = se(percentage_hydro)) %>%
  filter(hydroxylation == "trihydroxylated")

pd <- position_dodge(0.95)

monomeric_antho_perc_tri_hydroxylated_first_point<-ggplot(monomeric_antho_perc_tri_di_hydroxylated_first_point_avg_se, aes(hydroxylation,avg, fill =treatment)) + 
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
  theme(legend.position = "right")  +
  scale_y_continuous(breaks=seq(0,90,10), limits = c (0,90))

ggsave(monomeric_antho_perc_tri_hydroxylated_first_point, filename = "figures/monomeric_antho_perc_tri_hydroxylated_first_point_redo.pdf", device = cairo_pdf, 
       width = 7, height = 6)


#### ANOVA TRI AND DI HYDROXYLATED first POINT####


##### TRIHYDROXYLATED #####

monomeric_antho_perc_tri_di_hydroxylated_first_point<-read.csv("data_output/monomeric_antho_perc_tri_di_hydroxylated_first_point.csv")

monomeric_antho_perc_tri_di_hydroxylated_first_point_anova <-monomeric_antho_perc_tri_di_hydroxylated_first_point %>%
  filter(hydroxylation == "trihydroxylated") 

monomeric_antho_perc_tri_di_hydroxylated_first_point_anova %>%
  group_by(treatment)%>%
  tally()


is.factor(monomeric_antho_perc_tri_di_hydroxylated_first_point_anova$treatment)

monomeric_antho_perc_tri_di_hydroxylated_first_point_anova$treatment<- format(monomeric_antho_perc_tri_di_hydroxylated_first_point_anova$treatment)
monomeric_antho_perc_tri_di_hydroxylated_first_point_anova$treatment<- as.factor(monomeric_antho_perc_tri_di_hydroxylated_first_point_anova$treatment)

is.factor(monomeric_antho_perc_tri_di_hydroxylated_first_point_anova$treatment)

ggplot(monomeric_antho_perc_tri_di_hydroxylated_first_point_anova, aes (treatment,percentage_hydro, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (percentage_hydro~treatment, monomeric_antho_perc_tri_di_hydroxylated_first_point_anova )
summary (anova_mono_antho)

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_perc_tri_di_hydroxylated_first_point_anova_hist<-monomeric_antho_perc_tri_di_hydroxylated_first_point_anova %>%
  filter(treatment == 1)
hist(monomeric_antho_perc_tri_di_hydroxylated_first_point_anova_hist$percentage_hydro)

monomeric_antho_perc_tri_di_hydroxylated_first_point_anova_hist<-monomeric_antho_perc_tri_di_hydroxylated_first_point_anova %>%
  filter(treatment == 2)
hist(monomeric_antho_perc_tri_di_hydroxylated_first_point_anova_hist$percentage_hydro)

monomeric_antho_perc_tri_di_hydroxylated_first_point_anova_hist<-monomeric_antho_perc_tri_di_hydroxylated_first_point_anova %>%
  filter(treatment == 3)
hist(monomeric_antho_perc_tri_di_hydroxylated_first_point_anova_hist$percentage_hydro)


##### DIHYDROXYLATED #####

monomeric_antho_perc_tri_di_hydroxylated_first_point_anova <-monomeric_antho_perc_tri_di_hydroxylated_first_point %>%
  filter(!hydroxylation == "trihydroxylated") 


monomeric_antho_perc_tri_di_hydroxylated_first_point_anova %>%
  group_by(treatment)%>%
  tally()


is.factor(monomeric_antho_perc_tri_di_hydroxylated_first_point_anova$treatment)

monomeric_antho_perc_tri_di_hydroxylated_first_point_anova$treatment<- format(monomeric_antho_perc_tri_di_hydroxylated_first_point_anova$treatment)
monomeric_antho_perc_tri_di_hydroxylated_first_point_anova$treatment<- as.factor(monomeric_antho_perc_tri_di_hydroxylated_first_point_anova$treatment)

is.factor(monomeric_antho_perc_tri_di_hydroxylated_first_point_anova$treatment)

ggplot(monomeric_antho_perc_tri_di_hydroxylated_first_point_anova, aes (treatment,percentage_hydro, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (percentage_hydro~treatment, monomeric_antho_perc_tri_di_hydroxylated_first_point_anova )
summary (anova_mono_antho)

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_perc_tri_di_hydroxylated_first_point_anova_hist<-monomeric_antho_perc_tri_di_hydroxylated_first_point_anova %>%
  filter(treatment == 1)
hist(monomeric_antho_perc_tri_di_hydroxylated_first_point_anova_hist$percentage_hydro)

monomeric_antho_perc_tri_di_hydroxylated_first_point_anova_hist<-monomeric_antho_perc_tri_di_hydroxylated_first_point_anova %>%
  filter(treatment == 2)
hist(monomeric_antho_perc_tri_di_hydroxylated_first_point_anova_hist$percentage_hydro)

monomeric_antho_perc_tri_di_hydroxylated_first_point_anova_hist<-monomeric_antho_perc_tri_di_hydroxylated_first_point_anova %>%
  filter(treatment == 3)
hist(monomeric_antho_perc_tri_di_hydroxylated_first_point_anova_hist$percentage_hydro)


