library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(viridisLite)
library(viridis)


se <- function(x) sqrt(var(x)/length(x))



berry_basic_chemistry <- read.csv("data/berry_chemistry_2019_3.csv", header = TRUE)

berry_basic_chemistry_no_first_point <- berry_basic_chemistry%>%
  mutate(date = ï..date) %>%
  filter(!date == "8/2/2019")%>%
  mutate(Block_id = sample_id) %>%
  filter(!Block_id =="B1R2")%>%
  select(Block_id, date, treatment, TA, Brix, pH, Rep)
  

somers_data_2019<- read.csv("data/raw_data_somers_2019.csv", header = TRUE)

somers_data_2019<- somers_data_2019%>%
  mutate(dilution_factor = (Extract_final_vol/Extract_ini_vol))%>%
  mutate(total_antho_mg_l = (((20*((50*Treatment_D_corrected_abs_520)-1.6667*(10*Treatment_B_corrected_abs_520))))*dilution_factor))%>%
  mutate(total_Antho_mg_berry = ((total_antho_mg_l*Extract_ini_vol)/(1000*Berry_numb)))%>%
  mutate(total_antho_mg_g_berry_weight =((total_antho_mg_l*Extract_ini_vol)/(1000*Berry_weight)))%>%
  mutate(total_Antho_mg_g_Skin = ((total_antho_mg_l*Extract_ini_vol)/(1000*Skin_weight_aft))) %>%
  mutate(total_phenolic_w_DF = (((Treatment_D_corrected_abs_280*50)-4)*dilution_factor))%>%
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
  filter(!ï..Date_analysis == "1/14/2020")%>%
  filter(!ï..Date_analysis == "1/15/2020")



somers_berry_data<- somers_data_2019 %>%
  filter(!Block_id == "B1R2")

str(somers_berry_data$Date_sampled)

somers_berry_data<-somers_berry_data %>%
  filter(!is.na (Rep))%>%
  mutate(date = Date_sampled)%>%
  filter(!Block_id =="B1R2")%>%
  select(date, total_Antho_mg_berry, total_antho_mg_g_berry_weight, treatment, Block_id, Rep, Berry_weight) %>%
  mutate(berry_weight_one_berry = (Berry_weight/60))


somers_and_basic_berry_chemisrty<- merge(somers_berry_data,berry_basic_chemistry_no_first_point)

str(somers_and_basic_berry_chemisrty$treatment)

somers_and_basic_berry_chemisrty$treatment <- format(somers_and_basic_berry_chemisrty$treatment)
as.character(somers_and_basic_berry_chemisrty$treatment)

library(ggpubr)

somers_and_basic_berry_chemisrty_plot <-somers_and_basic_berry_chemisrty%>%
  ggplot(aes(Brix, total_Antho_mg_berry)) +
  geom_point(alpha =1, aes(color = treatment, group =treatment)) +
  geom_smooth(method = "lm", se= FALSE, formula = y ~ poly(x,3), aes(color = treatment, group =treatment)) +
  stat_regline_equation(aes(color = treatment, group =treatment, label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
                        formula = y ~ poly(x,3), size = 3.5
  ) +
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab ("Total anthocyanins (mg/berry)") +
  xlab("Total soluble solids (ºBx) ") +
  theme(axis.title.y = element_text(size=20, family = "serif")) +
  theme(axis.title.x = element_text(size=20, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,0.9,0.1), limits = c (0,0.9)) +
  scale_x_continuous (breaks=seq(15,28,2), limits = c (15,28))  +
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))

somers_and_basic_berry_chemisrty_plot <-somers_and_basic_berry_chemisrty%>%
  ggplot(aes(Brix, total_Antho_mg_berry)) +
  geom_point(alpha =1, aes(color = treatment, group =treatment)) +
  geom_smooth(method = "lm", se =FALSE, formula = y ~ x, aes(color = treatment, group =treatment)) +
  stat_fit_glance(aes(color = treatment, group =treatment, label  = sprintf('R^2~"="~%.3f~~italic(P)~"<"~%.2g ', stat(round(r.squared,2)),(p.value = 0.001))),formula = y ~ x, size = 5, parse =TRUE
  )+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab ("Total anthocyanins (mg/berry)") +
  xlab("Total soluble solids (ºBx) ") +
  theme(axis.title.y = element_text(size=18, family = "serif")) +
  theme(axis.title.x = element_text(size=18, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,0.9,0.1), limits = c (0,0.9)) +
  scale_x_continuous (breaks=seq(15,28,2), limits = c (15,28))  +
  theme(axis.text.x = element_text(size =16))+
  theme(axis.text.y = element_text(size =16))

ggsave(somers_and_basic_berry_chemisrty_plot, filename = "figures/somers_and_basic_berry_chemistry_plot.pdf", device = cairo_pdf, 
       width = 8, height = 6)


brix_and_berry_weight_plot <-somers_and_basic_berry_chemisrty%>%
  ggplot(aes(Brix, berry_weight_one_berry)) +
  geom_point(alpha =1, aes(color = treatment, group =treatment)) +
  geom_smooth(method = "lm", se =FALSE, formula = y ~ x, aes(color = treatment, group =treatment)) +
  stat_regline_equation(aes(color = treatment, group =treatment, label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
                        formula = y ~ x, size = 3.5, label.y.npc = 0.3
  ) +
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab ("berry weight (g)") +
  xlab("Brix (º Bx) ") +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,1.2,0.2), limits = c (0,1.2)) +
  scale_x_continuous (breaks=seq(15,28,2), limits = c (15,28))

ggsave(brix_and_berry_weight_plot, filename = "figures/brix_and_berry_weight_plot.pdf", device = cairo_pdf, 
       width = 8, height = 6)


####summary regression lines brix vs total anthocyanins ####
somers_and_basic_berry_chemisrty_lm<-somers_and_basic_berry_chemisrty%>%
  select(treatment,total_Antho_mg_berry, Brix)%>%
  filter(treatment == "1")

somers_and_basic_berry_chemisrty_lm<-  lm(formula = Brix ~ total_Antho_mg_berry, somers_and_basic_berry_chemisrty_lm)

summary(somers_and_basic_berry_chemisrty_lm)
summary(somers_and_basic_berry_chemisrty_lm)$coefficient
confint(somers_and_basic_berry_chemisrty_lm)


somers_and_basic_berry_chemisrty_lm<-somers_and_basic_berry_chemisrty%>%
  select(treatment,total_Antho_mg_berry, Brix)%>%
  filter(treatment == "2")

somers_and_basic_berry_chemisrty_lm<-  lm(formula = Brix ~ total_Antho_mg_berry, somers_and_basic_berry_chemisrty_lm)

summary(somers_and_basic_berry_chemisrty_lm)
summary(somers_and_basic_berry_chemisrty_lm)$coefficient
confint(somers_and_basic_berry_chemisrty_lm)


somers_and_basic_berry_chemisrty_lm<-somers_and_basic_berry_chemisrty%>%
  select(treatment,total_Antho_mg_berry, Brix)%>%
  filter(treatment == "3")

somers_and_basic_berry_chemisrty_lm<-  lm(formula = Brix ~ total_Antho_mg_berry, somers_and_basic_berry_chemisrty_lm)

summary(somers_and_basic_berry_chemisrty_lm)
summary(somers_and_basic_berry_chemisrty_lm)$coefficient
confint(somers_and_basic_berry_chemisrty_lm)