##### Plotting berry chemistry 2019

library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)

berry_chemistry_borden_hills_2019 <-read.csv("data/berry_chemistry_2019_3.csv", header = TRUE)

str(berry_chemistry_borden_hills_2019)

berry_chemistry_borden_hills_2019 <-berry_chemistry_borden_hills_2019%>%
  mutate(date = ï..date) %>%
  filter(!is.na(Rep)) %>%
  filter(!pixel_number == (34))

berry_chemistry_borden_hills_2019$date<- mdy(berry_chemistry_borden_hills_2019$date)

str(berry_chemistry_borden_hills_2019$date)

tz(berry_chemistry_borden_hills_2019$date)

str(berry_chemistry_borden_hills_2019$date)

se <- function(x) sqrt(var(x)/length(x))


berry_chemistry_borden_hills_2019_grouped <-berry_chemistry_borden_hills_2019 %>%
  group_by(treatment, date) %>%
  summarise(avg_brix = mean(Brix), se_brix = se(Brix), avg_ph = mean(pH), se_ph = se (pH), avg_ta = mean(TA), se_ta = se (TA))

berry_chemistry_borden_hills_2019 %>%
  group_by(treatment, date) %>%
  tally()


berry_chemistry_borden_hills_2019$treatment<- reorder(berry_chemistry_borden_hills_2019$treatment, berry_chemistry_borden_hills_2019$date)

berry_chemistry_borden_hills_2019_grouped$treatment<- reorder(berry_chemistry_borden_hills_2019_grouped$treatment, berry_chemistry_borden_hills_2019_grouped$date)

str(berry_chemistry_borden_hills_2019_grouped$treatment)

berry_chemistry_borden_hills_2019_grouped$treatment<- format(berry_chemistry_borden_hills_2019_grouped$treatment)
as.character(berry_chemistry_borden_hills_2019_grouped$treatment)



#Plot berry chemistry 2019
library(wesanderson)
library(ggsci)
library(extrafont)

pd <- position_dodge(1.5)


#Plot Brix

brix_BH_2019_2<-ggplot(berry_chemistry_borden_hills_2019_grouped, aes(date, avg_brix, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg_brix-se_brix, ymax=avg_brix+se_brix), width= 3, position=pd, stat = "identity", size =1.02) +
  geom_line(alpha =0.7, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha =0.65, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(label = "Brix (ºBx)") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Days") +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(12,28,2), limits = c (12,28)) +
  scale_x_date(date_labels="%b %d", date_breaks = ("7 day")) +
  annotate("text", x = as.Date("08-15-2019", format= "%m-%d-%Y"), y = 28, label = "*", size = 6) +
  annotate("text", x = as.Date("08-20-2019", format= "%m-%d-%Y"), y = 28, label = "*", size = 6) +
  annotate("text", x = as.Date("09-03-2019", format= "%m-%d-%Y"), y = 28, label = "*", size = 6) +
  geom_vline(xintercept = as.Date("08-12-2019", format ="%m-%d-%Y"), color =  "darkgrey", size = 0.5, linetype ="dashed") +
  geom_vline(xintercept = as.Date("08-16-2019", format ="%m-%d-%Y"), color = "darkgrey", size = 0.5, linetype ="dashed")


ggsave(brix_BH_2019_2, filename = "figures/brix_BH_2019_2.pdf", device = cairo_pdf, 
       width = 8, height = 6)



#Plot pH 

ph_BH_2019_2<-ggplot(berry_chemistry_borden_hills_2019_grouped, aes(date, avg_ph, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg_ph-se_ph, ymax=avg_ph+se_ph), width= 3, position=pd, stat = "identity", size =1.02) +
  geom_line(alpha =0.7, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha =0.65, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(label = "pH") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Days") +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(2.7,3.9,0.15), limits = c (2.7,3.9)) +
  scale_x_date(date_labels="%b %d", date_breaks = ("7 day")) +
  annotate("text", x = as.Date("08-15-2019", format= "%m-%d-%Y"), y = 3.85, label = "*", size = 6) +
  annotate("text", x = as.Date("08-20-2019", format= "%m-%d-%Y"), y = 3.85, label = "*", size = 6) +
  annotate("text", x = as.Date("09-03-2019", format= "%m-%d-%Y"), y = 3.85, label = "*", size = 6) +
  annotate("text", x = as.Date("09-18-2019", format= "%m-%d-%Y"), y = 3.85, label = "*", size = 6) +
  geom_vline(xintercept = as.Date("08-12-2019", format ="%m-%d-%Y"), color =  "darkgrey", size = 0.5, linetype ="dashed") +
  geom_vline(xintercept = as.Date("08-16-2019", format ="%m-%d-%Y"), color = "darkgrey", size = 0.5, linetype ="dashed")



ggsave(ph_BH_2019_2, filename = "figures/pH_Bh_2019_2.pdf", device = cairo_pdf, 
       width = 8, height = 6)


#Plot TA

ta_BH_2019_2<-ggplot(berry_chemistry_borden_hills_2019_grouped, aes(date, avg_ta, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg_ta-se_ta, ymax=avg_ta+se_ta), width= 3, position=pd, stat = "identity", size =1.02) +
  geom_line(alpha =0.7, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha =0.65, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(label = "Titrable acidity (g/L)") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Days") +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(2,20,2), limits = c (2,20)) +
  scale_x_date(date_labels="%b %d", date_breaks = ("7 day")) +
  annotate("text", x = as.Date("08-15-2019", format= "%m-%d-%Y"), y = 19, label = "*", size = 6) +
  annotate("text", x = as.Date("09-03-2019", format= "%m-%d-%Y"), y = 19, label = "*", size = 6) +
  annotate("text", x = as.Date("09-21-2019", format= "%m-%d-%Y"), y = 19, label = "*", size = 6) +
  geom_vline(xintercept = as.Date("08-12-2019", format ="%m-%d-%Y"), color =  "darkgrey", size = 0.5, linetype ="dashed") +
  geom_vline(xintercept = as.Date("08-16-2019", format ="%m-%d-%Y"), color = "darkgrey", size = 0.5, linetype ="dashed")



ggsave(ta_BH_2019_2, filename = "figures/ta_BH_2019_2.pdf", device = cairo_pdf, 
       width = 8, height = 6)

library(cowplot)
panel_plot_berry_chemistry_2019_se_ano1<- plot_grid (brix_BH_2019_2, ph_BH_2019_2, ta_BH_2019_2, ncol=3, nrow = 1)

ggsave(panel_plot_berry_chemistry_2019_se_ano1, filename = "figures/panel_plot_berry_chemistry_2019_se.pdf", device = cairo_pdf, width = 17, height = 6)

#####-------------------------------#########################

##oLD COLORS 

#Plot Brix

pd <- position_dodge(2)
brix_BH_2019_avg_se_2<-ggplot(berry_chemistry_borden_hills_2019_grouped, aes(date, avg_brix, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg_brix-se_brix, ymax=avg_brix+se_brix), width= 1.5, position=pd, stat = "identity") +
  geom_line(alpha =0.5, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha =0.3, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab(label = "Brix (ºBx)") +
  ggtitle( " Brix vs time BH 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Days") +
  theme(axis.title.y = element_text(size=16, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(13,28,2), limits = c (13,28)) +
  scale_x_date(date_labels="%b %d", date_breaks = ("7 day"))


ggsave(brix_BH_2019_avg_se_2, filename = "figures/brix_BH_2019_avg_se_2.pdf", device = cairo_pdf, 
       width = 8, height = 6)


#Plot pH 

ph_BH_2019_avg_se_2<-ggplot(berry_chemistry_borden_hills_2019_grouped, aes(date, avg_ph, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg_ph-se_ph, ymax=avg_ph+se_ph), width= 1.5, position=pd, stat = "identity") +
  geom_line(alpha =0.5, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha =0.3, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab(label = "pH") +
  ggtitle( " pH vs time BH 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Days") +
  theme(axis.title.y = element_text(size=16, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(2.7,3.9,0.15), limits = c (2.7,3.9)) +
  scale_x_date(date_labels="%b %d", date_breaks = ("7 day"))


ggsave(ph_BH_2019_avg_se_2, filename = "figures/ph_BH_2019_avg_se_2.pdf", device = cairo_pdf, 
       width = 8, height = 6)


#Plot TA

ta_BH_2019_avg_se_2<-ggplot(berry_chemistry_borden_hills_2019_grouped, aes(date, avg_ta, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg_ta-se_ta, ymax=avg_ta+se_ta), width= 1.5, position=pd, stat = "identity") +
  geom_line(alpha =0.5, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha =0.3, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab(label = "TA (g/L)") +
  ggtitle( " TA vs time BH 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Days") +
  theme(axis.title.y = element_text(size=16, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,25,2), limits = c (0,25)) +
  scale_x_date(date_labels="%b %d", date_breaks = ("7 day"))


ggsave(ta_BH_2019_avg_se_2, filename = "figures/ta_BH_2019_avg_se_2.pdf", device = cairo_pdf, 
       width = 8, height = 6)


panel_plot_berry_chemistry_2019_avg_se_2 <- plot_grid (brix_BH_2019_avg_se_2, ph_BH_2019_avg_se_2, ta_BH_2019_avg_se_2, ncol=3, nrow = 1)

ggsave(panel_plot_berry_chemistry_2019_avg_se_2 , filename = "figures/panel_plot_berry_chemistry_2019_avg_se_2.pdf", device = cairo_pdf, width = 16, height = 5)