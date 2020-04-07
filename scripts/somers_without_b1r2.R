library(tidyverse)
library(grDevices)
library(lubridate)
library(ggplot2)



no_leak_somers_first_results<- read.csv ("data/somers_berry_phenolics.csv", header = TRUE)

no_leak_somers_first_results<- no_leak_somers_first_results%>%
  filter(!Block_id == "B1R2")

se <- function(x) sqrt(var(x)/length(x))

str(no_leak_somers_first_results$Date_sampled)

####mg/berry####

no_leak_somers_first_results_ave_stdv_mg_berry<- no_leak_somers_first_results %>%
  select(Date_sampled, Block_id, Rep, Total_antho_mg_L_w_dil_factor, treatment, total_Antho_mg_berry, total_antho_mg_l, total_antho_mg_g_berry_weight, total_antho_mg_g_berry_weight, total_Antho_mg_g_Skin, Total_phenolics, Hue, degree_ionization_.) %>%
  group_by(Date_sampled, treatment)%>%
  filter(!is.na(total_Antho_mg_berry)) %>%
  summarise (avg = mean(total_Antho_mg_berry), sev = se(total_Antho_mg_berry), stdv = sd(total_Antho_mg_berry))


no_leak_somers_first_results_ave_stdv_mg_berry_blocks<- no_leak_somers_first_results %>%
  select(Date_sampled, Block_id, Rep, Total_antho_mg_L_w_dil_factor, treatment, total_Antho_mg_berry, total_antho_mg_l, total_antho_mg_g_berry_weight, total_antho_mg_g_berry_weight, total_Antho_mg_g_Skin, Total_phenolics, Hue, degree_ionization_.) %>%
  filter(!is.na(total_Antho_mg_berry)) %>%
  group_by(Date_sampled, Block_id)%>%
  summarise (avg = mean(total_Antho_mg_berry), sev = se(total_Antho_mg_berry), stdv = sd(total_Antho_mg_berry))


write.csv(no_leak_somers_first_results_ave_stdv_mg_berry_blocks,"data_output/no_leak_somers_first_results_ave_stdv_mg_berry_blocks.csv")

write.csv(no_leak_somers_first_results_ave_stdv_mg_berry, "data_output/no_leak_somers_first_results_ave_stdv_mg_berry.csv")

no_leak_somers_first_results_ave_stdv_mg_berry$Date_sampled<- mdy(no_leak_somers_first_results_ave_stdv_mg_berry$Date_sampled)

str(no_leak_somers_first_results_ave_stdv_mg_berry$Date_sampled)

tz(no_leak_somers_first_results_ave_stdv_mg_berry$Date_sampled)

no_leak_somers_first_results_ave_stdv_mg_berry_blocks$Date_sampled<- mdy(no_leak_somers_first_results_ave_stdv_mg_berry_blocks$Date_sampled)

str(no_leak_somers_first_results_ave_stdv_mg_berry_blocks$Date_sampled)

tz(no_leak_somers_first_results_ave_stdv_mg_berry_blocks$Date_sampled)


no_leak_somers_first_results_ave_stdv_mg_berry$treatment <-format(no_leak_somers_first_results_ave_stdv_mg_berry$treatment)
as.character(no_leak_somers_first_results_ave_stdv_mg_berry$treatment)


pd<- position_dodge(0.5)

no_leak_somers_mg_berry_treatment_date_ok<-ggplot(no_leak_somers_first_results_ave_stdv_mg_berry, aes(Date_sampled, avg, group = treatment, color = treatment)) + 
  geom_errorbar(alpha=1, aes(ymin=avg-sev, ymax=avg+sev), width= 3, position=pd, stat = "identity", size =1) +
  geom_line(alpha =1, size =1.1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(position = pd)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab ("mg anthocyanin/berry") +
  ggtitle( " Total anthocyanins") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,0.8,0.1), limits = c (0,0.8))+
  scale_x_date(date_labels="%b %d", date_breaks = ("7 day")) +
  geom_vline(xintercept = as.Date("08-12-2019", format ="%m-%d-%Y"), color =  "darkgrey", size = 0.5, linetype ="dashed") +
  geom_vline(xintercept = as.Date("08-16-2019", format ="%m-%d-%Y"), color = "darkgrey", size = 0.5, linetype ="dashed")+
  annotate("text", x = as.Date("08-14-2019", format= "%m-%d-%Y"), y = 0.75, label = "HW2", size = 4) 


library(wesanderson)
ggsave(no_leak_somers_mg_berry_treatment_date_ok, filename = "figures/no_leak_somers_mg_berry_treatment_date_ok.pdf", device = cairo_pdf, 
       width = 9, height = 7)

#### Antho based on blocks not good graph, change ###
library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)

pd<- position_dodge(8)

no_leak_somers_mg_berry_block<-ggplot(no_leak_somers_first_results_ave_stdv_mg_berry_blocks, aes(Date_sampled, avg, group = Block_id, color = Block_id)) + 
  geom_errorbar(aes(ymin=avg-sev, ymax=avg+sev), width= 1, position=pd, stat = "identity") +
  geom_bar(alpha =0.5, size =1, position=pd, stat = "identity", linetype = "solid", aes(fill = Block_id)) +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu") +
  scale_color_brewer(palette = "RdYlBu") +
  ylab ("mg/berry") +
  ggtitle( " Total anthocyanins") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=14, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14,face = "bold", family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,0.8,0.1), limits = c (0,0.8)) 

ggsave(no_leak_somers_mg_berry_block, filename = "figures/no_leak_somers_mg_berry_block.pdf", device = cairo_pdf, 
       width = 9, height = 7)

#####mg/g berry ####

no_leak_somers_first_results_ave_stdv_mg_g_berry_weight<- no_leak_somers_first_results %>%
  select(Date_sampled, Block_id, Rep, Total_antho_mg_L_w_dil_factor, treatment, total_Antho_mg_berry, total_antho_mg_l, total_antho_mg_g_berry_weight, total_antho_mg_g_berry_weight, total_Antho_mg_g_Skin, Total_phenolics, Hue, degree_ionization_.) %>%
  filter(!is.na(total_Antho_mg_berry)) %>%
  group_by(Date_sampled, treatment)%>%
  summarise (avg= mean(total_antho_mg_g_berry_weight), sev = se(total_antho_mg_g_berry_weight), stdv = sd(total_antho_mg_g_berry_weight))

no_leak_somers_first_results_ave_stdv_mg_g_berry_weight_blocks<- no_leak_somers_first_results %>%
  select(Date_sampled, Block_id, Rep, Total_antho_mg_L_w_dil_factor, treatment, total_Antho_mg_berry, total_antho_mg_l, total_antho_mg_g_berry_weight, total_antho_mg_g_berry_weight, total_Antho_mg_g_Skin, Total_phenolics, Hue, degree_ionization_.)%>%
  filter(!is.na(total_Antho_mg_berry)) %>%
  group_by(Date_sampled, Block_id) %>%
  summarise (avg= mean(total_antho_mg_g_berry_weight), sev = se(total_antho_mg_g_berry_weight), stdv = sd(total_antho_mg_g_berry_weight))

write.csv(no_leak_somers_first_results_ave_stdv_mg_g_berry_weight_blocks, "data_output/no_leak_somers_first_results_ave_stdv_mg_g_berry_weight_blocks.csv")

write.csv(no_leak_somers_first_results_ave_stdv_mg_g_berry_weight, "data_output/no_leak_somers_first_results_ave_stdv_mg_g_berry_weight.csv")

no_leak_somers_first_results_ave_stdv_mg_g_berry_weight$Date_sampled<- mdy(no_leak_somers_first_results_ave_stdv_mg_g_berry_weight$Date_sampled)

str(no_leak_somers_first_results_ave_stdv_mg_g_berry_weight$Date_sampled)

tz(no_leak_somers_first_results_ave_stdv_mg_g_berry_weight$Date_sampled)


no_leak_somers_first_results_ave_stdv_mg_g_berry_weight_blocks$Date_sampled<- mdy(no_leak_somers_first_results_ave_stdv_mg_g_berry_weight_blocks$Date_sampled)

str(no_leak_somers_first_results_ave_stdv_mg_g_berry_weight_blocks$Date_sampled)

tz(no_leak_somers_first_results_ave_stdv_mg_g_berry_weight_blocks$Date_sampled)

no_leak_somers_first_results_ave_stdv_mg_g_berry_weight$treatment <-format(no_leak_somers_first_results_ave_stdv_mg_g_berry_weight$treatment)
as.character(no_leak_somers_first_results_ave_stdv_mg_g_berry_weight$treatment)


pd<- position_dodge(0.5)
no_leak_somers_mg_g_berry_weight_treatment_date_ok<-ggplot(no_leak_somers_first_results_ave_stdv_mg_g_berry_weight, aes(Date_sampled, avg, group = treatment, color = treatment)) + 
  geom_errorbar(alpha =1, aes(ymin=avg-sev, ymax=avg+sev), width= 4, position=pd, stat = "identity", size =1) +
  geom_line(alpha =1, size =1.1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(position = pd)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab ("mg anthocyanin/g berry wt.") +
  ggtitle( " Total anthocyanins") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,0.7,0.1), limits = c (0,0.7)) +
  scale_x_date(date_labels="%b %d", date_breaks = ("7 day")) +
  geom_vline(xintercept = as.Date("08-12-2019", format ="%m-%d-%Y"), color =  "darkgrey", size = 0.5, linetype ="dashed") +
  geom_vline(xintercept = as.Date("08-16-2019", format ="%m-%d-%Y"), color = "darkgrey", size = 0.5, linetype ="dashed")+
  annotate("text", x = as.Date("08-14-2019", format= "%m-%d-%Y"), y = 0.7, label = "HW2", size = 4) 

ggsave(no_leak_somers_mg_g_berry_weight_treatment_date_ok, filename = "figures/no_leak_somers_mg_g_berry_weight_treatment_date_ok.pdf", device = cairo_pdf, 
       width = 9, height = 7)


####mg/g skin#####


no_leak_somers_first_results_ave_stdv_mg_g_skin<- no_leak_somers_first_results %>%
  select(Date_sampled, Block_id, Rep, Total_antho_mg_L_w_dil_factor, treatment, total_Antho_mg_berry, total_antho_mg_l, total_antho_mg_g_berry_weight, total_antho_mg_g_berry_weight, total_Antho_mg_g_Skin, Total_phenolics, Hue, degree_ionization_.) %>%
  filter(!is.na(total_Antho_mg_berry)) %>%
  group_by(Date_sampled, treatment)%>%
  summarise (avg_antho_mg_g_skin = mean(total_Antho_mg_g_Skin), sev = se(total_Antho_mg_g_Skin), stdv = sd(total_Antho_mg_g_Skin))


no_leak_somers_first_results_ave_stdv_mg_g_skin_blocks<- no_leak_somers_first_results %>%
  select(Date_sampled, Block_id, Rep,  Total_antho_mg_L_w_dil_factor, total_Antho_mg_berry, total_antho_mg_l, total_antho_mg_g_berry_weight, total_antho_mg_g_berry_weight, total_Antho_mg_g_Skin, Total_phenolics, Hue, degree_ionization_.) %>%
  group_by(Date_sampled, Block_id)%>%
  summarise (avg_antho_mg_g_skin = mean(total_Antho_mg_g_Skin), sev = se(total_Antho_mg_g_Skin), stdv = sd(total_Antho_mg_g_Skin))

write.csv(no_leak_somers_first_results_ave_stdv_mg_g_skin_blocks,"data_output/no_leak_somers_first_results_ave_stdv_mg_g_skin_blocks.csv")


write.csv(no_leak_somers_first_results_ave_stdv_mg_g_skin, "data_output/no_leak_somers_first_results_ave_stdv_mg_g_skin.csv")


no_leak_somers_first_results_ave_stdv_mg_g_skin$Date_sampled<- mdy(no_leak_somers_first_results_ave_stdv_mg_g_skin$Date_sampled)

str(no_leak_somers_first_results_ave_stdv_mg_g_skin$Date_sampled)

tz(no_leak_somers_first_results_ave_stdv_mg_g_skin$Date_sampled)

no_leak_somers_first_results_ave_stdv_mg_g_skin$treatment <-format(no_leak_somers_first_results_ave_stdv_mg_g_skin$treatment)
as.character(no_leak_somers_first_results_ave_stdv_mg_g_skin$treatment)


no_leak_somers_mg_g_skin_weight_treatment<-ggplot(no_leak_somers_first_results_ave_stdv_mg_g_skin, aes(Date_sampled, avg_antho_mg_g_skin, group = treatment, color = treatment)) + 
  geom_errorbar(alpha =0.9, aes(ymin=avg_antho_mg_g_skin-sev, ymax=avg_antho_mg_g_skin+sev), width= 1, position=pd, stat = "identity", size =1) +
  geom_line(alpha =0.65, size =1.1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(position = pd)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab ("mg anthocyanin/g skin wt.") +
  ggtitle( " Total anthocyanins") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,5.5,0.5), limits = c (0,5.5)) 

ggsave(no_leak_somers_mg_g_skin_weight_treatment, filename = "figures/no_leak_somers_mg_g_skin_weight_treatment.pdf", device = cairo_pdf, 
       width = 9, height = 7)


####Total phenolics ####


no_leak_somers_first_results_ave_stdv_total_phenolic<- no_leak_somers_first_results %>%
  select(Date_sampled, Block_id, Rep,Total_antho_mg_L_w_dil_factor, treatment, total_Antho_mg_berry, total_antho_mg_l, total_antho_mg_g_berry_weight, total_antho_mg_g_berry_weight, total_Antho_mg_g_Skin, Total_phenolics_w_DF, Hue, degree_ionization_.) %>%
  filter(!is.na(total_Antho_mg_berry)) %>%
  group_by(Date_sampled, treatment)%>%
  summarise (avg = mean(Total_phenolics_w_DF), sev = se(Total_phenolics_w_DF), stdv = sd(Total_phenolics_w_DF))


no_leak_somers_first_results_ave_stdv_total_phenolic_blocks<- no_leak_somers_first_results %>%
  select(Date_sampled, Block_id, Rep, Total_antho_mg_L_w_dil_factor, treatment, total_Antho_mg_berry, total_antho_mg_l, total_antho_mg_g_berry_weight, total_antho_mg_g_berry_weight, total_Antho_mg_g_Skin, Total_phenolics_w_DF, Hue, degree_ionization_.) %>%
  filter(!is.na(total_Antho_mg_berry)) %>%
  group_by(Date_sampled, treatment)%>%
  summarise (avg = mean(Total_phenolics_w_DF), sev = se(Total_phenolics_w_DF), stdv = sd(Total_phenolics_w_DF))

write.csv(no_leak_somers_first_results_ave_stdv_total_phenolic_blocks,"data_output/no_leak_somers_first_results_ave_stdv_total_phenolic_blocks.csv")


write.csv(no_leak_somers_first_results_ave_stdv_total_phenolic, "data_output/no_leak_somers_first_results_ave_stdv_total_phenolic.csv")


no_leak_somers_first_results_ave_stdv_total_phenolic$Date_sampled<- mdy(no_leak_somers_first_results_ave_stdv_total_phenolic$Date_sampled)

str(no_leak_somers_first_results_ave_stdv_total_phenolic$Date_sampled)

tz(no_leak_somers_first_results_ave_stdv_total_phenolic$Date_sampled)



no_leak_somers_first_results_ave_stdv_total_phenolic$treatment <-format(no_leak_somers_first_results_ave_stdv_total_phenolic$treatment)
as.character(no_leak_somers_first_results_ave_stdv_total_phenolic$treatment)

pd<-position_dodge(0.5)

no_leak_somers_total_phenolic<-ggplot(no_leak_somers_first_results_ave_stdv_total_phenolic, aes(Date_sampled, avg, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg-sev, ymax=avg+sev), width= 1, position=pd, stat = "identity") +
  geom_line(alpha =0.5, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(position = pd)+
  theme_classic() +
  scale_fill_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab ("AU") +
  ggtitle( " Total phenolics") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=14, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14,face = "bold", family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,80,10), limits = c (0,80)) 


ggsave(no_leak_somers_total_phenolic, filename = "figures/no_leak_somers_total_phenolic.pdf", device = cairo_pdf, 
       width = 9, height = 7)

####Plot all somers together ####

panel_plot_no_leak_somers_time_2019_date_ok <- plot_grid (no_leak_somers_mg_berry_treatment_date_ok, no_leak_somers_mg_g_berry_weight_treatment_date_ok, ncol=2, nrow = 1)

ggsave(panel_plot_no_leak_somers_time_2019_date_ok, filename = "figures/panel_plot_no_leak_somers_time_2019_date_ok.pdf", device = cairo_pdf, width = 15, height = 6)