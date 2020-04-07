library(tidyverse)
library(grDevices)
no_leak_mcp_first_results<- read.csv ("data/mcp_berry_phenolics.csv", header = TRUE)


no_leak_mcp_first_results<- no_leak_mcp_first_results %>%
  filter(!Block_id =="B1R2")

se <- function(x) sqrt(var(x)/length(x))

str(no_leak_mcp_first_results$Date_sampled)


####mg/berry ####

no_leak_mcp_first_results_ave_stdv_mg_berry<- no_leak_mcp_first_results %>%
  select(Date_sampled, Block_id, Rep, Conc_w_dil_factor, Total_tannin_mg_berry, Total_tannin_mg_g_berry_weight, Total_tannin_mg_g_skin, treatment) %>%
  group_by(Date_sampled, treatment)%>%
  summarise (avg_tannin_mg_berry = mean(Total_tannin_mg_berry), sev = se(Total_tannin_mg_berry), stdv = sd(Total_tannin_mg_berry))


no_leak_mcp_first_results_ave_stdv_mg_berry_blocks<- no_leak_mcp_first_results %>%
  select(Date_sampled, Block_id, Rep, Conc_w_dil_factor, Total_tannin_mg_berry, Total_tannin_mg_g_berry_weight, Total_tannin_mg_g_skin, treatment) %>%
  group_by(Date_sampled, Block_id)%>%
  summarise (avg_tannin_mg_berry = mean(Total_tannin_mg_berry), sev = se(Total_tannin_mg_berry), stdv = sd(Total_tannin_mg_berry))

write.csv(no_leak_mcp_first_results_ave_stdv_mg_berry_blocks,"data_output/no_leak_mcp_first_results_ave_stdv_mg_berry_blocks.csv")

write.csv(no_leak_mcp_first_results_ave_stdv_mg_berry, "data_output/no_leak_mcp_first_results_ave_stdv_mg_berry.csv")

no_leak_mcp_first_results_ave_stdv_mg_berry$Date_sampled<- mdy(no_leak_mcp_first_results_ave_stdv_mg_berry$Date_sampled)

str(no_leak_mcp_first_results_ave_stdv_mg_berry$Date_sampled)

tz(no_leak_mcp_first_results_ave_stdv_mg_berry$Date_sampled)

no_leak_mcp_first_results_ave_stdv_mg_berry_blocks$Date_sampled<- mdy(no_leak_mcp_first_results_ave_stdv_mg_berry_blocks$Date_sampled)

str(no_leak_mcp_first_results_ave_stdv_mg_berry_blocks$Date_sampled)

tz(no_leak_mcp_first_results_ave_stdv_mg_berry_blocks$Date_sampled)

no_leak_mcp_first_results_ave_stdv_mg_berry$treatment <-format(no_leak_mcp_first_results_ave_stdv_mg_berry$treatment)
as.character(no_leak_mcp_first_results_ave_stdv_mg_berry$treatment)

pd<- position_dodge(0.5)

no_leak_mcp_mg_berry_OK_DATE<-ggplot(no_leak_mcp_first_results_ave_stdv_mg_berry, aes(Date_sampled, avg_tannin_mg_berry, group = treatment, color = treatment)) + 
  geom_errorbar(alpha= 1,aes(ymin=avg_tannin_mg_berry-sev, ymax=avg_tannin_mg_berry+sev), width= 3, position=pd, stat = "identity", size =1) +
  geom_line(alpha = 1, size =1.1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(position = pd)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab ("mg tannin/berry") +
  ggtitle( " Total tannins") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,3.5,0.5), limits = c (0,3.5)) +
  scale_x_date(date_labels="%b %d", date_breaks = ("7 day")) +
  geom_vline(xintercept = as.Date("08-12-2019", format ="%m-%d-%Y"), color =  "darkgrey", size = 0.5, linetype ="dashed") +
  geom_vline(xintercept = as.Date("08-16-2019", format ="%m-%d-%Y"), color = "darkgrey", size = 0.5, linetype ="dashed")+
  annotate("text", x = as.Date("08-14-2019", format= "%m-%d-%Y"), y = 3.5, label = "HW2", size = 4.1) 



ggsave(no_leak_mcp_mg_berry_OK_DATE, filename = "figures/no_leak_mcp_mg_berry_OK_DATE.pdf", device = cairo_pdf, 
       width = 9, height = 7)

####All blocks don't use graph. See how to fix different harvest dates 

pd<- position_dodge(5)
no_leak_mcp_mg_berry_block<-ggplot(no_leak_mcp_first_results_ave_stdv_mg_berry_blocks, aes(Date_sampled, avg_tannin_mg_berry, group = Block_id, color = Block_id)) + 
  geom_errorbar(aes(ymin=avg_tannin_mg_berry-sev, ymax=avg_tannin_mg_berry+sev), width= 1, position=pd, stat = "identity") +
  geom_bar(alpha =0.5, size =1, position=pd, stat = "identity", linetype = "solid", aes(fill = Block_id)) +
  theme_classic() +
  scale_fill_brewer(palette = "YlOrRd") +
  scale_color_brewer(palette = "YlOrRd") +
  ylab ("mg tannin/berry") +
  ggtitle( " Total tannins") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=14, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14,face = "bold", family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,4,0.5), limits = c (0,4)) 


ggsave(no_leak_mcp_mg_berry_block, filename = "figures/no_leak_mcp_mg_berry_block.pdf", device = cairo_pdf, 
       width = 9, height = 7)

####mg/g berry ####

no_leak_mcp_first_results_ave_stdv_mg_g_berry_weight<- no_leak_mcp_first_results %>%
  select(Date_sampled, Block_id, Rep, Conc_w_dil_factor, Total_tannin_mg_berry, Total_tannin_mg_g_berry_weight, Total_tannin_mg_g_skin, treatment) %>%
  group_by(Date_sampled, treatment)%>%
  summarise (avg_tannin_mg_g_berry_weight = mean(Total_tannin_mg_g_berry_weight), sev = se(Total_tannin_mg_g_berry_weight), stdv = sd(Total_tannin_mg_g_berry_weight))

no_leak_mcp_first_results_ave_stdv_mg_g_berry_weight_blocks<- no_leak_mcp_first_results %>%
  select(Date_sampled, Block_id, Rep, Conc_w_dil_factor, Total_tannin_mg_berry, Total_tannin_mg_g_berry_weight, Total_tannin_mg_g_skin, treatment) %>%
  group_by(Date_sampled, Block_id)%>%
  summarise (avg_tannin_mg_g_berry_weight = mean(Total_tannin_mg_g_berry_weight), sev = se(Total_tannin_mg_g_berry_weight), stdv = sd(Total_tannin_mg_g_berry_weight))

write.csv(no_leak_mcp_first_results_ave_stdv_mg_g_berry_weight_blocks, "data_output/no_leak_mcp_first_results_ave_stdv_mg_g_berry_weight_blocks.csv")

write.csv(no_leak_mcp_first_results_ave_stdv_mg_g_berry_weight, "data_output/no_leak_mcp_first_results_ave_stdv_mg_g_berry_weight.csv")


no_leak_mcp_first_results_ave_stdv_mg_g_berry_weight$Date_sampled<- mdy(no_leak_mcp_first_results_ave_stdv_mg_g_berry_weight$Date_sampled)

str(no_leak_mcp_first_results_ave_stdv_mg_g_berry_weight$Date_sampled)

tz(no_leak_mcp_first_results_ave_stdv_mg_g_berry_weight$Date_sampled)


no_leak_mcp_first_results_ave_stdv_mg_g_berry_weight_blocks$Date_sampled<- mdy(no_leak_mcp_first_results_ave_stdv_mg_g_berry_weight_blocks$Date_sampled)

str(no_leak_mcp_first_results_ave_stdv_mg_g_berry_weight_blocks$Date_sampled)

tz(no_leak_mcp_first_results_ave_stdv_mg_g_berry_weight_blocks$Date_sampled)

no_leak_mcp_first_results_ave_stdv_mg_g_berry_weight$treatment <-format(no_leak_mcp_first_results_ave_stdv_mg_g_berry_weight$treatment)
as.character(no_leak_mcp_first_results_ave_stdv_mg_g_berry_weight$treatment)



no_leak_mcp_mg_g_berry_weight_ok_date<-ggplot(no_leak_mcp_first_results_ave_stdv_mg_g_berry_weight, aes(Date_sampled, avg_tannin_mg_g_berry_weight, group = treatment, color = treatment)) + 
  geom_errorbar(alpha=1,aes(ymin=avg_tannin_mg_g_berry_weight-sev, ymax=avg_tannin_mg_g_berry_weight+sev), width= 3, size=1, position=pd, stat = "identity") +
  geom_line(alpha =1, size =1.1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(position = pd)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab ("mg tannin/g berry wt.") +
  ggtitle( " Total tannins") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,3.7,0.5), limits = c (0,3.7)) +
  scale_x_date(date_labels="%b %d", date_breaks = ("7 day")) +
  geom_vline(xintercept = as.Date("08-12-2019", format ="%m-%d-%Y"), color =  "darkgrey", size = 0.5, linetype ="dashed") +
  geom_vline(xintercept = as.Date("08-16-2019", format ="%m-%d-%Y"), color = "darkgrey", size = 0.5, linetype ="dashed")+
  annotate("text", x = as.Date("08-14-2019", format= "%m-%d-%Y"), y = 3.5, label = "HW2", size = 4.1) 


ggsave(no_leak_mcp_mg_g_berry_weight_ok_date, filename = "figures/no_leak_mcp_mg_g_berry_weight_ok_date.pdf", device = cairo_pdf, 
       width = 9, height = 7)

####mg/g skin #####
no_leak_mcp_first_results_ave_stdv_mg_g_skin<- no_leak_mcp_first_results %>%
  select(Date_sampled, Block_id, Rep, Conc_w_dil_factor, Total_tannin_mg_berry, Total_tannin_mg_g_berry_weight, Total_tannin_mg_g_skin, treatment) %>%
  group_by(Date_sampled, treatment)%>%
  summarise (avg_tannin_mg_g_skin = mean(Total_tannin_mg_g_skin), sev = se(Total_tannin_mg_g_skin), stdv = sd(Total_tannin_mg_g_skin))


no_leak_mcp_first_results_ave_stdv_mg_g_skin_blocks<- no_leak_mcp_first_results %>%
  select(Date_sampled, Block_id, Rep, Conc_w_dil_factor, Total_tannin_mg_g_skin, treatment) %>%
  group_by(Date_sampled, treatment)%>%
  summarise (avg_tannin_mg_g_skin = mean(Total_tannin_mg_g_skin), sev = se(Total_tannin_mg_g_skin), stdv = sd(Total_tannin_mg_g_skin))

write.csv(no_leak_mcp_first_results_ave_stdv_mg_g_skin_blocks,"data_output/no_leak_mcp_first_results_ave_stdv_mg_g_skin_blocks.csv")


write.csv(no_leak_mcp_first_results_ave_stdv_mg_g_skin, "data_output/no_leak_mcp_first_results_ave_stdv_mg_g_skin.csv")



no_leak_mcp_first_results_ave_stdv_mg_g_skin$Date_sampled<- mdy(no_leak_mcp_first_results_ave_stdv_mg_g_skin$Date_sampled)

str(no_leak_mcp_first_results_ave_stdv_mg_g_skin$Date_sampled)

tz(no_leak_mcp_first_results_ave_stdv_mg_g_skin$Date_sampled)


no_leak_mcp_first_results_ave_stdv_mg_g_skin$treatment <-format(no_leak_mcp_first_results_ave_stdv_mg_g_skin$treatment)
as.character(no_leak_mcp_first_results_ave_stdv_mg_g_skin$treatment)


pd<-position_dodge(0.5)
no_leak_mcp_mg_g_skin_weight<-ggplot(no_leak_mcp_first_results_ave_stdv_mg_g_skin, aes(Date_sampled, avg_tannin_mg_g_skin, group = treatment, color = treatment)) + 
  geom_errorbar(alpha=0.9,aes(ymin=avg_tannin_mg_g_skin-sev, ymax=avg_tannin_mg_g_skin+sev), width= 3, size=1, position=pd, stat = "identity") +
  geom_line(alpha =0.65, size =1.1, position=pd, stat = "identity", linetype = "solid", aes(fill = treatment)) +
  geom_point(position = pd)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab ("mg tannin/g skin wt.") +
  ggtitle( " Total tannins") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,35,5), limits = c (0,35))


ggsave(no_leak_mcp_mg_g_skin_weight, filename = "figures/no_leak_mcp_mg_g_skin_weight.pdf", device = cairo_pdf, 
       width = 9, height = 7)


library(cowplot)
#### All tannin and somers together 
panel_plot_no_leak_mcp_time_2019_date_ok <- plot_grid (no_leak_mcp_mg_berry_OK_DATE, no_leak_mcp_mg_g_berry_weight_ok_date, ncol=2, nrow = 1)

ggsave(panel_plot_no_leak_mcp_time_2019_date_ok, filename = "figures/panel_plot_no_leak_mcp_time_2019_date_ok.pdf", device = cairo_pdf, width = 14, height = 6)