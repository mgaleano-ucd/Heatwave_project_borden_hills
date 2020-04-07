library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(viridis)
library(viridisLite)

se <- function(x) sqrt(var(x)/length(x))

somers_berry_harvest_replicates<-read.csv("data/somers_berry_harvest_replicates.csv", header = TRUE)

str(somers_berry_harvest_replicates)

somers_berry_harvest_replicates_anto_per_berry_treatment <- somers_berry_harvest_replicates%>%
  filter(!is.na(Rep))%>%
  select(Date_sampled, treatment, Block_id, total_antho_mg_g_berry_weight, total_Antho_mg_berry, total_Antho_mg_g_Skin) %>%
  group_by(treatment) 
  
  somers_berry_harvest_replicates_anto_per_berry_treatment_table <- somers_berry_harvest_replicates_anto_per_berry_treatment %>%
  summarise(avg =mean(total_Antho_mg_berry), sev = se(total_Antho_mg_berry), stdv = sd(total_Antho_mg_berry))


str(somers_berry_harvest_replicates_anto_per_berry_treatment)


somers_berry_harvest_replicates_anto_per_berry_treatment$treatment<- reorder(somers_berry_harvest_replicates_anto_per_berry_treatment$treatment, somers_berry_harvest_replicates_anto_per_berry_treatment$total_antho_mg_g_berry_weight)



somers_berry_harvest_replicates_anto_per_berry_treatment$treatment <- format(somers_berry_harvest_replicates_anto_per_berry_treatment$treatment )
as.character(somers_berry_harvest_replicates_anto_per_berry_treatment$treatment)

somers_berry_harvest_replicates_anto_per_berry_treatment %>%
  group_by(treatment) %>%
  tally()


pd2 <- position_dodge(0.75)

somers_berry_harvest_replicates_anto_per_berry_treatment <-somers_berry_harvest_replicates_anto_per_berry_treatment %>%
  ggplot(aes(treatment, total_Antho_mg_berry))+
  geom_boxplot(alpha =0.7, aes( fill = treatment))+
  geom_point(alpha =0.7, position = pd2, aes(color = treatment, group =treatment))+
  theme_classic()+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+ 
  ylab("Total anthocyanins mg/berry") +
  xlab("Treatment") +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_y_continuous(breaks=seq(0,1,0.1), limits = c (0, 1))  
 

ggsave(somers_berry_harvest_replicates_anto_per_berry_treatment, filename = "figures/somers_berry_harvest_replicates_anto_per_berry_treatment.pdf", device = cairo_pdf, 
       width = 11, height = 6)



##### with no leaky pixel

somers_berry_harvest_replicates_anto_per_berry_treatment <- somers_berry_harvest_replicates%>%
  filter(!is.na(Rep))%>%
  filter(!Block_id =="B1R2")
  select(Date_sampled, treatment, Block_id, total_Antho_mg_berry, total_antho_mg_g_berry_weight, total_Antho_mg_g_Skin) %>%
  group_by(treatment) 

somers_berry_harvest_replicates_anto_per_berry_treatmen_table <- somers_berry_harvest_replicates_anto_per_berry_treatment %>%
  summarise(avg =mean(total_Antho_mg_berry), sev = se(total_Antho_mg_berry), stdv = sd(total_Antho_mg_berry))


str(somers_berry_harvest_replicates_anto_per_berry_treatment)


somers_berry_harvest_replicates_anto_per_berry_treatment$treatment<- reorder(somers_berry_harvest_replicates_anto_per_berry_treatment$treatment, somers_berry_harvest_replicates_anto_per_berry_treatment$total_antho_mg_g_berry_weight)



somers_berry_harvest_replicates_anto_per_berry_treatment$treatment <- format(somers_berry_harvest_replicates_anto_per_berry_treatment$treatment )
as.character(somers_berry_harvest_replicates_anto_per_berry_treatment$treatment)

somers_berry_harvest_replicates_anto_per_berry_treatment %>%
  group_by(treatment)%>%
  tally()

pd2 <- position_dodge(0.75)

somers_berry_harvest_replicates_anto_per_berry_treatment_no_leak <-somers_berry_harvest_replicates_anto_per_berry_treatment %>%
  ggplot(aes(treatment, total_Antho_mg_berry))+
  geom_boxplot(alpha =0.7, aes( fill = treatment))+
  geom_point(alpha =0.7, position = pd2, aes(color = treatment, group =treatment))+
  theme_classic()+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+ 
  ylab("Total anthocyanins mg/berry") +
  xlab("Treatment") +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_y_continuous(breaks=seq(0,1,0.1), limits = c (0, 1))  



ggsave(somers_berry_harvest_replicates_anto_per_berry_treatment_no_leak, filename = "figures/somers_berry_harvest_replicates_anto_per_berry_treatment_mo_leak.pdf", device = cairo_pdf, 
       width = 11, height = 6)

#### Per treatment mg ber berry

somers_berry_harvest_replicates_anto_per_berry_treatment <- somers_berry_harvest_replicates%>%
  filter(!is.na(Rep))%>%
select(Date_sampled, treatment, Block_id, total_Antho_mg_berry, total_antho_mg_g_berry_weight, total_Antho_mg_g_Skin) %>%
  group_by(Block_id) 

somers_berry_harvest_replicates_anto_per_berry_treatmen_table <- somers_berry_harvest_replicates_anto_per_berry_treatment %>%
  summarise(avg =mean(total_Antho_mg_berry), sev = se(total_Antho_mg_berry), stdv = sd(total_Antho_mg_berry))


str(somers_berry_harvest_replicates_anto_per_berry_treatment)

somers_berry_harvest_replicates_anto_per_berry_treatment %>%
  group_by(treatment, Block_id)%>%
  tally()


somers_berry_harvest_replicates_anto_per_berry_block_id <-somers_berry_harvest_replicates_anto_per_berry_treatment %>%
  ggplot(aes(Block_id, total_Antho_mg_berry))+
  geom_boxplot(alpha =0.7, aes( fill = Block_id))+
  geom_point(alpha =0.7, position = pd2, aes(color = Block_id, group = Block_id))+
  theme_classic()+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Pixel")+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Pixel")+ 
  ylab("Total anthocyanins mg/berry") +
  xlab("Pixel") +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_y_continuous(breaks=seq(0,1,0.1), limits = c (0, 1))  



ggsave(somers_berry_harvest_replicates_anto_per_berry_block_id, filename = "figures/somers_berry_harvest_replicates_anto_per_berry_block_id.pdf", device = cairo_pdf, 
       width = 11, height = 6)



#### somers mg per g berry ####

somers_berry_harvest_replicates_anto_mg_g_berry_treatment <- somers_berry_harvest_replicates%>%
  filter(!is.na(Rep))%>%
  select(Date_sampled, treatment, Block_id, total_Antho_mg_berry, total_antho_mg_g_berry_weight, total_Antho_mg_g_Skin) %>%
  group_by(treatment) 

somers_berry_harvest_replicates_anto_mg_g_berry_treatment_table <- somers_berry_harvest_replicates_anto_mg_g_berry_treatment %>%
  summarise(avg =mean(total_antho_mg_g_berry_weight), sev = se(total_antho_mg_g_berry_weight), stdv = sd(total_antho_mg_g_berry_weight))


str(somers_berry_harvest_replicates_anto_mg_g_berry_treatment)


somers_berry_harvest_replicates_anto_mg_g_berry_treatment$treatment<- reorder(somers_berry_harvest_replicates_anto_mg_g_berry_treatment$treatment, somers_berry_harvest_replicates_anto_mg_g_berry_treatment$total_antho_mg_g_berry_weight)



somers_berry_harvest_replicates_anto_mg_g_berry_treatment$treatment <- format(somers_berry_harvest_replicates_anto_mg_g_berry_treatment$treatment )
as.character(somers_berry_harvest_replicates_anto_mg_g_berry_treatment$treatment)


somers_berry_harvest_replicates_anto_mg_g_berry_treatment %>%
  group_by(treatment)%>%
  tally()

pd2 <- position_dodge(0.75)

somers_berry_harvest_replicates_anto_mg_g_berry_treatment <-somers_berry_harvest_replicates_anto_mg_g_berry_treatment %>%
  ggplot(aes(treatment, total_antho_mg_g_berry_weight))+
  geom_boxplot(alpha =0.7, aes( fill = treatment))+
  geom_point(alpha =0.7, position = pd2, aes(color = treatment, group =treatment))+
  theme_classic()+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+ 
  ylab("Total anthocyanins mg/ g of berry wt") +
  xlab("Treatment") +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_y_continuous(breaks=seq(0,1,0.1), limits = c (0, 1))  



ggsave(somers_berry_harvest_replicates_anto_mg_g_berry_treatment, filename = "figures/somers_berry_harvest_replicates_anto_mg_g_berry_treatment.pdf", device = cairo_pdf, 
       width = 11, height = 6)



##### with no leaky pixel

somers_berry_harvest_replicates_anto_mg_g_berry_treatment <- somers_berry_harvest_replicates%>%
  filter(!is.na(Rep))%>%
  filter(!Block_id =="B1R2") %>%
select(Date_sampled, treatment, Block_id, total_antho_mg_g_berry_weight, total_Antho_mg_g_Skin) %>%
  group_by(treatment) 

somers_berry_harvest_replicates_anto_mg_g_berry_treatment_table <- somers_berry_harvest_replicates_anto_mg_g_berry_treatment %>%
  summarise(avg =mean(total_antho_mg_g_berry_weight), sev = se(total_antho_mg_g_berry_weight), stdv = sd(total_antho_mg_g_berry_weight))


str(somers_berry_harvest_replicates_anto_mg_g_berry_treatment)


somers_berry_harvest_replicates_anto_mg_g_berry_treatment$treatment<- reorder(somers_berry_harvest_replicates_anto_mg_g_berry_treatment$treatment, somers_berry_harvest_replicates_anto_mg_g_berry_treatment$total_antho_mg_g_berry_weight)



somers_berry_harvest_replicates_anto_mg_g_berry_treatment$treatment <- format(somers_berry_harvest_replicates_anto_mg_g_berry_treatment$treatment )
as.character(somers_berry_harvest_replicates_anto_mg_g_berry_treatment$treatment)


somers_berry_harvest_replicates_anto_mg_g_berry_treatment %>%
  group_by(treatment)%>%
  tally()

pd2 <- position_dodge(0.75)

somers_berry_harvest_replicates_anto_mg_g_berry_treatment_no_leak <-somers_berry_harvest_replicates_anto_mg_g_berry_treatment %>%
  ggplot(aes(treatment, total_antho_mg_g_berry_weight))+
  geom_boxplot(alpha =0.7, aes( fill = treatment))+
  geom_point(alpha =0.7, position = pd2, aes(color = treatment, group =treatment))+
  theme_classic()+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+ 
  ylab("Total anthocyanins mg/ g of berry wt") +
  xlab("Treatment") +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_y_continuous(breaks=seq(0,1,0.1), limits = c (0, 1))  



ggsave(somers_berry_harvest_replicates_anto_mg_g_berry_treatment_no_leak, filename = "figures/somers_berry_harvest_replicates_anto_mg_g_berry_treatment_mo_leak.pdf", device = cairo_pdf, 
       width = 11, height = 6)

#### Per treatment mg ber berry

somers_berry_harvest_replicates_anto_mg_g_berry_treatment <- somers_berry_harvest_replicates%>%
  filter(!is.na(Rep))%>%
  select(Date_sampled, treatment, Block_id, total_antho_mg_g_berry_weight, total_antho_mg_g_berry_weight, total_Antho_mg_g_Skin) %>%
  group_by(Block_id) 

somers_berry_harvest_replicates_anto_mg_g_berry_treatment_table <- somers_berry_harvest_replicates_anto_mg_g_berry_treatment %>%
  summarise(avg =mean(total_antho_mg_g_berry_weight), sev = se(total_antho_mg_g_berry_weight), stdv = sd(total_antho_mg_g_berry_weight))


str(somers_berry_harvest_replicates_anto_mg_g_berry_treatment)








somers_berry_harvest_replicates_anto_mg_g_berry_block_id <-somers_berry_harvest_replicates_anto_mg_g_berry_treatment %>%
  ggplot(aes(Block_id, total_antho_mg_g_berry_weight))+
  geom_boxplot(alpha =0.7, aes( fill = Block_id))+
  geom_point(alpha =0.7, position = pd2, aes(color = Block_id, group = Block_id))+
  theme_classic()+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Pixel")+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Pixel")+ 
  ylab("Total anthocyanins mg/ g of berry wt") +
  xlab("Pixel") +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_y_continuous(breaks=seq(0,1,0.1), limits = c (0, 1))  



ggsave(somers_berry_harvest_replicates_anto_mg_g_berry_block_id, filename = "figures/somers_berry_harvest_replicates_anto_mg_g_berry_block_id.pdf", device = cairo_pdf, 
       width = 11, height = 6)


###MCP MG/BERRY ####

mcp_harvest_point_replicates <-read.csv("data/mcp_berry_harvest_replicates.csv", header = TRUE)


mcp_harvest_point_replicates_block_id <- mcp_harvest_point_replicates %>%
  select(Date_sampled, treatment, conc_mg_g_berry_weight, con_mg_berry, Block_id)%>%
  group_by(Block_id)



str(mcp_harvest_point_replicates_block_id)


mcp_harvest_point_replicates_block_id$Block_id<- format(mcp_harvest_point_replicates_block_id$Block_id)
as.character(mcp_harvest_point_replicates$Block_id)


mcp_harvest_point_replicates_block_id%>%
  group_by(treatment, Block_id)%>%
  tally()

mcp_harvest_point_replicates_block_id_mg_berry <- mcp_harvest_point_replicates_block_id%>%
  ggplot(aes(Block_id, con_mg_berry))+
  geom_boxplot(alpha =0.7, aes( fill = Block_id))+
  geom_point(alpha =0.7, position = pd2, aes(color =Block_id, group =Block_id))+
  theme_classic()+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Pixel")+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Pixel")+ 
  ylab("Total tannin mg/berry") +
  xlab("Pixel") +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_y_continuous(breaks=seq(0,4,0.5), limits = c (0, 4))  
  

ggsave(mcp_harvest_point_replicates_block_id_mg_berry , filename = "figures/mcp_harvest_point_replicates_block_id_mg_berry .pdf", device = cairo_pdf, 
       width = 11, height = 6)

pd2 <- position_dodge(0.75)


#### mg/ g berry

mcp_harvest_point_replicates_block_id <- mcp_harvest_point_replicates %>%
  select(Date_sampled, treatment, conc_mg_g_berry_weight, con_mg_berry, Block_id)%>%
  group_by(Block_id)



str(mcp_harvest_point_replicates_block_id)


mcp_harvest_point_replicates_block_id$Block_id<- format(mcp_harvest_point_replicates_block_id$Block_id)
as.character(mcp_harvest_point_replicates$Block_id)

mcp_harvest_point_replicates_block_id%>%
  group_by(treatment, Block_id)%>%
  tally()


mcp_harvest_point_replicates_block_id_mg_g_berry_weight <- mcp_harvest_point_replicates_block_id%>%
  ggplot(aes(Block_id, conc_mg_g_berry_weight))+
  geom_boxplot(alpha =0.7, aes( fill = Block_id))+
  geom_point(alpha =0.7, position = pd2, aes(color =Block_id, group =Block_id))+
  theme_classic()+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Pixel")+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Pixel")+ 
  ylab("Total tannin mg/g berry wt.") +
  xlab("Pixel") +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_y_continuous(breaks=seq(0,4,0.5), limits = c (0, 4))  


ggsave(mcp_harvest_point_replicates_block_id_mg_g_berry_weight , filename = "figures/mcp_harvest_point_replicates_block_id_mg_g_berry_weight .pdf", device = cairo_pdf, 
       width = 11, height = 6)





