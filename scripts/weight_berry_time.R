library(tidyverse)
library(grDevices)
library(lubridate)
library(ggplot2)



berry_weight_vs_time<- read.csv ("data/mcp_berry_phenolics.csv", header = TRUE)

se <- function(x) sqrt(var(x)/length(x))

berry_weight_vs_time_table<- berry_weight_vs_time %>%
  select(Date_sampled, Block_id, Rep,Berry_weight, Skin_weight_aft, treatment) %>%
  filter(!Block_id == "B1R2")%>%
  mutate(berry_weight_one_berry = (Berry_weight/60))%>%
  group_by(Date_sampled, treatment)%>%
  summarise (avg = mean(berry_weight_one_berry), sev = se(berry_weight_one_berry), stdv = sd(berry_weight_one_berry))


write.csv(berry_weight_vs_time_table,"data_output/berry_weight_vs_time_table.csv")

berry_weight_vs_time_table$Date_sampled<- mdy(berry_weight_vs_time_table$Date_sampled)

str(berry_weight_vs_time_table$Date_sampled)

tz(berry_weight_vs_time_table$Date_sampled)

berry_weight_vs_time_table$treatment <-format(berry_weight_vs_time_table$treatment)
as.character(berry_weight_vs_time_table$treatment)


berry_weight_vs_time_table$treatment<- reorder(berry_weight_vs_time_table$treatment,berry_weight_vs_time_table$Date_sampled)

pd<- position_dodge(0.5)

berry_weight_vs_date<-ggplot(berry_weight_vs_time_table, aes(Date_sampled, avg, group = treatment, color = treatment)) + 
  geom_errorbar(alpha=0.9,aes(ymin=avg-sev, ymax=avg+sev), width= 3, position=pd, stat = "identity", size =1) +
  geom_line(alpha =0.65, size =1.1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(position = pd)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab ("g berry wt.") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Day") +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,1.2,0.2), limits = c (0,1.2)) +
  scale_x_date(date_labels="%b %d", date_breaks = ("7 day")) +
  geom_vline(xintercept = as.Date("08-12-2019", format ="%m-%d-%Y"), color =  "darkgrey", size = 0.5, linetype ="dashed") +
  geom_vline(xintercept = as.Date("08-16-2019", format ="%m-%d-%Y"), color = "darkgrey", size = 0.5, linetype ="dashed")+
  annotate("text", x = as.Date("08-14-2019", format= "%m-%d-%Y"), y = 1.2, label = "HW2", size = 4.1) 


ggsave(berry_weight_vs_date, filename = "figures/berry_weight_vs_date.pdf", device = cairo_pdf, 
       width = 9, height = 7)

####ANOVA####


berry_weight_vs_time

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

str(anova_mono_antho)
summary(anova_mono_antho$call)

library(xtable)
trial<-xtable(anova_mono_antho)

lapply(summary(anova_mono_antho), xtable)
library(agricolae)

tukey<-TukeyHSD(aov(percentage_hydro~treatment, monomeric_antho_perc_tri_di_hydroxylated_first_point_anova))

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

str(test)
as.data.frame(test$groups)


monomeric_antho_perc_tri_di_hydroxylated_first_point_anova_hist<-monomeric_antho_perc_tri_di_hydroxylated_first_point_anova %>%
  filter(treatment == 1)
hist(monomeric_antho_perc_tri_di_hydroxylated_first_point_anova_hist$percentage_hydro)

monomeric_antho_perc_tri_di_hydroxylated_first_point_anova_hist<-monomeric_antho_perc_tri_di_hydroxylated_first_point_anova %>%
  filter(treatment == 2)
hist(monomeric_antho_perc_tri_di_hydroxylated_first_point_anova_hist$percentage_hydro)

monomeric_antho_perc_tri_di_hydroxylated_first_point_anova_hist<-monomeric_antho_perc_tri_di_hydroxylated_first_point_anova %>%
  filter(treatment == 3)
hist(monomeric_antho_perc_tri_di_hydroxylated_first_point_anova_hist$percentage_hydro)



