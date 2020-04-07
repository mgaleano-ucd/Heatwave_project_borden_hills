# New column of datetime for diurnals 2019 
library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)

diurnals_borden_hills_2019 <-read.csv("data_output/diurnals_2019_old_and_new_blocks_cleaned_no_NAs.csv", header = TRUE)

str(diurnals_borden_hills_2019)

diurnals_2019_lwp_vs_time <- diurnals_borden_hills_2019 %>%
  mutate(time = hhmmss) %>%
  select(-hhmmss) %>%
  filter(!is.na(time))


diurnals_2019_lwp_vs_time$time<- format(strptime(diurnals_2019_lwp_vs_time$time,"%H:%M:%S"), format = "%H:%M", tz = "America_Los_Angeles")

diurnals_2019_lwp_vs_time$datetime <- paste(diurnals_2019_lwp_vs_time$date, " ", diurnals_2019_lwp_vs_time$time, sep = "")

glimpse(diurnals_2019_lwp_vs_time) 

diurnals_2019_lwp_vs_time$datetime <- mdy_hm(diurnals_2019_lwp_vs_time$datetime, tz = "UTC")

diurnals_2019_lwp_vs_time$round<-format(diurnals_2019_lwp_vs_time$round)
diurnals_2019_lwp_vs_time$round<-as.numeric(as.factor(diurnals_2019_lwp_vs_time$round))

str(diurnals_2019_lwp_vs_time$round)

str(diurnals_2019_lwp_vs_time)

tz(diurnals_2019_lwp_vs_time$datetime)

str(diurnals_2019_lwp_vs_time$datetime)


#Plotting lwp vs round and lwp vs time diurnals Jul 25 (right before first heatwave)

diurnals_2019_lwp_vs_time_jul_25<- diurnals_2019_lwp_vs_time %>%
  filter(!is.na(Leaf_wp_bar)) %>%
  select(datetime, day, Leaf_wp_bar,pixel_number, round, treatment, Rep) %>%
  filter(day == "206")

se <- function(x) sqrt(var(x)/length(x))

diurnals_2019_lwp_vs_time_jul_25_avg_se <-diurnals_2019_lwp_vs_time_jul_25 %>%
  group_by(treatment, round) %>%
  summarise(avg_leaf_wp = mean(Leaf_wp_bar), sev = se(Leaf_wp_bar))

diurnals_2019_lwp_vs_time_jul_25_avg_se$treatment<- reorder(diurnals_2019_lwp_vs_time_jul_25_avg_se$treatment, diurnals_2019_lwp_vs_time_jul_25_avg_se$round)

diurnals_2019_lwp_vs_time_jul_25$treatment<- reorder(diurnals_2019_lwp_vs_time_jul_25$treatment, diurnals_2019_lwp_vs_time_jul_25$round)



diurnals_2019_lwp_vs_time_jul_25$Rep<-format(diurnals_2019_lwp_vs_time_jul_25$Rep)
as.character(diurnals_2019_lwp_vs_time_jul_25$Rep)


#Plot JUL 25
library(wesanderson)
library(ggsci)
library(extrafont)

pd <- position_dodge(0.1)


lwpjul25_round_avg_se<-ggplot(diurnals_2019_lwp_vs_time_jul_25_avg_se, aes(round, avg_leaf_wp, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg_leaf_wp-sev, ymax=avg_leaf_wp+sev), width=.2, position=pd, stat = "identity") +
  geom_line(alpha =0.5, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha =0.3, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab(expression(paste(psi, "leaf"))) +
  ggtitle(expression(paste(psi, "leaf vs time diurnal Jul 25 2019"))) +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Round") +
  theme(axis.title.y = element_text(size=16, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(limit = c(-22,0)) 


ggsave(lwpjul25_round_avg_se, filename = "figures/lwpjul25_round_avg_se.pdf", device = cairo_pdf, 
       width = 8, height = 6)


####color option 2####
lwpjul25<-ggplot(diurnals_2019_lwp_vs_time_jul_25_avg_se, aes(round, avg_leaf_wp, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg_leaf_wp-sev, ymax=avg_leaf_wp+sev), width=.1, position=pd, stat = "identity") +
  geom_line(alpha =0.7, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha =0.3, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_color_manual(values = c("firebrick3", "darkorange", "khaki2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  geom_point(data = diurnals_2019_lwp_vs_time_jul_25, mapping = 
               aes(x = round, y = Leaf_wp_bar, group =treatment, color = treatment, shape = Rep), position =pd) +
  scale_shape_manual(values = c (0, 2, 8), name = "Replication", labels = c("Rep 1", "Rep 2", "Rep 3")) +
  ylab(expression(paste(psi, "leaf"))) +
  ggtitle(expression(paste(psi, "leaf vs time diurnal Jul 25 2019"))) +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Round") +
  theme(axis.title.y = element_text(size=16, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(limit = c(-22,0)) 


ggsave(lwpjul25, filename = "figures/lwpjul25_op_2.pdf", device = cairo_pdf, 
       width = 8, height = 6)

####color option 3####

lwpjul25<-ggplot(diurnals_2019_lwp_vs_time_jul_25_avg_se, aes(round, avg_leaf_wp, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg_leaf_wp-sev, ymax=avg_leaf_wp+sev), width=.1, position=pd, stat = "identity") +
  geom_line(alpha =0.5, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha =0.3, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_color_manual(values = wes_palette("GrandBudapest1", n = 3), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  geom_point(data = diurnals_2019_lwp_vs_time_jul_25, mapping = 
               aes(x = round, y = Leaf_wp_bar, group =treatment, color = treatment, shape = Rep), position =pd) +
  scale_shape_manual(values = c (0, 2, 8), name = "Replication", labels = c("Rep 1", "Rep 2", "Rep 3")) +
  ylab(expression(paste(psi, "leaf"))) +
  ggtitle(expression(paste(psi, "leaf vs time diurnal Jul 25 2019"))) +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Round") +
  theme(axis.title.y = element_text(size=16, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(limit = c(-22,0)) 


ggsave(lwpjul25, filename = "figures/lwpjul25_op_3.pdf", device = cairo_pdf, 
       width = 8, height = 6)


#Plotting lwp vs round and lwp vs time diurnals Jul 28 (during first heatwave)

diurnals_2019_lwp_vs_time_jul_28<- diurnals_2019_lwp_vs_time %>%
  filter(!is.na(Leaf_wp_bar)) %>%
  select(datetime, day, Leaf_wp_bar,pixel_number, round, treatment, Rep) %>%
  filter(day == "209")
diurnals_2019_lwp_vs_time_jul_28_avg_se <-diurnals_2019_lwp_vs_time_jul_28 %>%
  group_by(treatment, round) %>%
  summarise(avg_leaf_wp = mean(Leaf_wp_bar), sev = se(Leaf_wp_bar))

diurnals_2019_lwp_vs_time_jul_28_avg_se$treatment<- reorder(diurnals_2019_lwp_vs_time_jul_28_avg_se$treatment, diurnals_2019_lwp_vs_time_jul_28_avg_se$round)

diurnals_2019_lwp_vs_time_jul_28$treatment<- reorder(diurnals_2019_lwp_vs_time_jul_28$treatment, diurnals_2019_lwp_vs_time_jul_28$round)

diurnals_2019_lwp_vs_time_jul_28$Rep<-format(diurnals_2019_lwp_vs_time_jul_28$Rep)
as.character(diurnals_2019_lwp_vs_time_jul_28$Rep)

# Plot jul 28


lwpjul28_round_avg_se<-ggplot(diurnals_2019_lwp_vs_time_jul_28_avg_se, aes(round, avg_leaf_wp, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg_leaf_wp-sev, ymax=avg_leaf_wp+sev), width=.2, position=pd, stat = "identity") +
  geom_line(alpha =0.5, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha =0.3, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab(expression(paste(psi, "leaf"))) +
  ggtitle(expression(paste(psi, "leaf vs time diurnal Jul 28 2019"))) +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Round") +
  theme(axis.title.y = element_text(size=16, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(limit = c(-22,0)) 


ggsave(lwpjul28_round_avg_se, filename = "figures/lwpjul28_round_avg_se.pdf", device = cairo_pdf, 
       width = 8, height = 6)


#### Plotting lwp vs round and lwp vs time diurnals Aug 1 (after first heatwave) ####

diurnals_2019_lwp_vs_time_aug_1<- diurnals_2019_lwp_vs_time %>%
  filter(!is.na(Leaf_wp_bar)) %>%
  select(datetime, day, Leaf_wp_bar,pixel_number, round, treatment, Rep) %>%
  filter(day == "213")
diurnals_2019_lwp_vs_time_aug_1_avg_se <-diurnals_2019_lwp_vs_time_aug_1 %>%
  group_by(treatment, round) %>%
  summarise(avg_leaf_wp = mean(Leaf_wp_bar), sev = se(Leaf_wp_bar))

diurnals_2019_lwp_vs_time_aug_1_avg_se$treatment<- reorder(diurnals_2019_lwp_vs_time_aug_1_avg_se$treatment, diurnals_2019_lwp_vs_time_aug_1_avg_se$round)

diurnals_2019_lwp_vs_time_aug_1$treatment<- reorder(diurnals_2019_lwp_vs_time_aug_1_avg_se$treatment, diurnals_2019_lwp_vs_time_aug_1_avg_se$round)

diurnals_2019_lwp_vs_time_aug_1$Rep<-format(diurnals_2019_lwp_vs_time_aug_1$Rep)
as.character(diurnals_2019_lwp_vs_time_aug_1$Rep)

# Plot aug 1


lwpaug1_round_avg_se<-ggplot(diurnals_2019_lwp_vs_time_aug_1_avg_se, aes(round, avg_leaf_wp, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg_leaf_wp-sev, ymax=avg_leaf_wp+sev), width=.2, position=pd, stat = "identity") +
  geom_line(alpha = 0.5, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha = 0.3, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab(expression(paste(psi, "leaf"))) +
  ggtitle(expression(paste(psi, "leaf vs time diurnal Aug 1 2019"))) +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Round") +
  theme(axis.title.y = element_text(size=16, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(limit = c(-22,0)) 



ggsave(lwpaug1_round_avg_se, filename = "figures/lwpaug1_round_avg_se.pdf", device = cairo_pdf, 
       width = 8, height = 6)


#### Plotting lwp vs round and lwp vs time diurnals Aug 15 (before second heatwave) ####

diurnals_2019_lwp_vs_time_aug_15<- diurnals_2019_lwp_vs_time %>%
  filter(!is.na(Leaf_wp_bar)) %>%
  select(datetime, day, Leaf_wp_bar,pixel_number, round, treatment, Rep) %>%
  filter(day == "227")
diurnals_2019_lwp_vs_time_aug_15_avg_se <-diurnals_2019_lwp_vs_time_aug_15 %>%
  group_by(treatment, round) %>%
  summarise(avg_leaf_wp = mean(Leaf_wp_bar), sev = se(Leaf_wp_bar))

diurnals_2019_lwp_vs_time_aug_15_avg_se$treatment<- reorder(diurnals_2019_lwp_vs_time_aug_15_avg_se$treatment, diurnals_2019_lwp_vs_time_aug_15_avg_se$round)

diurnals_2019_lwp_vs_time_aug_15$treatment<- reorder(diurnals_2019_lwp_vs_time_aug_15_avg_se$treatment, diurnals_2019_lwp_vs_time_aug_15_avg_se$round)

diurnals_2019_lwp_vs_time_aug_15$Rep<-format(diurnals_2019_lwp_vs_time_aug_15$Rep)
as.character(diurnals_2019_lwp_vs_time_aug_15$Rep)

# Plot aug 15


lwpaug15_round_avg_se<-ggplot(diurnals_2019_lwp_vs_time_aug_15_avg_se, aes(round, avg_leaf_wp, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg_leaf_wp-sev, ymax=avg_leaf_wp+sev), width=.2, position=pd, stat = "identity") +
  geom_line(alpha =0.5, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha = 0.3, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(paste(psi, "leaf"))) +
  ggtitle(expression(paste(psi, "leaf vs time diurnal Aug 15 2019"))) +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Round") +
  theme(axis.title.y = element_text(size=16, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(limit = c(-22,0)) 


ggsave(lwpaug15_round_avg_se, filename = "figures/lwpaug15_round_avg_se.pdf", device = cairo_pdf, 
       width = 8, height = 6)

######## Plotting lwp vs round and lwp vs time diurnals Aug 20 (during second heatwave) ####


diurnals_2019_lwp_vs_time_aug_20<- diurnals_2019_lwp_vs_time %>%
  filter(!is.na(Leaf_wp_bar)) %>%
  select(datetime, day, Leaf_wp_bar,pixel_number, round, treatment, Rep) %>%
  filter(day == "232")
diurnals_2019_lwp_vs_time_aug_20_avg_se <-diurnals_2019_lwp_vs_time_aug_20 %>%
  group_by(treatment, round) %>%
  summarise(avg_leaf_wp = mean(Leaf_wp_bar), sev = se(Leaf_wp_bar))

diurnals_2019_lwp_vs_time_aug_20_avg_se$treatment<- reorder(diurnals_2019_lwp_vs_time_aug_20_avg_se$treatment, diurnals_2019_lwp_vs_time_aug_20_avg_se$round)

diurnals_2019_lwp_vs_time_aug_20$treatment<- reorder(diurnals_2019_lwp_vs_time_aug_20_avg_se$treatment, diurnals_2019_lwp_vs_time_aug_20_avg_se$round)

diurnals_2019_lwp_vs_time_aug_20$Rep<-format(diurnals_2019_lwp_vs_time_aug_20$Rep)
as.character(diurnals_2019_lwp_vs_time_aug_20$Rep)

# Plot aug 20


lwpaug20_round_avg_se<-ggplot(diurnals_2019_lwp_vs_time_aug_20_avg_se, aes(round, avg_leaf_wp, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg_leaf_wp-sev, ymax=avg_leaf_wp+sev), width=.2, position=pd, stat = "identity") +
  geom_line(alpha = 0.5, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha = 0.3, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(paste(psi, "leaf"))) +
  ggtitle(expression(paste(psi, "leaf vs time diurnal Aug 20 2019"))) +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Round") +
  theme(axis.title.y = element_text(size=16, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(limit = c(-22,0)) 


ggsave(lwpaug20_round_avg_se, filename = "figures/lwpaug20_round_avg_se.pdf", device = cairo_pdf, 
       width = 8, height = 6)

######## Plotting lwp vs round and lwp vs time diurnals sep 5 (after second heatwave) ####


diurnals_2019_lwp_vs_time_sep_5<- diurnals_2019_lwp_vs_time %>%
  filter(!is.na(Leaf_wp_bar)) %>%
  select(datetime, day, Leaf_wp_bar,pixel_number, round, treatment, Rep) %>%
  filter(day == "248")
diurnals_2019_lwp_vs_time_sep_5_avg_se <-diurnals_2019_lwp_vs_time_sep_5 %>%
  group_by(treatment, round) %>%
  summarise(avg_leaf_wp = mean(Leaf_wp_bar), sev = se(Leaf_wp_bar))

diurnals_2019_lwp_vs_time_sep_5_avg_se$treatment<- reorder(diurnals_2019_lwp_vs_time_sep_5_avg_se$treatment, diurnals_2019_lwp_vs_time_sep_5_avg_se$round)

diurnals_2019_lwp_vs_time_sep_5$treatment<- reorder(diurnals_2019_lwp_vs_time_sep_5_avg_se$treatment, diurnals_2019_lwp_vs_time_sep_5_avg_se$round)

diurnals_2019_lwp_vs_time_sep_5$Rep<-format(diurnals_2019_lwp_vs_time_sep_5$Rep)
as.character(diurnals_2019_lwp_vs_time_sep_5$Rep)


# Plot sep_5


lwpsep5_round_avg_se<-ggplot(diurnals_2019_lwp_vs_time_sep_5_avg_se, aes(round, avg_leaf_wp, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg_leaf_wp-sev, ymax=avg_leaf_wp+sev), width=.2, position=pd, stat = "identity") +
  geom_line(alpha =0.5, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha =0.3, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(paste(psi, "leaf"))) +
  ggtitle(expression(paste(psi, "leaf vs time diurnal sep 5 2019"))) +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Round") +
  theme(axis.title.y = element_text(size=16, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(limit = c(-22,0)) 


ggsave(lwpsep5_round_avg_se, filename = "figures/lwpsep5_round_avg_se.pdf", device = cairo_pdf, 
       width = 8, height = 6)

####Plotting lwp vs round for first diurnals before block ID change - Baseline ####


diurnals_2019_lwp_vs_time_jul_12<- diurnals_2019_lwp_vs_time %>%
  filter(!is.na(Leaf_wp_bar)) %>%
  select(datetime, day, Leaf_wp_bar,pixel_number, round, treatment, Rep) %>%
  filter(day == "193")

diurnals_2019_lwp_vs_time_jul_12_avg_se <-diurnals_2019_lwp_vs_time_jul_12 %>%
  group_by(treatment, round) %>%
  summarise(avg_leaf_wp = mean(Leaf_wp_bar), sev = se(Leaf_wp_bar))

(diurnals_2019_lwp_vs_time_jul_12$pixel_number)

diurnals_2019_lwp_vs_time_jul_12_avg_se$treatment<- reorder(diurnals_2019_lwp_vs_time_jul_12_avg_se$treatment, diurnals_2019_lwp_vs_time_jul_12_avg_se$round)

diurnals_2019_lwp_vs_time_jul_12$treatment <-reorder(diurnals_2019_lwp_vs_time_jul_12$treatment, diurnals_2019_lwp_vs_time_jul_12$round)

diurnals_2019_lwp_vs_time_jul_12$Rep<-format(diurnals_2019_lwp_vs_time_jul_12$Rep)
as.character(diurnals_2019_lwp_vs_time_jul_12$Rep)

# Plot jul_12


lwpjul12_round_avg_se<-ggplot(diurnals_2019_lwp_vs_time_jul_12_avg_se, aes(round, avg_leaf_wp, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg_leaf_wp-sev, ymax=avg_leaf_wp+sev), width=.2, position=pd, stat = "identity") +
  geom_line(alpha =0.5, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha =0.3, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(paste(psi, "leaf"))) +
  ggtitle(expression(paste(psi, "leaf vs time diurnal jul 12 2019"))) +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Round") +
  theme(axis.title.y = element_text(size=16, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(limit = c(-22, 0))

ggsave(lwpjul12_round_avg_se, filename = "figures/lwpjul12_round_avg_se.pdf", device = cairo_pdf, 
       width = 8, height = 6) #warning it saved differently as how it is shown in R

# Plot Jul1

diurnals_2019_lwp_vs_time_jul_1<- diurnals_2019_lwp_vs_time %>%
  filter(!is.na(Leaf_wp_bar)) %>%
  select(datetime, day, Leaf_wp_bar,pixel_number, round, treatment, Rep) %>%
  filter(day == "182")


diurnals_2019_lwp_vs_time_jul_1_avg_se <-diurnals_2019_lwp_vs_time_jul_1 %>%
  group_by(treatment, round) %>%
  summarise(avg_leaf_wp = mean(Leaf_wp_bar), sev = se(Leaf_wp_bar))

(diurnals_2019_lwp_vs_time_jul_1$pixel_number)

diurnals_2019_lwp_vs_time_jul_1_avg_se$treatment<- reorder(diurnals_2019_lwp_vs_time_jul_1_avg_se$treatment, diurnals_2019_lwp_vs_time_jul_1_avg_se$round)

diurnals_2019_lwp_vs_time_jul_1$treatment <-reorder(diurnals_2019_lwp_vs_time_jul_1$treatment, diurnals_2019_lwp_vs_time_jul_1$round)

diurnals_2019_lwp_vs_time_jul_1$Rep<-format(diurnals_2019_lwp_vs_time_jul_1$Rep)
as.character(diurnals_2019_lwp_vs_time_jul_1$Rep)

# Plot jul_1


lwpjul1_round_avg_se<-ggplot(diurnals_2019_lwp_vs_time_jul_1_avg_se, aes(round, avg_leaf_wp, group = treatment, color = treatment)) + 
  geom_errorbar(aes(ymin=avg_leaf_wp-sev, ymax=avg_leaf_wp+sev), width=.2, position=pd, stat = "identity") +
  geom_line(alpha =0.5, size =1, position=pd, stat = "identity", linetype = "solid") +
  geom_point(alpha =0.3, position=pd, size=2, stat = "identity") + 
  theme_classic()+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(paste(psi, "leaf"))) +
  ggtitle(expression(paste(psi, "leaf vs time diurnal jul 1 2019"))) +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Round") +
  theme(axis.title.y = element_text(size=16, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(limit = c(-22,0))

ggsave(lwpjul1_round_avg_se, filename = "figures/lwpjul1_round_avg_se.pdf", device = cairo_pdf,  width = 8, height = 6)

#### Plotting swp vs day with boxplots diurnals 2019 ####


diurnals_2019_swp_vs_time<- diurnals_2019_lwp_vs_time %>%
  filter(!is.na(Stem_wp_bar)) %>%
  select(datetime, day, Stem_wp_bar, treatment) %>%
  filter(!day == "186")

diurnals_2019_swp_vs_time_grouped<- diurnals_2019_swp_vs_time %>%
  group_by(day, treatment) %>%
  summarise(avg_swp = mean(Stem_wp_bar))

diurnals_2019_swp_vs_time$treatment<- reorder(diurnals_2019_swp_vs_time$treatment, diurnals_2019_swp_vs_time$day)
diurnals_2019_swp_vs_time$day<-factor(diurnals_2019_swp_vs_time$day, 
                                      labels = c ("Jul 1","Jul 12"," Jul 25","Jul 28", "Aug 1", "Aug 15","Aug 20", "Sep 5"))

str(diurnals_2019_swp_vs_time)
diurnals_2019_swp_vs_time_grouped$treatment<- reorder(diurnals_2019_swp_vs_time_grouped$treatment, diurnals_2019_swp_vs_time_grouped$day)


diurnals_2019_swp_vs_time$treatment <- format(diurnals_2019_swp_vs_time$treatment )
as.character(diurnals_2019_swp_vs_time$treatment)
pd2 <- position_dodge(0.75)

swp2019<-diurnals_2019_swp_vs_time %>%
  ggplot(aes(day, Stem_wp_bar))+
  geom_boxplot(alpha =0.1, aes(fill = treatment))+
  geom_point(alpha =0.5, position = pd2, aes(color = treatment, group =treatment))+
  theme_classic()+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  scale_fill_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+ 
  ylab(expression(paste(psi, "stem"))) +
  ggtitle(expression(paste(psi, "stem vs time diurnals 2019"))) +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Julian day") +
  theme(axis.title.y = element_text(size=16, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_y_continuous(limit = c(-17,0))

ggsave(swp2019, filename = "figures/swp2019.pdf", device = cairo_pdf, 
       width = 11, height = 6)

swp2019_v7<-diurnals_2019_swp_vs_time %>%
  ggplot(aes(day, Stem_wp_bar))+
  geom_boxplot(alpha =0.7, aes(fill = treatment))+
  geom_point(alpha = 0.9, position = pd2, aes(color = treatment, group =treatment))+
  theme_classic()+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+ 
  ylab(expression(paste(psi["stem"],  "  (bar)"))) +
  ggtitle("Borden Hills diurnals 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Day") +
  theme(axis.title.y = element_text(size=16, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_y_continuous(breaks=seq(-17,0,2), limits = c (-17,0)) + 
  annotate("text", x = "Aug 1", y = -1, label = "*        ", size = 6) +
  annotate("text", x = "Aug 20", y = -1, label ="*        ", size = 6) +
  geom_vline(xintercept = 3.5, color = "darkgrey", size = 0.5, linetype ="dashed") +
  geom_vline(xintercept = 4.5, color = "darkgrey", size = 0.5, linetype ="dashed") +
  geom_vline(xintercept = 5.5, color = "darkgrey", size = 0.5, linetype ="dashed") +
  geom_vline(xintercept = 6.5, color = "darkgrey", size = 0.5, linetype ="dashed") +
  annotate("text", x = "Jul 28", y = 0, label = "Heatwave", size = 4.5) +
  annotate("text", x = "Aug 15", y = 0, label ="Heatwave", size = 4.5)


ggsave(swp2019_v7, filename = "figures/swp2019_v7.pdf", device = cairo_pdf, 
       width = 11, height = 6)


#### Putting lwp vs round together in one frame

#Heatwaves with new blocks

library(cowplot)

panel_plot_lwp_round_2019_avg_se <- plot_grid (lwpjul25_round_avg_se, lwpjul28_round_avg_se, lwpaug1_round_avg_se, lwpaug15_round_avg_se, lwpaug20_round_avg_se, lwpsep5_round_avg_se, labels=c("Pre-heatwave", "Heatwave", "Post-heatwave","Heatwave", "Post-heatwave", "Post-heatwave", ncol=3, nrow = 2), vjust = 5.8, hjust = -1.5, label_size = 10)

ggsave(panel_plot_lwp_round_2019_avg_se  , filename = "figures/panel_plot_lwp_round_2019_avg_se.pdf", device = cairo_pdf, width = 14, height = 8)


# Everyhting all diurnals and blocks

panel_plot_lwp_round_2019_w_baseline_avg_se <- plot_grid (lwpjul1_round_avg_se,lwpjul12_round_avg_se, lwpjul25_round_avg_se, lwpjul28_round_avg_se, lwpaug1_round_avg_se, lwpaug15_round_avg_se, lwpaug20_round_avg_se, lwpsep5_round_avg_se, labels=c("Baseline","Baseline","Pre-heatwave", "Heatwave", "Post-heatwave","Heatwave", "Post-heatwave", "Post-heatwave", ncol=3, nrow = 3), vjust = 4.2, hjust = -1.5, label_size = 10)

ggsave(panel_plot_lwp_round_2019_w_baseline_avg_se , filename = "figures/panel_plot_lwp_round_2019_w_baseline_avg_se.pdf", device = cairo_pdf, width = 14, height = 12)

#### Leaf water potentials vs time not rounds jul 25 and no data points(impossible to plot everything together, time series too long) #### 

diurnals_2019_lwp_vs_time1_Jul25<- diurnals_2019_lwp_vs_time %>%
  filter(!is.na(Leaf_wp_bar)) %>%
  select(datetime, day, Leaf_wp_bar, pixel_number, round, treatment, Rep) %>%
  filter(day == "206") %>%
  mutate(interval = case_when(
    round == 1 ~ "07/25/2019  6:00",
    round == 2 ~ "07/25/2019  8:30",
    round == 3 ~ "07/25/2019  11:00", 
    round == 4 ~ "07/25/2019  13:00",
    round == 5 ~ "07/25/2019  17:00"
  ))

diurnals_2019_lwp_vs_time1_Jul25$interval1 <-cut(diurnals_2019_lwp_vs_time1_Jul25$datetime, breaks= "135 min")

diurnals_2019_lwp_vs_time1_Jul25$interval <- format((diurnals_2019_lwp_vs_time1_Jul25$interval))
diurnals_2019_lwp_vs_time1_Jul25$interval <- as.factor((diurnals_2019_lwp_vs_time1_Jul25$interval))

diurnals_2019_lwp_vs_time1_Jul25$interval<- mdy_hm(diurnals_2019_lwp_vs_time1_Jul25$interval)


str(diurnals_2019_lwp_vs_time1_Jul25$interval)

diurnals_2019_lwp_vs_time1_Jul25_avg_se <-diurnals_2019_lwp_vs_time1_Jul25 %>%
  group_by(interval, treatment, round) %>%
  summarise(avg_leaf_wp = mean(Leaf_wp_bar), sev = se(Leaf_wp_bar))

diurnals_2019_lwp_vs_time1_Jul25$Rep<- format(diurnals_2019_lwp_vs_time1_Jul25$Rep)
as.character(diurnals_2019_lwp_vs_time1_Jul25$Rep)
str(diurnals_2019_lwp_vs_time1_Jul25$Rep)


diurnals_2019_lwp_vs_time1_Jul25$treatment<- reorder(diurnals_2019_lwp_vs_time1_Jul25$treatment, diurnals_2019_lwp_vs_time1_Jul25$datetime) 

diurnals_2019_lwp_vs_time1_Jul25_avg_se$treatment<- reorder(diurnals_2019_lwp_vs_time1_Jul25_avg_se$treatment, diurnals_2019_lwp_vs_time1_Jul25_avg_se$interval)

str(diurnals_2019_lwp_vs_time1_Jul25_avg_se$interval)
tz(diurnals_2019_lwp_vs_time1_Jul25_avg_se$interval)
tz(diurnals_2019_lwp_vs_time1_Jul25$datetime)


# Plot Jul 25 vs time  

pd<- position_dodge(1400)


lwpjul25_time_avg_se<-ggplot(diurnals_2019_lwp_vs_time1_Jul25_avg_se, aes(interval, avg_leaf_wp, group = treatment, color = treatment)) + 
  geom_errorbar(alpha = 0.9, aes(ymin=avg_leaf_wp-sev, ymax=avg_leaf_wp+sev), width = 1800, position=pd, stat = "identity") +
  geom_line(alpha =0.7, size =1, position=pd, linetype = "solid", stat = "identity") +
  geom_point(alpha =0.6, position=pd, size=2,stat = "identity") + 
  theme_classic()+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab(expression(paste(psi["leaf"],  "  (bar)"))) +
  ggtitle("July 25 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=18, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=16, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-18,0,3), limits = c (-18,0)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  annotate("text", x = as.POSIXct("07-25-2019 17:10:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = -1, label = "*", size = 6)



  
ggsave(lwpjul25_time_avg_se, filename = "figures/lwpjul_25_time_avg_se.pdf", device = cairo_pdf, width = 8, height = 6)

#### Leaf water potentials vs time not rounds wit no data points jul 28 ####

diurnals_2019_lwp_vs_time1_Jul28<- diurnals_2019_lwp_vs_time %>%
  filter(!is.na(Leaf_wp_bar)) %>%
  select(datetime, day, Leaf_wp_bar, pixel_number, round, treatment, Rep) %>%
  filter(day == "209") 

diurnals_2019_lwp_vs_time1_Jul28$interval <-cut(diurnals_2019_lwp_vs_time1_Jul28$datetime, breaks= "150 min", labels = c ("07-28-2019 6:00", "07-28-2019 9:00","07-28-2019 11:30", "07-28-2019 14:00", "07-28-2019 17:00", "07-28-2019 17:00"))
write.csv(diurnals_2019_lwp_vs_time1_Jul28,"data_output/diurnals_2019_lwp_vs_time1_Jul28")

diurnals_2019_lwp_vs_time1_Jul28$interval <- format((diurnals_2019_lwp_vs_time1_Jul28$interval))

diurnals_2019_lwp_vs_time1_Jul28$interval<- mdy_hm(as.character(diurnals_2019_lwp_vs_time1_Jul28$interval))


str(diurnals_2019_lwp_vs_time1_Jul28$interval)
diurnals_2019_lwp_vs_time1_Jul28_avg_se <-diurnals_2019_lwp_vs_time1_Jul28 %>%
  group_by(interval, treatment, round) %>%
  summarise(avg_leaf_wp = mean(Leaf_wp_bar), sev = se(Leaf_wp_bar))

diurnals_2019_lwp_vs_time1_Jul28$Rep<- format(diurnals_2019_lwp_vs_time1_Jul28$Rep)
as.character(diurnals_2019_lwp_vs_time1_Jul28$Rep)
str(diurnals_2019_lwp_vs_time1_Jul28$Rep)


diurnals_2019_lwp_vs_time1_Jul28$treatment<- reorder(diurnals_2019_lwp_vs_time1_Jul28$treatment, diurnals_2019_lwp_vs_time1_Jul28$datetime) 

diurnals_2019_lwp_vs_time1_Jul28_avg_se$treatment<- reorder(diurnals_2019_lwp_vs_time1_Jul28_avg_se$treatment, diurnals_2019_lwp_vs_time1_Jul28_avg_se$interval)

str(diurnals_2019_lwp_vs_time1_Jul28_avg_se$interval)
tz(diurnals_2019_lwp_vs_time1_Jul28_avg_se$interval)
tz(diurnals_2019_lwp_vs_time1_Jul28$datetime)


# Plot Jul 28 vs time  

pd<- position_dodge(1400)


lwpjul28_time_avg_se<-ggplot(diurnals_2019_lwp_vs_time1_Jul28_avg_se, aes(interval, avg_leaf_wp, group = treatment, color = treatment)) + 
  geom_errorbar(alpha = 0.9, aes(ymin=avg_leaf_wp-sev, ymax=avg_leaf_wp+sev), width = 1800, position=pd, stat = "identity") +
  geom_line(alpha =0.7, size =1, position=pd, linetype = "solid", stat = "identity") +
  geom_point(alpha =0.6, position=pd, size=2,stat = "identity") + 
  theme_classic()+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab(expression(paste(psi["leaf"],  "  (bar)"))) +
  ggtitle( "July 28 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=18, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-18,0,3), limits = c (-18,0)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  annotate("text", x = as.POSIXct("07-28-2019 11:35:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = -1, label = "*", size = 6) +
  annotate("text", x = as.POSIXct("07-28-2019 17:10:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = -1, label = "*", size = 6)

ggsave(lwpjul28_time_avg_se, filename = "figures/lwpjul_28_time_avg_se.pdf", device = cairo_pdf, width = 8, height = 6)


#### Leaf water potentials vs time not rounds aug1 no all datapoints ####

diurnals_2019_lwp_vs_time1_aug1<- diurnals_2019_lwp_vs_time %>%
  filter(!is.na(Leaf_wp_bar)) %>%
  select(datetime, day, Leaf_wp_bar, pixel_number, round, treatment, Rep) %>%
  filter(day == "213") 

diurnals_2019_lwp_vs_time1_aug1$interval <-cut(diurnals_2019_lwp_vs_time1_aug1$datetime, breaks= "140 min", labels = c ("08-01-2019 5:30", "08-01-2019 8:30","08-01-2019 10:40", "08-01-2019 13:30", "08-01-2019 17:00", "08-01-2019 17:00"))
write.csv(diurnals_2019_lwp_vs_time1_aug1,"data_output/diurnals_2019_lwp_vs_time1_aug1")

diurnals_2019_lwp_vs_time1_aug1$interval <- format((diurnals_2019_lwp_vs_time1_aug1$interval))

diurnals_2019_lwp_vs_time1_aug1$interval<- mdy_hm(as.character(diurnals_2019_lwp_vs_time1_aug1$interval))


str(diurnals_2019_lwp_vs_time1_aug1$interval)
diurnals_2019_lwp_vs_time1_aug1_avg_se <-diurnals_2019_lwp_vs_time1_aug1 %>%
  group_by(interval, treatment, round) %>%
  summarise(avg_leaf_wp = mean(Leaf_wp_bar), sev = se(Leaf_wp_bar))

diurnals_2019_lwp_vs_time1_aug1$Rep<- format(diurnals_2019_lwp_vs_time1_aug1$Rep)
as.character(diurnals_2019_lwp_vs_time1_aug1$Rep)
str(diurnals_2019_lwp_vs_time1_aug1$Rep)


diurnals_2019_lwp_vs_time1_aug1$treatment<- reorder(diurnals_2019_lwp_vs_time1_aug1$treatment, diurnals_2019_lwp_vs_time1_aug1$datetime) 

diurnals_2019_lwp_vs_time1_aug1_avg_se$treatment<- reorder(diurnals_2019_lwp_vs_time1_aug1_avg_se$treatment, diurnals_2019_lwp_vs_time1_aug1_avg_se$interval)

str(diurnals_2019_lwp_vs_time1_aug1_avg_se$interval)
tz(diurnals_2019_lwp_vs_time1_aug1_avg_se$interval)
tz(diurnals_2019_lwp_vs_time1_aug1$datetime)


# Plot aug1 vs time  

pd<- position_dodge(1400)


lwpaug1_time_avg_se<-ggplot(diurnals_2019_lwp_vs_time1_aug1_avg_se, aes(interval, avg_leaf_wp, group = treatment, color = treatment)) + 
  geom_errorbar(alpha = 0.9, aes(ymin=avg_leaf_wp-sev, ymax=avg_leaf_wp+sev), width = 1800, position=pd, stat = "identity") +
  geom_line(alpha =0.7, size =1, position=pd, linetype = "solid", stat = "identity") +
  geom_point(alpha =0.6, position=pd, size=2,stat = "identity") + 
  theme_classic()+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab(expression(paste(psi["leaf"],  "  (bar)"))) +
  ggtitle( "August 1 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=18, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-18,0,3), limits = c (-18,0)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  annotate("text", x = as.POSIXct("08-01-2019 8:35:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = -1, label = "*", size = 6) +
  annotate("text", x = as.POSIXct("08-01-2019 10:55:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = -1, label = "*", size = 6) +
  annotate("text", x = as.POSIXct("08-01-2019 13:40:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y =-1, label = "*", size = 6) +
  annotate("text", x = as.POSIXct("08-01-2019 17:10:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = -1, label = "*", size = 6) 
  
ggsave(lwpaug1_time_avg_se, filename = "figures/lwpaug1_time_avg_se.pdf", device = cairo_pdf, width = 8, height = 6)

#### Leaf water potentials vs time not rounds aug15 ####

diurnals_2019_lwp_vs_time1_aug15<- diurnals_2019_lwp_vs_time %>%
  filter(!is.na(Leaf_wp_bar)) %>%
  select(datetime, day, Leaf_wp_bar, pixel_number, round, treatment, Rep) %>%
  filter(day == "227") 

diurnals_2019_lwp_vs_time1_aug15$interval <-cut(diurnals_2019_lwp_vs_time1_aug15$datetime, breaks= "140 min", labels = c ("08-15-2019 6:00", "08-15-2019 9:00","08-15-2019 11:15", "08-15-2019 13:30", "08-15-2019 17:00", "08-15-2019 17:00"))
write.csv(diurnals_2019_lwp_vs_time1_aug15,"data_output/diurnals_2019_lwp_vs_time1_aug15")

diurnals_2019_lwp_vs_time1_aug15$interval <- format((diurnals_2019_lwp_vs_time1_aug15$interval))

diurnals_2019_lwp_vs_time1_aug15$interval<- mdy_hm(as.character(diurnals_2019_lwp_vs_time1_aug15$interval))


str(diurnals_2019_lwp_vs_time1_aug15$interval)
diurnals_2019_lwp_vs_time1_aug15_avg_se <-diurnals_2019_lwp_vs_time1_aug15 %>%
  group_by(interval, treatment, round) %>%
  summarise(avg_leaf_wp = mean(Leaf_wp_bar), sev = se(Leaf_wp_bar))

diurnals_2019_lwp_vs_time1_aug15$Rep<- format(diurnals_2019_lwp_vs_time1_aug15$Rep)
as.character(diurnals_2019_lwp_vs_time1_aug15$Rep)
str(diurnals_2019_lwp_vs_time1_aug15$Rep)


diurnals_2019_lwp_vs_time1_aug15$treatment<- reorder(diurnals_2019_lwp_vs_time1_aug15$treatment, diurnals_2019_lwp_vs_time1_aug15$datetime) 

diurnals_2019_lwp_vs_time1_aug15_avg_se$treatment<- reorder(diurnals_2019_lwp_vs_time1_aug15_avg_se$treatment, diurnals_2019_lwp_vs_time1_aug15_avg_se$interval)

str(diurnals_2019_lwp_vs_time1_aug15_avg_se$interval)
tz(diurnals_2019_lwp_vs_time1_aug15_avg_se$interval)
tz(diurnals_2019_lwp_vs_time1_aug15$datetime)


# Plot aug15 vs time  

pd<- position_dodge(1400)


lwpaug15_time_avg_se<-ggplot(diurnals_2019_lwp_vs_time1_aug15_avg_se, aes(interval, avg_leaf_wp, group = treatment, color = treatment)) + 
  geom_errorbar(alpha = 0.9, aes(ymin=avg_leaf_wp-sev, ymax=avg_leaf_wp+sev), width = 1800, position=pd, stat = "identity") +
  geom_line(alpha =0.7, size =1, position=pd, linetype = "solid", stat = "identity") +
  geom_point(alpha =0.6, position=pd, size=2,stat = "identity") + 
  theme_classic()+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab(expression(paste(psi["leaf"],  "  (bar)"))) +
  ggtitle( "August 15 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=18, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-18,0,3), limits = c (-18,0)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  annotate("text", x = as.POSIXct("08-15-2019 9:10:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = -1, label = "*", size = 6) +
  annotate("text", x = as.POSIXct("08-15-2019 11:30:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = -1, label = "*", size = 6) +
  annotate("text", x = as.POSIXct("08-15-2019 17:10:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = -1, label = "*", size = 6)

ggsave(lwpaug15_time_avg_se, filename = "figures/lwpaug15_time_avg_se.pdf", device = cairo_pdf, width = 8, height = 6)

#### Leaf water potentials vs time not rounds aug20 ####

diurnals_2019_lwp_vs_time1_aug20<- diurnals_2019_lwp_vs_time %>%
  filter(!is.na(Leaf_wp_bar)) %>%
  select(datetime, day, Leaf_wp_bar, pixel_number, round, treatment, Rep) %>%
  filter(day == "232") 

diurnals_2019_lwp_vs_time1_aug20$interval <-cut(diurnals_2019_lwp_vs_time1_aug20$datetime, breaks= "145 min", labels = c ("08-20-2019 6:00", "08-20-2019 9:00","08-20-2019 11:15", "08-20-2019 13:30", "08-20-2019 17:00", "08-20-2019 17:00"))
write.csv(diurnals_2019_lwp_vs_time1_aug20,"data_output/diurnals_2019_lwp_vs_time1_aug20")

diurnals_2019_lwp_vs_time1_aug20$interval <- format((diurnals_2019_lwp_vs_time1_aug20$interval))

diurnals_2019_lwp_vs_time1_aug20$interval<- mdy_hm(as.character(diurnals_2019_lwp_vs_time1_aug20$interval))


str(diurnals_2019_lwp_vs_time1_aug20$interval)
diurnals_2019_lwp_vs_time1_aug20_avg_se <-diurnals_2019_lwp_vs_time1_aug20 %>%
  group_by(interval, treatment, round) %>%
  summarise(avg_leaf_wp = mean(Leaf_wp_bar), sev = se(Leaf_wp_bar))

diurnals_2019_lwp_vs_time1_aug20$Rep<- format(diurnals_2019_lwp_vs_time1_aug20$Rep)
as.character(diurnals_2019_lwp_vs_time1_aug20$Rep)
str(diurnals_2019_lwp_vs_time1_aug20$Rep)


diurnals_2019_lwp_vs_time1_aug20$treatment<- reorder(diurnals_2019_lwp_vs_time1_aug20$treatment, diurnals_2019_lwp_vs_time1_aug20$datetime) 

diurnals_2019_lwp_vs_time1_aug20_avg_se$treatment<- reorder(diurnals_2019_lwp_vs_time1_aug20_avg_se$treatment, diurnals_2019_lwp_vs_time1_aug20_avg_se$interval)

str(diurnals_2019_lwp_vs_time1_aug20_avg_se$interval)
tz(diurnals_2019_lwp_vs_time1_aug20_avg_se$interval)
tz(diurnals_2019_lwp_vs_time1_aug20$datetime)


# Plot aug20 vs time  

pd<- position_dodge(1400)


lwpaug20_time_avg_se<-ggplot(diurnals_2019_lwp_vs_time1_aug20_avg_se, aes(interval, avg_leaf_wp, group = treatment, color = treatment)) + 
  geom_errorbar(alpha = 0.9, aes(ymin=avg_leaf_wp-sev, ymax=avg_leaf_wp+sev), width = 1800, position=pd, stat = "identity") +
  geom_line(alpha =0.7, size =1, position=pd, linetype = "solid", stat = "identity") +
  geom_point(alpha =0.6, position=pd, size=2,stat = "identity") + 
  theme_classic()+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab(expression(paste(psi["leaf"],  "  (bar)"))) +
  ggtitle("August 20 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=18, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-18,0,3), limits = c (-18,0)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  annotate("text", x = as.POSIXct("08-20-2019 6:00:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = -0.5, label = "*", size = 6) +
  annotate("text", x = as.POSIXct("08-20-2019 11:20:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = -1, label = "*", size = 6) +
  annotate("text", x = as.POSIXct("08-20-2019 17:10:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = -1, label = "*", size = 6)

ggsave(lwpaug20_time_avg_se, filename = "figures/lwpaug20_time_avg_se.pdf", device = cairo_pdf, width = 8, height = 6)


#### Leaf water potentials vs time not rounds sep5 ####

diurnals_2019_lwp_vs_time1_sep5<- diurnals_2019_lwp_vs_time %>%
  filter(!is.na(Leaf_wp_bar)) %>%
  select(datetime, day, Leaf_wp_bar, pixel_number, round, treatment, Rep) %>%
  filter(day == "248") 

diurnals_2019_lwp_vs_time1_sep5$interval <-cut(diurnals_2019_lwp_vs_time1_sep5$datetime, breaks= "145 min", labels = c ("09-05-2019 6:00", "09-05-2019 9:00","09-05-2019 11:15", "09-05-2019 13:30", "09-05-2019 17:00"))

str(diurnals_2019_lwp_vs_time1_sep5$interval)
write.csv(diurnals_2019_lwp_vs_time1_sep5,"data_output/diurnals_2019_lwp_vs_time1_sep5")

diurnals_2019_lwp_vs_time1_sep5$interval <- format((diurnals_2019_lwp_vs_time1_sep5$interval))

diurnals_2019_lwp_vs_time1_sep5$interval<- mdy_hm(as.character(diurnals_2019_lwp_vs_time1_sep5$interval))


str(diurnals_2019_lwp_vs_time1_sep5$interval)
diurnals_2019_lwp_vs_time1_sep5_avg_se <-diurnals_2019_lwp_vs_time1_sep5 %>%
  group_by(interval, treatment, round) %>%
  summarise(avg_leaf_wp = mean(Leaf_wp_bar), sev = se(Leaf_wp_bar))

diurnals_2019_lwp_vs_time1_sep5$Rep<- format(diurnals_2019_lwp_vs_time1_sep5$Rep)
as.character(diurnals_2019_lwp_vs_time1_sep5$Rep)
str(diurnals_2019_lwp_vs_time1_sep5$Rep)


diurnals_2019_lwp_vs_time1_sep5$treatment<- reorder(diurnals_2019_lwp_vs_time1_sep5$treatment, diurnals_2019_lwp_vs_time1_sep5$datetime) 

diurnals_2019_lwp_vs_time1_sep5_avg_se$treatment<- reorder(diurnals_2019_lwp_vs_time1_sep5_avg_se$treatment, diurnals_2019_lwp_vs_time1_sep5_avg_se$interval)

str(diurnals_2019_lwp_vs_time1_sep5_avg_se$interval)
tz(diurnals_2019_lwp_vs_time1_sep5_avg_se$interval)
tz(diurnals_2019_lwp_vs_time1_sep5$datetime)


# Plot sep5 vs time  

pd<- position_dodge(1400)


lwpsep5_time_avg_se<-ggplot(diurnals_2019_lwp_vs_time1_sep5_avg_se, aes(interval, avg_leaf_wp, group = treatment, color = treatment)) + 
  geom_errorbar(alpha = 0.9, aes(ymin=avg_leaf_wp-sev, ymax=avg_leaf_wp+sev), width = 1800, position=pd, stat = "identity") +
  geom_line(alpha =0.7, size =1, position=pd, linetype = "solid", stat = "identity") +
  geom_point(alpha =0.6, position=pd, size=2,stat = "identity") + 
  theme_classic()+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab(expression(paste(psi["leaf"],  "  (bar)"))) +
  ggtitle( "September 5 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=18, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-18,0,3), limits = c (-18,0)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M")

ggsave(lwpsep5_time_avg_se, filename = "figures/lwpsep5_time_avg_se.pdf", device = cairo_pdf, width = 8, height = 6)

#### Leaf water potentials vs time not rounds no all data points Jul 1####

diurnals_2019_lwp_vs_time1_jul1<- diurnals_2019_lwp_vs_time %>%
  filter(!is.na(Leaf_wp_bar)) %>%
  select(datetime, day, Leaf_wp_bar, pixel_number, round, treatment, Rep) %>%
  filter(day == "182") %>%
  mutate(interval = case_when(
    round == 1 ~ "7/1/2019  7:15",
    round == 2 ~ "7/1/2019  11:00",
    round == 3 ~ "7/1/2019  14:00", 
    round == 4 ~ "7/1/2019  17:15",
    round == 5 ~ "7/1/2019  19:10"
  ))

diurnals_2019_lwp_vs_time1_jul1$interval <- format((diurnals_2019_lwp_vs_time1_jul1$interval))
diurnals_2019_lwp_vs_time1_jul1$interval<- as.factor(diurnals_2019_lwp_vs_time1_jul1$interval)

str(diurnals_2019_lwp_vs_time1_jul1$interval)

diurnals_2019_lwp_vs_time1_jul1$interval<- mdy_hm(diurnals_2019_lwp_vs_time1_jul1$interval)

str(diurnals_2019_lwp_vs_time1_jul1$interval)


diurnals_2019_lwp_vs_time1_jul1_avg_se <-diurnals_2019_lwp_vs_time1_jul1 %>%
  group_by(interval, treatment, round) %>%
  summarise(avg_leaf_wp = mean(Leaf_wp_bar), sev = se(Leaf_wp_bar))

diurnals_2019_lwp_vs_time1_jul1 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2019_lwp_vs_time1_jul1$Rep<- format(diurnals_2019_lwp_vs_time1_jul1$Rep)
as.character(diurnals_2019_lwp_vs_time1_jul1$Rep)
str(diurnals_2019_lwp_vs_time1_jul1$Rep)

diurnals_2019_lwp_vs_time1_jul1$treatment<- reorder(diurnals_2019_lwp_vs_time1_jul1$treatment, diurnals_2019_lwp_vs_time1_jul1$datetime) 

diurnals_2019_lwp_vs_time1_jul1_avg_se$treatment<- reorder(diurnals_2019_lwp_vs_time1_jul1_avg_se$treatment, diurnals_2019_lwp_vs_time1_jul1_avg_se$interval)

str(diurnals_2019_lwp_vs_time1_jul1_avg_se$interval)
tz(diurnals_2019_lwp_vs_time1_jul1_avg_se$interval)
tz(diurnals_2019_lwp_vs_time1_jul1$datetime)


# Plot jul1 vs time  

pd<- position_dodge(1400)


lwpjul1_time_avg_se<-ggplot(diurnals_2019_lwp_vs_time1_jul1_avg_se, aes(interval, avg_leaf_wp, group = treatment, color = treatment)) + 
  geom_errorbar(alpha = 0.9, aes(ymin=avg_leaf_wp-sev, ymax=avg_leaf_wp+sev), width = 1800, position=pd, stat = "identity") +
  geom_line(alpha =0.7, size =1, position=pd, linetype = "solid", stat = "identity") +
  geom_point(alpha =0.6, position=pd, size=2,stat = "identity") + 
  theme_classic()+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab(expression(paste(psi["leaf"],  "  (bar)"))) +
  ggtitle( "July 1 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=18, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-18,0,3), limits = c (-18,0)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  annotate("text", x = as.POSIXct("07-01-2019 7:35:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = -1, label = "*", size = 6) +
  annotate("text", x = as.POSIXct("07-01-2019 11:20:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y =-1, label = "*", size = 6)


ggsave(lwpjul1_time_avg_se, filename = "figures/lwpjul1_time_avg_se.pdf", device = cairo_pdf, width = 8, height = 6)

#### Leaf water potentials vs time not rounds Jul 12####

diurnals_2019_lwp_vs_time1_jul12<- diurnals_2019_lwp_vs_time %>%
  filter(!is.na(Leaf_wp_bar)) %>%
  select(datetime, day, Leaf_wp_bar, pixel_number, round, treatment, Rep) %>%
  filter(day == "193") %>%
  mutate(interval = case_when(
    round == 1 ~ "7/12/2019  6:20",
    round == 2 ~ "7/12/2019  9:00",
    round == 3 ~ "7/12/2019  11:45", 
    round == 4 ~ "7/12/2019  14:00",
    round == 5 ~ "7/12/2019  17:00", 
    round == 6 ~ "7/12/2019  19:30", 
  ))

diurnals_2019_lwp_vs_time1_jul12$interval <- format((diurnals_2019_lwp_vs_time1_jul12$interval))
diurnals_2019_lwp_vs_time1_jul12$interval<- as.factor(diurnals_2019_lwp_vs_time1_jul12$interval)

str(diurnals_2019_lwp_vs_time1_jul12$interval)

diurnals_2019_lwp_vs_time1_jul12$interval<- mdy_hm(diurnals_2019_lwp_vs_time1_jul12$interval)

str(diurnals_2019_lwp_vs_time1_jul12$interval)


diurnals_2019_lwp_vs_time1_jul12_avg_se <-diurnals_2019_lwp_vs_time1_jul12 %>%
  group_by(interval, treatment, round) %>%
  summarise(avg_leaf_wp = mean(Leaf_wp_bar), sev = se(Leaf_wp_bar))

diurnals_2019_lwp_vs_time1_jul12 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2019_lwp_vs_time1_jul12$Rep<- format(diurnals_2019_lwp_vs_time1_jul12$Rep)
as.character(diurnals_2019_lwp_vs_time1_jul12$Rep)
str(diurnals_2019_lwp_vs_time1_jul12$Rep)


diurnals_2019_lwp_vs_time1_jul12$treatment<- reorder(diurnals_2019_lwp_vs_time1_jul12$treatment, diurnals_2019_lwp_vs_time1_jul12$datetime) 

diurnals_2019_lwp_vs_time1_jul12_avg_se$treatment<- reorder(diurnals_2019_lwp_vs_time1_jul12_avg_se$treatment, diurnals_2019_lwp_vs_time1_jul12_avg_se$interval)

str(diurnals_2019_lwp_vs_time1_jul12_avg_se$interval)
tz(diurnals_2019_lwp_vs_time1_jul12_avg_se$interval)
tz(diurnals_2019_lwp_vs_time1_jul12$datetime)


# Plot jul12 vs time  

pd<- position_dodge(1400)


lwpjul12_time_avg_se<-ggplot(diurnals_2019_lwp_vs_time1_jul12_avg_se, aes(interval, avg_leaf_wp, group = treatment, color = treatment)) + 
  geom_errorbar(alpha = 0.9, aes(ymin=avg_leaf_wp-sev, ymax=avg_leaf_wp+sev), width = 1800, position=pd, stat = "identity") +
  geom_line(alpha =0.7, size =1, position=pd, linetype = "solid", stat = "identity") +
  geom_point(alpha =0.6, position=pd, size=2,stat = "identity") + 
  theme_classic()+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab(expression(paste(psi["leaf"],  "  (bar)"))) +
  ggtitle("July 12 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=16, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-18,0,3), limits = c (-18,0)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  annotate("text", x = as.POSIXct("07-12-2019 11:50:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = -1, label = "*", size = 6) +
  annotate("text", x = as.POSIXct("07-12-2019 14:00:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = -1, label = "*", size = 6) +
  annotate("text", x = as.POSIXct("07-12-2019 17:10:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = -1, label = "*", size = 6) +
  annotate("text", x = as.POSIXct("07-12-2019 19:50:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = -1, label = "*", size = 6) +
  annotate("text", x = as.POSIXct("07-12-2019 09:00:00", format="%m-%d-%Y %H:%M:%S", tz ="UTC"), y = -1, label = "*", size = 6) 


ggsave(lwpjul12_time_avg_se, filename = "figures/lwpjul12_time_avg_se.pdf", device = cairo_pdf, width = 8, height = 6)

#### Putting lwp vs round together in one frame with TIME ####

#Heatwaves with new blocks

library(cowplot)

panel_plot_lwp_time_2019_avg_se_ok <- plot_grid (lwpjul25_time_avg_se, lwpjul28_time_avg_se, lwpaug1_time_avg_se, lwpaug15_time_avg_se, lwpaug20_time_avg_se, lwpsep5_time_avg_se, labels=c("Pre-heatwave", "Heatwave", "Post-heatwave","Heatwave", "Post-heatwave", "Recovery", ncol=3, nrow = 2), vjust = 4, hjust = -1.5, label_size = 12)

ggsave(panel_plot_lwp_time_2019_avg_se_ok , filename = "figures/panel_plot_lwp_time_2019_avg_se_ok.pdf", device = cairo_pdf, width = 15, height = 8)


# Everyhting all diurnals and blocks

panel_plot_lwp_time_2019_w_baseline_avg_se_ok <- plot_grid (lwpjul1_time_avg_se,lwpjul12_time_avg_se, lwpjul25_time_avg_se, lwpjul28_time_avg_se, lwpaug1_time_avg_se, lwpaug15_time_avg_se,labels=c("Baseline","Baseline","Pre-heatwave", ncol = 3, nrow =2), vjust = 3.8, hjust = -1.5, label_size = 12)

ggsave(panel_plot_lwp_time_2019_w_baseline_avg_se_ok , filename = "figures/panel_plot_lwp_time_2019_w_baseline_avg_se_ok.pdf", device = cairo_pdf, width = 15, height = 8)
