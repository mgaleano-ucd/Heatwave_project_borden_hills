diurnals_borden_hills_2019 <-read.csv("data_output/diurnals_2019_old_and_new_blocks_cleaned_no_NAs.csv", header = TRUE)

diurnals_borden_hills_2019$round<-format(diurnals_borden_hills_2019$round)
diurnals_borden_hills_2019$round<-as.numeric(as.factor(diurnals_borden_hills_2019$round))

str(diurnals_borden_hills_2019$round)

str(diurnals_borden_hills_2019)

diurnals_2019leafT_vs_treat <- diurnals_borden_hills_2019 %>%
  mutate(time = hhmmss) %>%
  select(-hhmmss) %>%
  filter(!is.na(time)) %>%
  filter(!is.na(leaf_temp_C)) %>%
  filter(!leaf_temp_C == "-")


diurnals_2019leafT_vs_treat$time<- format(strptime(diurnals_2019leafT_vs_treat$time,"%H:%M:%S"), format = "%H:%M", tz = "UTC")

str(diurnals_2019leafT_vs_treat$time)



diurnals_2019leafT_vs_treat$datetime <- paste(diurnals_2019leafT_vs_treat$date, " ", diurnals_2019leafT_vs_treat$time, sep = "")

glimpse(diurnals_2019leafT_vs_treat) 

diurnals_2019leafT_vs_treat$datetime <- mdy_hm(diurnals_2019leafT_vs_treat$datetime, tz = "UTC")



str(diurnals_2019leafT_vs_treat)

tz(diurnals_2019leafT_vs_treat$datetime)

tz(diurnals_2019leafT_vs_treat$time)

diurnals_2019leafT_vs_treat$leaf_temp_C <- format(diurnals_2019leafT_vs_treat $leaf_temp_C)
diurnals_2019leafT_vs_treat$leaf_temp_C<-as.numeric(diurnals_2019leafT_vs_treat$leaf_temp_C)

str(diurnals_2019leafT_vs_treat$datetime)

str(diurnals_2019leafT_vs_treat$leaf_temp_C)

diurnals_2019leafT_vs_treat$time<- format(strptime(diurnals_2019leafT_vs_treat$date, format = "%m-%d-%Y"))

diurnals_2019leafT_vs_treat$date <- as.character(diurnals_2019leafT_vs_treat$date, format = "%m-%d-%Y")

str(diurnals_2019leafT_vs_treat$date)

diurnals_2019leafT_vs_treat$date<- as.Date(diurnals_2019leafT_vs_treat$date, format="%m-%d-%Y", tz = "UTC")

####Plotting  gsw vs IRT leaf T FISRT HW round 2-5 ####

diurnals_2019leafT_vs_treat_first_HW<- diurnals_2019leafT_vs_treat %>%
  filter(!is.na(leaf_temp_C)) %>%
  select(datetime, day, date, leaf_temp_C, Tleaf, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block, BH_Vine) %>%
  filter(day < 215) %>%
  filter(day > 205) %>%
  filter(round == "4")


str(diurnals_2019leafT_vs_treat_first_HW$leaf_temp_C)

se <- function(x) sqrt(var(x)/length(x))


diurnals_2019leafT_vs_treat_first_HW$treatment<-format(diurnals_2019leafT_vs_treat_first_HW$treatment)
as.character(diurnals_2019leafT_vs_treat_first_HW$treatment)

diurnals_2019leafT_vs_treat_first_HW$treatment<- reorder(diurnals_2019leafT_vs_treat_first_HW$treatment, diurnals_2019leafT_vs_treat_first_HW$date)

str(diurnals_2019leafT_vs_treat_first_HW$treatment)

diurnals_2019leafT_vs_treat_first_HW$Rep<-format(diurnals_2019leafT_vs_treat_first_HW$Rep)
as.character(diurnals_2019leafT_vs_treat_first_HW$Rep)

str(diurnals_2019leafT_vs_treat_first_HW$treatment)
diurnals_2019leafT_vs_treat_first_HW$treatment<-format(diurnals_2019leafT_vs_treat_first_HW$treatment)
as.character(diurnals_2019leafT_vs_treat_first_HW$treatment)

diurnals_2019leafT_vs_treat_first_HW$date<-factor(diurnals_2019leafT_vs_treat_first_HW$date, 
                                      labels = c ("Jul 25","Jul 28", "Aug 1"))



## Plot first HW RESPONSE

pd<- position_dodge(1)


                       
IRTtemp_firstHW_round4 <- diurnals_2019leafT_vs_treat_first_HW %>%
  ggplot(aes(date, leaf_temp_C))+
  geom_boxplot(alpha =0.1, aes(fill = treatment))+
  geom_point(alpha =0.5, position = pd2, aes(color = treatment, group =treatment))+
  theme_classic()+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  scale_fill_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+ 
  ylab("IRT leaf temperature (ºC)") +
  ggtitle( "IRT leaf tempertaure first Heatwave 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, family = "serif")) +
  xlab("Day") +
  theme(axis.title.y = element_text(size=14,, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_y_continuous(breaks=seq(25,50,3), limits = c (25,50)) + 
  annotate("text", x = "Jul 25", y = 43, label = "*          ", size = 6) +
  annotate("text", x = "Jul 25", y = 43, label ="            *", size = 6) +   annotate("text", x = "Jul 28", y = 43, label ="*          ", size = 6) 

ggsave(IRTtemp_firstHW_round4, filename = "figures/IRTtemp_firstHW_round4", device = cairo_pdf, 
       width = 8, height = 6)


#### LICOR FIRST HW

str(diurnals_2019leafT_vs_treat_first_HW)

LICORtemp_firstHW_round4 <-diurnals_2019leafT_vs_treat_first_HW %>%
  ggplot(aes(date, Tleaf))+
  geom_boxplot(alpha =0.1, aes(fill = treatment))+
  geom_point(alpha =0.5, position = pd2, aes(color = treatment, group =treatment))+
  theme_classic()+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  scale_fill_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+ 
  ylab("LICOR leaf temperature (ºC)") +
  ggtitle( "LICOR leaf tempertaure first Heatwave 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, family = "serif")) +
  xlab("Day") +
  theme(axis.title.y = element_text(size=14,, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_y_continuous(breaks=seq(25,50,3), limits = c (25,50)) + 
  annotate("text", x = "Jul 28", y = 49, label = "*          ", size = 6) +
  annotate("text", x = "Jul 28", y = 49, label ="*", size = 6) +   annotate("text", x = "Aug 1", y = 49, label ="*", size = 6) 

ggsave(LICORtemp_firstHW_round4 , filename = "figures/LICORtemp_firstHW_round4 ", device = cairo_pdf, 
       width = 8, height = 6)


#### Second HW

diurnals_2019leafT_vs_treat_second_HW<- diurnals_2019leafT_vs_treat %>%
  filter(!is.na(leaf_temp_C)) %>%
  select(datetime, day, date, leaf_temp_C, Tleaf, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block, BH_Vine) %>%
  filter(day < 233) %>%
  filter(day > 226) %>%
  filter(round == "4")


str(diurnals_2019leafT_vs_treat_second_HW$leaf_temp_C)

se <- function(x) sqrt(var(x)/length(x))


diurnals_2019leafT_vs_treat_second_HW$treatment<-format(diurnals_2019leafT_vs_treat_second_HW$treatment)
as.character(diurnals_2019leafT_vs_treat_second_HW$treatment)

str(diurnals_2019leafT_vs_treat_second_HW$treatment)

diurnals_2019leafT_vs_treat_second_HW$Rep<-format(diurnals_2019leafT_vs_treat_second_HW$Rep)
as.character(diurnals_2019leafT_vs_treat_second_HW$Rep)

str(diurnals_2019leafT_vs_treat_second_HW$treatment)
diurnals_2019leafT_vs_treat_second_HW$treatment<-format(diurnals_2019leafT_vs_treat_second_HW$treatment)
as.character(diurnals_2019leafT_vs_treat_second_HW$treatment)

str(diurnals_2019leafT_vs_treat_second_HW$date)
diurnals_2019leafT_vs_treat_second_HW$date<-factor(diurnals_2019leafT_vs_treat_second_HW$date, 
                                                  labels = c ("Aug 15","Aug 20"))


IRTtemp_secondHW_round4 <- diurnals_2019leafT_vs_treat_second_HW %>%
  ggplot(aes(date, leaf_temp_C))+
  geom_boxplot(alpha =0.1, aes(fill = treatment))+
  geom_point(alpha =0.5, position = pd2, aes(color = treatment, group =treatment))+
  theme_classic()+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  scale_fill_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+ 
  ylab("IRT leaf temperature (ºC)") +
  ggtitle( "IRT leaf tempertaure second Heatwave 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, family = "serif")) +
  xlab("Day") +
  theme(axis.title.y = element_text(size=14,, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_y_continuous(breaks=seq(25,50,3), limits = c (25,50)) + 
  annotate("text", x = "Aug 15", y = 46, label = "*               ", size = 6) 

ggsave(IRTtemp_secondHW_round4, filename = "figures/IRTtemp_secondHW_round4", device = cairo_pdf, 
       width = 8, height = 6)

##lICOR

LICORtemp_secondHW_round4 <-diurnals_2019leafT_vs_treat_second_HW %>%
  ggplot(aes(date, Tleaf))+
  geom_boxplot(alpha =0.1, aes(fill = treatment))+
  geom_point(alpha =0.5, position = pd2, aes(color = treatment, group =treatment))+
  theme_classic()+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  scale_fill_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+ 
  ylab("LICOR leaf temperature (ºC)") +
  ggtitle( "LICOR leaf tempertaure second Heatwave 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, family = "serif")) +
  xlab("Day") +
  theme(axis.title.y = element_text(size=14,, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_y_continuous(breaks=seq(25,50,3), limits = c (25,50)) + 
  annotate("text", x = "Aug 15", y = 49, label = "*               ", size = 6) 

ggsave(LICORtemp_secondHW_round4 , filename = "figures/LICORtemp_secondHW_round4 ", device = cairo_pdf, 
       width = 8, height = 6)

#### All together Round 4 leaftemp adn two HWS

panel_plot_leaftempr4_vs_HWS<- plot_grid (IRTtemp_firstHW_round4, LICORtemp_firstHW_round4, IRTtemp_secondHW_round4, LICORtemp_secondHW_round4,labels=c("Round 4 12:30-14:30", "Round 4 12:30-14:30", "Round 4 12:30-14:30","Round 4 12:30-14:30"), ncol=2, nrow = 2, vjust = 4.5, hjust = -0.5, label_size = 10)


ggsave(panel_plot_leaftempr4_vs_HWS , filename = "figures/panel_plot_leaftempr4_vs_HWS.pdf", device = cairo_pdf, width = 13, height = 9)