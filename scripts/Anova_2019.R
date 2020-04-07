library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(agricolae)


diurnals_borden_hills_2019 <-read.csv("data_output/diurnals_2019_old_and_new_blocks_cleaned_no_NAs.csv", header = TRUE)

str(diurnals_borden_hills_2019)

diurnals_2019_lwp_vs_time <- diurnals_borden_hills_2019 %>%
  mutate(time = hhmmss) %>%
  select(-hhmmss) %>%
  filter(!is.na(time))


str(diurnals_2019_lwp_vs_time$time)

diurnals_2019_lwp_vs_time$time<- format(strptime(diurnals_2019_lwp_vs_time$time,"%H:%M:%S"), format = "%H:%M", tz = "America_Los_Angeles")

diurnals_2019_lwp_vs_time$datetime <- paste(diurnals_2019_lwp_vs_time$date, " ", diurnals_2019_lwp_vs_time$time, sep = "")

glimpse(diurnals_2019_lwp_vs_time) 

diurnals_2019_lwp_vs_time$datetime <- mdy_hm(diurnals_2019_lwp_vs_time$datetime, tz = "UTC")

str(diurnals_2019_lwp_vs_time)

tz(diurnals_2019_lwp_vs_time$datetime)

str(diurnals_2019_lwp_vs_time$datetime)



#### Running one way ANOVA for swp ####

#### Jul 1 ####

diurnals_2019_swp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Stem_wp_bar))%>%
  filter(day == "182")

str(diurnals_2019_swp_vs_time)

diurnals_2019_swp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_swp_vs_time$treatment)

diurnals_2019_swp_vs_time$treatment<- format(diurnals_2019_swp_vs_time$treatment)
diurnals_2019_swp_vs_time$treatment<- as.factor(diurnals_2019_swp_vs_time$treatment)

is.factor(diurnals_2019_swp_vs_time$treatment)

ggplot(diurnals_2019_swp_vs_time, aes (treatment,Stem_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

Anova_stem<- aov (Stem_wp_bar~treatment, diurnals_2019_swp_vs_time)
summary (Anova_stem)

(test<- HSD.test(Anova_stem, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))
summary(test)

diurnals_2019_swp_vs_time_hist<-diurnals_2019_swp_vs_time %>%
 filter(treatment == 1)
hist(diurnals_2019_swp_vs_time_hist$Stem_wp_bar)

diurnals_2019_swp_vs_time_hist<-diurnals_2019_swp_vs_time %>%
filter(treatment == 2)
hist(diurnals_2019_swp_vs_time_hist$Stem_wp_bar)

diurnals_2019_swp_vs_time_hist<-diurnals_2019_swp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_swp_vs_time_hist$Stem_wp_bar)

####Jul 12 ####

diurnals_2019_swp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Stem_wp_bar))%>%
  filter(day == "193")

str(diurnals_2019_swp_vs_time)

diurnals_2019_swp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_swp_vs_time$treatment)

diurnals_2019_swp_vs_time$treatment<- format(diurnals_2019_swp_vs_time$treatment)
diurnals_2019_swp_vs_time$treatment<- as.factor(diurnals_2019_swp_vs_time$treatment)

is.factor(diurnals_2019_swp_vs_time$treatment)

ggplot(diurnals_2019_swp_vs_time, aes (treatment,Stem_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

Anova_stem<- aov (Stem_wp_bar~treatment, diurnals_2019_swp_vs_time)
summary (Anova_stem)

(test<- HSD.test(Anova_stem, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))
summary(test)

diurnals_2019_swp_vs_time_hist<-diurnals_2019_swp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_swp_vs_time_hist$Stem_wp_bar)

diurnals_2019_swp_vs_time_hist<-diurnals_2019_swp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_swp_vs_time_hist$Stem_wp_bar)

diurnals_2019_swp_vs_time_hist<-diurnals_2019_swp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_swp_vs_time_hist$Stem_wp_bar)

#### Jul 25 ####

diurnals_2019_swp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Stem_wp_bar))%>%
  filter(day == "206")

str(diurnals_2019_swp_vs_time)

diurnals_2019_swp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_swp_vs_time$treatment)

diurnals_2019_swp_vs_time$treatment<- format(diurnals_2019_swp_vs_time$treatment)
diurnals_2019_swp_vs_time$treatment<- as.factor(diurnals_2019_swp_vs_time$treatment)

is.factor(diurnals_2019_swp_vs_time$treatment)

ggplot(diurnals_2019_swp_vs_time, aes (treatment,Stem_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

Anova_stem<- aov (Stem_wp_bar~treatment, diurnals_2019_swp_vs_time)
summary (Anova_stem)

(test<- HSD.test(Anova_stem, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))
summary(test)

diurnals_2019_swp_vs_time_hist<-diurnals_2019_swp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_swp_vs_time_hist$Stem_wp_bar)

diurnals_2019_swp_vs_time_hist<-diurnals_2019_swp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_swp_vs_time_hist$Stem_wp_bar)

diurnals_2019_swp_vs_time_hist<-diurnals_2019_swp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_swp_vs_time_hist$Stem_wp_bar)

#### Jul 28 ####

diurnals_2019_swp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Stem_wp_bar))%>%
  filter(day == "209")

str(diurnals_2019_swp_vs_time)

diurnals_2019_swp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_swp_vs_time$treatment)

diurnals_2019_swp_vs_time$treatment<- format(diurnals_2019_swp_vs_time$treatment)
diurnals_2019_swp_vs_time$treatment<- as.factor(diurnals_2019_swp_vs_time$treatment)

is.factor(diurnals_2019_swp_vs_time$treatment)

ggplot(diurnals_2019_swp_vs_time, aes (treatment,Stem_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

Anova_stem<- aov (Stem_wp_bar~treatment, diurnals_2019_swp_vs_time)
summary (Anova_stem)

(test<- HSD.test(Anova_stem, trt = "treatment", alpha =0.05))
summary(test)

diurnals_2019_swp_vs_time_hist<-diurnals_2019_swp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_swp_vs_time_hist$Stem_wp_bar)

diurnals_2019_swp_vs_time_hist<-diurnals_2019_swp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_swp_vs_time_hist$Stem_wp_bar)

diurnals_2019_swp_vs_time_hist<-diurnals_2019_swp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_swp_vs_time_hist$Stem_wp_bar)


##### Aug 1 ####

diurnals_2019_swp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Stem_wp_bar))%>%
  filter(day == "213")

str(diurnals_2019_swp_vs_time)

diurnals_2019_swp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_swp_vs_time$treatment)

diurnals_2019_swp_vs_time$treatment<- format(diurnals_2019_swp_vs_time$treatment)
diurnals_2019_swp_vs_time$treatment<- as.factor(diurnals_2019_swp_vs_time$treatment)

is.factor(diurnals_2019_swp_vs_time$treatment)

ggplot(diurnals_2019_swp_vs_time, aes (treatment,Stem_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

Anova_stem<- aov (Stem_wp_bar~treatment, diurnals_2019_swp_vs_time)
summary (Anova_stem)

(test<- HSD.test(Anova_stem, trt = "treatment", alpha =0.05))
summary(test)

diurnals_2019_swp_vs_time_hist<-diurnals_2019_swp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_swp_vs_time_hist$Stem_wp_bar)

diurnals_2019_swp_vs_time_hist<-diurnals_2019_swp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_swp_vs_time_hist$Stem_wp_bar)

diurnals_2019_swp_vs_time_hist<-diurnals_2019_swp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_swp_vs_time_hist$Stem_wp_bar)

#### Aug 15 ####

diurnals_2019_swp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Stem_wp_bar))%>%
  filter(day == "227")

str(diurnals_2019_swp_vs_time)

diurnals_2019_swp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_swp_vs_time$treatment)

diurnals_2019_swp_vs_time$treatment<- format(diurnals_2019_swp_vs_time$treatment)
diurnals_2019_swp_vs_time$treatment<- as.factor(diurnals_2019_swp_vs_time$treatment)

is.factor(diurnals_2019_swp_vs_time$treatment)

ggplot(diurnals_2019_swp_vs_time, aes (treatment,Stem_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

Anova_stem<- aov (Stem_wp_bar~treatment, diurnals_2019_swp_vs_time)
summary (Anova_stem)

(test<- HSD.test(Anova_stem, trt = "treatment", alpha =0.05))
summary(test)

diurnals_2019_swp_vs_time_hist<-diurnals_2019_swp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_swp_vs_time_hist$Stem_wp_bar)

diurnals_2019_swp_vs_time_hist<-diurnals_2019_swp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_swp_vs_time_hist$Stem_wp_bar)

diurnals_2019_swp_vs_time_hist<-diurnals_2019_swp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_swp_vs_time_hist$Stem_wp_bar)


#### Aug 20 ####

diurnals_2019_swp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Stem_wp_bar))%>%
  filter(day == "232")

str(diurnals_2019_swp_vs_time)

diurnals_2019_swp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_swp_vs_time$treatment)

diurnals_2019_swp_vs_time$treatment<- format(diurnals_2019_swp_vs_time$treatment)
diurnals_2019_swp_vs_time$treatment<- as.factor(diurnals_2019_swp_vs_time$treatment)

is.factor(diurnals_2019_swp_vs_time$treatment)

ggplot(diurnals_2019_swp_vs_time, aes (treatment,Stem_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

Anova_stem<- aov (Stem_wp_bar~treatment, diurnals_2019_swp_vs_time)
summary (Anova_stem)

(test<- HSD.test(Anova_stem, trt = "treatment", alpha =0.05))
summary(test)

diurnals_2019_swp_vs_time_hist<-diurnals_2019_swp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_swp_vs_time_hist$Stem_wp_bar)

diurnals_2019_swp_vs_time_hist<-diurnals_2019_swp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_swp_vs_time_hist$Stem_wp_bar)

diurnals_2019_swp_vs_time_hist<-diurnals_2019_swp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_swp_vs_time_hist$Stem_wp_bar)

#### Sept 5 ####

diurnals_2019_swp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Stem_wp_bar))%>%
  filter(day == "248")

str(diurnals_2019_swp_vs_time)

diurnals_2019_swp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_swp_vs_time$treatment)

diurnals_2019_swp_vs_time$treatment<- format(diurnals_2019_swp_vs_time$treatment)
diurnals_2019_swp_vs_time$treatment<- as.factor(diurnals_2019_swp_vs_time$treatment)

is.factor(diurnals_2019_swp_vs_time$treatment)

ggplot(diurnals_2019_swp_vs_time, aes (treatment,Stem_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

Anova_stem<- aov (Stem_wp_bar~treatment, diurnals_2019_swp_vs_time)
summary (Anova_stem)

(test<- HSD.test(Anova_stem, trt = "treatment", alpha =0.05))
summary(test)

diurnals_2019_swp_vs_time_hist<-diurnals_2019_swp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_swp_vs_time_hist$Stem_wp_bar)

diurnals_2019_swp_vs_time_hist<-diurnals_2019_swp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_swp_vs_time_hist$Stem_wp_bar)

diurnals_2019_swp_vs_time_hist<-diurnals_2019_swp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_swp_vs_time_hist$Stem_wp_bar)

#### one way ANOVA for lwp each round  ####

#### Jul1 Round 1 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="1")%>%
  filter(day == "182")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

#### Jul1 Round 2  ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="2")%>%
  filter(day == "182")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

#### Jul 1 Round 3 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="3")%>%
  filter(day == "182")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)


#### Jul 1 Round 4 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="4")%>%
  filter(day == "182")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

#### Jul 1 Round 5 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="5")%>%
  filter(day == "182")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

####Jul 12 Round 1 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="1")%>%
  filter(day == "193")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

####Jul 12 Round 2####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="2")%>%
  filter(day == "193")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

#### Jul 12 Round 3 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="3")%>%
  filter(day == "193")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

#### Jul 12 Round 4 ####


diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="4")%>%
  filter(day == "193")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

#### jUL 12 round 5 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="5")%>%
  filter(day == "193")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

#### jUL 12 round 6 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="6")%>%
  filter(day == "193")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

####Jul 25 Round 1 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="1")%>%
  filter(day == "206")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)
#####Jul 25 round 2 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="2")%>%
  filter(day == "206")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

#### Jul 25 round 3 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="3")%>%
  filter(day == "206")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

####Jul 25 round 4 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="4")%>%
  filter(day == "206")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

####Jul 25 round 5 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="5")%>%
  filter(day == "206")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05))


diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

####Jul 28 round 1 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="1")%>%
  filter(day == "209")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

####Jul 28 round 2 ####


diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="2")%>%
  filter(day == "209")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)


####Jul 28 round 3 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="3")%>%
  filter(day == "209")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

#### Jul 28 round 4 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="4")%>%
  filter(day == "209")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

#### Jul 28 round 5 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="5")%>%
  filter(day == "209")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

#### Aug 1 round 1 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="1")%>%
  filter(day == "213")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

#### Aug 1 round 2 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="2")%>%
  filter(day == "213")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

#### Aug 1 round 3 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="3")%>%
  filter(day == "213")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

#### Aug 1 round 4 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="4")%>%
  filter(day == "213")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

#### Aug 1 round 5 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="5")%>%
  filter(day == "213")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

#### Aug 15 round 1 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="1")%>%
  filter(day == "227")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

#### Aug 15 round 2 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="2")%>%
  filter(day == "227")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

#### Aug 15 round 3 #####


diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="3")%>%
  filter(day == "227")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

#### Aug 15 round 4 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="4")%>%
  filter(day == "227")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

#### Aug 15 round 5 ####


diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="5")%>%
  filter(day == "227")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05))

# Didn't get differences with tukey with 0.05 but yes with 0.058 and ANOVA signifincant with 0.05

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.058))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

#### Aug 20 round 1 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="1")%>%
  filter(day == "232")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

#### Aug 20 round 2####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="2")%>%
  filter(day == "232")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

#### Aug 20 round 3 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="3")%>%
  filter(day == "232")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

####Aug 20 round 4 ####
diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="4")%>%
  filter(day == "232")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

#### Aug 20 round 5 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="5")%>%
  filter(day == "232")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)


#### Sep 5 round 1 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="1")%>%
  filter(day == "248")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

#### sep 5 round 2 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="2")%>%
  filter(day == "248")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

####Sep 5 round 3 ####
diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="3")%>%
  filter(day == "248")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

#### Sep 5 round 4####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="4")%>%
  filter(day == "248")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

#### Sep 5 round 5 ####

diurnals_2019_lwp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(Leaf_wp_bar))%>%
  filter(round =="5")%>%
  filter(day == "248")

diurnals_2019_lwp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_lwp_vs_time$treatment)

diurnals_2019_lwp_vs_time$treatment<- format(diurnals_2019_lwp_vs_time$treatment)
diurnals_2019_lwp_vs_time$treatment<- as.factor(diurnals_2019_lwp_vs_time$treatment)

is.factor(diurnals_2019_lwp_vs_time$treatment)


str(diurnals_2019_lwp_vs_time)

ggplot(diurnals_2019_lwp_vs_time, aes (treatment,Leaf_wp_bar, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_lwp_bar<- aov (Leaf_wp_bar~treatment, diurnals_2019_lwp_vs_time)
summary (anova_lwp_bar)

(test<- HSD.test(anova_lwp_bar , trt = "treatment", alpha =0.05))

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

diurnals_2019_lwp_vs_time_hist<-diurnals_2019_lwp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_lwp_vs_time_hist$Leaf_wp_bar)

####ANOVA A vs time####

#### Jul1 Round 1 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="1")%>%
  filter(day == "182")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

#### Jul1 Round 2  ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="2")%>%
  filter(day == "182")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

#### Jul 1 Round 3 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="3")%>%
  filter(day == "182")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)


#### Jul 1 Round 4 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="4")%>%
  filter(day == "182")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

#### Jul 1 Round 5 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="5")%>%
  filter(day == "182")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

####Jul 12 Round 1 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="1")%>%
  filter(day == "193")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

####Jul 12 Round 2####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="2")%>%
  filter(day == "193")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

#### Jul 12 Round 3 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="3")%>%
  filter(day == "193")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

#### Jul 12 Round 4 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="4")%>%
  filter(day == "193")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

#### jUL 12 round 5 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="5")%>%
  filter(day == "193")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

#### jUL 12 round 6 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="6")%>%
  filter(day == "193")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

####Jul 25 Round 1 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="1")%>%
  filter(day == "206")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

#####Jul 25 round 2 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="2")%>%
  filter(day == "206")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

#### Jul 25 round 3 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="3")%>%
  filter(day == "206")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

####Jul 25 round 4 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="4")%>%
  filter(day == "206")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

####Jul 25 round 5 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="5")%>%
  filter(day == "206")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

### ANOVA signifificant with alpha 0.05 tukeys NO with aplha 0.05 but yes with alpha 0.074

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.074))


### Fisher also significant differences with alpha 0.05 

(test<- LSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

####Jul 28 round 1 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="1")%>%
  filter(day == "209")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

####Jul 28 round 2 ####


diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="2")%>%
  filter(day == "209")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)


####Jul 28 round 3 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="3")%>%
  filter(day == "209")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

#### Jul 28 round 4 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="4")%>%
  filter(day == "209")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

#### Jul 28 round 5 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="5")%>%
  filter(day == "209")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

#### Aug 1 round 1 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="1")%>%
  filter(day == "213")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

#### Aug 1 round 2 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="2")%>%
  filter(day == "213")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

#### Aug 1 round 3 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="3")%>%
  filter(day == "213")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

#### Aug 1 round 4 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="4")%>%
  filter(day == "213")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

#### Aug 1 round 5 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="5")%>%
  filter(day == "213")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

#### Aug 15 round 1 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="1")%>%
  filter(day == "227")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

#### Aug 15 round 2 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="2")%>%
  filter(day == "227")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

#### Aug 15 round 3 #####


diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="3")%>%
  filter(day == "227")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

#### Aug 15 round 4 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="4")%>%
  filter(day == "227")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

#### Aug 15 round 5 ####


diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="5")%>%
  filter(day == "227")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

#### NO differences with tukey 0.05 but yes with 0.062. ANOVA significant with alpha 0.05 

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.062))


diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

#### Aug 20 round 1 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="1")%>%
  filter(day == "232")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

#### Aug 20 round 2####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="2")%>%
  filter(day == "232")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

#### Aug 20 round 3 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="3")%>%
  filter(day == "232")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

####Aug 20 round 4 ####
diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="4")%>%
  filter(day == "232")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

#### Aug 20 round 5 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="5")%>%
  filter(day == "232")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

#### Sep 5 round 1 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="1")%>%
  filter(day == "248")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

#### sep 5 round 2 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="2")%>%
  filter(day == "248")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

####Sep 5 round 3 ####
diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="3")%>%
  filter(day == "248")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

#### Sep 5 round 4####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="4")%>%
  filter(day == "248")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

#### Sep 5 round 5 ####

diurnals_2019_A_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(A))%>%
  filter(round =="5")%>%
  filter(day == "248")

diurnals_2019_A_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_A_vs_time$treatment)

diurnals_2019_A_vs_time$treatment<- format(diurnals_2019_A_vs_time$treatment)
diurnals_2019_A_vs_time$treatment<- as.factor(diurnals_2019_A_vs_time$treatment)

is.factor(diurnals_2019_A_vs_time$treatment)


str(diurnals_2019_A_vs_time)

ggplot(diurnals_2019_A_vs_time, aes (treatment,A, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (A~treatment, diurnals_2019_A_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_A_vs_time_hist$A)

diurnals_2019_A_vs_time_hist<-diurnals_2019_A_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_A_vs_time_hist$A)

####ANOVA gsw vs time####

####Jul 1 round 1####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="1")%>%
  filter(day == "182") %>%
  filter(gsw < 1.0)

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

#### Jul1 Round 2  ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="2")%>%
  filter(day == "182") 

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

#### Jul 1 Round 3 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="3")%>%
  filter(day == "182")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)


#### Jul 1 Round 4 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="4")%>%
  filter(day == "182")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

#### Jul 1 Round 5 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="5")%>%
  filter(day == "182")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

####Jul 12 Round 1 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="1")%>%
  filter(day == "193") %>%
  filter(gsw < 1.0)

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

####Jul 12 Round 2####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="2")%>%
  filter(day == "193")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

#### Jul 12 Round 3 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="3")%>%
  filter(day == "193")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

#### Jul 12 Round 4 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="4")%>%
  filter(day == "193")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

#### jUL 12 round 5 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="5")%>%
  filter(day == "193")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

#### jUL 12 round 6 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="6")%>%
  filter(day == "193")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

####Jul 25 Round 1 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="1")%>%
  filter(day == "206")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

#####Jul 25 round 2 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="2")%>%
  filter(day == "206")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

#### Jul 25 round 3 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="3")%>%
  filter(day == "206")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

####Jul 25 round 4 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="4")%>%
  filter(day == "206")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

####Jul 25 round 5 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="5")%>%
  filter(day == "206")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

####Jul 28 round 1 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="1")%>%
  filter(day == "209")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

####Jul 28 round 2 ####


diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="2")%>%
  filter(day == "209")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)


####Jul 28 round 3 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="3")%>%
  filter(day == "209")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

#### Jul 28 round 4 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="4")%>%
  filter(day == "209")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

#### Jul 28 round 5 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="5")%>%
  filter(day == "209")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

#### Aug 1 round 1 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="1")%>%
  filter(day == "213") %>%
  filter(gsw < 2.0)

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

#### Aug 1 round 2 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="2")%>%
  filter(day == "213")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

#### Aug 1 round 3 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="3")%>%
  filter(day == "213")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

#### Aug 1 round 4 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="4")%>%
  filter(day == "213")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

#### Aug 1 round 5 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="5")%>%
  filter(day == "213")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

#### Aug 15 round 1 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="1")%>%
  filter(day == "227")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

#### Aug 15 round 2 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="2")%>%
  filter(day == "227")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

#### Aug 15 round 3 #####


diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="3")%>%
  filter(day == "227")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

#### Aug 15 round 4 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="4")%>%
  filter(day == "227")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

#### Aug 15 round 5 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="5")%>%
  filter(day == "227")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

#### Aug 20 round 1 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="1")%>%
  filter(day == "232")%>%
  filter(gsw < 0.9)

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

#### Aug 20 round 2####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="2")%>%
  filter(day == "232")  %>%
  filter(gsw < 0.9)

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

#### Aug 20 round 3 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="3")%>%
  filter(day == "232")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

####Aug 20 round 4 ####
diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="4")%>%
  filter(day == "232")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

#### Aug 20 round 5 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="5")%>%
  filter(day == "232")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

#### Sep 5 round 1 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="1")%>%
  filter(day == "248")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

#### sep 5 round 2 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="2")%>%
  filter(day == "248")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

####Sep 5 round 3 ####
diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="3")%>%
  filter(day == "248")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

#### Sep 5 round 4####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="4")%>%
  filter(day == "248")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

#### Sep 5 round 5 ####

diurnals_2019_gsw_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(gsw))%>%
  filter(round =="5")%>%
  filter(day == "248")

diurnals_2019_gsw_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_gsw_vs_time$treatment)

diurnals_2019_gsw_vs_time$treatment<- format(diurnals_2019_gsw_vs_time$treatment)
diurnals_2019_gsw_vs_time$treatment<- as.factor(diurnals_2019_gsw_vs_time$treatment)

is.factor(diurnals_2019_gsw_vs_time$treatment)


str(diurnals_2019_gsw_vs_time)

ggplot(diurnals_2019_gsw_vs_time, aes (treatment,gsw, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_A_bar<- aov (gsw~treatment, diurnals_2019_gsw_vs_time)
summary (anova_A_bar)

(test<- HSD.test(anova_A_bar , trt = "treatment", alpha =0.05))

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_gsw_vs_time_hist$gsw)

diurnals_2019_gsw_vs_time_hist<-diurnals_2019_gsw_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_gsw_vs_time_hist$gsw)


#### Temperature ANOVA vs time ####


#####Jul 25 round 2 ####

diurnals_2019_temp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(leaf_temp_C))%>%
  filter(round =="2")%>%
  filter(day == "206")%>%
  filter(!leaf_temp_C =="-")

diurnals_2019_temp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$treatment<- format(diurnals_2019_temp_vs_time$treatment)
diurnals_2019_temp_vs_time$treatment<- as.factor(diurnals_2019_temp_vs_time$treatment)

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$leaf_temp_C<-format(diurnals_2019_temp_vs_time$leaf_temp_C)
diurnals_2019_temp_vs_time$leaf_temp_C<- as.numeric(diurnals_2019_temp_vs_time$leaf_temp_C)

str(diurnals_2019_temp_vs_time)

ggplot(diurnals_2019_temp_vs_time, aes (treatment,leaf_temp_C, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_temp<- aov (leaf_temp_C~treatment, diurnals_2019_temp_vs_time)
summary (anova_temp)

(test<- HSD.test(anova_temp , trt = "treatment", alpha =0.05))

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

#### Jul 25 round 3 ####

diurnals_2019_temp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(leaf_temp_C))%>%
  filter(round =="3")%>%
  filter(day == "206")

diurnals_2019_temp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$treatment<- format(diurnals_2019_temp_vs_time$treatment)
diurnals_2019_temp_vs_time$treatment<- as.factor(diurnals_2019_temp_vs_time$treatment)

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$leaf_temp_C<-format(diurnals_2019_temp_vs_time$leaf_temp_C)
diurnals_2019_temp_vs_time$leaf_temp_C<- as.numeric(diurnals_2019_temp_vs_time$leaf_temp_C)
str(diurnals_2019_temp_vs_time)

ggplot(diurnals_2019_temp_vs_time, aes (treatment,leaf_temp_C, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_temp<- aov (leaf_temp_C~treatment, diurnals_2019_temp_vs_time)
summary (anova_temp)

(test<- HSD.test(anova_temp , trt = "treatment", alpha =0.05))

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

####Jul 25 round 4 ####

diurnals_2019_temp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(leaf_temp_C))%>%
  filter(round =="4")%>%
  filter(day == "206")

diurnals_2019_temp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$treatment<- format(diurnals_2019_temp_vs_time$treatment)
diurnals_2019_temp_vs_time$treatment<- as.factor(diurnals_2019_temp_vs_time$treatment)

is.factor(diurnals_2019_temp_vs_time$treatment)
diurnals_2019_temp_vs_time$leaf_temp_C<-format(diurnals_2019_temp_vs_time$leaf_temp_C)
diurnals_2019_temp_vs_time$leaf_temp_C<- as.numeric(diurnals_2019_temp_vs_time$leaf_temp_C)

str(diurnals_2019_temp_vs_time)

ggplot(diurnals_2019_temp_vs_time, aes (treatment,leaf_temp_C, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_temp<- aov (leaf_temp_C~treatment, diurnals_2019_temp_vs_time)
summary (anova_temp)

(test<- HSD.test(anova_temp , trt = "treatment", alpha =0.05))

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

####Jul 25 round 5 ####

diurnals_2019_temp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(leaf_temp_C))%>%
  filter(round =="5")%>%
  filter(day == "206")

diurnals_2019_temp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$treatment<- format(diurnals_2019_temp_vs_time$treatment)
diurnals_2019_temp_vs_time$treatment<- as.factor(diurnals_2019_temp_vs_time$treatment)

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$leaf_temp_C<-format(diurnals_2019_temp_vs_time$leaf_temp_C)
diurnals_2019_temp_vs_time$leaf_temp_C<- as.numeric(diurnals_2019_temp_vs_time$leaf_temp_C)
str(diurnals_2019_temp_vs_time)

ggplot(diurnals_2019_temp_vs_time, aes (treatment,leaf_temp_C, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_temp<- aov (leaf_temp_C~treatment, diurnals_2019_temp_vs_time)
summary (anova_temp)

(test<- HSD.test(anova_temp , trt = "treatment", alpha =0.05))

####Not significant with ANOVA alpha 0.05. Differences with tukey alpha 0.05 

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

####Jul 28 round 2 ####


diurnals_2019_temp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(leaf_temp_C))%>%
  filter(round =="2")%>%
  filter(day == "209")

diurnals_2019_temp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$treatment<- format(diurnals_2019_temp_vs_time$treatment)
diurnals_2019_temp_vs_time$treatment<- as.factor(diurnals_2019_temp_vs_time$treatment)

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$leaf_temp_C<-format(diurnals_2019_temp_vs_time$leaf_temp_C)
diurnals_2019_temp_vs_time$leaf_temp_C<- as.numeric(diurnals_2019_temp_vs_time$leaf_temp_C)
str(diurnals_2019_temp_vs_time)

str(diurnals_2019_temp_vs_time)

ggplot(diurnals_2019_temp_vs_time, aes (treatment,leaf_temp_C, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_temp<- aov (leaf_temp_C~treatment, diurnals_2019_temp_vs_time)
summary (anova_temp)

(test<- HSD.test(anova_temp , trt = "treatment", alpha =0.05))

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)


####Jul 28 round 3 ####

diurnals_2019_temp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(leaf_temp_C))%>%
  filter(round =="3")%>%
  filter(day == "209")

diurnals_2019_temp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$treatment<- format(diurnals_2019_temp_vs_time$treatment)
diurnals_2019_temp_vs_time$treatment<- as.factor(diurnals_2019_temp_vs_time$treatment)

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$leaf_temp_C<-format(diurnals_2019_temp_vs_time$leaf_temp_C)
diurnals_2019_temp_vs_time$leaf_temp_C<- as.numeric(diurnals_2019_temp_vs_time$leaf_temp_C)
str(diurnals_2019_temp_vs_time)

str(diurnals_2019_temp_vs_time)

ggplot(diurnals_2019_temp_vs_time, aes (treatment,leaf_temp_C, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_temp<- aov (leaf_temp_C~treatment, diurnals_2019_temp_vs_time)
summary (anova_temp)

(test<- HSD.test(anova_temp , trt = "treatment", alpha =0.05))

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

#### Jul 28 round 4 ####

diurnals_2019_temp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(leaf_temp_C))%>%
  filter(round =="4")%>%
  filter(day == "209")

diurnals_2019_temp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$treatment<- format(diurnals_2019_temp_vs_time$treatment)
diurnals_2019_temp_vs_time$treatment<- as.factor(diurnals_2019_temp_vs_time$treatment)

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$leaf_temp_C<-format(diurnals_2019_temp_vs_time$leaf_temp_C)
diurnals_2019_temp_vs_time$leaf_temp_C<- as.numeric(diurnals_2019_temp_vs_time$leaf_temp_C)
str(diurnals_2019_temp_vs_time)

str(diurnals_2019_temp_vs_time)

ggplot(diurnals_2019_temp_vs_time, aes (treatment,leaf_temp_C, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_temp<- aov (leaf_temp_C~treatment, diurnals_2019_temp_vs_time)
summary (anova_temp)

(test<- HSD.test(anova_temp , trt = "treatment", alpha =0.05))

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

#### Jul 28 round 5 ####

diurnals_2019_temp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(leaf_temp_C))%>%
  filter(round =="5")%>%
  filter(day == "209")

diurnals_2019_temp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$treatment<- format(diurnals_2019_temp_vs_time$treatment)
diurnals_2019_temp_vs_time$treatment<- as.factor(diurnals_2019_temp_vs_time$treatment)

is.factor(diurnals_2019_temp_vs_time$treatment)
diurnals_2019_temp_vs_time$leaf_temp_C<-format(diurnals_2019_temp_vs_time$leaf_temp_C)
diurnals_2019_temp_vs_time$leaf_temp_C<- as.numeric(diurnals_2019_temp_vs_time$leaf_temp_C)
str(diurnals_2019_temp_vs_time)


ggplot(diurnals_2019_temp_vs_time, aes (treatment,leaf_temp_C, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_temp<- aov (leaf_temp_C~treatment, diurnals_2019_temp_vs_time)
summary (anova_temp)

(test<- HSD.test(anova_temp , trt = "treatment", alpha =0.05))

#### No differences with tukey alpha 0.05, yes with alpha 0.066. ANOVA significant with alpha 0.05

(test<- HSD.test(anova_temp , trt = "treatment", alpha =0.066))

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)



#### Aug 1 round 2 ####

diurnals_2019_temp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(leaf_temp_C))%>%
  filter(round =="2")%>%
  filter(day == "213") %>%
  filter(!leaf_temp_C =="-")

diurnals_2019_temp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$treatment<- format(diurnals_2019_temp_vs_time$treatment)
diurnals_2019_temp_vs_time$treatment<- as.factor(diurnals_2019_temp_vs_time$treatment)

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$leaf_temp_C<-format(diurnals_2019_temp_vs_time$leaf_temp_C)
diurnals_2019_temp_vs_time$leaf_temp_C<- as.numeric(diurnals_2019_temp_vs_time$leaf_temp_C)

str(diurnals_2019_temp_vs_time)

ggplot(diurnals_2019_temp_vs_time, aes (treatment,leaf_temp_C, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_temp<- aov (leaf_temp_C~treatment, diurnals_2019_temp_vs_time)
summary (anova_temp)

(test<- HSD.test(anova_temp , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

#### Aug 1 round 3 ####

diurnals_2019_temp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(leaf_temp_C))%>%
  filter(round =="3")%>%
  filter(day == "213")

diurnals_2019_temp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$treatment<- format(diurnals_2019_temp_vs_time$treatment)
diurnals_2019_temp_vs_time$treatment<- as.factor(diurnals_2019_temp_vs_time$treatment)

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$leaf_temp_C<-format(diurnals_2019_temp_vs_time$leaf_temp_C)
diurnals_2019_temp_vs_time$leaf_temp_C<- as.numeric(diurnals_2019_temp_vs_time$leaf_temp_C)

str(diurnals_2019_temp_vs_time)

ggplot(diurnals_2019_temp_vs_time, aes (treatment,leaf_temp_C, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_temp<- aov (leaf_temp_C~treatment, diurnals_2019_temp_vs_time)
summary (anova_temp)

(test<- HSD.test(anova_temp , trt = "treatment", alpha =0.05))

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

#### Aug 1 round 4 ####

diurnals_2019_temp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(leaf_temp_C))%>%
  filter(round =="4")%>%
  filter(day == "213")

diurnals_2019_temp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$treatment<- format(diurnals_2019_temp_vs_time$treatment)
diurnals_2019_temp_vs_time$treatment<- as.factor(diurnals_2019_temp_vs_time$treatment)

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$leaf_temp_C<-format(diurnals_2019_temp_vs_time$leaf_temp_C)
diurnals_2019_temp_vs_time$leaf_temp_C<- as.numeric(diurnals_2019_temp_vs_time$leaf_temp_C)

str(diurnals_2019_temp_vs_time)

ggplot(diurnals_2019_temp_vs_time, aes (treatment,leaf_temp_C, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_temp<- aov (leaf_temp_C~treatment, diurnals_2019_temp_vs_time)
summary (anova_temp)

(test<- HSD.test(anova_temp , trt = "treatment", alpha =0.05))

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

#### Aug 1 round 5 ####

diurnals_2019_temp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(leaf_temp_C))%>%
  filter(round =="5")%>%
  filter(day == "213")

diurnals_2019_temp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$treatment<- format(diurnals_2019_temp_vs_time$treatment)
diurnals_2019_temp_vs_time$treatment<- as.factor(diurnals_2019_temp_vs_time$treatment)

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$leaf_temp_C<-format(diurnals_2019_temp_vs_time$leaf_temp_C)
diurnals_2019_temp_vs_time$leaf_temp_C<- as.numeric(diurnals_2019_temp_vs_time$leaf_temp_C)

str(diurnals_2019_temp_vs_time)

ggplot(diurnals_2019_temp_vs_time, aes (treatment,leaf_temp_C, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_temp<- aov (leaf_temp_C~treatment, diurnals_2019_temp_vs_time)
summary (anova_temp)

(test<- HSD.test(anova_temp , trt = "treatment", alpha =0.05))

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)


#### Aug 15 round 2 ####

diurnals_2019_temp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(leaf_temp_C))%>%
  filter(round =="2")%>%
  filter(day == "227")

diurnals_2019_temp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$treatment<- format(diurnals_2019_temp_vs_time$treatment)
diurnals_2019_temp_vs_time$treatment<- as.factor(diurnals_2019_temp_vs_time$treatment)

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$leaf_temp_C<-format(diurnals_2019_temp_vs_time$leaf_temp_C)
diurnals_2019_temp_vs_time$leaf_temp_C<- as.numeric(diurnals_2019_temp_vs_time$leaf_temp_C)


str(diurnals_2019_temp_vs_time)

ggplot(diurnals_2019_temp_vs_time, aes (treatment,leaf_temp_C, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_temp<- aov (leaf_temp_C~treatment, diurnals_2019_temp_vs_time)
summary (anova_temp)

(test<- HSD.test(anova_temp , trt = "treatment", alpha =0.05))

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

#### Aug 15 round 3 #####


diurnals_2019_temp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(leaf_temp_C))%>%
  filter(round =="3")%>%
  filter(day == "227")

diurnals_2019_temp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$treatment<- format(diurnals_2019_temp_vs_time$treatment)
diurnals_2019_temp_vs_time$treatment<- as.factor(diurnals_2019_temp_vs_time$treatment)

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$leaf_temp_C<-format(diurnals_2019_temp_vs_time$leaf_temp_C)
diurnals_2019_temp_vs_time$leaf_temp_C<- as.numeric(diurnals_2019_temp_vs_time$leaf_temp_C)

str(diurnals_2019_temp_vs_time)

ggplot(diurnals_2019_temp_vs_time, aes (treatment,leaf_temp_C, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_temp<- aov (leaf_temp_C~treatment, diurnals_2019_temp_vs_time)
summary (anova_temp)

(test<- HSD.test(anova_temp , trt = "treatment", alpha =0.05))

#### 0.05 ALPHA TUKEY no differences, YES WITH ALPHA 0.055. ANOVA significant with alpha 0.05 

(test<- HSD.test(anova_temp , trt = "treatment", alpha =0.055))

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

#### Aug 15 round 4 ####

diurnals_2019_temp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(leaf_temp_C))%>%
  filter(round =="4")%>%
  filter(day == "227")

diurnals_2019_temp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$treatment<- format(diurnals_2019_temp_vs_time$treatment)
diurnals_2019_temp_vs_time$treatment<- as.factor(diurnals_2019_temp_vs_time$treatment)

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$leaf_temp_C<-format(diurnals_2019_temp_vs_time$leaf_temp_C)
diurnals_2019_temp_vs_time$leaf_temp_C<- as.numeric(diurnals_2019_temp_vs_time$leaf_temp_C)

str(diurnals_2019_temp_vs_time)

ggplot(diurnals_2019_temp_vs_time, aes (treatment,leaf_temp_C, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_temp<- aov (leaf_temp_C~treatment, diurnals_2019_temp_vs_time)
summary (anova_temp)

(test<- HSD.test(anova_temp , trt = "treatment", alpha =0.05))

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

#### Aug 15 round 5 ####


diurnals_2019_temp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(leaf_temp_C))%>%
  filter(round =="5")%>%
  filter(day == "227")

diurnals_2019_temp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$treatment<- format(diurnals_2019_temp_vs_time$treatment)
diurnals_2019_temp_vs_time$treatment<- as.factor(diurnals_2019_temp_vs_time$treatment)

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$leaf_temp_C<-format(diurnals_2019_temp_vs_time$leaf_temp_C)
diurnals_2019_temp_vs_time$leaf_temp_C<- as.numeric(diurnals_2019_temp_vs_time$leaf_temp_C)

str(diurnals_2019_temp_vs_time)

ggplot(diurnals_2019_temp_vs_time, aes (treatment,leaf_temp_C, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_temp<- aov (leaf_temp_C~treatment, diurnals_2019_temp_vs_time)
summary (anova_temp)

(test<- HSD.test(anova_temp , trt = "treatment", alpha =0.05))

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)


diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)


#### Aug 20 round 2####

diurnals_2019_temp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(leaf_temp_C))%>%
  filter(round =="2")%>%
  filter(day == "232")

diurnals_2019_temp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$treatment<- format(diurnals_2019_temp_vs_time$treatment)
diurnals_2019_temp_vs_time$treatment<- as.factor(diurnals_2019_temp_vs_time$treatment)

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$leaf_temp_C<-format(diurnals_2019_temp_vs_time$leaf_temp_C)
diurnals_2019_temp_vs_time$leaf_temp_C<- as.numeric(diurnals_2019_temp_vs_time$leaf_temp_C)

str(diurnals_2019_temp_vs_time)

ggplot(diurnals_2019_temp_vs_time, aes (treatment,leaf_temp_C, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_temp<- aov (leaf_temp_C~treatment, diurnals_2019_temp_vs_time)
summary (anova_temp)

(test<- HSD.test(anova_temp , trt = "treatment", alpha =0.05))

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

#### Aug 20 round 3 ####

diurnals_2019_temp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(leaf_temp_C))%>%
  filter(round =="3")%>%
  filter(day == "232")

diurnals_2019_temp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$treatment<- format(diurnals_2019_temp_vs_time$treatment)
diurnals_2019_temp_vs_time$treatment<- as.factor(diurnals_2019_temp_vs_time$treatment)

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$leaf_temp_C<-format(diurnals_2019_temp_vs_time$leaf_temp_C)
diurnals_2019_temp_vs_time$leaf_temp_C<- as.numeric(diurnals_2019_temp_vs_time$leaf_temp_C)

str(diurnals_2019_temp_vs_time)

ggplot(diurnals_2019_temp_vs_time, aes (treatment,leaf_temp_C, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_temp<- aov (leaf_temp_C~treatment, diurnals_2019_temp_vs_time)
summary (anova_temp)

(test<- HSD.test(anova_temp , trt = "treatment", alpha =0.05))

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

####Aug 20 round 4 ####
diurnals_2019_temp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(leaf_temp_C))%>%
  filter(round =="4")%>%
  filter(day == "232")

diurnals_2019_temp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$treatment<- format(diurnals_2019_temp_vs_time$treatment)
diurnals_2019_temp_vs_time$treatment<- as.factor(diurnals_2019_temp_vs_time$treatment)

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$leaf_temp_C<-format(diurnals_2019_temp_vs_time$leaf_temp_C)
diurnals_2019_temp_vs_time$leaf_temp_C<- as.numeric(diurnals_2019_temp_vs_time$leaf_temp_C)

str(diurnals_2019_temp_vs_time)

ggplot(diurnals_2019_temp_vs_time, aes (treatment,leaf_temp_C, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_temp<- aov (leaf_temp_C~treatment, diurnals_2019_temp_vs_time)
summary (anova_temp)

(test<- HSD.test(anova_temp , trt = "treatment", alpha =0.05))

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

#### Aug 20 round 5 ####

diurnals_2019_temp_vs_time <-diurnals_borden_hills_2019%>%
  filter(!is.na(leaf_temp_C))%>%
  filter(round =="5")%>%
  filter(day == "232")

diurnals_2019_temp_vs_time %>%
  group_by(treatment)%>%
  tally()

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$treatment<- format(diurnals_2019_temp_vs_time$treatment)
diurnals_2019_temp_vs_time$treatment<- as.factor(diurnals_2019_temp_vs_time$treatment)

is.factor(diurnals_2019_temp_vs_time$treatment)

diurnals_2019_temp_vs_time$leaf_temp_C<-format(diurnals_2019_temp_vs_time$leaf_temp_C)
diurnals_2019_temp_vs_time$leaf_temp_C<- as.numeric(diurnals_2019_temp_vs_time$leaf_temp_C)

str(diurnals_2019_temp_vs_time)

ggplot(diurnals_2019_temp_vs_time, aes (treatment,leaf_temp_C, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_temp<- aov (leaf_temp_C~treatment, diurnals_2019_temp_vs_time)
summary (anova_temp)

(test<- HSD.test(anova_temp , trt = "treatment", alpha =0.05))

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 1)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 2)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)

diurnals_2019_temp_vs_time_hist<-diurnals_2019_temp_vs_time %>%
  filter(treatment == 3)
hist(diurnals_2019_temp_vs_time_hist$leaf_temp_C)


###### ANOVA BERRY BASIC CHEMISTRY####

berry_chemistry_borden_hills_2019 <-read.csv("data/berry_chemistry_2019_3.csv", header = TRUE)

berry_chemistry_borden_hills_2019 <-berry_chemistry_borden_hills_2019%>%
  mutate(date = ï..date)

str(berry_chemistry_borden_hills_2019)

berry_chemistry_borden_hills_2019 <-berry_chemistry_borden_hills_2019%>%
  select(-ï..date) %>%
  filter(!pixel_number == (34)) 
  
berry_chemistry_borden_hills_2019$date<- mdy(berry_chemistry_borden_hills_2019$date)

str(berry_chemistry_borden_hills_2019$date)

tz(berry_chemistry_borden_hills_2019$date)

str(berry_chemistry_borden_hills_2019$date)

##### Brix Aug 02 ####

berry_chemistry_borden_hills_2019_brix <- berry_chemistry_borden_hills_2019%>%
  filter(date == "2019-08-02") %>%
  select(date, Brix, treatment) 

berry_chemistry_borden_hills_2019_brix %>%
  group_by(treatment)%>%
  tally()

is.factor(berry_chemistry_borden_hills_2019_brix$treatment)

berry_chemistry_borden_hills_2019_brix$treatment<- format(berry_chemistry_borden_hills_2019_brix$treatment)
berry_chemistry_borden_hills_2019_brix$treatment<- as.factor(berry_chemistry_borden_hills_2019_brix$treatment)

is.factor(berry_chemistry_borden_hills_2019_brix$treatment)

str(berry_chemistry_borden_hills_2019_brix)

ggplot(berry_chemistry_borden_hills_2019_brix, aes (treatment,Brix, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_brix <- aov (Brix~treatment, berry_chemistry_borden_hills_2019_brix )
summary (anova_brix)

(Tukey_brix<- HSD.test(anova_brix , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

berry_chemistry_borden_hills_2019_brix_hist<-berry_chemistry_borden_hills_2019_brix %>%
  filter(treatment == 1)
hist(berry_chemistry_borden_hills_2019_brix_hist$Brix)

berry_chemistry_borden_hills_2019_brix_hist<-berry_chemistry_borden_hills_2019_brix %>%
  filter(treatment == 2)
hist(berry_chemistry_borden_hills_2019_brix_hist$Brix)

berry_chemistry_borden_hills_2019_brix_hist<-berry_chemistry_borden_hills_2019_brix %>%
  filter(treatment == 3)
hist(berry_chemistry_borden_hills_2019_brix_hist$Brix)

#### Brix Aug 8 ####

berry_chemistry_borden_hills_2019_brix <- berry_chemistry_borden_hills_2019%>%
  filter(date == "2019-08-08") %>%
  select(date, Brix, treatment) 

berry_chemistry_borden_hills_2019_brix %>%
  group_by(treatment)%>%
  tally()

is.factor(berry_chemistry_borden_hills_2019_brix$treatment)

berry_chemistry_borden_hills_2019_brix$treatment<- format(berry_chemistry_borden_hills_2019_brix$treatment)
berry_chemistry_borden_hills_2019_brix$treatment<- as.factor(berry_chemistry_borden_hills_2019_brix$treatment)

is.factor(berry_chemistry_borden_hills_2019_brix$treatment)

str(berry_chemistry_borden_hills_2019_brix)

ggplot(berry_chemistry_borden_hills_2019_brix, aes (treatment,Brix, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_brix <- aov (Brix~treatment, berry_chemistry_borden_hills_2019_brix )
summary (anova_brix)

(Tukey_brix<- HSD.test(anova_brix , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))


berry_chemistry_borden_hills_2019_brix_hist<-berry_chemistry_borden_hills_2019_brix %>%
  filter(treatment == 1)
hist(berry_chemistry_borden_hills_2019_brix_hist$Brix)

berry_chemistry_borden_hills_2019_brix_hist<-berry_chemistry_borden_hills_2019_brix %>%
  filter(treatment == 2)
hist(berry_chemistry_borden_hills_2019_brix_hist$Brix)

berry_chemistry_borden_hills_2019_brix_hist<-berry_chemistry_borden_hills_2019_brix %>%
  filter(treatment == 3)
hist(berry_chemistry_borden_hills_2019_brix_hist$Brix)

#### Brix Aug 15  ####

berry_chemistry_borden_hills_2019_brix <- berry_chemistry_borden_hills_2019%>%
  filter(date == "2019-08-15") %>%
  select(date, Brix, treatment) 

berry_chemistry_borden_hills_2019_brix %>%
  group_by(treatment)%>%
  tally()

is.factor(berry_chemistry_borden_hills_2019_brix$treatment)

berry_chemistry_borden_hills_2019_brix$treatment<- format(berry_chemistry_borden_hills_2019_brix$treatment)
berry_chemistry_borden_hills_2019_brix$treatment<- as.factor(berry_chemistry_borden_hills_2019_brix$treatment)

is.factor(berry_chemistry_borden_hills_2019_brix$treatment)

str(berry_chemistry_borden_hills_2019_brix)

ggplot(berry_chemistry_borden_hills_2019_brix, aes (treatment,Brix, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_brix <- aov (Brix~treatment, berry_chemistry_borden_hills_2019_brix )
summary (anova_brix)

(Tukey_brix<- HSD.test(anova_brix , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))


berry_chemistry_borden_hills_2019_brix_hist<-berry_chemistry_borden_hills_2019_brix %>%
  filter(treatment == 1)
hist(berry_chemistry_borden_hills_2019_brix_hist$Brix)

berry_chemistry_borden_hills_2019_brix_hist<-berry_chemistry_borden_hills_2019_brix %>%
  filter(treatment == 2)
hist(berry_chemistry_borden_hills_2019_brix_hist$Brix)

berry_chemistry_borden_hills_2019_brix_hist<-berry_chemistry_borden_hills_2019_brix %>%
  filter(treatment == 3)
hist(berry_chemistry_borden_hills_2019_brix_hist$Brix)

####  Brix Aug 20 ####

berry_chemistry_borden_hills_2019_brix <- berry_chemistry_borden_hills_2019%>%
  filter(date == "2019-08-20") %>%
  select(date, Brix, treatment) 

berry_chemistry_borden_hills_2019_brix %>%
  group_by(treatment)%>%
  tally()

is.factor(berry_chemistry_borden_hills_2019_brix$treatment)

berry_chemistry_borden_hills_2019_brix$treatment<- format(berry_chemistry_borden_hills_2019_brix$treatment)
berry_chemistry_borden_hills_2019_brix$treatment<- as.factor(berry_chemistry_borden_hills_2019_brix$treatment)

is.factor(berry_chemistry_borden_hills_2019_brix$treatment)

str(berry_chemistry_borden_hills_2019_brix)

ggplot(berry_chemistry_borden_hills_2019_brix, aes (treatment,Brix, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_brix <- aov (Brix~treatment, berry_chemistry_borden_hills_2019_brix )
summary (anova_brix)

(Tukey_brix<- HSD.test(anova_brix , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))


berry_chemistry_borden_hills_2019_brix_hist<-berry_chemistry_borden_hills_2019_brix %>%
  filter(treatment == 1)
hist(berry_chemistry_borden_hills_2019_brix_hist$Brix)

berry_chemistry_borden_hills_2019_brix_hist<-berry_chemistry_borden_hills_2019_brix %>%
  filter(treatment == 2)
hist(berry_chemistry_borden_hills_2019_brix_hist$Brix)

berry_chemistry_borden_hills_2019_brix_hist<-berry_chemistry_borden_hills_2019_brix %>%
  filter(treatment == 3)
hist(berry_chemistry_borden_hills_2019_brix_hist$Brix)

#### Brix Sep 3 ####

berry_chemistry_borden_hills_2019_brix <- berry_chemistry_borden_hills_2019%>%
  filter(date == "2019-09-03") %>%
  select(date, Brix, treatment) 

berry_chemistry_borden_hills_2019_brix %>%
  group_by(treatment)%>%
  tally()

is.factor(berry_chemistry_borden_hills_2019_brix$treatment)

berry_chemistry_borden_hills_2019_brix$treatment<- format(berry_chemistry_borden_hills_2019_brix$treatment)
berry_chemistry_borden_hills_2019_brix$treatment<- as.factor(berry_chemistry_borden_hills_2019_brix$treatment)

is.factor(berry_chemistry_borden_hills_2019_brix$treatment)

str(berry_chemistry_borden_hills_2019_brix)

ggplot(berry_chemistry_borden_hills_2019_brix, aes (treatment,Brix, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_brix <- aov (Brix~treatment, berry_chemistry_borden_hills_2019_brix )
summary (anova_brix)

(Tukey_brix<- HSD.test(anova_brix , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

berry_chemistry_borden_hills_2019_brix_hist<-berry_chemistry_borden_hills_2019_brix %>%
  filter(treatment == 1)
hist(berry_chemistry_borden_hills_2019_brix_hist$Brix)

berry_chemistry_borden_hills_2019_brix_hist<-berry_chemistry_borden_hills_2019_brix %>%
  filter(treatment == 2)
hist(berry_chemistry_borden_hills_2019_brix_hist$Brix)

berry_chemistry_borden_hills_2019_brix_hist<-berry_chemistry_borden_hills_2019_brix %>%
  filter(treatment == 3)
hist(berry_chemistry_borden_hills_2019_brix_hist$Brix)

#### Brix Harvest point Sep 12 and 19 ####

berry_chemistry_borden_hills_2019_brix <- berry_chemistry_borden_hills_2019%>%
  filter(date >= "2019-09-12") %>%
  select(date, Brix, treatment) 

berry_chemistry_borden_hills_2019_brix %>%
  group_by(treatment)%>%
  tally()

is.factor(berry_chemistry_borden_hills_2019_brix$treatment)

berry_chemistry_borden_hills_2019_brix$treatment<- format(berry_chemistry_borden_hills_2019_brix$treatment)
berry_chemistry_borden_hills_2019_brix$treatment<- as.factor(berry_chemistry_borden_hills_2019_brix$treatment)

is.factor(berry_chemistry_borden_hills_2019_brix$treatment)


str(berry_chemistry_borden_hills_2019_brix)

ggplot(berry_chemistry_borden_hills_2019_brix, aes (treatment,Brix, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_brix <- aov (Brix~treatment, berry_chemistry_borden_hills_2019_brix )
summary (anova_brix)

(Tukey_brix<- HSD.test(anova_brix , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

berry_chemistry_borden_hills_2019_brix_hist<-berry_chemistry_borden_hills_2019_brix %>%
  filter(treatment == 1)
hist(berry_chemistry_borden_hills_2019_brix_hist$Brix)

berry_chemistry_borden_hills_2019_brix_hist<-berry_chemistry_borden_hills_2019_brix %>%
  filter(treatment == 2)
hist(berry_chemistry_borden_hills_2019_brix_hist$Brix)

berry_chemistry_borden_hills_2019_brix_hist<-berry_chemistry_borden_hills_2019_brix %>%
  filter(treatment == 3)
hist(berry_chemistry_borden_hills_2019_brix_hist$Brix)


##### pH Aug 02 ####

berry_chemistry_borden_hills_2019_pH <- berry_chemistry_borden_hills_2019%>%
  filter(date == "2019-08-02") %>%
  select(date, pH, treatment) 

berry_chemistry_borden_hills_2019_pH %>%
  group_by(treatment)%>%
  tally()

is.factor(berry_chemistry_borden_hills_2019_pH$treatment)

berry_chemistry_borden_hills_2019_pH$treatment<- format(berry_chemistry_borden_hills_2019_pH$treatment)
berry_chemistry_borden_hills_2019_pH$treatment<- as.factor(berry_chemistry_borden_hills_2019_pH$treatment)

is.factor(berry_chemistry_borden_hills_2019_pH$treatment)

str(berry_chemistry_borden_hills_2019_pH)

ggplot(berry_chemistry_borden_hills_2019_pH, aes (treatment,pH, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_pH <- aov (pH~treatment, berry_chemistry_borden_hills_2019_pH )
summary (anova_pH)

(Tukey_pH<- HSD.test(anova_pH , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

berry_chemistry_borden_hills_2019_pH_hist<-berry_chemistry_borden_hills_2019_pH %>%
  filter(treatment == 1)
hist(berry_chemistry_borden_hills_2019_pH_hist$pH)

berry_chemistry_borden_hills_2019_pH_hist<-berry_chemistry_borden_hills_2019_pH %>%
  filter(treatment == 2)
hist(berry_chemistry_borden_hills_2019_pH_hist$pH)

berry_chemistry_borden_hills_2019_pH_hist<-berry_chemistry_borden_hills_2019_pH %>%
  filter(treatment == 3)
hist(berry_chemistry_borden_hills_2019_pH_hist$pH)

#### pH Aug 8 ####

berry_chemistry_borden_hills_2019_pH <- berry_chemistry_borden_hills_2019%>%
  filter(date == "2019-08-08") %>%
  select(date, pH, treatment) 

berry_chemistry_borden_hills_2019_pH %>%
  group_by(treatment)%>%
  tally()

is.factor(berry_chemistry_borden_hills_2019_pH$treatment)

berry_chemistry_borden_hills_2019_pH$treatment<- format(berry_chemistry_borden_hills_2019_pH$treatment)
berry_chemistry_borden_hills_2019_pH$treatment<- as.factor(berry_chemistry_borden_hills_2019_pH$treatment)

is.factor(berry_chemistry_borden_hills_2019_pH$treatment)

str(berry_chemistry_borden_hills_2019_pH)

ggplot(berry_chemistry_borden_hills_2019_pH, aes (treatment,pH, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_pH <- aov (pH~treatment, berry_chemistry_borden_hills_2019_pH )
summary (anova_pH)

(Tukey_pH<- HSD.test(anova_pH , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

berry_chemistry_borden_hills_2019_pH_hist<-berry_chemistry_borden_hills_2019_pH %>%
  filter(treatment == 1)
hist(berry_chemistry_borden_hills_2019_pH_hist$pH)

berry_chemistry_borden_hills_2019_pH_hist<-berry_chemistry_borden_hills_2019_pH %>%
  filter(treatment == 2)
hist(berry_chemistry_borden_hills_2019_pH_hist$pH)

berry_chemistry_borden_hills_2019_pH_hist<-berry_chemistry_borden_hills_2019_pH %>%
  filter(treatment == 3)
hist(berry_chemistry_borden_hills_2019_pH_hist$pH)

#### pH Aug 15  ####

berry_chemistry_borden_hills_2019_pH <- berry_chemistry_borden_hills_2019%>%
  filter(date == "2019-08-15") %>%
  select(date, pH, treatment) 

berry_chemistry_borden_hills_2019_pH %>%
  group_by(treatment)%>%
  tally()

is.factor(berry_chemistry_borden_hills_2019_pH$treatment)

berry_chemistry_borden_hills_2019_pH$treatment<- format(berry_chemistry_borden_hills_2019_pH$treatment)
berry_chemistry_borden_hills_2019_pH$treatment<- as.factor(berry_chemistry_borden_hills_2019_pH$treatment)

is.factor(berry_chemistry_borden_hills_2019_pH$treatment)

str(berry_chemistry_borden_hills_2019_pH)

ggplot(berry_chemistry_borden_hills_2019_pH, aes (treatment,pH, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_pH <- aov (pH~treatment, berry_chemistry_borden_hills_2019_pH )
summary (anova_pH)

(Tukey_pH<- HSD.test(anova_pH , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))


berry_chemistry_borden_hills_2019_pH_hist<-berry_chemistry_borden_hills_2019_pH %>%
  filter(treatment == 1)
hist(berry_chemistry_borden_hills_2019_pH_hist$pH)

berry_chemistry_borden_hills_2019_pH_hist<-berry_chemistry_borden_hills_2019_pH %>%
  filter(treatment == 2)
hist(berry_chemistry_borden_hills_2019_pH_hist$pH)

berry_chemistry_borden_hills_2019_pH_hist<-berry_chemistry_borden_hills_2019_pH %>%
  filter(treatment == 3)
hist(berry_chemistry_borden_hills_2019_pH_hist$pH)

####  pH Aug 20 ####

berry_chemistry_borden_hills_2019_pH <- berry_chemistry_borden_hills_2019%>%
  filter(date == "2019-08-20") %>%
  select(date, pH, treatment) 

berry_chemistry_borden_hills_2019_pH %>%
  group_by(treatment)%>%
  tally()

is.factor(berry_chemistry_borden_hills_2019_pH$treatment)

berry_chemistry_borden_hills_2019_pH$treatment<- format(berry_chemistry_borden_hills_2019_pH$treatment)
berry_chemistry_borden_hills_2019_pH$treatment<- as.factor(berry_chemistry_borden_hills_2019_pH$treatment)

is.factor(berry_chemistry_borden_hills_2019_pH$treatment)

str(berry_chemistry_borden_hills_2019_pH)

ggplot(berry_chemistry_borden_hills_2019_pH, aes (treatment,pH, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_pH <- aov (pH~treatment, berry_chemistry_borden_hills_2019_pH )
summary (anova_pH)

(Tukey_pH<- HSD.test(anova_pH , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))


berry_chemistry_borden_hills_2019_pH_hist<-berry_chemistry_borden_hills_2019_pH %>%
  filter(treatment == 1)
hist(berry_chemistry_borden_hills_2019_pH_hist$pH)

berry_chemistry_borden_hills_2019_pH_hist<-berry_chemistry_borden_hills_2019_pH %>%
  filter(treatment == 2)
hist(berry_chemistry_borden_hills_2019_pH_hist$pH)

berry_chemistry_borden_hills_2019_pH_hist<-berry_chemistry_borden_hills_2019_pH %>%
  filter(treatment == 3)
hist(berry_chemistry_borden_hills_2019_pH_hist$pH)

#### pH Sep 3 ####

berry_chemistry_borden_hills_2019_pH <- berry_chemistry_borden_hills_2019%>%
  filter(date == "2019-09-03") %>%
  select(date, pH, treatment) 

berry_chemistry_borden_hills_2019_pH %>%
  group_by(treatment)%>%
  tally()

is.factor(berry_chemistry_borden_hills_2019_pH$treatment)

berry_chemistry_borden_hills_2019_pH$treatment<- format(berry_chemistry_borden_hills_2019_pH$treatment)
berry_chemistry_borden_hills_2019_pH$treatment<- as.factor(berry_chemistry_borden_hills_2019_pH$treatment)

is.factor(berry_chemistry_borden_hills_2019_pH$treatment)

str(berry_chemistry_borden_hills_2019_pH)

ggplot(berry_chemistry_borden_hills_2019_pH, aes (treatment,pH, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_pH <- aov (pH~treatment, berry_chemistry_borden_hills_2019_pH )
summary (anova_pH)

(Tukey_pH<- HSD.test(anova_pH , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

berry_chemistry_borden_hills_2019_pH_hist<-berry_chemistry_borden_hills_2019_pH %>%
  filter(treatment == 1)
hist(berry_chemistry_borden_hills_2019_pH_hist$pH)

berry_chemistry_borden_hills_2019_pH_hist<-berry_chemistry_borden_hills_2019_pH %>%
  filter(treatment == 2)
hist(berry_chemistry_borden_hills_2019_pH_hist$pH)

berry_chemistry_borden_hills_2019_pH_hist<-berry_chemistry_borden_hills_2019_pH %>%
  filter(treatment == 3)
hist(berry_chemistry_borden_hills_2019_pH_hist$pH)

#### pH Harvest point Sep 12 and 19 ####

berry_chemistry_borden_hills_2019_pH <- berry_chemistry_borden_hills_2019%>%
  filter(date >= "2019-09-12") %>%
  select(date, pH, treatment) 

berry_chemistry_borden_hills_2019_pH %>%
  group_by(treatment)%>%
  tally()

is.factor(berry_chemistry_borden_hills_2019_pH$treatment)

berry_chemistry_borden_hills_2019_pH$treatment<- format(berry_chemistry_borden_hills_2019_pH$treatment)
berry_chemistry_borden_hills_2019_pH$treatment<- as.factor(berry_chemistry_borden_hills_2019_pH$treatment)

is.factor(berry_chemistry_borden_hills_2019_pH$treatment)


str(berry_chemistry_borden_hills_2019_pH)

ggplot(berry_chemistry_borden_hills_2019_pH, aes (treatment,pH, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_pH <- aov (pH~treatment, berry_chemistry_borden_hills_2019_pH )
summary (anova_pH)

(Tukey_pH<- HSD.test(anova_pH , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

berry_chemistry_borden_hills_2019_pH_hist<-berry_chemistry_borden_hills_2019_pH %>%
  filter(treatment == 1)
hist(berry_chemistry_borden_hills_2019_pH_hist$pH)

berry_chemistry_borden_hills_2019_pH_hist<-berry_chemistry_borden_hills_2019_pH %>%
  filter(treatment == 2)
hist(berry_chemistry_borden_hills_2019_pH_hist$pH)

berry_chemistry_borden_hills_2019_pH_hist<-berry_chemistry_borden_hills_2019_pH %>%
  filter(treatment == 3)
hist(berry_chemistry_borden_hills_2019_pH_hist$pH)

#####____________________

##### TA Aug 02 ####

berry_chemistry_borden_hills_2019_TA <- berry_chemistry_borden_hills_2019%>%
  filter(date == "2019-08-02") %>%
  select(date, TA, treatment) 

berry_chemistry_borden_hills_2019_TA %>%
  group_by(treatment)%>%
  tally()

is.factor(berry_chemistry_borden_hills_2019_TA$treatment)

berry_chemistry_borden_hills_2019_TA$treatment<- format(berry_chemistry_borden_hills_2019_TA$treatment)
berry_chemistry_borden_hills_2019_TA$treatment<- as.factor(berry_chemistry_borden_hills_2019_TA$treatment)

is.factor(berry_chemistry_borden_hills_2019_TA$treatment)

str(berry_chemistry_borden_hills_2019_TA)

ggplot(berry_chemistry_borden_hills_2019_TA, aes (treatment,TA, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_TA <- aov (TA~treatment, berry_chemistry_borden_hills_2019_TA )
summary (anova_TA)

(Tukey_TA<- HSD.test(anova_TA , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

berry_chemistry_borden_hills_2019_TA_hist<-berry_chemistry_borden_hills_2019_TA %>%
  filter(treatment == 1)
hist(berry_chemistry_borden_hills_2019_TA_hist$TA)

berry_chemistry_borden_hills_2019_TA_hist<-berry_chemistry_borden_hills_2019_TA %>%
  filter(treatment == 2)
hist(berry_chemistry_borden_hills_2019_TA_hist$TA)

berry_chemistry_borden_hills_2019_TA_hist<-berry_chemistry_borden_hills_2019_TA %>%
  filter(treatment == 3)
hist(berry_chemistry_borden_hills_2019_TA_hist$TA)

#### TA Aug 8 ####

berry_chemistry_borden_hills_2019_TA <- berry_chemistry_borden_hills_2019%>%
  filter(date == "2019-08-08") %>%
  select(date, TA, treatment) 

berry_chemistry_borden_hills_2019_TA %>%
  group_by(treatment)%>%
  tally()

is.factor(berry_chemistry_borden_hills_2019_TA$treatment)

berry_chemistry_borden_hills_2019_TA$treatment<- format(berry_chemistry_borden_hills_2019_TA$treatment)
berry_chemistry_borden_hills_2019_TA$treatment<- as.factor(berry_chemistry_borden_hills_2019_TA$treatment)

is.factor(berry_chemistry_borden_hills_2019_TA$treatment)

str(berry_chemistry_borden_hills_2019_TA)

ggplot(berry_chemistry_borden_hills_2019_TA, aes (treatment,TA, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_TA <- aov (TA~treatment, berry_chemistry_borden_hills_2019_TA )
summary (anova_TA)

(Tukey_TA<- HSD.test(anova_TA , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

berry_chemistry_borden_hills_2019_TA_hist<-berry_chemistry_borden_hills_2019_TA %>%
  filter(treatment == 1)
hist(berry_chemistry_borden_hills_2019_TA_hist$TA)

berry_chemistry_borden_hills_2019_TA_hist<-berry_chemistry_borden_hills_2019_TA %>%
  filter(treatment == 2)
hist(berry_chemistry_borden_hills_2019_TA_hist$TA)

berry_chemistry_borden_hills_2019_TA_hist<-berry_chemistry_borden_hills_2019_TA %>%
  filter(treatment == 3)
hist(berry_chemistry_borden_hills_2019_TA_hist$TA)

#### TA Aug 15  ####

berry_chemistry_borden_hills_2019_TA <- berry_chemistry_borden_hills_2019%>%
  filter(date == "2019-08-15") %>%
  select(date, TA, treatment) 

berry_chemistry_borden_hills_2019_TA %>%
  group_by(treatment)%>%
  tally()

is.factor(berry_chemistry_borden_hills_2019_TA$treatment)

berry_chemistry_borden_hills_2019_TA$treatment<- format(berry_chemistry_borden_hills_2019_TA$treatment)
berry_chemistry_borden_hills_2019_TA$treatment<- as.factor(berry_chemistry_borden_hills_2019_TA$treatment)

is.factor(berry_chemistry_borden_hills_2019_TA$treatment)

str(berry_chemistry_borden_hills_2019_TA)

ggplot(berry_chemistry_borden_hills_2019_TA, aes (treatment,TA, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_TA <- aov (TA~treatment, berry_chemistry_borden_hills_2019_TA )
summary (anova_TA)

(Tukey_TA<- HSD.test(anova_TA , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))


berry_chemistry_borden_hills_2019_TA_hist<-berry_chemistry_borden_hills_2019_TA %>%
  filter(treatment == 1)
hist(berry_chemistry_borden_hills_2019_TA_hist$TA)

berry_chemistry_borden_hills_2019_TA_hist<-berry_chemistry_borden_hills_2019_TA %>%
  filter(treatment == 2)
hist(berry_chemistry_borden_hills_2019_TA_hist$TA)

berry_chemistry_borden_hills_2019_TA_hist<-berry_chemistry_borden_hills_2019_TA %>%
  filter(treatment == 3)
hist(berry_chemistry_borden_hills_2019_TA_hist$TA)

####  TA Aug 20 ####

berry_chemistry_borden_hills_2019_TA <- berry_chemistry_borden_hills_2019%>%
  filter(date == "2019-08-20") %>%
  select(date, TA, treatment) 

berry_chemistry_borden_hills_2019_TA %>%
  group_by(treatment)%>%
  tally()

is.factor(berry_chemistry_borden_hills_2019_TA$treatment)

berry_chemistry_borden_hills_2019_TA$treatment<- format(berry_chemistry_borden_hills_2019_TA$treatment)
berry_chemistry_borden_hills_2019_TA$treatment<- as.factor(berry_chemistry_borden_hills_2019_TA$treatment)

is.factor(berry_chemistry_borden_hills_2019_TA$treatment)

str(berry_chemistry_borden_hills_2019_TA)

ggplot(berry_chemistry_borden_hills_2019_TA, aes (treatment,TA, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_TA <- aov (TA~treatment, berry_chemistry_borden_hills_2019_TA )
summary (anova_TA)

(Tukey_TA<- HSD.test(anova_TA , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))


berry_chemistry_borden_hills_2019_TA_hist<-berry_chemistry_borden_hills_2019_TA %>%
  filter(treatment == 1)
hist(berry_chemistry_borden_hills_2019_TA_hist$TA)

berry_chemistry_borden_hills_2019_TA_hist<-berry_chemistry_borden_hills_2019_TA %>%
  filter(treatment == 2)
hist(berry_chemistry_borden_hills_2019_TA_hist$TA)

berry_chemistry_borden_hills_2019_TA_hist<-berry_chemistry_borden_hills_2019_TA %>%
  filter(treatment == 3)
hist(berry_chemistry_borden_hills_2019_TA_hist$TA)

#### TA Sep 3 ####

berry_chemistry_borden_hills_2019_TA <- berry_chemistry_borden_hills_2019%>%
  filter(date == "2019-09-03") %>%
  select(date, TA, treatment) 

berry_chemistry_borden_hills_2019_TA %>%
  group_by(treatment)%>%
  tally()

is.factor(berry_chemistry_borden_hills_2019_TA$treatment)

berry_chemistry_borden_hills_2019_TA$treatment<- format(berry_chemistry_borden_hills_2019_TA$treatment)
berry_chemistry_borden_hills_2019_TA$treatment<- as.factor(berry_chemistry_borden_hills_2019_TA$treatment)

is.factor(berry_chemistry_borden_hills_2019_TA$treatment)

str(berry_chemistry_borden_hills_2019_TA)

ggplot(berry_chemistry_borden_hills_2019_TA, aes (treatment,TA, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_TA <- aov (TA~treatment, berry_chemistry_borden_hills_2019_TA )
summary (anova_TA)

(Tukey_TA<- HSD.test(anova_TA , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

berry_chemistry_borden_hills_2019_TA_hist<-berry_chemistry_borden_hills_2019_TA %>%
  filter(treatment == 1)
hist(berry_chemistry_borden_hills_2019_TA_hist$TA)

berry_chemistry_borden_hills_2019_TA_hist<-berry_chemistry_borden_hills_2019_TA %>%
  filter(treatment == 2)
hist(berry_chemistry_borden_hills_2019_TA_hist$TA)

berry_chemistry_borden_hills_2019_TA_hist<-berry_chemistry_borden_hills_2019_TA %>%
  filter(treatment == 3)
hist(berry_chemistry_borden_hills_2019_TA_hist$TA)

#### TA Harvest point Sep 12 and 19 ####

berry_chemistry_borden_hills_2019_TA <- berry_chemistry_borden_hills_2019%>%
  filter(date >= "2019-09-12") %>%
  select(date, TA, treatment) 

berry_chemistry_borden_hills_2019_TA %>%
  group_by(treatment)%>%
  tally()

is.factor(berry_chemistry_borden_hills_2019_TA$treatment)

berry_chemistry_borden_hills_2019_TA$treatment<- format(berry_chemistry_borden_hills_2019_TA$treatment)
berry_chemistry_borden_hills_2019_TA$treatment<- as.factor(berry_chemistry_borden_hills_2019_TA$treatment)

is.factor(berry_chemistry_borden_hills_2019_TA$treatment)


str(berry_chemistry_borden_hills_2019_TA)

ggplot(berry_chemistry_borden_hills_2019_TA, aes (treatment,TA, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_TA <- aov (TA~treatment, berry_chemistry_borden_hills_2019_TA )
summary (anova_TA)

(Tukey_TA<- HSD.test(anova_TA , trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

berry_chemistry_borden_hills_2019_TA_hist<-berry_chemistry_borden_hills_2019_TA %>%
  filter(treatment == 1)
hist(berry_chemistry_borden_hills_2019_TA_hist$TA)

berry_chemistry_borden_hills_2019_TA_hist<-berry_chemistry_borden_hills_2019_TA %>%
  filter(treatment == 2)
hist(berry_chemistry_borden_hills_2019_TA_hist$TA)

berry_chemistry_borden_hills_2019_TA_hist<-berry_chemistry_borden_hills_2019_TA %>%
  filter(treatment == 3)
hist(berry_chemistry_borden_hills_2019_TA_hist$TA)

##### Yield ANOVA####

yield_bh_2019<- read.csv("data/Yield_borden_hills_2019.csv", header =TRUE)

yield_bh_2019_grouped<- yield_bh_2019 %>%
  mutate(gr_cluster = (Weight_kg/Clusters)*1000) %>%
  filter(!is.na(gr_cluster)) 

yield_bh_2019_grouped$Treatment<- reorder(yield_bh_2019_grouped$Treatment, yield_bh_2019_grouped$gr_cluster)

str(yield_bh_2019_grouped$Treatment)

yield_bh_2019_grouped$Treatment<-format(yield_bh_2019_grouped$Treatment)
as.character(yield_bh_2019_grouped$Treatment)

str(yield_bh_2019_grouped$Treatment)

yield_bh_2019_grouped$Rep<-format(yield_bh_2019_grouped$Rep)
as.character(yield_bh_2019_grouped$Rep)

str(yield_bh_2019_grouped$Rep)

####grs cluster ANOVA####
yield_bh_2019_grouped_grs_cluster <- yield_bh_2019_grouped%>%
  filter(!pixel_number == "34") %>%
  select(gr_cluster, Treatment) 

yield_bh_2019_grouped_grs_cluster %>%
  group_by(Treatment)%>%
  tally()

is.factor(yield_bh_2019_grouped_grs_cluster$Treatment)

yield_bh_2019_grouped_grs_cluster$Treatment<- format(yield_bh_2019_grouped_grs_cluster$Treatment)
yield_bh_2019_grouped_grs_cluster$Treatment<- as.factor(yield_bh_2019_grouped_grs_cluster$Treatment)

is.factor(yield_bh_2019_grouped_grs_cluster$Treatment)


str(yield_bh_2019_grouped_grs_cluster)

ggplot(yield_bh_2019_grouped_grs_cluster, aes (Treatment,gr_cluster, group =Treatment)) +
  geom_boxplot(aes(fill = Treatment, color =Treatment)) +
  geom_point()


ANOVA_grs_cluster<- aov (gr_cluster~Treatment, yield_bh_2019_grouped_grs_cluster)
summary (ANOVA_grs_cluster)

(tukey_grs_cluster<- HSD.test(ANOVA_grs_cluster, trt = "Treatment", alpha =0.05, unbalanced = "TRUE"))

yield_bh_2019_grouped_grs_cluster_hist<-yield_bh_2019_grouped_grs_cluster %>%
  filter(Treatment == 1)
hist(yield_bh_2019_grouped_grs_cluster_hist$gr_cluster)

yield_bh_2019_grouped_grs_cluster_hist<-yield_bh_2019_grouped_grs_cluster %>%
  filter(Treatment == 2)
hist(yield_bh_2019_grouped_grs_cluster_hist$gr_cluster)

yield_bh_2019_grouped_grs_cluster_hist<-yield_bh_2019_grouped_grs_cluster %>%
  filter(Treatment == 3)
hist(yield_bh_2019_grouped_grs_cluster_hist$gr_cluster)

##### Kg/vine ANOVA####


yield_bh_2019_grouped<- yield_bh_2019 %>%
  filter(!is.na(Weight_kg)) 

yield_bh_2019_grouped$Treatment<- reorder(yield_bh_2019_grouped$Treatment, yield_bh_2019_grouped$Weight_kg)

str(yield_bh_2019_grouped$Treatment)

yield_bh_2019_grouped$Treatment<-format(yield_bh_2019_grouped$Treatment)
as.character(yield_bh_2019_grouped$Treatment)

str(yield_bh_2019_grouped$Treatment)

yield_bh_2019_grouped$Rep<-format(yield_bh_2019_grouped$Rep)
as.character(yield_bh_2019_grouped$Rep)

str(yield_bh_2019_grouped$Rep)

yield_bh_2019_grouped_kg_vine<- yield_bh_2019_grouped%>%
  filter(!pixel_number == "34") %>%
  select(Weight_kg, Treatment) 

yield_bh_2019_grouped_kg_vine %>%
  group_by(Treatment)%>%
  tally()

is.factor(yield_bh_2019_grouped_kg_vine$Treatment)

yield_bh_2019_grouped_kg_vine$Treatment<- format(yield_bh_2019_grouped_kg_vine$Treatment)
yield_bh_2019_grouped_kg_vine$Treatment<- as.factor(yield_bh_2019_grouped_kg_vine$Treatment)

is.factor(yield_bh_2019_grouped_kg_vine$Treatment)


str(yield_bh_2019_grouped_kg_vine)

ggplot(yield_bh_2019_grouped_kg_vine, aes (Treatment,Weight_kg, group =Treatment)) +
  geom_boxplot(aes(fill = Treatment, color =Treatment)) +
  geom_point()


anova_kg_vine <- aov (Weight_kg~Treatment, yield_bh_2019_grouped_kg_vine)
summary (anova_kg_vine)

(tukey_kg_vine<- HSD.test(anova_kg_vine, trt = "Treatment", alpha =0.05, unbalanced = "TRUE"))

yield_bh_2019_grouped_kg_vine_hist<-yield_bh_2019_grouped_kg_vine %>%
  filter(Treatment == 1)
hist(yield_bh_2019_grouped_kg_vine_hist$Weight_kg)

yield_bh_2019_grouped_kg_vine_hist<-yield_bh_2019_grouped_kg_vine %>%
  filter(Treatment == 2)
hist(yield_bh_2019_grouped_kg_vine_hist$Weight_kg)

yield_bh_2019_grouped_kg_vine_hist<-yield_bh_2019_grouped_kg_vine %>%
  filter(Treatment == 3)
hist(yield_bh_2019_grouped_kg_vine_hist$Weight_kg)


##### Phenolics anova MCP Tannins ####

mcp_2019<- read.csv("data/mcp_berry_phenolics.csv", header =TRUE)


se <- function(x) sqrt(var(x)/length(x))


#### Total tannins  ANOVA MG/BERRY####

 #### Aug 8 ####

mcp_2019_tannins_mg_berry <- mcp_2019%>%
  filter(Date_sampled == "8/8/2019") %>%
  filter(!Block_id == "B1R2") %>%
  select(Date_sampled, Total_tannin_mg_berry, treatment) 

str(mcp_2019_tannins_mg_berry)

mcp_2019_tannins_mg_berry %>%
  group_by(treatment)%>%
  tally()

is.factor(mcp_2019_tannins_mg_berry$treatment)

mcp_2019_tannins_mg_berry$treatment<- format(mcp_2019_tannins_mg_berry$treatment)
mcp_2019_tannins_mg_berry$treatment<- as.factor(mcp_2019_tannins_mg_berry$treatment)

is.factor(mcp_2019_tannins_mg_berry$treatment)


str(mcp_2019_tannins_mg_berry)

ggplot(mcp_2019_tannins_mg_berry, aes (treatment,Total_tannin_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_tannins_mg_berry <- aov (Total_tannin_mg_berry~treatment, mcp_2019_tannins_mg_berry )
summary (anova_tannins_mg_berry)


(tukey_tannins_mg_berry<- HSD.test(anova_tannins_mg_berry, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

mcp_2019_tannins_mg_berry_hist<-mcp_2019_tannins_mg_berry %>%
  filter(treatment == 1)
hist(mcp_2019_tannins_mg_berry_hist$Total_tannin_mg_berry)

mcp_2019_tannins_mg_berry_hist<-mcp_2019_tannins_mg_berry %>%
  filter(treatment == 2)
hist(mcp_2019_tannins_mg_berry_hist$Total_tannin_mg_berry)

mcp_2019_tannins_mg_berry_hist<-mcp_2019_tannins_mg_berry %>%
  filter(treatment == 3)
hist(mcp_2019_tannins_mg_berry_hist$Total_tannin_mg_berry)

### Aug 15 ####

mcp_2019_tannins_mg_berry <- mcp_2019%>%
  filter(Date_sampled == "8/15/2019") %>%
  filter(!Block_id == "B1R2") %>%
  select(Date_sampled, Total_tannin_mg_berry, treatment) 

str(mcp_2019_tannins_mg_berry)

mcp_2019_tannins_mg_berry %>%
  group_by(treatment)%>%
  tally()

is.factor(mcp_2019_tannins_mg_berry$treatment)

mcp_2019_tannins_mg_berry$treatment<- format(mcp_2019_tannins_mg_berry$treatment)
mcp_2019_tannins_mg_berry$treatment<- as.factor(mcp_2019_tannins_mg_berry$treatment)

is.factor(mcp_2019_tannins_mg_berry$treatment)


str(mcp_2019_tannins_mg_berry)

ggplot(mcp_2019_tannins_mg_berry, aes (treatment,Total_tannin_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_tannins_mg_berry <- aov (Total_tannin_mg_berry~treatment, mcp_2019_tannins_mg_berry )
summary (anova_tannins_mg_berry)


(tukey_tannins_mg_berry<- HSD.test(anova_tannins_mg_berry, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

mcp_2019_tannins_mg_berry_hist<-mcp_2019_tannins_mg_berry %>%
  filter(treatment == 1)
hist(mcp_2019_tannins_mg_berry_hist$Total_tannin_mg_berry)

mcp_2019_tannins_mg_berry_hist<-mcp_2019_tannins_mg_berry %>%
  filter(treatment == 2)
hist(mcp_2019_tannins_mg_berry_hist$Total_tannin_mg_berry)

mcp_2019_tannins_mg_berry_hist<-mcp_2019_tannins_mg_berry %>%
  filter(treatment == 3)
hist(mcp_2019_tannins_mg_berry_hist$Total_tannin_mg_berry)

#### Aug 20 ####

mcp_2019_tannins_mg_berry <- mcp_2019%>%
  filter(Date_sampled == "8/20/2019") %>%
  filter(!Block_id == "B1R2") %>%
  select(Date_sampled, Total_tannin_mg_berry, treatment) 

str(mcp_2019_tannins_mg_berry)

mcp_2019_tannins_mg_berry %>%
  group_by(treatment)%>%
  tally()

is.factor(mcp_2019_tannins_mg_berry$treatment)

mcp_2019_tannins_mg_berry$treatment<- format(mcp_2019_tannins_mg_berry$treatment)
mcp_2019_tannins_mg_berry$treatment<- as.factor(mcp_2019_tannins_mg_berry$treatment)

is.factor(mcp_2019_tannins_mg_berry$treatment)


str(mcp_2019_tannins_mg_berry)

ggplot(mcp_2019_tannins_mg_berry, aes (treatment,Total_tannin_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_tannins_mg_berry <- aov (Total_tannin_mg_berry~treatment, mcp_2019_tannins_mg_berry )
summary (anova_tannins_mg_berry)


(tukey_tannins_mg_berry<- HSD.test(anova_tannins_mg_berry, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

mcp_2019_tannins_mg_berry_hist<-mcp_2019_tannins_mg_berry %>%
  filter(treatment == 1)
hist(mcp_2019_tannins_mg_berry_hist$Total_tannin_mg_berry)

mcp_2019_tannins_mg_berry_hist<-mcp_2019_tannins_mg_berry %>%
  filter(treatment == 2)
hist(mcp_2019_tannins_mg_berry_hist$Total_tannin_mg_berry)

mcp_2019_tannins_mg_berry_hist<-mcp_2019_tannins_mg_berry %>%
  filter(treatment == 3)
hist(mcp_2019_tannins_mg_berry_hist$Total_tannin_mg_berry)


#### Sep 3 ####

mcp_2019_tannins_mg_berry <- mcp_2019%>%
  filter(Date_sampled == "9/3/2019") %>%
  filter(!Block_id == "B1R2") %>%
  select(Date_sampled, Total_tannin_mg_berry, treatment) 

str(mcp_2019_tannins_mg_berry)

mcp_2019_tannins_mg_berry %>%
  group_by(treatment)%>%
  tally()

is.factor(mcp_2019_tannins_mg_berry$treatment)

mcp_2019_tannins_mg_berry$treatment<- format(mcp_2019_tannins_mg_berry$treatment)
mcp_2019_tannins_mg_berry$treatment<- as.factor(mcp_2019_tannins_mg_berry$treatment)

is.factor(mcp_2019_tannins_mg_berry$treatment)


str(mcp_2019_tannins_mg_berry)

ggplot(mcp_2019_tannins_mg_berry, aes (treatment,Total_tannin_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_tannins_mg_berry <- aov (Total_tannin_mg_berry~treatment, mcp_2019_tannins_mg_berry )
summary (anova_tannins_mg_berry)


(tukey_tannins_mg_berry<- HSD.test(anova_tannins_mg_berry, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

mcp_2019_tannins_mg_berry_hist<-mcp_2019_tannins_mg_berry %>%
  filter(treatment == 1)
hist(mcp_2019_tannins_mg_berry_hist$Total_tannin_mg_berry)

mcp_2019_tannins_mg_berry_hist<-mcp_2019_tannins_mg_berry %>%
  filter(treatment == 2)
hist(mcp_2019_tannins_mg_berry_hist$Total_tannin_mg_berry)

mcp_2019_tannins_mg_berry_hist<-mcp_2019_tannins_mg_berry %>%
  filter(treatment == 3)
hist(mcp_2019_tannins_mg_berry_hist$Total_tannin_mg_berry)

#### Harvest poitn sept 12 and 19 ####
mcp_2019_tannins_mg_berry <- mcp_2019%>%
  filter(Date_sampled == "9/12/2019" | Date_sampled == "9/19/2019") %>%
  filter(!Block_id == "B1R2") %>%
  select(Date_sampled, Total_tannin_mg_berry, treatment) 

str(mcp_2019_tannins_mg_berry)

mcp_2019_tannins_mg_berry %>%
  group_by(treatment)%>%
  tally()

is.factor(mcp_2019_tannins_mg_berry$treatment)

mcp_2019_tannins_mg_berry$treatment<- format(mcp_2019_tannins_mg_berry$treatment)
mcp_2019_tannins_mg_berry$treatment<- as.factor(mcp_2019_tannins_mg_berry$treatment)

is.factor(mcp_2019_tannins_mg_berry$treatment)


str(mcp_2019_tannins_mg_berry)

ggplot(mcp_2019_tannins_mg_berry, aes (treatment,Total_tannin_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_tannins_mg_berry <- aov (Total_tannin_mg_berry~treatment, mcp_2019_tannins_mg_berry )
summary (anova_tannins_mg_berry)


(tukey_tannins_mg_berry<- HSD.test(anova_tannins_mg_berry, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

mcp_2019_tannins_mg_berry_hist<-mcp_2019_tannins_mg_berry %>%
  filter(treatment == 1)
hist(mcp_2019_tannins_mg_berry_hist$Total_tannin_mg_berry)

mcp_2019_tannins_mg_berry_hist<-mcp_2019_tannins_mg_berry %>%
  filter(treatment == 2)
hist(mcp_2019_tannins_mg_berry_hist$Total_tannin_mg_berry)

mcp_2019_tannins_mg_berry_hist<-mcp_2019_tannins_mg_berry %>%
  filter(treatment == 3)
hist(mcp_2019_tannins_mg_berry_hist$Total_tannin_mg_berry)

#### TANNINS mg/g berry weight ANOVA ####

#### Aug 8 ####

mcp_2019_tannins_mg_per_g_berry_wt <- mcp_2019%>%
  filter(Date_sampled == "8/8/2019") %>%
  filter(!Block_id == "B1R2") %>%
  select(Date_sampled, Total_tannin_mg_g_berry_weight, treatment) 

str(mcp_2019_tannins_mg_per_g_berry_wt)

mcp_2019_tannins_mg_per_g_berry_wt %>%
  group_by(treatment)%>%
  tally()

is.factor(mcp_2019_tannins_mg_per_g_berry_wt$treatment)

mcp_2019_tannins_mg_per_g_berry_wt$treatment<- format(mcp_2019_tannins_mg_per_g_berry_wt$treatment)
mcp_2019_tannins_mg_per_g_berry_wt$treatment<- as.factor(mcp_2019_tannins_mg_per_g_berry_wt$treatment)

is.factor(mcp_2019_tannins_mg_per_g_berry_wt$treatment)


str(mcp_2019_tannins_mg_per_g_berry_wt)

ggplot(mcp_2019_tannins_mg_per_g_berry_wt, aes (treatment,Total_tannin_mg_g_berry_weight, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_tannins_mg_per_g_berry_wt <- aov (Total_tannin_mg_g_berry_weight~treatment, mcp_2019_tannins_mg_per_g_berry_wt )
summary (anova_tannins_mg_per_g_berry_wt)


(tukey_tannins_mg_per_g_berry_wt<- HSD.test(anova_tannins_mg_per_g_berry_wt, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

mcp_2019_tannins_mg_per_g_berry_wt_hist<-mcp_2019_tannins_mg_per_g_berry_wt %>%
  filter(treatment == 1)
hist(mcp_2019_tannins_mg_per_g_berry_wt_hist$Total_tannin_mg_g_berry_weight)

mcp_2019_tannins_mg_per_g_berry_wt_hist<-mcp_2019_tannins_mg_per_g_berry_wt %>%
  filter(treatment == 2)
hist(mcp_2019_tannins_mg_per_g_berry_wt_hist$Total_tannin_mg_g_berry_weight)

mcp_2019_tannins_mg_per_g_berry_wt_hist<-mcp_2019_tannins_mg_per_g_berry_wt %>%
  filter(treatment == 3)
hist(mcp_2019_tannins_mg_per_g_berry_wt_hist$Total_tannin_mg_g_berry_weight)

### Aug 15 ####

mcp_2019_tannins_mg_per_g_berry_wt <- mcp_2019%>%
  filter(Date_sampled == "8/15/2019") %>%
  filter(!Block_id == "B1R2") %>%
  select(Date_sampled, Total_tannin_mg_g_berry_weight, treatment) 

str(mcp_2019_tannins_mg_per_g_berry_wt)

mcp_2019_tannins_mg_per_g_berry_wt %>%
  group_by(treatment)%>%
  tally()

is.factor(mcp_2019_tannins_mg_per_g_berry_wt$treatment)

mcp_2019_tannins_mg_per_g_berry_wt$treatment<- format(mcp_2019_tannins_mg_per_g_berry_wt$treatment)
mcp_2019_tannins_mg_per_g_berry_wt$treatment<- as.factor(mcp_2019_tannins_mg_per_g_berry_wt$treatment)

is.factor(mcp_2019_tannins_mg_per_g_berry_wt$treatment)


str(mcp_2019_tannins_mg_per_g_berry_wt)

ggplot(mcp_2019_tannins_mg_per_g_berry_wt, aes (treatment,Total_tannin_mg_g_berry_weight, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_tannins_mg_per_g_berry_wt <- aov (Total_tannin_mg_g_berry_weight~treatment, mcp_2019_tannins_mg_per_g_berry_wt )
summary (anova_tannins_mg_per_g_berry_wt)


(tukey_tannins_mg_per_g_berry_wt<- HSD.test(anova_tannins_mg_per_g_berry_wt, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

mcp_2019_tannins_mg_per_g_berry_wt_hist<-mcp_2019_tannins_mg_per_g_berry_wt %>%
  filter(treatment == 1)
hist(mcp_2019_tannins_mg_per_g_berry_wt_hist$Total_tannin_mg_g_berry_weight)

mcp_2019_tannins_mg_per_g_berry_wt_hist<-mcp_2019_tannins_mg_per_g_berry_wt %>%
  filter(treatment == 2)
hist(mcp_2019_tannins_mg_per_g_berry_wt_hist$Total_tannin_mg_g_berry_weight)

mcp_2019_tannins_mg_per_g_berry_wt_hist<-mcp_2019_tannins_mg_per_g_berry_wt %>%
  filter(treatment == 3)
hist(mcp_2019_tannins_mg_per_g_berry_wt_hist$Total_tannin_mg_g_berry_weight)

#### Aug 20 ####

mcp_2019_tannins_mg_per_g_berry_wt <- mcp_2019%>%
  filter(Date_sampled == "8/20/2019") %>%
  filter(!Block_id == "B1R2") %>%
  select(Date_sampled, Total_tannin_mg_g_berry_weight, treatment) 

str(mcp_2019_tannins_mg_per_g_berry_wt)

mcp_2019_tannins_mg_per_g_berry_wt %>%
  group_by(treatment)%>%
  tally()

is.factor(mcp_2019_tannins_mg_per_g_berry_wt$treatment)

mcp_2019_tannins_mg_per_g_berry_wt$treatment<- format(mcp_2019_tannins_mg_per_g_berry_wt$treatment)
mcp_2019_tannins_mg_per_g_berry_wt$treatment<- as.factor(mcp_2019_tannins_mg_per_g_berry_wt$treatment)

is.factor(mcp_2019_tannins_mg_per_g_berry_wt$treatment)


str(mcp_2019_tannins_mg_per_g_berry_wt)

ggplot(mcp_2019_tannins_mg_per_g_berry_wt, aes (treatment,Total_tannin_mg_g_berry_weight, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_tannins_mg_per_g_berry_wt <- aov (Total_tannin_mg_g_berry_weight~treatment, mcp_2019_tannins_mg_per_g_berry_wt )
summary (anova_tannins_mg_per_g_berry_wt)


(tukey_tannins_mg_per_g_berry_wt<- HSD.test(anova_tannins_mg_per_g_berry_wt, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

mcp_2019_tannins_mg_per_g_berry_wt_hist<-mcp_2019_tannins_mg_per_g_berry_wt %>%
  filter(treatment == 1)
hist(mcp_2019_tannins_mg_per_g_berry_wt_hist$Total_tannin_mg_g_berry_weight)

mcp_2019_tannins_mg_per_g_berry_wt_hist<-mcp_2019_tannins_mg_per_g_berry_wt %>%
  filter(treatment == 2)
hist(mcp_2019_tannins_mg_per_g_berry_wt_hist$Total_tannin_mg_g_berry_weight)

mcp_2019_tannins_mg_per_g_berry_wt_hist<-mcp_2019_tannins_mg_per_g_berry_wt %>%
  filter(treatment == 3)
hist(mcp_2019_tannins_mg_per_g_berry_wt_hist$Total_tannin_mg_g_berry_weight)


#### Sep 3 ####

mcp_2019_tannins_mg_per_g_berry_wt <- mcp_2019%>%
  filter(Date_sampled == "9/3/2019") %>%
  filter(!Block_id == "B1R2") %>%
  select(Date_sampled, Total_tannin_mg_g_berry_weight, treatment) 

str(mcp_2019_tannins_mg_per_g_berry_wt)

mcp_2019_tannins_mg_per_g_berry_wt %>%
  group_by(treatment)%>%
  tally()

is.factor(mcp_2019_tannins_mg_per_g_berry_wt$treatment)

mcp_2019_tannins_mg_per_g_berry_wt$treatment<- format(mcp_2019_tannins_mg_per_g_berry_wt$treatment)
mcp_2019_tannins_mg_per_g_berry_wt$treatment<- as.factor(mcp_2019_tannins_mg_per_g_berry_wt$treatment)

is.factor(mcp_2019_tannins_mg_per_g_berry_wt$treatment)


str(mcp_2019_tannins_mg_per_g_berry_wt)

ggplot(mcp_2019_tannins_mg_per_g_berry_wt, aes (treatment,Total_tannin_mg_g_berry_weight, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_tannins_mg_per_g_berry_wt <- aov (Total_tannin_mg_g_berry_weight~treatment, mcp_2019_tannins_mg_per_g_berry_wt )
summary (anova_tannins_mg_per_g_berry_wt)


(tukey_tannins_mg_per_g_berry_wt<- HSD.test(anova_tannins_mg_per_g_berry_wt, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

mcp_2019_tannins_mg_per_g_berry_wt_hist<-mcp_2019_tannins_mg_per_g_berry_wt %>%
  filter(treatment == 1)
hist(mcp_2019_tannins_mg_per_g_berry_wt_hist$Total_tannin_mg_g_berry_weight)

mcp_2019_tannins_mg_per_g_berry_wt_hist<-mcp_2019_tannins_mg_per_g_berry_wt %>%
  filter(treatment == 2)
hist(mcp_2019_tannins_mg_per_g_berry_wt_hist$Total_tannin_mg_g_berry_weight)

mcp_2019_tannins_mg_per_g_berry_wt_hist<-mcp_2019_tannins_mg_per_g_berry_wt %>%
  filter(treatment == 3)
hist(mcp_2019_tannins_mg_per_g_berry_wt_hist$Total_tannin_mg_g_berry_weight)

#### Harvest poitn sept 12 and 19 ####
mcp_2019_tannins_mg_per_g_berry_wt <- mcp_2019%>%
  filter(Date_sampled == "9/12/2019" | Date_sampled == "9/19/2019") %>%
  filter(!Block_id == "B1R2") %>%
  select(Date_sampled, Total_tannin_mg_g_berry_weight, treatment) 

str(mcp_2019_tannins_mg_per_g_berry_wt)

mcp_2019_tannins_mg_per_g_berry_wt %>%
  group_by(treatment)%>%
  tally()

is.factor(mcp_2019_tannins_mg_per_g_berry_wt$treatment)

mcp_2019_tannins_mg_per_g_berry_wt$treatment<- format(mcp_2019_tannins_mg_per_g_berry_wt$treatment)
mcp_2019_tannins_mg_per_g_berry_wt$treatment<- as.factor(mcp_2019_tannins_mg_per_g_berry_wt$treatment)

is.factor(mcp_2019_tannins_mg_per_g_berry_wt$treatment)


str(mcp_2019_tannins_mg_per_g_berry_wt)

ggplot(mcp_2019_tannins_mg_per_g_berry_wt, aes (treatment,Total_tannin_mg_g_berry_weight, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_tannins_mg_per_g_berry_wt <- aov (Total_tannin_mg_g_berry_weight~treatment, mcp_2019_tannins_mg_per_g_berry_wt )
summary (anova_tannins_mg_per_g_berry_wt)


(tukey_tannins_mg_per_g_berry_wt<- HSD.test(anova_tannins_mg_per_g_berry_wt, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

mcp_2019_tannins_mg_per_g_berry_wt_hist<-mcp_2019_tannins_mg_per_g_berry_wt %>%
  filter(treatment == 1)
hist(mcp_2019_tannins_mg_per_g_berry_wt_hist$Total_tannin_mg_g_berry_weight)

mcp_2019_tannins_mg_per_g_berry_wt_hist<-mcp_2019_tannins_mg_per_g_berry_wt %>%
  filter(treatment == 2)
hist(mcp_2019_tannins_mg_per_g_berry_wt_hist$Total_tannin_mg_g_berry_weight)

mcp_2019_tannins_mg_per_g_berry_wt_hist<-mcp_2019_tannins_mg_per_g_berry_wt %>%
  filter(treatment == 3)
hist(mcp_2019_tannins_mg_per_g_berry_wt_hist$Total_tannin_mg_g_berry_weight)

#### somers Total anthocyanins anova mg/berry ####

somers_2019<- read.csv("data/somers_berry_phenolics.csv", header =TRUE)

#### Aug 8 ####

somers_2019_antho_mg_berry <- somers_2019%>%
  filter(Date_sampled == "8/8/2019") %>%
  filter(!Block_id == "B1R2") %>%
  select(Date_sampled, total_Antho_mg_berry, treatment) 

str(somers_2019_antho_mg_berry)

somers_2019_antho_mg_berry %>%
  group_by(treatment)%>%
  tally()

is.factor(somers_2019_antho_mg_berry$treatment)

somers_2019_antho_mg_berry$treatment<- format(somers_2019_antho_mg_berry$treatment)
somers_2019_antho_mg_berry$treatment<- as.factor(somers_2019_antho_mg_berry$treatment)

is.factor(somers_2019_antho_mg_berry$treatment)


str(somers_2019_antho_mg_berry)

ggplot(somers_2019_antho_mg_berry, aes (treatment,total_Antho_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_antho_mg_berry<- aov (total_Antho_mg_berry~treatment, somers_2019_antho_mg_berry )
summary (anova_antho_mg_berry)

(tukey_antho_mg_berry<- HSD.test(anova_antho_mg_berry, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

somers_2019_antho_mg_berry_hist<-somers_2019_antho_mg_berry %>%
  filter(treatment == 1)
hist(somers_2019_antho_mg_berry_hist$total_Antho_mg_berry)

somers_2019_antho_mg_berry_hist<-somers_2019_antho_mg_berry %>%
  filter(treatment == 2)
hist(somers_2019_antho_mg_berry_hist$total_Antho_mg_berry)

somers_2019_antho_mg_berry_hist<-somers_2019_antho_mg_berry %>%
  filter(treatment == 3)
hist(somers_2019_antho_mg_berry_hist$total_Antho_mg_berry)

#### Aug 15 ####

somers_2019_antho_mg_berry <- somers_2019%>%
  filter(Date_sampled == "8/15/2019") %>%
  filter(!Block_id == "B1R2") %>%
  select(Date_sampled, total_Antho_mg_berry, treatment) 

str(somers_2019_antho_mg_berry)

somers_2019_antho_mg_berry %>%
  group_by(treatment)%>%
  tally()

is.factor(somers_2019_antho_mg_berry$treatment)

somers_2019_antho_mg_berry$treatment<- format(somers_2019_antho_mg_berry$treatment)
somers_2019_antho_mg_berry$treatment<- as.factor(somers_2019_antho_mg_berry$treatment)

is.factor(somers_2019_antho_mg_berry$treatment)


str(somers_2019_antho_mg_berry)

ggplot(somers_2019_antho_mg_berry, aes (treatment,total_Antho_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_antho_mg_berry<- aov (total_Antho_mg_berry~treatment, somers_2019_antho_mg_berry )
summary (anova_antho_mg_berry)

(tukey_antho_mg_berry<- HSD.test(anova_antho_mg_berry, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

somers_2019_antho_mg_berry_hist<-somers_2019_antho_mg_berry %>%
  filter(treatment == 1)
hist(somers_2019_antho_mg_berry_hist$total_Antho_mg_berry)

somers_2019_antho_mg_berry_hist<-somers_2019_antho_mg_berry %>%
  filter(treatment == 2)
hist(somers_2019_antho_mg_berry_hist$total_Antho_mg_berry)

somers_2019_antho_mg_berry_hist<-somers_2019_antho_mg_berry %>%
  filter(treatment == 3)
hist(somers_2019_antho_mg_berry_hist$total_Antho_mg_berry)

#### Aug 20 ####

somers_2019_antho_mg_berry <- somers_2019%>%
  filter(Date_sampled == "8/20/2019") %>%
  filter(!Block_id == "B1R2") %>%
  select(Date_sampled, total_Antho_mg_berry, treatment) 

str(somers_2019_antho_mg_berry)

somers_2019_antho_mg_berry %>%
  group_by(treatment)%>%
  tally()

is.factor(somers_2019_antho_mg_berry$treatment)

somers_2019_antho_mg_berry$treatment<- format(somers_2019_antho_mg_berry$treatment)
somers_2019_antho_mg_berry$treatment<- as.factor(somers_2019_antho_mg_berry$treatment)

is.factor(somers_2019_antho_mg_berry$treatment)


str(somers_2019_antho_mg_berry)

ggplot(somers_2019_antho_mg_berry, aes (treatment,total_Antho_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_antho_mg_berry<- aov (total_Antho_mg_berry~treatment, somers_2019_antho_mg_berry )
summary (anova_antho_mg_berry)

(tukey_antho_mg_berry<- HSD.test(anova_antho_mg_berry, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

somers_2019_antho_mg_berry_hist<-somers_2019_antho_mg_berry %>%
  filter(treatment == 1)
hist(somers_2019_antho_mg_berry_hist$total_Antho_mg_berry)

somers_2019_antho_mg_berry_hist<-somers_2019_antho_mg_berry %>%
  filter(treatment == 2)
hist(somers_2019_antho_mg_berry_hist$total_Antho_mg_berry)

somers_2019_antho_mg_berry_hist<-somers_2019_antho_mg_berry %>%
  filter(treatment == 3)
hist(somers_2019_antho_mg_berry_hist$total_Antho_mg_berry)

#### Sept 03 ####

somers_2019_antho_mg_berry <- somers_2019%>%
  filter(Date_sampled == "9/3/2019") %>%
  filter(!Block_id == "B1R2") %>%
  select(Date_sampled, total_Antho_mg_berry, treatment) 

str(somers_2019_antho_mg_berry)

somers_2019_antho_mg_berry %>%
  group_by(treatment)%>%
  tally()

is.factor(somers_2019_antho_mg_berry$treatment)

somers_2019_antho_mg_berry$treatment<- format(somers_2019_antho_mg_berry$treatment)
somers_2019_antho_mg_berry$treatment<- as.factor(somers_2019_antho_mg_berry$treatment)

is.factor(somers_2019_antho_mg_berry$treatment)


str(somers_2019_antho_mg_berry)

ggplot(somers_2019_antho_mg_berry, aes (treatment,total_Antho_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_antho_mg_berry<- aov (total_Antho_mg_berry~treatment, somers_2019_antho_mg_berry )
summary (anova_antho_mg_berry)

(tukey_antho_mg_berry<- HSD.test(anova_antho_mg_berry, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

somers_2019_antho_mg_berry_hist<-somers_2019_antho_mg_berry %>%
  filter(treatment == 1)
hist(somers_2019_antho_mg_berry_hist$total_Antho_mg_berry)

somers_2019_antho_mg_berry_hist<-somers_2019_antho_mg_berry %>%
  filter(treatment == 2)
hist(somers_2019_antho_mg_berry_hist$total_Antho_mg_berry)

somers_2019_antho_mg_berry_hist<-somers_2019_antho_mg_berry %>%
  filter(treatment == 3)
hist(somers_2019_antho_mg_berry_hist$total_Antho_mg_berry)

#### Harvest point sept 12 and 19 #####

somers_2019_antho_mg_berry <- somers_2019%>%
  filter(Date_sampled == "9/12/2019" | Date_sampled == "9/19/2019") %>%
  filter(!Block_id == "B1R2") %>%
  select(Date_sampled, total_Antho_mg_berry, treatment) 

str(somers_2019_antho_mg_berry)

somers_2019_antho_mg_berry %>%
  group_by(treatment)%>%
  tally()

is.factor(somers_2019_antho_mg_berry$treatment)

somers_2019_antho_mg_berry$treatment<- format(somers_2019_antho_mg_berry$treatment)
somers_2019_antho_mg_berry$treatment<- as.factor(somers_2019_antho_mg_berry$treatment)

is.factor(somers_2019_antho_mg_berry$treatment)


str(somers_2019_antho_mg_berry)

ggplot(somers_2019_antho_mg_berry, aes (treatment,total_Antho_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_antho_mg_berry<- aov (total_Antho_mg_berry~treatment, somers_2019_antho_mg_berry )
summary (anova_antho_mg_berry)

(tukey_antho_mg_berry<- HSD.test(anova_antho_mg_berry, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

#### Differences with tuey alpha 0.05 no wiwhen running anova with 0.05###

somers_2019_antho_mg_berry_hist<-somers_2019_antho_mg_berry %>%
  filter(treatment == 1)
hist(somers_2019_antho_mg_berry_hist$total_Antho_mg_berry)

somers_2019_antho_mg_berry_hist<-somers_2019_antho_mg_berry %>%
  filter(treatment == 2)
hist(somers_2019_antho_mg_berry_hist$total_Antho_mg_berry)

somers_2019_antho_mg_berry_hist<-somers_2019_antho_mg_berry %>%
  filter(treatment == 3)
hist(somers_2019_antho_mg_berry_hist$total_Antho_mg_berry)

#### Total antho mg/g berry wt ANOVA ####

#### Aug 8 ####

somers_2019_antho_mg_g_berry_wt <- somers_2019%>%
  filter(Date_sampled == "8/8/2019") %>%
  filter(!Block_id == "B1R2") %>%
  select(Date_sampled, total_antho_mg_g_berry_weight, treatment) 

str(somers_2019_antho_mg_g_berry_wt)

somers_2019_antho_mg_g_berry_wt %>%
  group_by(treatment)%>%
  tally()

is.factor(somers_2019_antho_mg_g_berry_wt$treatment)

somers_2019_antho_mg_g_berry_wt$treatment<- format(somers_2019_antho_mg_g_berry_wt$treatment)
somers_2019_antho_mg_g_berry_wt$treatment<- as.factor(somers_2019_antho_mg_g_berry_wt$treatment)

is.factor(somers_2019_antho_mg_g_berry_wt$treatment)


str(somers_2019_antho_mg_g_berry_wt)

ggplot(somers_2019_antho_mg_g_berry_wt, aes (treatment,total_antho_mg_g_berry_weight, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_antho_mg_g_berry_wt<- aov (total_antho_mg_g_berry_weight~treatment, somers_2019_antho_mg_g_berry_wt )
summary (anova_antho_mg_g_berry_wt)

(tukey_antho_mg_g_berry_wt<- HSD.test(anova_antho_mg_g_berry_wt, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

somers_2019_antho_mg_g_berry_wt_hist<-somers_2019_antho_mg_g_berry_wt %>%
  filter(treatment == 1)
hist(somers_2019_antho_mg_g_berry_wt_hist$total_antho_mg_g_berry_weight)

somers_2019_antho_mg_g_berry_wt_hist<-somers_2019_antho_mg_g_berry_wt %>%
  filter(treatment == 2)
hist(somers_2019_antho_mg_g_berry_wt_hist$total_antho_mg_g_berry_weight)

somers_2019_antho_mg_g_berry_wt_hist<-somers_2019_antho_mg_g_berry_wt %>%
  filter(treatment == 3)
hist(somers_2019_antho_mg_g_berry_wt_hist$total_antho_mg_g_berry_weight)

#### Aug 15 ####

somers_2019_antho_mg_g_berry_wt <- somers_2019%>%
  filter(Date_sampled == "8/15/2019") %>%
  filter(!Block_id == "B1R2") %>%
  select(Date_sampled, total_antho_mg_g_berry_weight, treatment) 

str(somers_2019_antho_mg_g_berry_wt)

somers_2019_antho_mg_g_berry_wt %>%
  group_by(treatment)%>%
  tally()

is.factor(somers_2019_antho_mg_g_berry_wt$treatment)

somers_2019_antho_mg_g_berry_wt$treatment<- format(somers_2019_antho_mg_g_berry_wt$treatment)
somers_2019_antho_mg_g_berry_wt$treatment<- as.factor(somers_2019_antho_mg_g_berry_wt$treatment)

is.factor(somers_2019_antho_mg_g_berry_wt$treatment)


str(somers_2019_antho_mg_g_berry_wt)

ggplot(somers_2019_antho_mg_g_berry_wt, aes (treatment,total_antho_mg_g_berry_weight, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_antho_mg_g_berry_wt<- aov (total_antho_mg_g_berry_weight~treatment, somers_2019_antho_mg_g_berry_wt )
summary (anova_antho_mg_g_berry_wt)

(tukey_antho_mg_g_berry_wt<- HSD.test(anova_antho_mg_g_berry_wt, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

somers_2019_antho_mg_g_berry_wt_hist<-somers_2019_antho_mg_g_berry_wt %>%
  filter(treatment == 1)
hist(somers_2019_antho_mg_g_berry_wt_hist$total_antho_mg_g_berry_weight)

somers_2019_antho_mg_g_berry_wt_hist<-somers_2019_antho_mg_g_berry_wt %>%
  filter(treatment == 2)
hist(somers_2019_antho_mg_g_berry_wt_hist$total_antho_mg_g_berry_weight)

somers_2019_antho_mg_g_berry_wt_hist<-somers_2019_antho_mg_g_berry_wt %>%
  filter(treatment == 3)
hist(somers_2019_antho_mg_g_berry_wt_hist$total_antho_mg_g_berry_weight)

#### Aug 20 ####

somers_2019_antho_mg_g_berry_wt <- somers_2019%>%
  filter(Date_sampled == "8/20/2019") %>%
  filter(!Block_id == "B1R2") %>%
  select(Date_sampled, total_antho_mg_g_berry_weight, treatment) 

str(somers_2019_antho_mg_g_berry_wt)

somers_2019_antho_mg_g_berry_wt %>%
  group_by(treatment)%>%
  tally()

is.factor(somers_2019_antho_mg_g_berry_wt$treatment)

somers_2019_antho_mg_g_berry_wt$treatment<- format(somers_2019_antho_mg_g_berry_wt$treatment)
somers_2019_antho_mg_g_berry_wt$treatment<- as.factor(somers_2019_antho_mg_g_berry_wt$treatment)

is.factor(somers_2019_antho_mg_g_berry_wt$treatment)


str(somers_2019_antho_mg_g_berry_wt)

ggplot(somers_2019_antho_mg_g_berry_wt, aes (treatment,total_antho_mg_g_berry_weight, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_antho_mg_g_berry_wt<- aov (total_antho_mg_g_berry_weight~treatment, somers_2019_antho_mg_g_berry_wt )
summary (anova_antho_mg_g_berry_wt)

(tukey_antho_mg_g_berry_wt<- HSD.test(anova_antho_mg_g_berry_wt, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

somers_2019_antho_mg_g_berry_wt_hist<-somers_2019_antho_mg_g_berry_wt %>%
  filter(treatment == 1)
hist(somers_2019_antho_mg_g_berry_wt_hist$total_antho_mg_g_berry_weight)

somers_2019_antho_mg_g_berry_wt_hist<-somers_2019_antho_mg_g_berry_wt %>%
  filter(treatment == 2)
hist(somers_2019_antho_mg_g_berry_wt_hist$total_antho_mg_g_berry_weight)

somers_2019_antho_mg_g_berry_wt_hist<-somers_2019_antho_mg_g_berry_wt %>%
  filter(treatment == 3)
hist(somers_2019_antho_mg_g_berry_wt_hist$total_antho_mg_g_berry_weight)

#### Sept 03 ####

somers_2019_antho_mg_g_berry_wt <- somers_2019%>%
  filter(Date_sampled == "9/3/2019") %>%
  filter(!Block_id == "B1R2") %>%
  select(Date_sampled, total_antho_mg_g_berry_weight, treatment) 

str(somers_2019_antho_mg_g_berry_wt)

somers_2019_antho_mg_g_berry_wt %>%
  group_by(treatment)%>%
  tally()

is.factor(somers_2019_antho_mg_g_berry_wt$treatment)

somers_2019_antho_mg_g_berry_wt$treatment<- format(somers_2019_antho_mg_g_berry_wt$treatment)
somers_2019_antho_mg_g_berry_wt$treatment<- as.factor(somers_2019_antho_mg_g_berry_wt$treatment)

is.factor(somers_2019_antho_mg_g_berry_wt$treatment)


str(somers_2019_antho_mg_g_berry_wt)

ggplot(somers_2019_antho_mg_g_berry_wt, aes (treatment,total_antho_mg_g_berry_weight, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_antho_mg_g_berry_wt<- aov (total_antho_mg_g_berry_weight~treatment, somers_2019_antho_mg_g_berry_wt )
summary (anova_antho_mg_g_berry_wt)

(tukey_antho_mg_g_berry_wt<- HSD.test(anova_antho_mg_g_berry_wt, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

somers_2019_antho_mg_g_berry_wt_hist<-somers_2019_antho_mg_g_berry_wt %>%
  filter(treatment == 1)
hist(somers_2019_antho_mg_g_berry_wt_hist$total_antho_mg_g_berry_weight)

somers_2019_antho_mg_g_berry_wt_hist<-somers_2019_antho_mg_g_berry_wt %>%
  filter(treatment == 2)
hist(somers_2019_antho_mg_g_berry_wt_hist$total_antho_mg_g_berry_weight)

somers_2019_antho_mg_g_berry_wt_hist<-somers_2019_antho_mg_g_berry_wt %>%
  filter(treatment == 3)
hist(somers_2019_antho_mg_g_berry_wt_hist$total_antho_mg_g_berry_weight)

#### Harvest point sept 12 and 19 #####

somers_2019_antho_mg_g_berry_wt <- somers_2019%>%
  filter(Date_sampled == "9/12/2019" | Date_sampled == "9/19/2019") %>%
  filter(!Block_id == "B1R2") %>%
  select(Date_sampled, total_antho_mg_g_berry_weight, treatment) 

str(somers_2019_antho_mg_g_berry_wt)

somers_2019_antho_mg_g_berry_wt %>%
  group_by(treatment)%>%
  tally()

is.factor(somers_2019_antho_mg_g_berry_wt$treatment)

somers_2019_antho_mg_g_berry_wt$treatment<- format(somers_2019_antho_mg_g_berry_wt$treatment)
somers_2019_antho_mg_g_berry_wt$treatment<- as.factor(somers_2019_antho_mg_g_berry_wt$treatment)

is.factor(somers_2019_antho_mg_g_berry_wt$treatment)


str(somers_2019_antho_mg_g_berry_wt)

ggplot(somers_2019_antho_mg_g_berry_wt, aes (treatment,total_antho_mg_g_berry_weight, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_antho_mg_g_berry_wt<- aov (total_antho_mg_g_berry_weight~treatment, somers_2019_antho_mg_g_berry_wt )
summary (anova_antho_mg_g_berry_wt)

(tukey_antho_mg_g_berry_wt<- HSD.test(anova_antho_mg_g_berry_wt, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

somers_2019_antho_mg_g_berry_wt_hist<-somers_2019_antho_mg_g_berry_wt %>%
  filter(treatment == 1)
hist(somers_2019_antho_mg_g_berry_wt_hist$total_antho_mg_g_berry_weight)

somers_2019_antho_mg_g_berry_wt_hist<-somers_2019_antho_mg_g_berry_wt %>%
  filter(treatment == 2)
hist(somers_2019_antho_mg_g_berry_wt_hist$total_antho_mg_g_berry_weight)

somers_2019_antho_mg_g_berry_wt_hist<-somers_2019_antho_mg_g_berry_wt %>%
  filter(treatment == 3)
hist(somers_2019_antho_mg_g_berry_wt_hist$total_antho_mg_g_berry_weight)


##### berry weight ANOVA #####
berry_weight_2019<- read.csv("data/mcp_berry_phenolics.csv", header =TRUE)

berry_weight_2019 <- berry_weight_2019%>%
  mutate(berry_weight_g_berry  = (Berry_weight/60))

#### Aug 8 ####

berry_weight_2019_grouped <- berry_weight_2019%>%
  filter(Date_sampled == "8/8/2019") %>%
  filter(!Block_id == "B1R2") %>%
  select(Date_sampled, berry_weight_g_berry, treatment)
str(berry_weight_2019_grouped )

berry_weight_2019_grouped  %>%
  group_by(treatment)%>%
  tally()

is.factor(berry_weight_2019_grouped $treatment)

berry_weight_2019_grouped $treatment<- format(berry_weight_2019_grouped $treatment)
berry_weight_2019_grouped $treatment<- as.factor(berry_weight_2019_grouped $treatment)

is.factor(berry_weight_2019_grouped $treatment)


str(berry_weight_2019_grouped )

ggplot(berry_weight_2019_grouped , aes (treatment, berry_weight_g_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_berry_weight<- aov (berry_weight_g_berry~treatment, berry_weight_2019_grouped  )
summary (anova_berry_weight)


(tukey_berry_wt<-HSD.test(anova_berry_weight, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

berry_weight_2019_grouped_hist<-berry_weight_2019_grouped %>%
  filter(treatment == 1)
hist(berry_weight_2019_grouped_hist$berry_weight_g_berry)

berry_weight_2019_grouped_hist<-berry_weight_2019_grouped %>%
  filter(treatment == 2)
hist(berry_weight_2019_grouped_hist$berry_weight_g_berry)

berry_weight_2019_grouped_hist<-berry_weight_2019_grouped %>%
  filter(treatment == 3)
hist(berry_weight_2019_grouped_hist$berry_weight_g_berry)


#### Aug 15 #####

berry_weight_2019_grouped <- berry_weight_2019%>%
  filter(Date_sampled == "8/15/2019") %>%
  filter(!Block_id == "B1R2") %>%
  select(Date_sampled, berry_weight_g_berry, treatment)
str(berry_weight_2019_grouped )

berry_weight_2019_grouped  %>%
  group_by(treatment)%>%
  tally()

is.factor(berry_weight_2019_grouped $treatment)

berry_weight_2019_grouped $treatment<- format(berry_weight_2019_grouped $treatment)
berry_weight_2019_grouped $treatment<- as.factor(berry_weight_2019_grouped $treatment)

is.factor(berry_weight_2019_grouped $treatment)


str(berry_weight_2019_grouped )

ggplot(berry_weight_2019_grouped , aes (treatment, berry_weight_g_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_berry_weight<- aov (berry_weight_g_berry~treatment, berry_weight_2019_grouped  )
summary (anova_berry_weight)


(tukey_berry_wt<-HSD.test(anova_berry_weight, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

berry_weight_2019_grouped_hist<-berry_weight_2019_grouped %>%
  filter(treatment == 1)
hist(berry_weight_2019_grouped_hist$berry_weight_g_berry)

berry_weight_2019_grouped_hist<-berry_weight_2019_grouped %>%
  filter(treatment == 2)
hist(berry_weight_2019_grouped_hist$berry_weight_g_berry)

berry_weight_2019_grouped_hist<-berry_weight_2019_grouped %>%
  filter(treatment == 3)
hist(berry_weight_2019_grouped_hist$berry_weight_g_berry)

#### Aug 20 ####


berry_weight_2019_grouped <- berry_weight_2019%>%
  filter(Date_sampled == "8/20/2019") %>%
  filter(!Block_id == "B1R2") %>%
  select(Date_sampled, berry_weight_g_berry, treatment)
str(berry_weight_2019_grouped )

berry_weight_2019_grouped  %>%
  group_by(treatment)%>%
  tally()

is.factor(berry_weight_2019_grouped $treatment)

berry_weight_2019_grouped $treatment<- format(berry_weight_2019_grouped $treatment)
berry_weight_2019_grouped $treatment<- as.factor(berry_weight_2019_grouped $treatment)

is.factor(berry_weight_2019_grouped $treatment)


str(berry_weight_2019_grouped )

ggplot(berry_weight_2019_grouped , aes (treatment, berry_weight_g_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_berry_weight<- aov (berry_weight_g_berry~treatment, berry_weight_2019_grouped  )
summary (anova_berry_weight)


(tukey_berry_wt<-HSD.test(anova_berry_weight, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

berry_weight_2019_grouped_hist<-berry_weight_2019_grouped %>%
  filter(treatment == 1)
hist(berry_weight_2019_grouped_hist$berry_weight_g_berry)

berry_weight_2019_grouped_hist<-berry_weight_2019_grouped %>%
  filter(treatment == 2)
hist(berry_weight_2019_grouped_hist$berry_weight_g_berry)

berry_weight_2019_grouped_hist<-berry_weight_2019_grouped %>%
  filter(treatment == 3)
hist(berry_weight_2019_grouped_hist$berry_weight_g_berry)

#### Sept 03 ####

berry_weight_2019_grouped <- berry_weight_2019%>%
  filter(Date_sampled == "9/3/2019") %>%
  filter(!Block_id == "B1R2") %>%
  select(Date_sampled, berry_weight_g_berry, treatment)
str(berry_weight_2019_grouped )

berry_weight_2019_grouped  %>%
  group_by(treatment)%>%
  tally()

is.factor(berry_weight_2019_grouped $treatment)

berry_weight_2019_grouped $treatment<- format(berry_weight_2019_grouped $treatment)
berry_weight_2019_grouped $treatment<- as.factor(berry_weight_2019_grouped $treatment)

is.factor(berry_weight_2019_grouped $treatment)


str(berry_weight_2019_grouped )

ggplot(berry_weight_2019_grouped , aes (treatment, berry_weight_g_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_berry_weight<- aov (berry_weight_g_berry~treatment, berry_weight_2019_grouped  )
summary (anova_berry_weight)


(tukey_berry_wt<-HSD.test(anova_berry_weight, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

berry_weight_2019_grouped_hist<-berry_weight_2019_grouped %>%
  filter(treatment == 1)
hist(berry_weight_2019_grouped_hist$berry_weight_g_berry)

berry_weight_2019_grouped_hist<-berry_weight_2019_grouped %>%
  filter(treatment == 2)
hist(berry_weight_2019_grouped_hist$berry_weight_g_berry)

berry_weight_2019_grouped_hist<-berry_weight_2019_grouped %>%
  filter(treatment == 3)
hist(berry_weight_2019_grouped_hist$berry_weight_g_berry)

#### Harvest point sept 12 and 19 ####

berry_weight_2019_grouped <- berry_weight_2019%>%
  filter(Date_sampled == "9/12/2019" | Date_sampled == "9/19/2019") %>%
  filter(!Block_id == "B1R2") %>%
  select(Date_sampled, berry_weight_g_berry, treatment)
str(berry_weight_2019_grouped )

berry_weight_2019_grouped  %>%
  group_by(treatment)%>%
  tally()

is.factor(berry_weight_2019_grouped $treatment)

berry_weight_2019_grouped $treatment<- format(berry_weight_2019_grouped $treatment)
berry_weight_2019_grouped $treatment<- as.factor(berry_weight_2019_grouped $treatment)

is.factor(berry_weight_2019_grouped $treatment)


str(berry_weight_2019_grouped )

ggplot(berry_weight_2019_grouped , aes (treatment, berry_weight_g_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_berry_weight<- aov (berry_weight_g_berry~treatment, berry_weight_2019_grouped  )
summary (anova_berry_weight)


(tukey_berry_wt<-HSD.test(anova_berry_weight, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

berry_weight_2019_grouped_hist<-berry_weight_2019_grouped %>%
  filter(treatment == 1)
hist(berry_weight_2019_grouped_hist$berry_weight_g_berry)

berry_weight_2019_grouped_hist<-berry_weight_2019_grouped %>%
  filter(treatment == 2)
hist(berry_weight_2019_grouped_hist$berry_weight_g_berry)

berry_weight_2019_grouped_hist<-berry_weight_2019_grouped %>%
  filter(treatment == 3)
hist(berry_weight_2019_grouped_hist$berry_weight_g_berry)


####monomeric phenolics anova ####


monomeric_antho_no_leak<- read.csv("data/monomeric_antho_hplc_harvest_and_first_point.csv", header =TRUE)

#### Harvest ####

###### D3G mb/berry ####

monomeric_antho_no_leak_2019_harvest <- monomeric_antho_no_leak%>%
  filter(Set == "Harvest") %>%
  filter(!ï..Sample == "B1R2") %>%
  select(dephiglu_mg_berry, treatment) 

str(monomeric_antho_no_leak_2019_harvest)

monomeric_antho_no_leak_2019_harvest %>%
  group_by(treatment)%>%
  tally()

is.factor(monomeric_antho_no_leak_2019_harvest$treatment)

monomeric_antho_no_leak_2019_harvest$treatment<- format(monomeric_antho_no_leak_2019_harvest$treatment)
monomeric_antho_no_leak_2019_harvest$treatment<- as.factor(monomeric_antho_no_leak_2019_harvest$treatment)

is.factor(monomeric_antho_no_leak_2019_harvest$treatment)


str(monomeric_antho_no_leak_2019_harvest)

ggplot(monomeric_antho_no_leak_2019_harvest, aes (treatment,dephiglu_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_mono_antho <- aov (dephiglu_mg_berry~treatment, monomeric_antho_no_leak_2019_harvest )
summary (anova_mono_antho)


(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_harvest_hist$dephiglu_mg_berry)

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_harvest_hist$dephiglu_mg_berry)

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_harvest_hist$dephiglu_mg_berry)

#### C3G mb/berry Harvest ####

monomeric_antho_no_leak_2019_harvest <- monomeric_antho_no_leak%>%
  filter(Set == "Harvest") %>%
  filter(!ï..Sample == "B1R2") %>%
  select(cyaniglu_mg_berry, treatment) 

str(monomeric_antho_no_leak_2019_harvest)

monomeric_antho_no_leak_2019_harvest %>%
  group_by(treatment)%>%
  tally()

is.factor(monomeric_antho_no_leak_2019_harvest$treatment)

monomeric_antho_no_leak_2019_harvest$treatment<- format(monomeric_antho_no_leak_2019_harvest$treatment)
monomeric_antho_no_leak_2019_harvest$treatment<- as.factor(monomeric_antho_no_leak_2019_harvest$treatment)

is.factor(monomeric_antho_no_leak_2019_harvest$treatment)


str(monomeric_antho_no_leak_2019_harvest)

ggplot(monomeric_antho_no_leak_2019_harvest, aes (treatment,cyaniglu_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_mono_antho <- aov (cyaniglu_mg_berry~treatment, monomeric_antho_no_leak_2019_harvest )
summary (anova_mono_antho)


(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_harvest_hist$cyaniglu_mg_berry)

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_harvest_hist$cyaniglu_mg_berry)

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_harvest_hist$cyaniglu_mg_berry)

#### Pet3G Harvest mg/berry #####

monomeric_antho_no_leak_2019_harvest <- monomeric_antho_no_leak%>%
  filter(Set == "Harvest") %>%
  filter(!ï..Sample == "B1R2") %>%
  select(petuglu_mg_berry, treatment) 

str(monomeric_antho_no_leak_2019_harvest)

monomeric_antho_no_leak_2019_harvest %>%
  group_by(treatment)%>%
  tally()

is.factor(monomeric_antho_no_leak_2019_harvest$treatment)

monomeric_antho_no_leak_2019_harvest$treatment<- format(monomeric_antho_no_leak_2019_harvest$treatment)
monomeric_antho_no_leak_2019_harvest$treatment<- as.factor(monomeric_antho_no_leak_2019_harvest$treatment)

is.factor(monomeric_antho_no_leak_2019_harvest$treatment)


str(monomeric_antho_no_leak_2019_harvest)

ggplot(monomeric_antho_no_leak_2019_harvest, aes (treatment,petuglu_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_mono_antho <- aov (petuglu_mg_berry~treatment, monomeric_antho_no_leak_2019_harvest )
summary (anova_mono_antho)


(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_harvest_hist$petuglu_mg_berry)

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_harvest_hist$petuglu_mg_berry)

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_harvest_hist$petuglu_mg_berry)

#### Peo3G Harvest ####

monomeric_antho_no_leak_2019_harvest <- monomeric_antho_no_leak%>%
  filter(Set == "Harvest") %>%
  filter(!ï..Sample == "B1R2") %>%
  select(peoni_glu_mg_berry, treatment) 

str(monomeric_antho_no_leak_2019_harvest)

monomeric_antho_no_leak_2019_harvest %>%
  group_by(treatment)%>%
  tally()

is.factor(monomeric_antho_no_leak_2019_harvest$treatment)

monomeric_antho_no_leak_2019_harvest$treatment<- format(monomeric_antho_no_leak_2019_harvest$treatment)
monomeric_antho_no_leak_2019_harvest$treatment<- as.factor(monomeric_antho_no_leak_2019_harvest$treatment)

is.factor(monomeric_antho_no_leak_2019_harvest$treatment)


str(monomeric_antho_no_leak_2019_harvest)

ggplot(monomeric_antho_no_leak_2019_harvest, aes (treatment,peoni_glu_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_mono_antho <- aov (peoni_glu_mg_berry~treatment, monomeric_antho_no_leak_2019_harvest )
summary (anova_mono_antho)


(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_harvest_hist$peoni_glu_mg_berry)

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_harvest_hist$peoni_glu_mg_berry)

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_harvest_hist$peoni_glu_mg_berry)

#### M3G mb/berry Harvest #####

monomeric_antho_no_leak_2019_harvest <- monomeric_antho_no_leak%>%
  filter(Set == "Harvest") %>%
  filter(!ï..Sample == "B1R2") %>%
  select(malvi_glu_mg_berry, treatment) 

str(monomeric_antho_no_leak_2019_harvest)

monomeric_antho_no_leak_2019_harvest %>%
  group_by(treatment)%>%
  tally()

is.factor(monomeric_antho_no_leak_2019_harvest$treatment)

monomeric_antho_no_leak_2019_harvest$treatment<- format(monomeric_antho_no_leak_2019_harvest$treatment)
monomeric_antho_no_leak_2019_harvest$treatment<- as.factor(monomeric_antho_no_leak_2019_harvest$treatment)

is.factor(monomeric_antho_no_leak_2019_harvest$treatment)


str(monomeric_antho_no_leak_2019_harvest)

ggplot(monomeric_antho_no_leak_2019_harvest, aes (treatment,malvi_glu_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (malvi_glu_mg_berry~treatment, monomeric_antho_no_leak_2019_harvest )
summary (anova_mono_antho)


(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_harvest_hist$malvi_glu_mg_berry)

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_harvest_hist$malvi_glu_mg_berry)

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_harvest_hist$malvi_glu_mg_berry)

#### D3Gac Harvest ####
monomeric_antho_no_leak_2019_harvest <- monomeric_antho_no_leak%>%
  filter(Set == "Harvest") %>%
  filter(!ï..Sample == "B1R2") %>%
  select(delphi_ac_mg_berry, treatment) 

str(monomeric_antho_no_leak_2019_harvest)

monomeric_antho_no_leak_2019_harvest %>%
  group_by(treatment)%>%
  tally()

is.factor(monomeric_antho_no_leak_2019_harvest$treatment)

monomeric_antho_no_leak_2019_harvest$treatment<- format(monomeric_antho_no_leak_2019_harvest$treatment)
monomeric_antho_no_leak_2019_harvest$treatment<- as.factor(monomeric_antho_no_leak_2019_harvest$treatment)

is.factor(monomeric_antho_no_leak_2019_harvest$treatment)


str(monomeric_antho_no_leak_2019_harvest)

ggplot(monomeric_antho_no_leak_2019_harvest, aes (treatment,delphi_ac_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (delphi_ac_mg_berry~treatment, monomeric_antho_no_leak_2019_harvest )
summary (anova_mono_antho)


(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_harvest_hist$delphi_ac_mg_berry)

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_harvest_hist$delphi_ac_mg_berry)

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_harvest_hist$delphi_ac_mg_berry)


#### C3Gac Harvest ####

monomeric_antho_no_leak_2019_harvest <- monomeric_antho_no_leak%>%
  filter(Set == "Harvest") %>%
  filter(!ï..Sample == "B1R2") %>%
  select(cyani_ac_mg_berry, treatment) 

str(monomeric_antho_no_leak_2019_harvest)

monomeric_antho_no_leak_2019_harvest %>%
  group_by(treatment)%>%
  tally()

is.factor(monomeric_antho_no_leak_2019_harvest$treatment)

monomeric_antho_no_leak_2019_harvest$treatment<- format(monomeric_antho_no_leak_2019_harvest$treatment)
monomeric_antho_no_leak_2019_harvest$treatment<- as.factor(monomeric_antho_no_leak_2019_harvest$treatment)

is.factor(monomeric_antho_no_leak_2019_harvest$treatment)


str(monomeric_antho_no_leak_2019_harvest)

ggplot(monomeric_antho_no_leak_2019_harvest, aes (treatment,cyani_ac_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (cyani_ac_mg_berry~treatment, monomeric_antho_no_leak_2019_harvest )
summary (anova_mono_antho)

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05))

#### Differences whether if it is unbalanced or not 

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_harvest_hist$cyani_ac_mg_berry)

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_harvest_hist$cyani_ac_mg_berry)

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_harvest_hist$cyani_ac_mg_berry)

#### Pet3Gac Harvest mg/berry ####

monomeric_antho_no_leak_2019_harvest <- monomeric_antho_no_leak%>%
  filter(Set == "Harvest") %>%
  filter(!ï..Sample == "B1R2") %>%
  select(petu_ac_mg_berry, treatment) 

str(monomeric_antho_no_leak_2019_harvest)

monomeric_antho_no_leak_2019_harvest %>%
  group_by(treatment)%>%
  tally()

is.factor(monomeric_antho_no_leak_2019_harvest$treatment)

monomeric_antho_no_leak_2019_harvest$treatment<- format(monomeric_antho_no_leak_2019_harvest$treatment)
monomeric_antho_no_leak_2019_harvest$treatment<- as.factor(monomeric_antho_no_leak_2019_harvest$treatment)

is.factor(monomeric_antho_no_leak_2019_harvest$treatment)


str(monomeric_antho_no_leak_2019_harvest)

ggplot(monomeric_antho_no_leak_2019_harvest, aes (treatment,petu_ac_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (petu_ac_mg_berry~treatment, monomeric_antho_no_leak_2019_harvest )
summary (anova_mono_antho)

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_harvest_hist$petu_ac_mg_berry)

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_harvest_hist$petu_ac_mg_berry)

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_harvest_hist$petu_ac_mg_berry)

#### Peo3Gac mg/berry Harvest ####

monomeric_antho_no_leak_2019_harvest <- monomeric_antho_no_leak%>%
  filter(Set == "Harvest") %>%
  filter(!ï..Sample == "B1R2") %>%
  select(peoni_ac_mg_berry, treatment) 

str(monomeric_antho_no_leak_2019_harvest)

monomeric_antho_no_leak_2019_harvest %>%
  group_by(treatment)%>%
  tally()

is.factor(monomeric_antho_no_leak_2019_harvest$treatment)

monomeric_antho_no_leak_2019_harvest$treatment<- format(monomeric_antho_no_leak_2019_harvest$treatment)
monomeric_antho_no_leak_2019_harvest$treatment<- as.factor(monomeric_antho_no_leak_2019_harvest$treatment)

is.factor(monomeric_antho_no_leak_2019_harvest$treatment)


str(monomeric_antho_no_leak_2019_harvest)

ggplot(monomeric_antho_no_leak_2019_harvest, aes (treatment,peoni_ac_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (peoni_ac_mg_berry~treatment, monomeric_antho_no_leak_2019_harvest )
summary (anova_mono_antho)

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_harvest_hist$peoni_ac_mg_berry)

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_harvest_hist$peoni_ac_mg_berry)

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_harvest_hist$peoni_ac_mg_berry)

#### M3Gac Harvest mb/berry ####

monomeric_antho_no_leak_2019_harvest <- monomeric_antho_no_leak%>%
  filter(Set == "Harvest") %>%
  filter(!ï..Sample == "B1R2") %>%
  select(malvi_ac_mg_berry, treatment) 

str(monomeric_antho_no_leak_2019_harvest)

monomeric_antho_no_leak_2019_harvest %>%
  group_by(treatment)%>%
  tally()

is.factor(monomeric_antho_no_leak_2019_harvest$treatment)

monomeric_antho_no_leak_2019_harvest$treatment<- format(monomeric_antho_no_leak_2019_harvest$treatment)
monomeric_antho_no_leak_2019_harvest$treatment<- as.factor(monomeric_antho_no_leak_2019_harvest$treatment)

is.factor(monomeric_antho_no_leak_2019_harvest$treatment)


str(monomeric_antho_no_leak_2019_harvest)

ggplot(monomeric_antho_no_leak_2019_harvest, aes (treatment,malvi_ac_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (malvi_ac_mg_berry~treatment, monomeric_antho_no_leak_2019_harvest )
summary (anova_mono_antho)

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_harvest_hist$malvi_ac_mg_berry)

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_harvest_hist$malvi_ac_mg_berry)

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_harvest_hist$malvi_ac_mg_berry)

#### Peo3Gcoum Harvest mb/berry ####

monomeric_antho_no_leak_2019_harvest <- monomeric_antho_no_leak%>%
  filter(Set == "Harvest") %>%
  filter(!ï..Sample == "B1R2") %>%
  select(peoni_coum_mg_berry, treatment) 

str(monomeric_antho_no_leak_2019_harvest)

monomeric_antho_no_leak_2019_harvest %>%
  group_by(treatment)%>%
  tally()

is.factor(monomeric_antho_no_leak_2019_harvest$treatment)

monomeric_antho_no_leak_2019_harvest$treatment<- format(monomeric_antho_no_leak_2019_harvest$treatment)
monomeric_antho_no_leak_2019_harvest$treatment<- as.factor(monomeric_antho_no_leak_2019_harvest$treatment)

is.factor(monomeric_antho_no_leak_2019_harvest$treatment)


str(monomeric_antho_no_leak_2019_harvest)

ggplot(monomeric_antho_no_leak_2019_harvest, aes (treatment,peoni_coum_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (peoni_coum_mg_berry~treatment, monomeric_antho_no_leak_2019_harvest )
summary (anova_mono_antho)

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_harvest_hist$peoni_coum_mg_berry)

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_harvest_hist$peoni_coum_mg_berry)

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_harvest_hist$peoni_coum_mg_berry)

####M3Gcoum Harvest mb/berry ####

monomeric_antho_no_leak_2019_harvest <- monomeric_antho_no_leak%>%
  filter(Set == "Harvest") %>%
  filter(!ï..Sample == "B1R2") %>%
  select(malvi_coum_mg_berry, treatment) 

str(monomeric_antho_no_leak_2019_harvest)

monomeric_antho_no_leak_2019_harvest %>%
  group_by(treatment)%>%
  tally()

is.factor(monomeric_antho_no_leak_2019_harvest$treatment)

monomeric_antho_no_leak_2019_harvest$treatment<- format(monomeric_antho_no_leak_2019_harvest$treatment)
monomeric_antho_no_leak_2019_harvest$treatment<- as.factor(monomeric_antho_no_leak_2019_harvest$treatment)

is.factor(monomeric_antho_no_leak_2019_harvest$treatment)


str(monomeric_antho_no_leak_2019_harvest)

ggplot(monomeric_antho_no_leak_2019_harvest, aes (treatment,malvi_coum_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (malvi_coum_mg_berry~treatment, monomeric_antho_no_leak_2019_harvest )
summary (anova_mono_antho)

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_harvest_hist$malvi_coum_mg_berry)

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_harvest_hist$malvi_coum_mg_berry)

monomeric_antho_no_leak_2019_harvest_hist<-monomeric_antho_no_leak_2019_harvest %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_harvest_hist$malvi_coum_mg_berry)

#### ~50% VERAISON ####
monomeric_antho_no_leak<- read.csv("data/monomeric_antho_hplc_harvest_and_first_point.csv", header =TRUE)

###### D3G mb/berry ####

monomeric_antho_no_leak_2019_1stpoint <- monomeric_antho_no_leak %>%
  filter(Set == "1st point") %>%
  filter(!ï..Sample == "B1R2") %>%
  select(dephiglu_mg_berry, treatment) 

str(monomeric_antho_no_leak_2019_1stpoint)

monomeric_antho_no_leak_2019_1stpoint %>%
  group_by(treatment)%>%
  tally()

is.factor(monomeric_antho_no_leak_2019_1stpoint$treatment) 

monomeric_antho_no_leak_2019_1stpoint$treatment<- format(monomeric_antho_no_leak_2019_1stpoint$treatment)
monomeric_antho_no_leak_2019_1stpoint$treatment<- as.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)

is.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)


str(monomeric_antho_no_leak_2019_1stpoint)

ggplot(monomeric_antho_no_leak_2019_1stpoint, aes (treatment,dephiglu_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_mono_antho <- aov (dephiglu_mg_berry~treatment, monomeric_antho_no_leak_2019_1stpoint )
summary (anova_mono_antho)


(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$dephiglu_mg_berry)

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$dephiglu_mg_berry)

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$dephiglu_mg_berry)

#### C3G mb/berry 1stpoint ####

monomeric_antho_no_leak_2019_1stpoint <- monomeric_antho_no_leak%>%
  filter(Set == "1st point") %>%
  filter(!ï..Sample == "B1R2") %>%
  select(cyaniglu_mg_berry, treatment) 

str(monomeric_antho_no_leak_2019_1stpoint)

monomeric_antho_no_leak_2019_1stpoint %>%
  group_by(treatment)%>%
  tally()

is.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)

monomeric_antho_no_leak_2019_1stpoint$treatment<- format(monomeric_antho_no_leak_2019_1stpoint$treatment)
monomeric_antho_no_leak_2019_1stpoint$treatment<- as.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)

is.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)


str(monomeric_antho_no_leak_2019_1stpoint)

ggplot(monomeric_antho_no_leak_2019_1stpoint, aes (treatment,cyaniglu_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_mono_antho <- aov (cyaniglu_mg_berry~treatment, monomeric_antho_no_leak_2019_1stpoint )
summary (anova_mono_antho)


(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$cyaniglu_mg_berry)

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$cyaniglu_mg_berry)

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$cyaniglu_mg_berry)

#### Pet3G 1st point mg/berry #####

monomeric_antho_no_leak_2019_1stpoint <- monomeric_antho_no_leak%>%
  filter(Set == "1st point") %>%
  filter(!ï..Sample == "B1R2") %>%
  select(petuglu_mg_berry, treatment) 

str(monomeric_antho_no_leak_2019_1stpoint)

monomeric_antho_no_leak_2019_1stpoint %>%
  group_by(treatment)%>%
  tally()

is.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)

monomeric_antho_no_leak_2019_1stpoint$treatment<- format(monomeric_antho_no_leak_2019_1stpoint$treatment)
monomeric_antho_no_leak_2019_1stpoint$treatment<- as.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)

is.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)


str(monomeric_antho_no_leak_2019_1stpoint)

ggplot(monomeric_antho_no_leak_2019_1stpoint, aes (treatment,petuglu_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_mono_antho <- aov (petuglu_mg_berry~treatment, monomeric_antho_no_leak_2019_1stpoint )
summary (anova_mono_antho)


(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$petuglu_mg_berry)

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$petuglu_mg_berry)

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$petuglu_mg_berry)

#### Peo3G 1st point ####

monomeric_antho_no_leak_2019_1stpoint <- monomeric_antho_no_leak%>%
  filter(Set == "1st point") %>%
  filter(!ï..Sample == "B1R2") %>%
  select(peoni_glu_mg_berry, treatment) 

str(monomeric_antho_no_leak_2019_1stpoint)

monomeric_antho_no_leak_2019_1stpoint %>%
  group_by(treatment)%>%
  tally()

is.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)

monomeric_antho_no_leak_2019_1stpoint$treatment<- format(monomeric_antho_no_leak_2019_1stpoint$treatment)
monomeric_antho_no_leak_2019_1stpoint$treatment<- as.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)

is.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)


str(monomeric_antho_no_leak_2019_1stpoint)

ggplot(monomeric_antho_no_leak_2019_1stpoint, aes (treatment,peoni_glu_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()


anova_mono_antho <- aov (peoni_glu_mg_berry~treatment, monomeric_antho_no_leak_2019_1stpoint )
summary (anova_mono_antho)


(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$peoni_glu_mg_berry)

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$peoni_glu_mg_berry)

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$peoni_glu_mg_berry)

#### M3G mb/berry 1st point #####

monomeric_antho_no_leak_2019_1stpoint <- monomeric_antho_no_leak%>%
  filter(Set == "1st point") %>%
  filter(!ï..Sample == "B1R2") %>%
  select(malvi_glu_mg_berry, treatment) 

str(monomeric_antho_no_leak_2019_1stpoint)

monomeric_antho_no_leak_2019_1stpoint %>%
  group_by(treatment)%>%
  tally()

is.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)

monomeric_antho_no_leak_2019_1stpoint$treatment<- format(monomeric_antho_no_leak_2019_1stpoint$treatment)
monomeric_antho_no_leak_2019_1stpoint$treatment<- as.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)

is.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)


str(monomeric_antho_no_leak_2019_1stpoint)

ggplot(monomeric_antho_no_leak_2019_1stpoint, aes (treatment,malvi_glu_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (malvi_glu_mg_berry~treatment, monomeric_antho_no_leak_2019_1stpoint )
summary (anova_mono_antho)


(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$malvi_glu_mg_berry)

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$malvi_glu_mg_berry)

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$malvi_glu_mg_berry)

#### D3Gac 1st point ####
monomeric_antho_no_leak_2019_1stpoint <- monomeric_antho_no_leak%>%
  filter(Set == "1st point") %>%
  filter(!ï..Sample == "B1R2") %>%
  select(delphi_ac_mg_berry, treatment) 

str(monomeric_antho_no_leak_2019_1stpoint)

monomeric_antho_no_leak_2019_1stpoint %>%
  group_by(treatment)%>%
  tally()

is.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)

monomeric_antho_no_leak_2019_1stpoint$treatment<- format(monomeric_antho_no_leak_2019_1stpoint$treatment)
monomeric_antho_no_leak_2019_1stpoint$treatment<- as.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)

is.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)


str(monomeric_antho_no_leak_2019_1stpoint)

ggplot(monomeric_antho_no_leak_2019_1stpoint, aes (treatment,delphi_ac_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (delphi_ac_mg_berry~treatment, monomeric_antho_no_leak_2019_1stpoint )
summary (anova_mono_antho)


(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$delphi_ac_mg_berry)

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$delphi_ac_mg_berry)

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$delphi_ac_mg_berry)


#### C3Gac 1st point ####

monomeric_antho_no_leak_2019_1stpoint <- monomeric_antho_no_leak%>%
  filter(Set == "1st point") %>%
  filter(!ï..Sample == "B1R2") %>%
  select(cyani_ac_mg_berry, treatment) 

str(monomeric_antho_no_leak_2019_1stpoint)

monomeric_antho_no_leak_2019_1stpoint %>%
  group_by(treatment)%>%
  tally()

is.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)

monomeric_antho_no_leak_2019_1stpoint$treatment<- format(monomeric_antho_no_leak_2019_1stpoint$treatment)
monomeric_antho_no_leak_2019_1stpoint$treatment<- as.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)

is.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)


str(monomeric_antho_no_leak_2019_1stpoint)

ggplot(monomeric_antho_no_leak_2019_1stpoint, aes (treatment,cyani_ac_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (cyani_ac_mg_berry~treatment, monomeric_antho_no_leak_2019_1stpoint )
summary (anova_mono_antho)


(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$cyani_ac_mg_berry)

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$cyani_ac_mg_berry)

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$cyani_ac_mg_berry)

#### Pet3Gac 1st point mg/berry ####

monomeric_antho_no_leak_2019_1stpoint <- monomeric_antho_no_leak%>%
  filter(Set == "1st point") %>%
  filter(!ï..Sample == "B1R2") %>%
  select(petu_ac_mg_berry, treatment) 

str(monomeric_antho_no_leak_2019_1stpoint)

monomeric_antho_no_leak_2019_1stpoint %>%
  group_by(treatment)%>%
  tally()

is.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)

monomeric_antho_no_leak_2019_1stpoint$treatment<- format(monomeric_antho_no_leak_2019_1stpoint$treatment)
monomeric_antho_no_leak_2019_1stpoint$treatment<- as.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)

is.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)


str(monomeric_antho_no_leak_2019_1stpoint)

ggplot(monomeric_antho_no_leak_2019_1stpoint, aes (treatment,petu_ac_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (petu_ac_mg_berry~treatment, monomeric_antho_no_leak_2019_1stpoint )
summary (anova_mono_antho)

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$petu_ac_mg_berry)

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$petu_ac_mg_berry)

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$petu_ac_mg_berry)

#### Peo3Gac mg/berry 1st point ####

monomeric_antho_no_leak_2019_1stpoint <- monomeric_antho_no_leak%>%
  filter(Set == "1st point") %>%
  filter(!ï..Sample == "B1R2") %>%
  select(peoni_ac_mg_berry, treatment) 

str(monomeric_antho_no_leak_2019_1stpoint)

monomeric_antho_no_leak_2019_1stpoint %>%
  group_by(treatment)%>%
  tally()

is.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)

monomeric_antho_no_leak_2019_1stpoint$treatment<- format(monomeric_antho_no_leak_2019_1stpoint$treatment)
monomeric_antho_no_leak_2019_1stpoint$treatment<- as.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)

is.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)


str(monomeric_antho_no_leak_2019_1stpoint)

ggplot(monomeric_antho_no_leak_2019_1stpoint, aes (treatment,peoni_ac_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (peoni_ac_mg_berry~treatment, monomeric_antho_no_leak_2019_1stpoint )
summary (anova_mono_antho)

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$peoni_ac_mg_berry)

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$peoni_ac_mg_berry)

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$peoni_ac_mg_berry)

#### M3Gac 1stpoint mb/berry ####

monomeric_antho_no_leak_2019_1stpoint <- monomeric_antho_no_leak%>%
  filter(Set == "1st point") %>%
  filter(!ï..Sample == "B1R2") %>%
  select(malvi_ac_mg_berry, treatment) 

str(monomeric_antho_no_leak_2019_1stpoint)

monomeric_antho_no_leak_2019_1stpoint %>%
  group_by(treatment)%>%
  tally()

is.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)

monomeric_antho_no_leak_2019_1stpoint$treatment<- format(monomeric_antho_no_leak_2019_1stpoint$treatment)
monomeric_antho_no_leak_2019_1stpoint$treatment<- as.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)

is.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)


str(monomeric_antho_no_leak_2019_1stpoint)

ggplot(monomeric_antho_no_leak_2019_1stpoint, aes (treatment,malvi_ac_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (malvi_ac_mg_berry~treatment, monomeric_antho_no_leak_2019_1stpoint )
summary (anova_mono_antho)

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$malvi_ac_mg_berry)

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$malvi_ac_mg_berry)

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$malvi_ac_mg_berry)

#### Peo3Gcoum 1st point mb/berry ####

monomeric_antho_no_leak_2019_1stpoint <- monomeric_antho_no_leak%>%
  filter(Set == "1st point") %>%
  filter(!ï..Sample == "B1R2") %>%
  select(peoni_coum_mg_berry, treatment) 

str(monomeric_antho_no_leak_2019_1stpoint)

monomeric_antho_no_leak_2019_1stpoint %>%
  group_by(treatment)%>%
  tally()

is.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)

monomeric_antho_no_leak_2019_1stpoint$treatment<- format(monomeric_antho_no_leak_2019_1stpoint$treatment)
monomeric_antho_no_leak_2019_1stpoint$treatment<- as.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)

is.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)


str(monomeric_antho_no_leak_2019_1stpoint)

ggplot(monomeric_antho_no_leak_2019_1stpoint, aes (treatment,peoni_coum_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (peoni_coum_mg_berry~treatment, monomeric_antho_no_leak_2019_1stpoint )
summary (anova_mono_antho)

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$peoni_coum_mg_berry)

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$peoni_coum_mg_berry)

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$peoni_coum_mg_berry)

####M3Gcoum 1st point mb/berry ####

monomeric_antho_no_leak_2019_1stpoint <- monomeric_antho_no_leak%>%
  filter(Set == "1st point") %>%
  filter(!ï..Sample == "B1R2") %>%
  select(malvi_coum_mg_berry, treatment) 

str(monomeric_antho_no_leak_2019_1stpoint)

monomeric_antho_no_leak_2019_1stpoint %>%
  group_by(treatment)%>%
  tally()

is.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)

monomeric_antho_no_leak_2019_1stpoint$treatment<- format(monomeric_antho_no_leak_2019_1stpoint$treatment)
monomeric_antho_no_leak_2019_1stpoint$treatment<- as.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)

is.factor(monomeric_antho_no_leak_2019_1stpoint$treatment)


str(monomeric_antho_no_leak_2019_1stpoint)

ggplot(monomeric_antho_no_leak_2019_1stpoint, aes (treatment,malvi_coum_mg_berry, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_mono_antho <- aov (malvi_coum_mg_berry~treatment, monomeric_antho_no_leak_2019_1stpoint )
summary (anova_mono_antho)

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))

####ANOVA significant not tukey with alpha 0.05 yes with alpha 0.057 

(test<- HSD.test(anova_mono_antho, trt = "treatment", alpha =0.057, unbalanced = "TRUE"))

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 1)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$malvi_coum_mg_berry)

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 2)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$malvi_coum_mg_berry)

monomeric_antho_no_leak_2019_1stpoint_hist<-monomeric_antho_no_leak_2019_1stpoint %>%
  filter(treatment == 3)
hist(monomeric_antho_no_leak_2019_1stpoint_hist$malvi_coum_mg_berry)