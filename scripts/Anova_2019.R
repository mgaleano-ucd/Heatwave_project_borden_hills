library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(agricolae)


diurnals_borden_hills_2019 <-read.csv("data_output/diurnals_2019_old_and_new_blocks_cleaned_no_NAs.csv", header = TRUE)

diurnals_borden_hills_2019<- diurnals_borden_hills_2019 %>%
  filter(!pixel_number == 34 )

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

(test<- HSD.test(Anova_stem, trt = "treatment", alpha =0.05,unbalanced = "TRUE"))
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

(test<- HSD.test(Anova_stem, trt = "treatment", alpha =0.05,unbalanced = "TRUE"))
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

(test<- HSD.test(Anova_stem, trt = "treatment", alpha =0.05,unbalanced = "TRUE"))
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

(test<- HSD.test(Anova_stem, trt = "treatment", alpha =0.05,unbalanced = "TRUE"))
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
str(anova_pH)
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
####Yield####

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


##### number of clusters per vine ######

yield_bh_2019_grouped_num_cluster <- yield_bh_2019_grouped%>%
  filter(!pixel_number == "34") %>%
  select(Clusters, Treatment) 

yield_bh_2019_grouped_num_cluster%>%
  group_by(Treatment) %>%
  tally()


is.factor(yield_bh_2019_grouped_num_cluster$Treatment)

yield_bh_2019_grouped_num_cluster$Treatment<- format(yield_bh_2019_grouped_num_cluster$Treatment)
yield_bh_2019_grouped_num_cluster$Treatment<- as.factor(yield_bh_2019_grouped_num_cluster$Treatment)

is.factor(yield_bh_2019_grouped_num_cluster$Treatment)

ggplot(yield_bh_2019_grouped_num_cluster, aes (Treatment,Clusters, group =Treatment)) +
  geom_boxplot(aes(fill = Treatment, color =Treatment)) +
  geom_point()

anova_yield_Clusters <- aov (Clusters~Treatment, yield_bh_2019_grouped_num_cluster)
summary  (anova_yield_Clusters)

str(anova_yield_Clusters)


anova_yield_Clusters_print<-xtable(anova_yield_Clusters)

lapply(summary(anova_yield_Clusters), xtable)


tukey<-TukeyHSD(aov(Clusters~Treatment, yield_bh_2019_grouped_num_cluster))

(test<- HSD.test(anova_yield_Clusters, trt = "Treatment", alpha =0.05, unbalanced = "TRUE"))

str(test)
as.data.frame(test$groups)

yield_bh_2019_grouped_num_cluster_hist<-yield_bh_2019_grouped_num_cluster %>%
  filter(Treatment == 1)
hist(yield_bh_2019_grouped_num_cluster_hist$Clusters)

yield_bh_2019_grouped_num_cluster_hist<-yield_bh_2019_grouped_num_cluster %>%
  filter(Treatment == 2)
hist(yield_bh_2019_grouped_num_cluster_hist$Clusters)

yield_bh_2019_grouped_num_cluster_hist<-yield_bh_2019_grouped_num_cluster %>%
  filter(Treatment == 3)
hist(yield_bh_2019_grouped_num_cluster_hist$Clusters)


##### Phenolics anova MCP Tannins ####

mcp_2019<- read.csv("data/raw_data_mcp_2019.csv", header = TRUE)

mcp_2019<- mcp_2019%>%
  mutate(dilution_factor = (Extract_final_vol/Extract_ini_vol))%>%
  mutate(total_tannin_mg_l = ((((Control-Treated)-0.029112)/0.00032)*dilution_factor))%>%
  mutate(Total_tannin_mg_berry = ((total_tannin_mg_l*Extract_ini_vol)/(1000*Berry_numb)))%>%
  mutate(Total_tannin_mg_g_berry_weight =((total_tannin_mg_l*Extract_ini_vol)/(1000*Berry_weight)))%>%
  mutate(Total_tannin_mg_g_skin = ((total_tannin_mg_l*Extract_ini_vol)/(1000*Skin_weight_aft)))%>%
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


mcp_2019<-mcp_2019%>%
  filter(!Block_id =="B1R2")

se <- function(x) sqrt(var(x)/length(x))

str(mcp_2019$Date_sample)

mcp_2019%>%
  group_by(Date_sampled, treatment)%>%
  tally()


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

somers_2019<- read.csv("data/raw_data_somers_2019.csv", header = TRUE)

somers_2019<- somers_2019%>%
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


somers_2019<- somers_2019 %>%
  filter(!Block_id == "B1R2")


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

#### Differences with tukey alpha 0.05 no when running anova with 0.05###

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


berry_weight_2019 <- somers_2019%>%
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


