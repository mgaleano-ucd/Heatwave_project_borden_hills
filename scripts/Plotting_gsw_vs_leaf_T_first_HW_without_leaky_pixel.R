library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)

diurnals_borden_hills_2019 <-read.csv("data_output/diurnals_2019_old_and_new_blocks_cleaned_no_NAs.csv", header = TRUE)

diurnals_borden_hills_2019 <-diurnals_borden_hills_2019%>%
  filter(!pixel_number == 34 )
diurnals_borden_hills_2019$round<-format(diurnals_borden_hills_2019$round)
diurnals_borden_hills_2019$round<-as.numeric(as.factor(diurnals_borden_hills_2019$round))

str(diurnals_borden_hills_2019$round)


str(diurnals_borden_hills_2019)

diurnals_2019_gsw_vs_leafT <- diurnals_borden_hills_2019 %>%
  mutate(time = hhmmss) %>%
  select(-hhmmss) %>%
  filter(!is.na(time)) %>%
  filter(!is.na(leaf_temp_C)) %>%
  filter(!leaf_temp_C == "-")


diurnals_2019_gsw_vs_leafT$time<- format(strptime(diurnals_2019_gsw_vs_leafT$time,"%H:%M:%S"), format = "%H:%M", tz = "UTC")

str(diurnals_2019_gsw_vs_leafT$time)



diurnals_2019_gsw_vs_leafT$datetime <- paste(diurnals_2019_gsw_vs_leafT$date, " ", diurnals_2019_gsw_vs_leafT$time, sep = "")

glimpse(diurnals_2019_gsw_vs_leafT) 

diurnals_2019_gsw_vs_leafT$datetime <- mdy_hm(diurnals_2019_gsw_vs_leafT$datetime, tz = "UTC")

str(diurnals_2019_gsw_vs_leafT)

tz(diurnals_2019_gsw_vs_leafT$datetime)

tz(diurnals_2019_gsw_vs_leafT$time)

diurnals_2019_gsw_vs_leafT$leaf_temp_C <- format(diurnals_2019_gsw_vs_leafT $leaf_temp_C)
diurnals_2019_gsw_vs_leafT$leaf_temp_C<-as.numeric(diurnals_2019_gsw_vs_leafT$leaf_temp_C)

str(diurnals_2019_gsw_vs_leafT$datetime)

str(diurnals_2019_gsw_vs_leafT$leaf_temp_C)





####Plotting  gsw vs LICOR leaf T FISRT HW round 3-4 ####

diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4<- diurnals_borden_hills_2019 %>%
  filter(!is.na(Tleaf)) %>%
  select( day, Tleaf, gsw, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block, BH_Vine) %>%
  filter(day < 215) %>%
  filter(day > 205) %>%
  filter(round > 2) %>%
  filter(round < 5)

diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4%>%
  group_by(treatment)%>%
  tally()
str(diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4$Tleaf)


diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4$treatment<-format(diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4$treatment)
as.character(diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4$treatment)

diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4$treatment<- reorder(diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4$treatment, diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4$Tleaf)

str(diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4$treatment)

diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4$Rep<-format(diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4$Rep)
as.character(diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4$Rep)

diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4$treatment<-format(diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4$treatment)
as.character(diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4$treatment)

str(diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4$treatment)


## Plot first HW RESPONSE
gsw_vs_leafT_licor_first_HW_r_3_4 <-ggplot(diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4, aes(Tleaf, gsw, group = treatment, color = treatment)) +
  geom_point(alpha =0.4, size = 1.7 ) + 
  stat_smooth(aes(y = gsw, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.4) +
  stat_poly_eq(aes(label =  paste(stat(rr.label),stat(p.value.label),stat(eq.label), sep = "*\", \"*")),
               formula =  y ~ x + I(x^2), parse = TRUE, size = 5, label.x.npc = "left", vjust =-0.1)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab ("Stomatal conductance ( mol m⁻² s⁻¹)") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("LI-COR Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=21, family = "serif")) +
  theme(axis.title.x = element_text(size=21, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(30,50,5), limits = c (30,50)) +
  scale_y_continuous(breaks=seq(0,0.7,0.1), limits = c (0,0.78)) +
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))

ggsave(gsw_vs_leafT_licor_first_HW_r_3_4, filename = "figures/gsw_vs_leafT_licor_first_HW_r_3_4_without_leaky_pixel.pdf", device = cairo_pdf, 
       width = 8, height = 6)

#####Confidence intervarls upper and lower####

####change treatments 1,2 and 3 to see each fit ### 


diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4_trial<-diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4%>%
  filter(treatment == 3)

diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4_trial%>%
  tally()

physio_respone_fit <- lm(gsw ~ poly(Tleaf, 2), data = diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4_trial)


summary(physio_respone_fit$fitted.values)
summary(physio_respone_fit$coefficients)
summary(physio_respone_fit$model)


str(physio_respone_fit)

physio_respone_fit <- predict(physio_respone_fit,diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4_trial, interval = "confidence")

summary(physio_respone_fit)

diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4_trial <- diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4_triall %>% 
  mutate(fit = physio_respone_fit[,"fit"],
         lwr = physio_respone_fit[,"lwr"],
         upr = physio_respone_fit[,"upr"])

