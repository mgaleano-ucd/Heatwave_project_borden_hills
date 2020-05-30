

yield_bh_2019<- read.csv("data/Yield_borden_hills_2019.csv", header =TRUE)

yield_bh_2019_grouped2<- yield_bh_2019 %>%
  mutate(gr_cluster = (Weight_kg/Clusters)*1000) %>%
  mutate(kg_cluster =Weight_kg/Clusters)%>%
  filter(!pixel_number == 34 ) %>%
  filter(!is.na(Rep))

yield_bh_2019_grouped2$Treatment<-format(yield_bh_2019_grouped2$Treatment)
as.character(yield_bh_2019_grouped2$Treatment)

str(yield_bh_2019_grouped2$Treatment)


yield_bh_2019_grouped2$Rep<-format(yield_bh_2019_grouped2$Rep)
as.character(yield_bh_2019_grouped2$Rep)

str(yield_bh_2019_grouped2$Rep)

str(yield_bh_2019_grouped2)

yield_bh_2019_boxplot_2<-yield_bh_2019_grouped2 %>%
  ggplot(aes(Treatment, gr_cluster))+
  geom_boxplot(alpha =0.75, aes(fill = Treatment, group = Treatment))+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  geom_point(alpha =0.9, aes(color = Treatment, group =Treatment, shape =Rep), size = 2.3)+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic()+
  scale_shape_manual(name = "Replication", values = c( 15, 16, 17),labels = c ("Rep 1", "Rep 2", "Rep 3")) +
  theme_classic()+
  ylab("grs/cluster") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Treatment") +
  scale_x_discrete(labels=c("1" = "Baseline (60% ET)", "2" = "2x baseline ET",  "3" = "3x baseline ET")) +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_y_continuous (breaks=seq(40,160,20), limits = c (40,160)) +
  annotate("text", x = "1", y = 140, label = "*", size = 7) 

ggsave(yield_bh_2019_boxplot_2, filename = "figures/yield_bh_2019_boxplot_2.pdf", device = cairo_pdf, 
       width = 9, height = 7)

##### Kg/vine boxplot####


yield_bh_2019_boxplot_2_kg_vine<-yield_bh_2019_grouped2 %>%
  ggplot(aes(Treatment, Weight_kg))+
  geom_boxplot(alpha =0.75, aes(fill = Treatment, group = Treatment))+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  geom_point(alpha =0.9, aes(color = Treatment, group =Treatment, shape =Rep), size = 2.3)+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic()+
  scale_shape_manual(name = "Replication", values = c( 15, 16, 17),labels = c ("Rep 1", "Rep 2", "Rep 3")) +
  theme_classic()+
  ylab("Kg/vine") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Treatment") +
  scale_x_discrete(labels=c("1" = "Baseline (60% ET)", "2" = "2x baseline ET",  "3" = "3x baseline ET")) +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_y_continuous (breaks=seq(2,16,2), limits = c (2,16)) +
  annotate("text", x = "1", y = 14.5, label = "*", size = 7) 


ggsave(yield_bh_2019_boxplot_2_kg_vine, filename = "figures/yield_bh_2019_boxplot_2_kg_vine.pdf", device = cairo_pdf, 
       width = 9, height = 7)

yield_bh_2019_final_Color <- plot_grid (yield_bh_2019_boxplot_2_kg_vine,yield_bh_2019_boxplot_2, ncol=2, nrow = 1)

ggsave(yield_bh_2019_final_Color , filename = "figures/yield_bh_2019_final_Color.pdf", device = cairo_pdf, width = 14, height = 6)


#### Mean and SE yield components####

yield_bh_2019_grouped2 %>%
  group_by(Treatment)%>%
  tally()

se <- function(x) sqrt(var(x)/length(x))

yield_bh_2019_avg_se<- yield_bh_2019_grouped2 %>%
  group_by(Treatment)%>%
  summarise(avg_kg_vine = mean(Weight_kg), sev_kg_vine = se(Weight_kg), avg_num_clusters = mean(Clusters), sev_num_clusters = se(Clusters),avg_gr_cluster = mean(gr_cluster), sev_gr_cluster = se(gr_cluster))

write.csv(yield_bh_2019_avg_se,"data_output/yield_bh_2019_avg_se.csv")

#####ANOVA yield components######

#### KG PER VINE####
yield_bh_2019_grouped2 %>%
  group_by(Treatment) %>%
  tally()


is.factor(yield_bh_2019_grouped2$Treatment)

yield_bh_2019_grouped2$Treatment<- format(yield_bh_2019_grouped2$Treatment)
yield_bh_2019_grouped2$Treatment<- as.factor(yield_bh_2019_grouped2$Treatment)

is.factor(yield_bh_2019_grouped2$Treatment)

ggplot(yield_bh_2019_grouped2, aes (Treatment,Weight_kg, group =Treatment)) +
  geom_boxplot(aes(fill = Treatment, color =Treatment)) +
  geom_point()

anova_yield_kg_vine <- aov (Weight_kg~Treatment, yield_bh_2019_grouped2)
summary  (anova_yield_kg_vine)

str(anova_yield_kg_vine)

library(xtable)
anova_yield_kg_vine_print<-xtable(anova_yield_kg_vine)

lapply(summary(anova_yield_kg_vine), xtable)
library(agricolae)

tukey<-TukeyHSD(aov(Weight_kg~Treatment, yield_bh_2019_grouped2))

(test<- HSD.test(anova_yield_kg_vine, trt = "Treatment", alpha =0.05, unbalanced = "TRUE"))

str(test)
as.data.frame(test$groups)

yield_bh_2019_grouped2_hist<-yield_bh_2019_grouped2 %>%
  filter(Treatment == 1)
hist(yield_bh_2019_grouped2_hist$Weight_kg)

yield_bh_2019_grouped2_hist<-yield_bh_2019_grouped2 %>%
  filter(Treatment == 2)
hist(yield_bh_2019_grouped2_hist$Weight_kg)

yield_bh_2019_grouped2_hist<-yield_bh_2019_grouped2 %>%
  filter(Treatment == 3)
hist(yield_bh_2019_grouped2_hist$Weight_kg)

#### gr PER CLUSTER####

yield_bh_2019_grouped2 %>%
  group_by(Treatment) %>%
  tally()


is.factor(yield_bh_2019_grouped2$Treatment)

yield_bh_2019_grouped2$Treatment<- format(yield_bh_2019_grouped2$Treatment)
yield_bh_2019_grouped2$Treatment<- as.factor(yield_bh_2019_grouped2$Treatment)

is.factor(yield_bh_2019_grouped2$Treatment)

ggplot(yield_bh_2019_grouped2, aes (Treatment,gr_cluster, group =Treatment)) +
  geom_boxplot(aes(fill = Treatment, color =Treatment)) +
  geom_point()

anova_yield_gr_cluster <- aov (gr_cluster~Treatment, yield_bh_2019_grouped2)
summary  (anova_yield_gr_cluster)

str(anova_yield_gr_cluster)


anova_yield_kg_cluster_print<-xtable(anova_yield_gr_cluster)

lapply(summary(anova_yield_gr_cluster), xtable)


tukey<-TukeyHSD(aov(gr_cluster~Treatment, yield_bh_2019_grouped2))

(test<- HSD.test(anova_yield_gr_cluster, trt = "Treatment", alpha =0.05, unbalanced = "TRUE"))

str(test)
as.data.frame(test$groups)

yield_bh_2019_grouped2_hist<-yield_bh_2019_grouped2 %>%
  filter(Treatment == 1)
hist(yield_bh_2019_grouped2_hist$gr_cluster)

yield_bh_2019_grouped2_hist<-yield_bh_2019_grouped2 %>%
  filter(Treatment == 2)
hist(yield_bh_2019_grouped2_hist$gr_cluster)

yield_bh_2019_grouped2_hist<-yield_bh_2019_grouped2 %>%
  filter(Treatment == 3)
hist(yield_bh_2019_grouped2_hist$gr_cluster)

####number of clusters ####

yield_bh_2019_grouped2 %>%
  group_by(Treatment) %>%
  tally()


is.factor(yield_bh_2019_grouped2$Treatment)

yield_bh_2019_grouped2$Treatment<- format(yield_bh_2019_grouped2$Treatment)
yield_bh_2019_grouped2$Treatment<- as.factor(yield_bh_2019_grouped2$Treatment)

is.factor(yield_bh_2019_grouped2$Treatment)

ggplot(yield_bh_2019_grouped2, aes (Treatment,Clusters, group =Treatment)) +
  geom_boxplot(aes(fill = Treatment, color =Treatment)) +
  geom_point()

anova_yield_Clusters <- aov (Clusters~Treatment, yield_bh_2019_grouped2)
summary  (anova_yield_Clusters)

str(anova_yield_Clusters)


anova_yield_Clusters_print<-xtable(anova_yield_Clusters)

lapply(summary(anova_yield_Clusters), xtable)


tukey<-TukeyHSD(aov(Clusters~Treatment, yield_bh_2019_grouped2))

(test<- HSD.test(anova_yield_Clusters, trt = "Treatment", alpha =0.05, unbalanced = "TRUE"))

str(test)
as.data.frame(test$groups)

yield_bh_2019_grouped2_hist<-yield_bh_2019_grouped2 %>%
  filter(Treatment == 1)
hist(yield_bh_2019_grouped2_hist$Clusters)

yield_bh_2019_grouped2_hist<-yield_bh_2019_grouped2 %>%
  filter(Treatment == 2)
hist(yield_bh_2019_grouped2_hist$Clusters)

yield_bh_2019_grouped2_hist<-yield_bh_2019_grouped2 %>%
  filter(Treatment == 3)
hist(yield_bh_2019_grouped2_hist$Clusters)
