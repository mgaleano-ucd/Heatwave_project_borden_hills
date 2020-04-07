

yield_bh_2019<- read.csv("data/Yield_borden_hills_2019.csv", header =TRUE)

yield_bh_2019_grouped2<- yield_bh_2019 %>%
  mutate(gr_cluster = (Weight_kg/Clusters)*1000) %>%
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