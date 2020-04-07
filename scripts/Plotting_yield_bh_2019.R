library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)


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

yield_bh_2019_boxplot<-yield_bh_2019_grouped %>%
  ggplot(aes(Treatment, gr_cluster))+
  geom_boxplot(alpha =0.1, aes(fill = Treatment, group = Treatment))+
  scale_fill_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+ 
  geom_point(alpha =0.5, aes(color = Treatment, group =Treatment, shape =Rep), size = 2.3)+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic()+
  scale_shape_manual(name = "Replication", values = c( 15, 16, 17),labels = c ("Rep 1", "Rep 2", "Rep 3")) +
  theme_classic()+
  ylab("grs/cluster") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Treatment") +
  scale_x_discrete(labels=c("1" = "Baseline (60% ET)", "2" = "2x baseline ET",
                            "3" = "3x baseline ET")) +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_y_continuous (breaks=seq(40,160,20), limits = c (40,160))

ggsave(yield_bh_2019_boxplot, filename = "figures/yield_bh_2019_boxplot.pdf", device = cairo_pdf, 
       width = 9, height = 7)


##### Kg/vine boxplot alll blocks ####

yield_bh_2019_boxplot_kg_vine<-yield_bh_2019_grouped %>%
  ggplot(aes(Treatment, Weight_kg))+
  geom_boxplot(alpha =0.1, aes(fill = Treatment, group = Treatment))+
  scale_fill_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+ 
  geom_point(alpha =0.5, aes(color = Treatment, group =Treatment, shape =Rep), size = 2.3)+
  scale_color_manual(values = c("springgreen4", "darkorange1", "dodgerblue2"), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic()+
  scale_shape_manual(name = "Replication", values = c( 15, 16, 17),labels = c ("Rep 1", "Rep 2", "Rep 3")) +
  theme_classic()+
  ylab("Kg/vine") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Treatment") +
  scale_x_discrete(labels=c("1" = "Baseline (60% ET)", "2" = "2x baseline ET", "3" = "3x baseline ET")) +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_y_continuous (breaks=seq(2,20,2), limits = c (2,20))


ggsave(yield_bh_2019_boxplot_kg_vine, filename = "figures/yield_bh_2019_boxplot_kg_vine.pdf", device = cairo_pdf, 
       width = 9, height = 7)


#### Yield all together ####

library(cowplot)

panel_plot_yield_bh_2019 <- plot_grid (yield_bh_2019_boxplot, yield_bh_2019_boxplot_2, yield_bh_2019_boxplot_kg_vine, yield_bh_2019_boxplot_2_kg_vine, labels=c("All pixel numbers","Pixel 34 removed", "All pixel numbers", "Pixel 34 removed", ncol = 2, nrow = 2), vjust = 3.9, hjust = -1.5, label_size = 12)

ggsave(panel_plot_yield_bh_2019, filename = "figures/panel_plot_yield_bh_2019.pdf", device = cairo_pdf, width = 14, height = 12)
