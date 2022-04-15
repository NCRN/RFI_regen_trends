library(tidyverse)

reg_stat <- read.csv("./results/20220325/results_for_Fig2_ash_subcan.csv") %>% 
  filter(park == "GETT") %>% mutate(type = "Probable Failure \n Ash as Subcan.")
reg_stat_frax <- read.csv("./results/20220325/results_for_Fig2_ash_as_can.csv") %>% 
  mutate(type = "Secure \n Ash as Canopy")

names(reg_stat)
names(reg_stat_frax)

reg_comb <- rbind(reg_stat, reg_stat_frax) %>% arrange(desc(type), order)
#reg_comb$type_ord <- reorder(reg_comb$type, desc(reg_comb$type))
head(reg_comb)

reg_comb$label_order <- factor(reg_comb$labels, 
                               levels = c("Tree BA", "Tree Density", "Sapling BA", "Sapling Density", "Seedling Density", 
                                          "% Stocked Plots", "Stocking Index","Deer Browse Impacts", "Flat Tree Diam. Dist.",
                                          "Sapling Composition", "Seedling Composition", 
                                          "Sorensen Sapling", "Sorensen Seedling"))

reg_comb$metgrp <- factor(reg_comb$metgrp, levels = c("Status", "Total", "Native Canopy", "Native Subcan.",
                                                      "Exotic"))
#reg_comb$type <- reorder(reg_comb$type, desc(reg_comb$type))
head(reg_comb)

#write.csv(reg_comb, "./results/20220325/results_for_Fig2_ash_can_nocan.csv", row.names = F)

results_plot <- 
  ggplot(reg_comb, aes(x = type, y = label_order))+
  geom_tile(aes(fill = sign), color = 'grey')+
  geom_text(aes(label = case_when(sign == "critical" ~ "●",
                                  sign == "caution" ~ "○",
                                  sign == "signdec_bad" ~ "-",
                                  sign == "signinc_bad" ~ "+", 
                                  sign == "signdec_good" ~ "-",
                                  sign == "signinc_good" ~ "+", 
                                  TRUE ~ "")))+
  facet_grid(metgrp ~ type, scales = 'free', space = 'free', switch = "y")+
  scale_fill_manual(values = c('critical' = "#FF5B5B", 
                               'caution' = "#FFFF79",
                               'acceptable' = '#BDEBA7',
                               "signdec_bad" = "#CD5C5C", 
                               "signinc_good" = "#7DD66D", #"#228B22",
                               "signdec_good" = "#6DA6D6", 
                               "signinc_bad" = "#FF8C00",
                               "nonsign" = "#E3E3E3", "notmod" = "white"),
                    labels = c("●          Status: Critical",
                               "○          Status: Caution",
                               "Status: Acceptable",
                               " -         Sign. Decline (Native)",
                               "+          Sign. Increase (Native)",
                               " -         Sign. Decline (Exotic)",
                               "+          Sign. Increase (Exotic)",
                               "Not Significant",
                               "Not Modeled"),
                    name = NULL)+
  scale_x_discrete(position = 'top')+
  scale_y_discrete(limits = rev)+
  theme_bw()+
  theme(axis.text.x = element_blank(),#(angle = 90),
        axis.ticks.x = element_blank(),
        axis.text = element_text(size = 9.5),
        strip.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 9.5),
        text = element_text(size = 9),
        #strip.placement = 'outside',
        legend.spacing.x = unit(0.5, 'cm'),
        legend.position = 'right', legend.justification = 'bottom',
        legend.title = element_text(size = 9), 
        legend.text = element_text(size = 9, margin = margin(l = -0.9, unit = 'cm')),
        legend.background = element_rect(color = "#B9B9B9", size = 0.25))+
  labs(x = NULL, y = NULL)+ 
  guides(fill = guide_legend(ncol = 1, # byrow = FALSE, 
                             hjust = 1))

results_plot

ggsave('./results/20220325/results_grid_symbols_GETT.svg', height = 8, width = 6, units = 'in')
