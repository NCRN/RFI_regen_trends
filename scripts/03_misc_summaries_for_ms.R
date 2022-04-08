library(tidyverse)

reg_stat <- read.csv("./results/20220325/results_for_Fig2.csv")
reg_stat_frax <- read.csv("./results/20220325/results_for_Fig2_Ash_as_Canopy.csv")

head(reg_stat_frax)
head(reg_stat)

reg_comb <- left_join(reg_stat, reg_stat_frax, by = c("park", "order", "labels", 'metgrp'), 
                      suffix = c('', 'ash_can'))

head(reg_comb)

write.csv(reg_comb, "./results/20220325/results_for_Fig2_ash_can_nocan.csv", row.names = F)
