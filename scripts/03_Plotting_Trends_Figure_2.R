#---------------------------------------------------------------------------------------------------
# Plotting staus and trends grid for Figure 2 in ms
#   Code written by Kate Miller 20220426
#---------------------------------------------------------------------------------------------------
library(forestTrends)
library(tidyverse)
library(grid) # for digging into str of ggplot grobs
library(gridExtra) # for ggarrange
library(gtable)
library(stringr) #for word()
library(latex2exp)

options("scipen" = 100, "digits" = 4) # keeps scientific notation from showing up

datapath <- "./data/"
dens_df1 <- read.csv(paste0(datapath, "EFWG_full_dataset_20220325.csv"))
spp_df <- read.csv(paste0(datapath, "EFWG_species_dataset_20220325.csv"))
dens_df <- left_join(dens_df1, spp_df, by =  c("Plot_Name", "Unit_Code", "Year", "Network", 
                                               "cycle", "lat_rank", "excludeEvent"))
dens_df$park_ord <- reorder(dens_df$Unit_Code, desc(dens_df$lat_rank))
names(dens_df)
dens_df <- dens_df[,c(1:16, 107, 17:106)]
dens_c3 <- dens_df %>% filter(cycle == 3) 

boot_results <- read.csv("./results/20220325/all_metrics_randint_20220325.csv")
#boot_results_spp <- read.csv("./results/20220325/spp_metrics_randint_20220325.csv")
#boot_results_comb <- rbind(boot_results1, boot_results_spp)

# lat_rank <- read.csv(paste0(datapath, "EFWG_lat_order.csv"))[,c("park", "network", "lat.rank")]
# boot_results <- left_join(boot_results1, lat_rank, by = 'park') %>% rename(lat_rank = lat.rank)
# boot_results$park_ord <- reorder(boot_results$park, desc(boot_results$lat_rank))
# boot_results$network[boot_results$park %in% c("COLO", "SAHI", "THST")] <- "NCBN" # in case this hasn't been changed
# boot_results$network_ord <- factor(boot_results$network, levels = c("NETN", "ERMN", "NCRN", "NCBN", "MIDN"))
# table(boot_results$network)

# Percent Composition
comp <- read.csv("./data/EFWG_proportion_regen_20220325.csv")
head(comp)

# comp_natcan <- left_join(comp, dens_c3 %>% select(Unit_Code, lat_rank) %>% unique(),
#                          by = "Unit_Code") #%>% 
  # filter(Species == "Native Species") %>% # actually Native Canopy
  # filter(Metric %in% c("Sapling Density", "Seedling Density"))   

#---- Set up lists to iterate over ----
# column names for response
names(dens_df)
head(dens_df)
#metrics <- c(names(dens_df[, c(18:ncol(dens_df))]))

metrics <- c(names(dens_df[, c(18:107)]))

#----- RESULTS GRID -----

# Trend metrics
tile_metrics <- data.frame(
  resp = c(
  # "Tree_BA_Total", "Tree_Dens_Total", "Sap_BA_Total", "Sap_Dens_Total", "Seed_Dens_Total",
  "Tree_BA_NatCan", "Tree_Dens_NatCan", "Sap_BA_NatCan", "Sap_Dens_NatCan", "Seed_Dens_NatCan", "stock_final",
  "Tree_BA_NatOth", "Tree_Dens_NatOth", "Sap_BA_NatOth", "Sap_Dens_NatOth", "Seed_Dens_NatOth",
  "Tree_BA_Exotic", "Tree_Dens_Exotic", "Sap_BA_Exotic", "Sap_Dens_Exotic", "Seed_Dens_Exotic"
   #, "Sor_sap", "Hor_sap", "Sor_seed", "Hor_seed"
  ),
  labels = c(
  #"Tree BA", "Tree Density", "Sapling BA", "Sapling Density", "Seedling Density", 
  "Tree BA", "Tree Density", "Sapling BA", "Sapling Density", "Seedling Density", "Stocking Index",
  "Tree BA", "Tree Density", "Sapling BA", "Sapling Density", "Seedling Density", 
  "Tree BA", "Tree Density", "Sapling BA", "Sapling Density", "Seedling Density" 
  ),# "Sorensen Sapling", "Horn Sapling", "Sorensen Seedling", "Horn Seedling")
   order = 10:25) # first 9 are status metrics
#1:15)

tile_metrics

result_sum <- boot_results %>% filter(term == "Slope") %>% 
  mutate(sign = case_when(lower95 > 0 & !grepl("Exotic", resp) ~ "signinc_good",
                          upper95 < 0 & !grepl("Exotic", resp) ~ "signdec_bad",
                          lower95 > 0 & grepl("Exotic", resp) ~ "signinc_bad",
                          upper95 < 0 & grepl("Exotic", resp) ~ "signdec_good",
                          is.na(lower95) ~ "notmod",
                          TRUE ~ "nonsign")) %>% 
  #select(park, park_ord, resp, estimate, sign, strp_col) %>% 
  left_join(tile_metrics, ., by = "resp") %>% arrange(park_ord, order)

result_sum2 <- result_sum %>% group_by(park_ord) %>% 
  mutate(num_sign_bad = sum(sign %in% c("signdec_bad", "signinc_bad"))) %>% ungroup()

head(result_sum2)

result_sum2$num_sign_bad[result_sum2$park %in% c("SAHI", "WOTR")] <- -1
#result_sum2$label_order <- reorder(result_sum2$labels, desc(result_sum2$order))
#result_sum2$park_order <- reorder(result_sum2$park, desc(result_sum2$num_sign_bad))
result_sum2 <- result_sum2 %>% mutate(metgrp = factor(case_when(#grepl("Total", resp) ~ "Total",
                                                                grepl("NatCan|stock", resp) ~ "Native Canopy \n Trends",
                                                                grepl("NatOth", resp) ~ "Native Subcan. \n Trends",
                                                                grepl("Exotic", resp) ~ "Exotic \n Trends",
                                                                #grepl("Sor|Hor", resp) ~ "Similarity", 
                                                                TRUE ~ "Unk"),
                                                      levels = c(#"Total", 
                                                                 "Native Canopy \n Trends", "Native Subcan. \n Trends",
                                                                 "Exotic \n Trends", "Unk")
)) %>% select(park, order, labels, metgrp, sign)

head(result_sum2)
table(result_sum2$metgrp)

# decided to drop similarity trends because more confusing than helpful
result_sum3 <- result_sum2 %>% filter(metgrp != "Similarity") %>% droplevels()
levels(result_sum3$metgrp)
head(dens_df)

status_metrics_3a <- dens_df %>% filter(cycle == 3) %>% 
  group_by(Unit_Code) %>% 
  summarize(avg_sap_dens = mean(Sap_Dens_NatCan, na.rm = T), 
            avg_seed_dens = mean(Seed_Dens_NatCan, na.rm = T),
            avg_stock = mean(stock_final, na.rm = T),
            avg_sor_sap = mean(Sor_sap, na.rm = T),
            avg_sor_seed = mean(Sor_seed, na.rm = T),
            avg_dbi = mean(DBI, na.rm = T)) %>% ungroup() %>% 
  
  mutate(`Sapling Density` = case_when(avg_sap_dens < 0.1 ~ "critical", 
                                       between(avg_sap_dens, 0.1, 0.15999) ~ "caution",
                                       avg_sap_dens >= 0.16 ~ "acceptable",
                                       TRUE ~ "unknown"),
         `Seedling Density` = case_when(avg_seed_dens < 0.25 ~ "critical", 
                                        between(avg_seed_dens, 0.25, 1.99) ~ "caution",
                                        avg_seed_dens >= 2 ~ "acceptable", 
                                        TRUE ~ 'unknown'),
         `Stocking Index` = case_when(avg_stock < 25 ~ "critical",
                                      between(avg_stock, 25, 100) ~ "caution", 
                                      avg_stock >= 100 ~ "acceptable",
                                      TRUE ~ "unknown"),
         `Sorensen Sapling` = ifelse(avg_sor_sap < 0.2, "critical", "acceptable"),
         `Sorensen Seedling` = ifelse(avg_sor_seed < 0.2, "critical", "acceptable"),
         `Deer Browse Impacts` = case_when(avg_dbi >= 4 ~ 'critical', 
                                           between(avg_dbi, 3.01, 4) ~ 'caution', 
                                           avg_dbi <= 3 ~ 'acceptable',
                                           TRUE ~ "unknown"),
         `Flat Tree Diam. Dist.` = ifelse(Unit_Code %in% c('SAHI', 'MORR'), "critical", "acceptable"))


status_metrics_3b <- left_join(status_metrics_3a, 
                               comp %>% select(Network, Unit_Code, sap_dens_pct_NatCan, seed_dens_pct_NatCan), 
                               by = "Unit_Code") 

status_metrics_3 <- status_metrics_3b %>% mutate(
  `Sapling Composition` = case_when(sap_dens_pct_NatCan < 0.5 ~ "critical",
                                    between(sap_dens_pct_NatCan, 0.5, 0.7) ~ "caution",
                                    sap_dens_pct_NatCan > 0.7 ~ "acceptable"),
  `Seedling Composition` = case_when(seed_dens_pct_NatCan < 0.5 ~ "critical",
                                     between(seed_dens_pct_NatCan, 0.5, 0.7) ~ "caution",
  seed_dens_pct_NatCan > 0.7 ~ "acceptable")) %>% 
  select(-starts_with("pctNatCan"))

head(status_metrics_3)
# Reorder columns
status_metrics_3 <- status_metrics_3 %>% select(Network, Unit_Code, avg_sap_dens:`Stocking Index`, 
                                                `Sapling Composition`, `Seedling Composition`,
                                                everything())

# Flat Tree Diam. Dist. was determined by modelling linear and exponential fit to diameter distribution.
# If linear had the lowest AIC, then the distribution has fewer small trees than expected.

status_met_long <- status_metrics_3 %>% select(Unit_Code, `Sapling Density`:`Flat Tree Diam. Dist.`) %>% 
  pivot_longer(-Unit_Code, names_to = "labels", values_to = "sign") %>% 
  mutate(metgrp = "Regen. Status", order = rep(c(1:2,4:10), 39)) %>% rename(park = Unit_Code) %>% 
  select(park, order, labels, metgrp, sign)


# Add proportion of stocked plots based on park average DBI
dens_c3 <- dens_df %>% filter(cycle == 3) 

dbi_stock_c3 <- dens_c3 %>% group_by(Unit_Code) %>% 
  mutate(avg_DBI = mean(DBI, na.rm = T)) %>% ungroup() %>% 
  mutate(stocked = case_when(avg_DBI <= 3 & stock_final >= 50 ~ 1,
                             avg_DBI > 3 & stock_final >= 100 ~ 1,
                             TRUE ~ 0))

dbi_stock_sum <- dbi_stock_c3 %>% group_by(Unit_Code) %>% 
  summarize(avg_stock = mean(stock_final, na.rm = TRUE),
            avg_DBI = first(avg_DBI),
            num_stocked = sum(stocked, na.rm = TRUE),
            num_plots = sum(!is.na(stocked)),
            prop_stocked = num_stocked/num_plots) %>% 
  rename(park = Unit_Code) %>% mutate(order = 3)

table(dbi_stock_sum$avg_DBI)

prop_stock <- dbi_stock_sum %>% mutate(labels = "% Stocked Plots",
                                       metgrp = "Regen. Status", 
                                       sign = case_when(prop_stocked < 0.33 ~ "critical", 
                                                        between(prop_stocked, 0.33, 0.669) ~ 'caution', 
                                                        prop_stocked > 0.67 ~ "acceptable",
                                                        TRUE ~ 'unknown')) %>% 
  select(park, order, labels, metgrp, sign)

head(prop_stock)
head(status_met_long)

# Determing groupings based on # critical status metrics
status_met_long_final <- rbind(status_met_long, prop_stock) %>% arrange(park, order)

regstat_group <- status_met_long_final %>% filter(metgrp == "Regen. Status") %>% 
  group_by(park) %>% summarize(num_crit = sum(sign == "critical", na.rm = T),
                               park_reggrp = case_when(num_crit > 5 ~ "Imminent Failure",
                                                       between(num_crit, 4, 5) ~ "Probable Failure",
                                                       between(num_crit, 1, 3) ~ "Insecure",
                                                       num_crit < 2 ~ "Sec.",
                                                       TRUE ~ 'undefined'))

results_comb <- rbind(status_met_long_final, result_sum3) %>% arrange(park, order) 


results_comb2 <- left_join(results_comb, regstat_group, by = 'park') %>% arrange(park, order)

results_comb2$label_order <- factor(results_comb2$labels, 
                                   levels = c("Tree BA", "Tree Density", "Sapling BA", "Sapling Density", "Seedling Density", 
                                              "% Stocked Plots", "Stocking Index","Deer Browse Impacts", "Flat Tree Diam. Dist.",
                                              "Sapling Composition", "Seedling Composition", 
                                              "Sorensen Sapling", "Sorensen Seedling"))

results_comb2$metgrp <- factor(results_comb2$metgrp, levels = c("Regen. Status", "Native Canopy \n Trends", "Native Subcan. \n Trends",
                                                               "Exotic \n Trends"))

results_comb2$park_reggrp <- factor(results_comb2$park_reggrp, levels = c("Imminent Failure", "Probable Failure", "Insecure", "Sec."))

# Star parks with deer management
dm_parks = c("ANTI", "CATO", "CHOH", "HAFE", "MANA", "MONO", "ROCR", "GETT", "VAFO")
results_comb2 <- results_comb2 %>% mutate(park_dm = ifelse(park %in% dm_parks, paste0(park, "*"), park))

results_comb2$park_ord <- reorder(results_comb2$park_dm, desc(results_comb2$num_crit))

table(results_comb2$park_reggrp)
sort(unique(results_comb2$park_ord))

#sort(unique(results_comb2$sign))

results_tally <- results_comb2 %>% group_by(park, park_reggrp) %>% 
  filter(sign %in% c("critical", "signdec_bad", "signinc_bad")) %>% 
  summarize(num_bad = n())    

head(results_tally)

#results_comb$park_order <- reorder(results_comb$park, results_comb$park_reggrp)
results_final <- results_comb2 %>% arrange(park_ord, metgrp, label_order)

results_plot <- 
  ggplot(results_final, aes(x = park_ord, y = label_order))+
  geom_tile(aes(fill = sign), color = 'grey')+
  geom_text(aes(label = case_when(sign == "critical" ~ "●",
                                  sign == "caution" ~ "○",
                                  sign == "signdec_bad" ~ "-",
                                  sign == "signinc_bad" ~ "+", 
                                  sign == "signdec_good" ~ "-",
                                  sign == "signinc_good" ~ "+", 
                                  TRUE ~ "")))+
  facet_grid(metgrp ~ park_reggrp, scales = 'free', space = 'free', switch = "y")+
  scale_fill_manual(values = c('critical' = "#FF5B5B", 
                               'caution' = "#FFFF79",
                               'acceptable' = '#BDEBA7',
                               "signdec_bad" = "#CD5C5C", 
                               "signinc_good" = "#7DD66D", #"#228B22",
                               "signdec_good" = "#6DA6D6", 
                               "signinc_bad" = "#FF8C00",
                               "nonsign" = "#E3E3E3", 
                               "notmod" = "white"),
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
  theme(axis.text.x.top = element_text(angle = 90, size = 10, hjust = 0, vjust = 0.5),
        axis.text.y = element_text(size = 9),
        strip.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 9.5),
        text = element_text(size = 9),
        #strip.placement = 'outside',
        legend.spacing.x = unit(0.5, 'cm'),
        legend.position = 'bottom',
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10, margin = margin(l = -0.9, unit = 'cm')),
        legend.background = element_rect(color = "#B9B9B9", size = 0.25))+
  labs(x = NULL, y = NULL)+ 
  guides(fill = guide_legend(ncol = 4, byrow = T, hjust = 1))


results_plot


ggsave("./results/20220325/results_grid_symbols_20220426_4col.svg", 
       height = 8, width = 11, units = 'in')

# Had to open the svg in notepad and tweak the legend by hand to get symbols and
# font to line up correctly.

# 
# head(results_final)
# sort(unique(results_final$sign))
# 
# results_plot <- 
#   ggplot(results_final, aes(x = park_ord, y = label_order))+
#   geom_tile(aes(fill = sign), color = 'grey')+
#   geom_text(aes(label = case_when(sign == "critical" ~ "●",
#                                   sign == "caution" ~ "○",
#                                   sign == "signdec_bad" ~ "-",
#                                   sign == "signinc_bad" ~ "+", 
#                                   sign == "signdec_good" ~ "-",
#                                   sign == "signinc_good" ~ "+", 
#                                   TRUE ~ "")))+
#   facet_grid(metgrp ~ park_reggrp, scales = 'free', space = 'free', switch = "y")+
#   scale_fill_manual(values = c('critical' = "#FF5B5B", 
#                                'caution' = "#FFFF79",
#                                'acceptable' = '#BDEBA7',
#                                "signdec_bad" = "#CD5C5C", 
#                                "signinc_good" = "#7DD66D", #"#228B22",
#                                "signdec_good" = "#6DA6D6", 
#                                "signinc_bad" = "#FF8C00",
#                                "nonsign" = "#E3E3E3", 
#                                "notmod" = "white"),
#                     labels = c("●          Status: Critical",
#                                "○          Status: Caution",
#                                "Status: Acceptable",
#                                " -         Sign. Decline (Native)",
#                                "+          Sign. Increase (Native)",
#                                " -         Sign. Decline (Exotic)",
#                                "+          Sign. Increase (Exotic)",
#                                "Not Significant",
#                                "Not Modeled"),
#                     name = NULL)+
#   scale_x_discrete(position = 'top')+
#   scale_y_discrete(limits = rev)+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 90),
#         axis.text = element_text(size = 9),
#         strip.text.y = element_text(size = 10),
#         strip.text.x = element_text(size = 10),
#         text = element_text(size = 9),
#         #strip.placement = 'outside',
#         legend.spacing.x = unit(0.5, 'cm'),
#         legend.position = 'bottom',
#         legend.title = element_text(size = 9), 
#         legend.text = element_text(size = 9, margin = margin(l = -0.9, unit = 'cm')),
#         legend.background = element_rect(color = "#B9B9B9", size = 0.25), 
#         strip.background = element_rect(fill = "white", color = 'black'))+
#   labs(x = NULL, y = NULL)+ 
#   guides(fill = guide_legend(nrow = 3, byrow = FALSE, hjust = 1))
# 
# results_plot
# 
# 
# #----- Setting up facet color code by status or trend
# met_cols <- data.frame(met_ord = unique(results_final$metgrp),
#                        strp_col = c("#ffffff", rep("#CFCFCF", 3)), 
#                        facet_ord = 38:41) 
# 
# fills <- c(met_cols$strp_col)
# 
# # color-coding facet strips
# g <- ggplot_gtable(ggplot_build(results_plot))
# 
# strp_col <- which(grepl('strip-l', g$layout$name))
# 
# 
# k <- 1
# for (i in strp_col) {
#   g$grobs[[i]]$grobs[[1]]$children[[1]]$gp$fill <- fills[k]
#   k <- k+1
# }
# 
# grid.draw(g)
# 
# #write.csv(results_final, "./results/20220325/results_for_Fig2_ash_subcan.csv", row.names = F)
# 
# ggsave("./results/20220325/results_grid_symbols_20220426_option1.svg", 
#        height = 8, width = 11, units = 'in')


