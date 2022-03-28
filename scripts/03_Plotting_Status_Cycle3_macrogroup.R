#---------------------------------------------------------------------------------------------------
# Compiling cycle 3 (2016:2019) data for comparisons of tree and regeneration status across parks
#   Code written by Kate Miller 20211012 
#---------------------------------------------------------------------------------------------------
library(tidyverse)
library(boot)
library(grid) # for digging into str of ggplot grobs
library(gridExtra) # for ggarrange
library(gtable)

options("scipen" = 100, "digits" = 4) # keeps scientific notation from showing up

datapath <- "./data"

#----- Status plots -----
# For status plots, plot the raw mean and 95% confidence interval derived from bootstrapping
dens_df1 <- read.csv("./data/EFWG_full_dataset_20220325.csv")
veggrp <- read.csv("./data/EFWG_veggroups.csv")

dens_df <- right_join(veggrp %>% select(Plot_Name, MACROGROUP_NAME, MACROGROUP_KEY, GROUP_NAME, GROUP_KEY),
                      dens_df1 %>% select(-lat_rank), by = "Plot_Name") %>% 
  mutate(mg_short = 
           case_when(
             grepl("Appalachian-Northeastern Oak", .data$MACROGROUP_NAME) ~ "Northern Oak - Pine",
             grepl("Eastern North American Ruderal Forest", .data$MACROGROUP_NAME) ~ "Northern Ruderal",
             grepl("Laurentian-Acadian Mesic Hardwood - Conifer", .data$MACROGROUP_NAME) ~ "Acadian Mixed",
             grepl("Appalachian-Interior-Northeastern", .data$MACROGROUP_NAME) ~ "Appalachian Mesic",
             grepl("North Atlantic Coastal", .data$MACROGROUP_NAME) ~ "Coastal Plain",
             grepl("Southeastern North American Ruderal Forest", .data$MACROGROUP_NAME) ~ "Southern Ruderal",       
             grepl("Central Hardwood Floodplain", .data$MACROGROUP_NAME) ~ "Floodplain", 
             grepl("Southern & South", .data$MACROGROUP_NAME) ~ "Southern Oak - Pine",
             TRUE ~ "Not Assigned")) %>% 
  filter(!mg_short %in% "Not Assigned")

table(dens_df$MACROGROUP_NAME, dens_df$mg_short)

# Only include macrogroups with at least 50 plots
macrogrps <- dens_df %>% filter(cycle == 3) %>%
  group_by(mg_short) %>% summarize(num_plots = sum(!is.na(Plot_Name))) %>%
  filter(num_plots >= 50) %>% select(mg_short) 

macrogrp_list <- sort(unique(macrogrps$mg_short))

macrogrp_list

dens_grp <- dens_df %>% filter(mg_short %in% macrogrp_list)
head(dens_grp)

# Need to add lat/long for each plot, so I can figure out latitude rank for the macrogroups
import_data <- function(network, data){
  dat <- read.csv(paste0("./data/", network, "_data/", data, ".csv")) %>% 
         select(Plot_Name, Latitude, Longitude) %>% unique()
  
}

plots <- rbind(import_data("ERMN", "Plots"),
               import_data("MIDN", "Plots"),
               import_data("NCRN", "Plots"),
               import_data("NETN", "Plots"))

table(complete.cases(plots$Latitude)) #all true

dens_grp <- left_join(dens_grp, plots, by = c("Plot_Name"))
names(dens_grp)

table(complete.cases(dens_grp$Latitude)) #all true

mg_mean_lat <- dens_grp %>% group_by(mg_short, MACROGROUP_NAME) %>% 
  summarize(mean_lat = mean(Latitude))

mg_mean_lat$lat_rank <- rank(mg_mean_lat$mean_lat) 

dens_df <- left_join(dens_grp, mg_mean_lat %>% select(-mean_lat), by = c("mg_short", "MACROGROUP_NAME"))

# Set up data for cycle 3
dens_c3 <- dens_df %>% filter(cycle == 3) 

names(dens_c3)
status_metrics <- c("stock_final", 
                    "Sap_Dens_Total", "Seed_Dens_Total",
                    "Sap_Dens_NatCan", "Seed_Dens_NatCan",
                    "Sor_sap", "Hor_sap", "Sor_seed", "Hor_seed")

# Load trend results for later use
boot_results1 <- read.csv("./results/20220325/all_metrics_randint_macrogroup_20220325.csv")

boot_results <- left_join(boot_results1, mg_mean_lat %>% select(-mean_lat), 
                          by = c("Macrogroup" = 'MACROGROUP_NAME')) %>% 
                filter(mg_short %in% macrogrp_list)

boot_results$mg_ord <- reorder(boot_results$Macrogroup, desc(boot_results$lat_rank))

boot_sppgrp <- boot_results %>% filter(!resp %in% c("Hor_sap", "Hor_seed", "Sor_sap", "Sor_seed", "stock_final"))

# Set up groups for comp plot
boot_sppgrp$sppgrp <- gsub("^.*_", "", boot_sppgrp$resp)
sppgrp_list <- c(paste0("_", unique(boot_sppgrp$sppgrp), collapse = "|"))
boot_sppgrp$metric <- gsub(sppgrp_list, "", boot_sppgrp$resp)

boot_sppgrp_comb <- boot_sppgrp %>% filter(sppgrp %in% c("NatCan", "NatOth", "Exotic")) %>% 
  arrange(Macrogroup, resp, term)

boot_sppgrp_comb$mg_ord <- reorder(boot_sppgrp_comb$Macrogroup, desc(boot_sppgrp_comb$lat_rank))
boot_sppgrp_comb$estimate[boot_sppgrp_comb$estimate == 0] <- NA
boot_sppgrp_comb$lower95[is.na(boot_sppgrp_comb$estimate)] <- NA
boot_sppgrp_comb$upper95[is.na(boot_sppgrp_comb$estimate)] <- NA

# Set up park lists
mg_metrics <- expand.grid(macrogrp_list, status_metrics)
mgs <- mg_metrics[,1]
metrics <- mg_metrics[,2]

# Create mean function used in bootstrap
mean_fun <- function(data, i){
  d <- data[i, ]
  return(mean(d, na.rm = TRUE))
}


# Iterate over parks and metrics to calc. mean and 95% CI of mean 
c3_stats1 <- map2_dfr(mgs, metrics, function(mg, metric){
  df <- dens_c3 %>% filter(mg_short == mg) 
  results_df <- boot(df[, paste0(metric), drop = FALSE], mean_fun, R = 1000)
  df2 <- data.frame(macrogroup = mg, 
                    metric = metric, 
                    mean = results_df$t0,
                    lower95 = quantile(results_df$t, probs = 0.025, na.rm = T),
                    upper95 = quantile(results_df$t, probs = 0.975, na.rm = T),
                    row.names = NULL)
  return(df2)
})

mg_df <- dens_c3 %>% select(mg_short, lat_rank) %>% unique()

c3_stats <- right_join(mg_df,
                       c3_stats1, by = c("mg_short" = 'macrogroup'))

write.csv(c3_stats, "./results/20220325/EFWG_cycle_3_status_macrogroup.csv", row.names = FALSE)
#c3_stats <- read.csv(paste0(datapath, "results/EFWG_cycle_3_status.csv"))

head(c3_stats)
sort(unique(c3_stats$mg_short))

# Plotting function that sorts from high to low and color codes by network
plot_c3_status <- function(df, met, xlabel, ylabel){
  df2 <- df %>% filter(metric == met)
  df2$mg_ord <- reorder(df2$mg_short, desc(df2$mean))
  
  p <- ggplot(df2, 
              aes(x = mg_ord, y = mean, fill = as.factor(mg_short)))+
    geom_bar(stat = 'identity', aes(fill = as.factor(mg_short)))+ 
    geom_errorbar(aes(ymin = lower95, ymax = upper95), color = "#B9B9B9", 
                  width = 0.1, size = 0.5, na.rm = TRUE)+
    scale_fill_manual(values = c("Acadian Mixed" = "#9883CA", 
                                 "Appalachian Mesic" = "#92CA83",
                                 "Coastal Plain" = "#E7DA7E",
                                 "Floodplain" = "#83CAB2",
                                 "Northern Oak - Pine" = "#90B2EC", 
                                 "Northern Ruderal" = "#E4ACE1", 
                                 "Southern Oak - Pine" = "#F6CC78", 
                                 "Southern Ruderal" = "#FF6960"), 
                      name = "Group:")+
    theme_bw()+
    labs(x = xlabel, y = ylabel)+
    theme(axis.text = element_text(size = 11),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_text(size = 11),
          plot.title = element_text(hjust = 0.5, size = 11, 
                                    margin = margin(t = 10, b = -15)),
          legend.title = element_text(size = 11),
          legend.text = element_text(size = 11),
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.line = element_blank(), 
          legend.position = 'bottom')
  
  return(p)
  
}

# Not going to iterate, so I can customize some of the plots before saving.

plot_c3_status(c3_stats, "stock_final", xlabel = NULL, ylabel = "Mean Stocking Index")+
  geom_hline(yintercept = 25, linetype = 2, lwd = 1, color = '#CD5C5C')+
  geom_text(x = 39, y = 22.5, label = "Severely Understocked", hjust = 1, size = 3.5, check_overlap = TRUE)+
  geom_hline(yintercept = 100, linetype = 4, lwd = 1, color = "#228B22")+
  geom_text(x = 39, y = 104, hjust = 1, label = "Sufficiently Stocked", size = 3.5, check_overlap = TRUE)

ggsave("./results/20220325/cycle_3_status_plots/Cycle_3_stocking_index_mg.svg", width = 11, height = 8, units = 'in')

plot_c3_status(c3_stats, "stock_final", xlabel = NULL, ylabel = "Mean Stocking Index")
ggsave("./results/20220325/cycle_3_status_plots/Cycle_3_stocking_index_no_lines_mg.svg", width = 11, height = 8, units = 'in')

plot_c3_status(c3_stats, "Sap_Dens_NatCan", xlabel = NULL, ylabel = "Sapling Density (stems/sq.m)")
ggsave("./results/20220325/cycle_3_status_plots/Cycle_3_sapling_density_natcan_mg.svg", width = 11, height = 8, units = 'in')

plot_c3_status(c3_stats, "Seed_Dens_NatCan", xlabel = NULL, ylabel = "Seedling Density (stems/sq.m)")
ggsave("./results/20220325/cycle_3_status_plots/Cycle_3_seedling_density_natcan_mg.svg", width = 11, height = 8, units = 'in')

plot_c3_status(c3_stats, "Sor_sap", xlabel = NULL, ylabel = "Sorensen: Sapling vs. Canopy")
ggsave("./results/20220325/cycle_3_status_plots/Cycle_3_Sorensen_sapling_vs_canopy_mg.svg", width = 11, height = 8, units = 'in')

plot_c3_status(c3_stats, "Sor_seed", xlabel = NULL, ylabel = "Sorensen: Seedling vs. Canopy")
ggsave("./results/20220325/cycle_3_status_plots/Cycle_3_Sorensen_seedling_vs_canopy_mg.svg", width = 11, height = 8, units = 'in')

plot_c3_status(c3_stats, "Hor_sap", xlabel = NULL, ylabel = "Horn: Sapling vs. Canopy")
ggsave("./results/20220325/cycle_3_status_plots/Cycle_3_Horn_sapling_vs_canopy_mg.svg", width = 11, height = 8, units = 'in')

plot_c3_status(c3_stats, "Hor_seed", xlabel = NULL, ylabel = "Horn: Seedling vs. Canopy")
ggsave("./results/20220325/cycle_3_status_plots/Cycle_3_Horn_seedling_vs_canopy_mg.svg", width = 11, height = 8, units = 'in')

#---- Summarize proportion of stocked plots based on deer browse index
dbi_stock_c3 <- dens_c3 %>% group_by(mg_short, lat_rank) %>% 
  mutate(avg_DBI = mean(DBI, na.rm = T)) %>% ungroup() %>% 
  mutate(stocked = case_when(avg_DBI <= 3 & stock_final >= 50 ~ 1,
                             avg_DBI > 3 & stock_final >= 100 ~ 1,
                             TRUE ~ 0))

dbi_stock_sum <- dbi_stock_c3 %>% group_by(mg_short, lat_rank) %>% 
                   summarize(avg_stock = mean(stock_final, na.rm = TRUE),
                             avg_DBI = first(avg_DBI),
                             num_stocked = sum(stocked, na.rm = TRUE),
                             num_plots = sum(!is.na(stocked)),
                             prop_stocked = num_stocked/num_plots) 
head(dbi_stock_sum)

dbi_stock_sum$mg_ord <- reorder(dbi_stock_sum$mg_short, desc(dbi_stock_sum$prop_stocked))

write.csv(dbi_stock_sum, "./results/20220325/cycle_3_status_plots/cycle_3_DBI_prop_stock_mg.csv", row.names = FALSE)

p <- 
  ggplot(dbi_stock_sum,
            aes(x = mg_ord, y = prop_stocked, fill = as.factor(mg_short)))+
  geom_bar(stat = 'identity', aes(fill = as.factor(mg_short)))+ 
  scale_fill_manual(values = c("Acadian Mixed" = "#9883CA", 
                               "Appalachian Mesic" = "#92CA83",
                               "Coastal Plain" = "#E7DA7E",
                               "Floodplain" = "#83CAB2",
                               "Northern Oak - Pine" = "#90B2EC", 
                               "Northern Ruderal" = "#E4ACE1", 
                               "Southern Oak - Pine" = "#F6CC78", 
                               "Southern Ruderal" = "#FF6960"), 
                    name = "Group:")+
  scale_y_continuous(limits = c(0, 0.5), breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5), 
                     labels = c("0", "10", "20", "30", "40", "50"))+  
  theme_bw()+
  labs(x = NULL, y = "% Stocked Plots")+
  theme(axis.text = element_text(size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 11, 
                                  margin = margin(t = 10, b = -15)),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 11),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_blank(), 
        legend.position = 'bottom')+
  geom_hline(yintercept = 0.25, lty = 2)
p

ggsave("./results/20220325/cycle_3_status_plots/Pct_stocked_plots_mg.svg", width = 11, height = 8, units = 'in')


#---- Composition by SppGroup plots -----
# Setting up plotting
# Fake plot for species group legend
leg_barsp <- ggplot(data = data.frame(spgrp = c("Exotic", "NatCan", "NatOth"),
                                      x = c(1, 2, 3)),
                    aes(x = x, color = spgrp, fill = spgrp, shape = spgrp))+
  theme_bw()+
  geom_histogram()+
  scale_fill_manual(values = c("NatCan" = "#99D899", "NatOth" = "#A7AFA7", "Exotic" = "#CD5C5C"),
                    labels = c("Native Canopy", "Native Other", "Exotic"),
                    name = "Groups:",
                    drop = FALSE)+
  scale_color_manual(values = c("NatCan" = "#99D899", "NatOth" = "#A7AFA7", "Exotic" = "#CD5C5C"),
                     labels = c("Native Canopy", "Native Other", "Exotic"),
                     name = "Groups:",
                     drop = FALSE)+
  theme(legend.position = 'bottom', legend.title = element_text(size = 10),
        legend.text = element_text(size = 10), #legend.background = element_blank()
        legend.background = element_rect(colour = "#B9B9B9", size = 0.25)
  )

leg_gbarsp <- gtable_filter(ggplot_gtable(ggplot_build(leg_barsp)), "guide-box")

#----- Option 1, plot all 3 groups, so adds to 1
comp_mg_rel <- read.csv("./data/EFWG_proportion_regen_20220325_mg.csv")
comp_long <- comp_mg_rel %>% select(mg_short, lat_rank, sap_ba_pct_NatCan_rel:seed_dens_pct_Exotic_rel) %>% 
                          pivot_longer(sap_ba_pct_NatCan_rel:seed_dens_pct_Exotic_rel, 
                                       names_to = "Metric", values_to = "Mean") %>% 
                          mutate(Species = case_when(grepl("NatCan", .$Metric) ~ "Native Canopy",
                                                     grepl("NatOth", .$Metric) ~ "Other Native Species",
                                                     grepl("Exotic", .$Metric) ~ "Exotic"),
                                 Metric = case_when(grepl("sap_ba", .$Metric) ~ "Sapling BA", 
                                                    grepl("sap_dens", .$Metric) ~ "Sapling Density",
                                                    grepl("seed_dens", .$Metric) ~ "Seedling Density"))

head(comp_long)

comp_long$Species <- factor(comp_long$Species, levels = c("Native Canopy", "Other Native Species", "Exotic"))

comp_long$mg_order <- reorder(comp_long$mg_short, desc(comp_long$lat_rank))

p <-
      ggplot(comp_long,
                aes(x = Metric, y = Mean, fill = Species)) +
       geom_bar(stat = 'identity') + facet_wrap(~mg_order, ncol = 4) +
       scale_fill_manual(values = c("Native Canopy" = "#99D899",
                                    "Other Native Species" = "#A7AFA7",
                                    "Exotic" = "#CD5C5C"))+
       labs(y = "Avg. % of Total Saplings or Seedlings", x = NULL) +
       coord_flip() +
       scale_y_continuous(limits = c(-0.01,1.01),
                          breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
                          labels = c("0", "25", "50", "75", "100"))+

       theme_bw()+
       theme(axis.text = element_text(size = 10),
             axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
             axis.text.y = element_text(size = 10),
             axis.title = element_text(size = 10),
             plot.title = element_text(hjust = 0.5, size = 11, margin = margin(t = 10, b = -15)),
             panel.background = element_blank(),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             axis.line = element_blank(),
             legend.position = 'none')

p

ggsave("./results/20220325/cycle_3_status_plots/Prop_species_groups_mg.svg", width = 11, height = 8, units = 'in')

  svg("./results/Prop_species_groups_option1_mg.svg",
      height = 8, width = 11)

  grid.arrange(grobs = list(p, leg_gbarsp),
               heights = c(6.5, 0.5),
               widths = c(0.55, 7.125, 0.25),
               layout_matrix = rbind(c(1, 1, 1),
                                     c(NA, 2, NA)))
  dev.off()


#----- Species level composition plot  -----
comp_mg <- read.csv("./data/EFWG_proportion_regen_species_20220325_mg.csv")
head(comp_mg)

comp_mg_long <- comp_mg %>% select(mg_short, lat_rank, sap_ba_pct_FRAX:seed_dens_pct_ASITRI) %>% 
    pivot_longer(sap_ba_pct_FRAX:seed_dens_pct_ASITRI, 
                 names_to = "Metric", values_to = "Mean") %>% 
    mutate(Species = case_when(grepl("FRAX", .$Metric) ~ "Ash species",
                               grepl("FAGGRA", .$Metric) ~ "American Beech",
                               grepl("ASITRI", .$Metric) ~ "Paw paw"),
           Metric = case_when(grepl("sap_ba", .$Metric) ~ "Sapling BA", 
                              grepl("sap_dens", .$Metric) ~ "Sapling Density",
                              grepl("seed_dens", .$Metric) ~ "Seedling Density"))
  
head(comp_mg_long)
  
comp_mg_long$Species <- factor(comp_mg_long$Species, 
                               levels = c("Ash species", 
                                          "American Beech", 
                                          "Paw paw"))
  
comp_mg_long$mg_order <- reorder(comp_mg_long$mg_short, desc(comp_mg_long$lat_rank))
  
# Fake plot for species group legend
leg_bar_spp <- ggplot(data = data.frame(spgrp = c("Ash", "Beech", "Pawpaw"),
                                        x = c(1, 2, 3)), 
                      aes(x = x, color = spgrp, fill = spgrp, shape = spgrp))+
  theme_bw()+ 
  geom_histogram()+
  scale_fill_manual(values = c("Ash" = "#76AB81", "American Beech" = "#F9CB80", "Paw paw" = "#94BDEC"), 
                    labels = c("Ash species (Fraxinus spp.)", 
                               "American beech (Fagus grandifolia)", 
                               "Paw paw (Asimina triloba)"),
                    name = "Species:",
                    drop = FALSE)+
  scale_color_manual(values = c("Ash" = "#76AB81", 
                                "American Beech" = "#F9CB80", 
                                "Paw paw" = "#94BDEC"), 
                     labels = c("Ash species (Fraxinus spp.)", 
                                "American beech (Fagus grandifolia)", 
                                "Paw paw (Asimina triloba)"),
                     name = "Species:",
                     drop = FALSE)+
  theme(legend.position = 'bottom', legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10), #legend.background = element_blank()
        legend.background = element_rect(colour = "#B9B9B9", size = 0.25)
  )

leg_gbar_spp <- gtable_filter(ggplot_gtable(ggplot_build(leg_bar_spp)), "guide-box")


p <- 
  ggplot(comp_mg_long, 
         aes(x = Metric, y = Mean, fill = Species)) +
  geom_bar(stat = 'identity') + facet_wrap(~mg_order, ncol = 4) + 
  labs(y = "Average % of Total Saplings or Seedlings", x = NULL) + 
  
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
                     labels = c("0", "25", "50", "75", "100"))+
  scale_fill_manual(values = c("Ash species" = "#76AB81", #"#A87000", 
                               "American Beech" = "#F9CB80", 
                               "Paw paw" = "#94BDEC"), 
                    labels = c("Ash species", "American Beech", "Paw paw"),
                    name = "Species:",
                    drop = FALSE)+
  coord_flip()+
  theme_bw()+
  theme(#axis.text = element_text(size = 10),
    axis.text.x = element_text(size = 11, angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 11),
    plot.title = element_text(hjust = 0.5, size = 11, margin = margin(t = 10, b = -15)),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    legend.position = 'none')

p

svg("./results/Prop_species_mg.svg",
    height = 8, width = 11)

grid.arrange(grobs = list(p, leg_gbar_spp),
             heights = c(6.5, 0.5),
             widths = c(0.55, 7.125, 0.25),
             layout_matrix = rbind(c(1, 1, 1),
                                   c(NA, 2, NA)))
dev.off()

