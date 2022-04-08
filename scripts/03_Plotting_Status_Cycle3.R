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
dens_df <- read.csv("./data/EFWG_full_dataset_20220325.csv")
table(dens_df$Network)
#dens_df$Network <- ifelse(dens_df$Unit_Code %in% c("COLO", "GEWA", "SAHI", "THST"), "NCBN", dens_df$Network)

dens_c3 <- dens_df %>% filter(cycle == 3) 
park_list <- unique(dens_df$Unit_Code)
status_metrics <- c("stock_final", 
                    "Sap_Dens_Total", "Seed_Dens_Total",
                    "Sap_Dens_NatCan", "Seed_Dens_NatCan",
                    "Sor_sap", "Hor_sap", "Sor_seed", "Hor_seed")

# Load trend results for later use
boot_results1 <- read.csv("./results/20220325/all_metrics_randint_20220325.csv")
lat_rank <- read.csv("./data/EFWG_lat_order.csv")[,c("park", "network", "lat.rank")]
boot_results <- left_join(boot_results1, lat_rank, by = 'park') %>% rename(lat_rank = lat.rank)
boot_results$park_ord <- reorder(boot_results$park, desc(boot_results$lat_rank))
boot_results$network[boot_results$park %in% c("COLO", "SAHI", "THST")] <- "NCBN" # in case this hasn't been changed
boot_results$network_ord <- factor(boot_results$network, levels = c("NETN", "ERMN", "NCRN", "NCBN", "MIDN"))
table(boot_results$network)

boot_results$strp_col <- case_when(boot_results$network == "ERMN" ~ "#A5BDCD",
                                   boot_results$network == "MIDN" ~ "#E7CDA4",
                                   boot_results$network == "NCBN" ~ "#CFB9D9",
                                   boot_results$network == "NCRN" ~ "#E1E59B",
                                   boot_results$network == "NETN" ~ "#AACCA7") 

boot_results <- boot_results %>% rename(Network = network)
table(boot_results$Network)

boot_sppgrp <- boot_results %>% filter(!resp %in% c("Hor_sap", "Hor_seed", "Sor_sap", "Sor_seed", "stock_final"))

# Set up groups for comp plot
boot_sppgrp$sppgrp <- gsub("^.*_", "", boot_sppgrp$resp)
sppgrp_list <- c(paste0("_", unique(boot_sppgrp$sppgrp), collapse = "|"))
boot_sppgrp$metric <- gsub(sppgrp_list, "", boot_sppgrp$resp)

boot_sppgrp_comb <- boot_sppgrp %>% filter(sppgrp %in% c("NatCan", "NatOth", "Exotic")) %>% arrange(park, resp, term)

boot_sppgrp_comb$park_ord <- reorder(boot_sppgrp_comb$park, desc(boot_sppgrp_comb$lat_rank))
boot_sppgrp_comb$estimate[boot_sppgrp_comb$estimate == 0] <- NA
boot_sppgrp_comb$lower95[is.na(boot_sppgrp_comb$estimate)] <- NA
boot_sppgrp_comb$upper95[is.na(boot_sppgrp_comb$estimate)] <- NA

# Set up park lists
park_metrics <- expand.grid(park_list, status_metrics)
parks <- park_metrics[,1]
metrics <- park_metrics[,2]

# Create mean function used in bootstrap
mean_fun <- function(data, i){
  d <- data[i, ]
  return(mean(d, na.rm = TRUE))
}

# Iterate over parks and metrics to calc. mean and 95% CI of mean 
c3_stats <- map2_dfr(parks, metrics, function(park, metric){
  df <- dens_c3 %>% filter(Unit_Code == park) 
  results_df <- boot(df[, paste0(metric), drop = FALSE], mean_fun, R = 1000)
  df2 <- data.frame(park = park, 
                    Network = unique(df$Network),
                    metric = metric, 
                    mean = results_df$t0,
                    lower95 = quantile(results_df$t, probs = 0.025, na.rm = T),
                    upper95 = quantile(results_df$t, probs = 0.975, na.rm = T),
                    row.names = NULL)
  return(df2)
})

write.csv(c3_stats, "./results/20220325/EFWG_cycle_3_status.csv", row.names = FALSE)
#c3_stats <- read.csv("./results/EFWG_cycle_3_status.csv")

# Plotting function that sorts from high to low and color codes by network
plot_c3_status <- function(df, met, xlabel, ylabel){
  df2 <- df %>% filter(metric == met)
  df2$park_ord <- reorder(df2$park, desc(df2$mean))
  
  p <- ggplot(df2, 
              aes(x = park_ord, y = mean, fill = as.factor(Network)))+
    geom_bar(stat = 'identity', aes(fill = as.factor(Network)), col = "#868686")+ 
    geom_errorbar(aes(ymin = lower95, ymax = upper95), color = "#868686", 
                  width = 0.15, size = 0.4, na.rm = TRUE)+
    scale_fill_manual(values = c("ERMN" = "#A5BDCD", "MIDN" = "#E7CDA4", "NCBN" = "#CFB9D9", 
                                 "NCRN" = "#E1E59B", "NETN" = "#AACCA7"), 
                      name = "Network:")+
    theme_bw()+
    labs(x = xlabel, y = ylabel)+
    theme(axis.text = element_text(size = 14),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 14),
          plot.title = element_text(hjust = 0.5, size = 14, 
                                    margin = margin(t = 10, b = -15)),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.line = element_blank(), 
          legend.position = 'bottom')
  
  return(p)
  
}

# Not going to iterate, so I can customize some of the plots before saving.

plot_c3_status(c3_stats, "stock_final", xlabel = NULL, ylabel = "Mean Stocking Index")+
  geom_hline(yintercept = 25, linetype = 2, lwd = 0.75, color = "DimGrey")+ #'#CD5C5C')+
  geom_text(x = 39, y = 22.5, label = "Critical", hjust = 1, size = 4.5, check_overlap = TRUE, color = "#696969")+
  geom_text(x = 39, y = 64.5, label = "Caution", hjust = 1, size = 4.5, check_overlap = TRUE, color = "#696969")+
  geom_hline(yintercept = 100, linetype = 2, lwd = 0.75, color = "DimGrey")+ #"#228B22")+
  geom_text(x = 39, y = 104, hjust = 1, label = "Acceptable", size = 4.5, check_overlap = TRUE, color = "#696969")

ggsave("./results/20220325/cycle_3_status_plots/Cycle_3_stocking_index.svg", width = 11, height = 8, units = 'in')

plot_c3_status(c3_stats, "stock_final", xlabel = NULL, ylabel = "Mean Stocking Index")
ggsave("./results/20220325/cycle_3_status_plots/Cycle_3_stocking_index_no_lines.svg", width = 11, height = 8, units = 'in')

plot_c3_status(c3_stats, "Sap_Dens_NatCan", xlabel = NULL, ylabel = "Sapling Density (stems/sq.m)")
ggsave("./results/20220325/cycle_3_status_plots/Cycle_3_sapling_density_natcan.svg", width = 11, height = 8, units = 'in')

plot_c3_status(c3_stats, "Seed_Dens_NatCan", xlabel = NULL, ylabel = "Seedling Density (stems/sq.m)")
ggsave("./results/20220325/cycle_3_status_plots/Cycle_3_seedling_density_natcan.svg", width = 11, height = 8, units = 'in')

plot_c3_status(c3_stats, "Sor_sap", xlabel = NULL, ylabel = "Sorensen: Sapling vs. Canopy")
ggsave("./results/20220325/cycle_3_status_plots/Cycle_3_Sorensen_sapling_vs_canopy.svg", width = 11, height = 8, units = 'in')

plot_c3_status(c3_stats, "Sor_seed", xlabel = NULL, ylabel = "Sorensen: Seedling vs. Canopy")
ggsave("./results/20220325/cycle_3_status_plots/Cycle_3_Sorensen_seedling_vs_canopy.svg", width = 11, height = 8, units = 'in')

plot_c3_status(c3_stats, "Hor_sap", xlabel = NULL, ylabel = "Horn: Sapling vs. Canopy")
ggsave("./results/20220325/cycle_3_status_plots/Cycle_3_Horn_sapling_vs_canopy.svg", width = 11, height = 8, units = 'in')

plot_c3_status(c3_stats, "Hor_seed", xlabel = NULL, ylabel = "Horn: Seedling vs. Canopy")
ggsave("./results/20220325/cycle_3_status_plots/Cycle_3_Horn_seedling_vs_canopy.svg", width = 11, height = 8, units = 'in')

#---- Summarize proportion of stocked plots based on deer browse index
dens_df <- read.csv("./data/EFWG_full_dataset_20220325.csv")
table(dens_df$Network)
head(dens_c3)

dens_c3 <- dens_df %>% filter(cycle == 3) 

dbi_stock_c3 <- dens_c3 %>% group_by(Network, Unit_Code, lat_rank) %>% 
  mutate(avg_DBI = mean(DBI, na.rm = T)) %>% ungroup() %>% 
  mutate(stocked = case_when(avg_DBI <= 3 & stock_final >= 50 ~ 1,
                             avg_DBI > 3 & stock_final >= 100 ~ 1,
                             TRUE ~ 0))

dbi_stock_sum <- dbi_stock_c3 %>% group_by(Network, Unit_Code, lat_rank) %>% 
                   summarize(avg_stock = mean(stock_final, na.rm = TRUE),
                             avg_DBI = first(avg_DBI),
                             num_stocked = sum(stocked, na.rm = TRUE),
                             num_plots = sum(!is.na(stocked)),
                             prop_stocked = num_stocked/num_plots) %>% 
                   rename(park = Unit_Code)

dbi_stock_sum$park_ord <- reorder(dbi_stock_sum$park, desc(dbi_stock_sum$prop_stocked))

write.csv(dbi_stock_sum, "./results/20220325/cycle_3_status_plots/cycle_3_DBI_prop_stock.csv", row.names = FALSE)

head(dbi_stock_sum)
head(c3_stats)

p <- 
  ggplot(dbi_stock_sum,
            aes(x = park_ord, y = prop_stocked, fill = as.factor(Network)))+
  geom_bar(stat = 'identity', aes(fill = as.factor(Network)), color = "#868686")+ 
  scale_fill_manual(values = c("ERMN" = "#A5BDCD", "MIDN" = "#E7CDA4", "NCBN" = "#CFB9D9", 
                               "NCRN" = "#E1E59B", "NETN" = "#AACCA7"), 
                    name = "Network:")+
  scale_y_continuous(limits = c(0, 0.7), breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7), 
                     labels = c("0", "10", "20", "30", "40", "50", "60", "70"))+  
  theme_bw()+
  labs(x = NULL, y = "% Stocked Plots")+
  theme(axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 14, 
                                  margin = margin(t = 10, b = -15)),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_blank(), 
        legend.position = 'bottom')+
  geom_hline(yintercept = 0.32, lty = 2, lwd = 0.75, color = "#696969")+
  geom_text(x = 39, y = 0.30, label = "Critical", hjust = 1, size = 4.5, check_overlap = TRUE, color = "#696969")+
  geom_hline(yintercept = 0.66, lty = 2, lwd = 0.75, color = "#696969")+
  geom_text(x = 39, y = 0.495, label = "Caution", hjust = 1, size = 4.5, check_overlap = TRUE, color = "#696969")+
  geom_text(x = 39, y = 0.68, hjust = 1, label = "Acceptable", size = 4.5, check_overlap = TRUE, color = "#696969")
  
p

ggsave("./results/20220325/cycle_3_status_plots/Pct_stocked_plots.svg", width = 11, height = 8, units = 'in')


#---- Composition plots -----
comp <- read.csv("./data/EFWG_proportion_regen_20220325.csv")
comp_lat <- left_join(comp, dens_c3 %>% select(Unit_Code, lat_rank) %>% unique(),
                      by = c("Unit_Code", "lat_rank"))

head(boot_sppgrp_comb)

# Matching networks to colors for facet strips
park_cols <- rbind(data.frame(park_ord = "BLNK", strp_col = "white"),
                   unique(data.frame(park_ord = boot_sppgrp_comb$park_ord,
                                     strp_col = boot_sppgrp_comb$strp_col)) %>% arrange(desc(park_ord))
)

park_cols <- park_cols %>% mutate(facet_ord = c(rev(202:209), rev(210:217), rev(218:225), 
                                                rev(226:233), rev(234:241))) %>% 
  arrange(facet_ord)

fills <- c(park_cols$strp_col) 

# Fake plot for network legend
leg_net <- ggplot(data = data.frame(network = c("ERMN", "MIDN", "NCBN", "NCRN", "NETN"),
                                    x = c(1, 2, 3, 4, 5)),
                  aes(x = x, fill = network))+
  geom_histogram()+
  scale_fill_manual(values = c("#A5BDCD", "#E7CDA4", "#CFB9D9", "#E1E59B", "#AACCA7"), 
                    name = "Network:")+ theme_bw()+
  theme(legend.position = 'bottom', 
        legend.title = element_text(size = 10), 
        legend.text=element_text(size = 10),
        #legend.background = element_blank()
        legend.background = element_rect(color = "#B9B9B9", size = 0.25)
  )

leg_gnet <- gtable_filter(ggplot_gtable(ggplot_build(leg_net)), "guide-box")

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

#----- Plot all 3 groups, so adds to 1
comp_long <- comp_lat %>% select(Network, Unit_Code, lat_rank, sap_ba_pct_NatCan_rel:seed_dens_pct_Exotic_rel) %>% 
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

comp_long$park_order <- reorder(comp_long$Unit_Code, desc(comp_long$lat_rank))

p <-
      ggplot(comp_long,
                aes(x = Metric, y = Mean, fill = Species)) +
       geom_bar(stat = 'identity') + facet_wrap(~park_order, ncol = 8) +
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
  svg("./results/Prop_species_groups_option1.svg",
      height = 8, width = 11)

  grid.arrange(grobs = list(p, leg_gbarsp),
               heights = c(6.5, 0.5),
               widths = c(0.55, 7.125, 0.25),
               layout_matrix = rbind(c(1, 1, 1),
                                     c(NA, 2, NA)))
  dev.off()


# Species level composition plot  
comp <- read.csv("./data/EFWG_proportion_regen_20211104.csv")

comp_lat <- left_join(comp, dens_c3 %>% select(Unit_Code, lat_rank) %>% unique(),
                        by = c("Unit_Code"))
  
comp_spp <- comp_lat %>% filter(Species %in% c("Ash", "American Beech", "Paw paw"))
comp_spp$park_order <- reorder(comp_spp$Unit_Code, desc(comp_spp$lat_rank))

# Fake plot for species group legend
leg_bar_spp <- ggplot(data = data.frame(spgrp = c("Exotic", "NatOth"),
                                        x = c(1, 2)), 
                      aes(x = x, color = spgrp, fill = spgrp, shape = spgrp))+
  theme_bw()+ 
  geom_histogram()+
  scale_fill_manual(values = c("Ash" = "#76AB81", "American Beech" = "#F9CB80", "Paw paw" = "#94BDEC"), 
                    labels = c("Ash species", "American Beech", "Paw paw"),
                    name = "Species:",
                    drop = FALSE)+
  scale_color_manual(values = c("Ash" = "#76AB81", "American Beech" = "#F9CB80", "Paw paw" = "#94BDEC"), 
                     labels = c("Ash species", "American Beech", "Paw paw"),
                     name = "Species:",
                     drop = FALSE)+
  theme(legend.position = 'bottom', legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10), #legend.background = element_blank()
        legend.background = element_rect(colour = "#B9B9B9", size = 0.25)
  )

leg_gbar_spp <- gtable_filter(ggplot_gtable(ggplot_build(leg_bar_spp)), "guide-box")


p <- 
  ggplot(comp_spp, 
         aes(x = Metric, y = Mean, fill = Species)) +
  geom_bar(stat = 'identity') + facet_wrap(~park_order, ncol = 8) + 
  labs(y = "Average % of Total Saplings or Seedlings", x = NULL) + 
  
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
                     labels = c("0", "25", "50", "75", "100"))+
  scale_fill_manual(values = c("Ash" = "#76AB81", #"#A87000", 
                               "American Beech" = "#F9CB80", 
                               "Paw paw" = "#94BDEC"), 
                    labels = c("Ash spp.", "American Beech", "Pawpaw"),
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

g <- ggplot_gtable(ggplot_build(p))
strp_col <- which(grepl('strip-t', g$layout$name))

k <- 1
for (i in strp_col) {
  g$grobs[[i]]$grobs[[1]]$children[[1]]$gp$fill <- fills[k]
  k <- k+1
}

svg("./results/Prop_species.svg", 
    height = 8, width = 11) 

grid.arrange(grobs = list(g, leg_gbar_spp, leg_gnet),
             heights = c(6.5, 0.5),
             widths = c(0.5, 3.5, 3, 0.5),
             layout_matrix = rbind(c(1, 1, 1, 1),
                                   c(NA, 2, 3, NA)))
dev.off()
