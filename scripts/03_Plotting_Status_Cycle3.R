#---------------------------------------------------------------------------------------------------
# Compiling cycle 3 (2016:2019) data for comparisons of tree and regeneration status across parks
#   Code written by Kate Miller 20211012 
#---------------------------------------------------------------------------------------------------
library(tidyverse)
library(boot)

options("scipen" = 100, "digits" = 4) # keeps scientific notation from showing up

datapath <- "./data"

#----- Status plots -----
# For status plots, plot the raw mean and 95% confidence interval derived from bootstrapping
dens_df <- read.csv("./data/EFWG_full_dataset_20220325.csv"))
table(dens_df$Network)
#dens_df$Network <- ifelse(dens_df$Unit_Code %in% c("COLO", "GEWA", "SAHI", "THST"), "NCBN", dens_df$Network)

dens_c3 <- dens_df %>% filter(cycle == 3) 
park_list <- unique(dens_df$Unit_Code)
status_metrics <- c("stock_final", 
                    "Sap_Dens_Total", "Seed_Dens_Total",
                    "Sap_Dens_NatCan", "Seed_Dens_NatCan",
                    "Sor_sap", "Hor_sap", "Sor_seed", "Hor_seed")

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

write.csv(c3_stats, "./results/20220325/20220325/EFWG_cycle_3_status.csv", row.names = FALSE)
#c3_stats <- read.csv(paste0(datapath, "results/EFWG_cycle_3_status.csv"))

# Plotting function that sorts from high to low and color codes by network
plot_c3_status <- function(df, met, xlabel, ylabel){
  df2 <- df %>% filter(metric == met)
  df2$park_ord <- reorder(df2$park, desc(df2$mean))
  
  p <- ggplot(df2, 
              aes(x = park_ord, y = mean, fill = as.factor(Network)))+
    geom_bar(stat = 'identity', aes(fill = as.factor(Network)))+ 
    geom_errorbar(aes(ymin = lower95, ymax = upper95), color = "#B9B9B9", 
                  width = 0.1, size = 0.5, na.rm = TRUE)+
    scale_fill_manual(values = c("ERMN" = "#A5BDCD", "MIDN" = "#E7CDA4", "NCBN" = "#CFB9D9", 
                                 "NCRN" = "#E1E59B", "NETN" = "#AACCA7"), 
                      name = "Network:")+
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
library(tidyverse)

options("scipen" = 100, "digits" = 4) # keeps scientific notation from showing up

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
  geom_bar(stat = 'identity', aes(fill = as.factor(Network)))+ 
  scale_fill_manual(values = c("ERMN" = "#A5BDCD", "MIDN" = "#E7CDA4", "NCBN" = "#CFB9D9", 
                               "NCRN" = "#E1E59B", "NETN" = "#AACCA7"), 
                    name = "Network:")+
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

ggsave("./results/20220325/cycle_3_status_plots/Pct_stocked_plots.svg", width = 11, height = 8, units = 'in')
