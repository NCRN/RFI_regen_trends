#----------------------------------
# GETT trend analysis
#----------------------------------

#devtools::install_github("KateMMiller/forestTrends") #uncomment to install
library(forestTrends)
library(tidyverse)
# library(furrr) # for parallel processing with map
# library(future)

options("scipen" = 100, "digits" = 4) # keeps scientific notation from showing up

dens_df <- read.csv("./data/EFWG_full_dataset_20220325_ash_as_can.csv")

dens_df2 <- dens_df %>% select(-contains("Native")) # Only interested in NatCan and NatOth

stay <- names(dens_df[,1:16])
reord <- sort(names(dens_df2[,17:ncol(dens_df2)]))

dens_df2 <- dens_df2[, c(stay, reord)]
names(dens_df2)

# Added park b/c Unit_Code gets dropped from data after nested. Only need 'park' for progress in console
# Don't need to drop SAHI or WOTR b/c case_boot_lmer will exclude parks with < 7 plots from
# the bootstrap, but will fit a model to the raw data for plotting

#---- Set up lists to iterate over ----
# column names for response
metrics <- c(names(dens_df2[, c(19:33, 66:ncol(dens_df2))]))
metrics

# response titles for plotting
metric_names <- c(#"Horn Sapling", "Horn Seedling", 
                  rep("Sapling BA (sq.m/ha)", 4),
                  rep("Sapling Density (stems/sq.m)", 4),
                  rep("Seedling Density (stems/sq.m)", 4), 
                  "Sorensen Sapling", "Sorensen Seedling",
                  "Stocking Index",
                  # rep("Tree BA 10 cm (sq.m/ha)", 4),  
                  # rep("Tree Dens. 10 cm (stems/ha)", 4),  
                  # rep("Tree BA 20 cm (sq.m/ha)", 4), 
                  # rep("Tree Dens. 20 cm (stems/ha)", 4), 
                  # rep("Tree BA 30 cm (sq.m/ha)", 4),
                  # rep("Tree Dens. 30 cm (stems/ha)", 4),
                  # rep("Tree Dens. 40 cm+ (stems/ha)", 4),
                  # rep("Tree BA 40 cm+ (sq.m/ha)", 4),
                  rep("Tree BA (sq.m/ha)", 4), 
                  rep("Tree Density (stems/ha)", 4)
)

# match metric columns and titles
met_df <- data.frame(metrics, metric_names)
met_df

park_met_list <- data.frame(expand.grid("GETT", metrics)) %>% 
  set_names("Unit_Code", "metrics") %>% 
  arrange(Unit_Code, metrics) %>% 
  mutate(across(where(is.factor), as.character)) %>% 
  left_join(., met_df, by = "metrics")

park_list <- c(park_met_list[,1])
met_list <- c(park_met_list[,2])
park_list
met_list

# # Set up parallel processing
# availableCores() #12
# future::plan(multicore, workers = 11)
gett <- dens_df2 %>% filter(Unit_Code == "GETT")

# Run the random intercept model for each combination of park and metric.
boot_results <- map_dfr(met_list,
                  function(y){case_boot_lmer(df = gett,
                                             x = "cycle", y = y, ID = "Plot_Name",
                                             random_type = 'intercept',
                                             num_reps = 1000, chatty = TRUE) %>%
                  mutate(park = "GETT", resp = paste(y))})


head(boot_results2)
head(boot_results)
#started 1:10 pm 11/2/21
#write.csv(boot_results, "./results/20220325/all_metrics_randint_20220325_gett_ash_as_can.csv", row.names = F)

boot_results <- read.csv("./results/20220325/all_metrics_randint_20220325_gett_ash_as_can.csv") %>% 
  filter(resp != "stock_final")

# boot_results3 <- left_join(boot_results2, unique(boot_results %>% select(park, Network:strp_col)), 
#                            by = "park")
# 
# boot_results <- rbind(boot_results, boot_results3) %>% arrange(Network, park, resp)
# 
# write.csv(boot_results, "./results/20220325/all_metrics_randint_20220325_as_as_can.csv", row.names = F)
