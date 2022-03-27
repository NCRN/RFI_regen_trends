#-----------------------------------------
# Trend Analysis by Vegetation MacroGroup
#-----------------------------------------
library(forestTrends)
library(tidyverse)

options("scipen" = 100, "digits" = 4) # keeps scientific notation from showing up

dens_df1 <- read.csv("./data/EFWG_full_dataset_20220325.csv") %>% select(-contains("Native"))
veggrp <- read.csv("./data/EFWG_veggroups.csv")
names(veggrp)

dens_df <- right_join(veggrp %>% select(Plot_Name, MACROGROUP_NAME, MACROGROUP_KEY, GROUP_NAME, GROUP_KEY),
                      dens_df1, by = "Plot_Name")

#---- Set up lists to iterate over ----
# response titles for plotting
# Clean up column order to make lists easier
names(dens_df)
stay <- names(dens_df[,c(1, 6:20, 2:5)])
reord <- sort(names(dens_df[,21:ncol(dens_df)]))

dens_df <- dens_df[, c(stay, reord)]

# column names for response
metrics <- c(names(dens_df[, c(21:ncol(dens_df))]))
metrics

metric_names <- c("Horn Sapling", "Horn Seedling",
                  rep("Sapling BA (m2/ha)", 4),
                  rep("Sapling Density (stems/sq.m)", 4),
                  rep("Seedling Density (sq.m/ha)", 4),
                  "Sorensen Sapling", "Sorensen Seedling",
                  "Stocking Index",
                  rep("Tree BA 10 cm (sq.m/ha)", 4),
                  rep("Tree Dens. 10 cm (stems/ha)", 4),
                  rep("Tree BA 20 cm (sq.m/ha)", 4),
                  rep("Tree Dens. 20 cm (stems/ha)", 4),
                  rep("Tree BA 30 cm (sq.m/ha)", 4),
                  rep("Tree Dens. 30 cm (stems/ha)", 4),
                  rep("Tree Dens. 40 cm+ (stems/ha)", 4),
                  rep("Tree BA 40 cm+ (sq.m/ha)", 4),
                  rep("Tree BA (sq.m/ha)", 4),
                  rep("Tree Density (stems/ha)", 4))

met_df <- data.frame(metrics, metric_names)

#----- Macrogroup analyses -----
# Only run for macrogroups with > 50 plots
macrogrps <- dens_df %>% filter(cycle == 3) %>%
  group_by(MACROGROUP_NAME) %>% summarize(num_plots = sum(!is.na(Plot_Name))) %>%
  filter(num_plots >= 50) %>% select(MACROGROUP_NAME)

macrogrps <- sort(unique(macrogrps$MACROGROUP_NAME))

macrogrp_met_list <- data.frame(expand.grid(macrogrps, metrics)) %>%
  set_names("MACROGROUP_NAME", "metrics") %>%
  arrange(MACROGROUP_NAME, metrics) %>%
  mutate(across(where(is.factor), as.character)) %>%
  left_join(., met_df, by = "metrics")

dens_grp <- dens_df %>% filter(MACROGROUP_NAME %in% macrogrps)

# works when sample = T, but not sample = F
boot_results_mg <- map2_dfr(macrogrp_met_list[,1], macrogrp_met_list[,2],
                           function(mgrp, y){case_boot_lmer(df = dens_grp %>% filter(MACROGROUP_NAME == mgrp),
                                                            x = "cycle", y = y, ID = "Plot_Name",
                                                            group = "MACROGROUP_NAME",
                                                            random_type = 'custom',
                                                            random_formula = "(1|Unit_Code/Plot_Name)",
                                                            nest_var = "Unit_Code",
                                                            num_reps = 1000, chatty = TRUE) %>%
                   mutate(Macrogroup = paste(mgrp), resp = paste(y))})

write.csv(boot_results_mg, "./results/20220325/all_metrics_randint_macrogroup_20220325.csv", row.names = F)

