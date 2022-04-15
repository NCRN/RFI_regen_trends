#--------------------------------------------
# Compiling Tree, Sapling and Seedling Data for Eastern Forest Regen Project
#   Code written by Kate Miller 202010916 to check results using NPSForVeg package
#--------------------------------------------

#---- Dependencies and import data using NPSForVeg ----
#devtools::install_github("NCRN/NPSForVeg")
library(NPSForVeg)
library(tidyverse)
# library(rlang)
# library(vegan)

options(scipen = 100)

datapath <- "./data/" 

#------- Composition Proportion Data Compile --------
reg_final <- read.csv("./data/EFWG_full_dataset_20220325_ash_as_can.csv")
names(reg_final)
comp_data <- reg_final %>% select(Plot_Name, Network, Unit_Code, Year, cycle, lat_rank, excludeEvent,
                                  Sap_BA_NatCan, Sap_BA_NatOth, Sap_BA_Exotic,
                                  Sap_Dens_NatCan, Sap_Dens_NatOth, Sap_Dens_Exotic,
                                  Seed_Dens_NatCan, Seed_Dens_NatOth, Seed_Dens_Exotic)
comp_data_c3 <- comp_data %>% filter(cycle == 3 & excludeEvent == 0) 
table(complete.cases(comp_data_c3)) #4 F
comp_data_c3[which(!complete.cases(comp_data_c3)),]
# DEWA-159-2017 NA Seedling and Sapling Data (Converted to 0)
# CATO-0331-2018 NA Seedling Data (Converted to 0)
# CHOH-0015-2017 NA Seedling Data (Converted to 0)
# NACE-0493-2017 NA Seedling Data (Converted to 0)
names(comp_data_c3)

comp_data_c3[, 8:16][is.na(comp_data_c3[,8:16])] <- 0

table(comp_data_c3$excludeEvent) #all 0

# Calculate plot-level average, then average over all plots, then rescale to range between 0 & 1
comp_plot <- comp_data_c3 %>% group_by(Plot_Name, Network, Unit_Code, lat_rank) %>% 
  summarize(sap_ba_tot = sum(Sap_BA_NatCan + Sap_BA_NatOth + Sap_BA_Exotic),
            sap_dens_tot = sum(Sap_Dens_NatCan + Sap_Dens_NatOth + Sap_Dens_Exotic),
            seed_dens_tot = sum(Seed_Dens_NatCan + Seed_Dens_NatOth + Seed_Dens_Exotic),
            sap_ba_pct_NatCan = sum(Sap_BA_NatCan)/sap_ba_tot,
            sap_ba_pct_NatOth = sum(Sap_BA_NatOth)/sap_ba_tot,
            sap_ba_pct_Exotic = sum(Sap_BA_Exotic)/sap_ba_tot,
            sap_dens_pct_NatCan = sum(Sap_Dens_NatCan)/sap_dens_tot,
            sap_dens_pct_NatOth = sum(Sap_Dens_NatOth)/sap_dens_tot,
            sap_dens_pct_Exotic = sum(Sap_Dens_Exotic)/sap_dens_tot,
            seed_dens_pct_NatCan = sum(Seed_Dens_NatCan)/seed_dens_tot,
            seed_dens_pct_NatOth = sum(Seed_Dens_NatOth)/seed_dens_tot,
            seed_dens_pct_Exotic = sum(Seed_Dens_Exotic)/seed_dens_tot, 
            .groups = "drop") %>% data.frame()

comp_plot <- comp_plot %>% mutate(sap_ba_check = sap_ba_pct_NatCan + sap_ba_pct_NatOth + sap_ba_pct_Exotic,
                                  sap_dens_check = sap_dens_pct_NatCan + sap_dens_pct_NatOth + sap_dens_pct_Exotic,
                                  seed_dens_check = seed_dens_pct_NatCan + seed_dens_pct_NatOth + seed_dens_pct_Exotic)

comp_plot[, 5:19][is.na(comp_plot[, 5:19])] <- 0
names(comp_plot) #all plots with seedlings or saplings sum to 1

# Average composition at park level
comp_park <- comp_plot %>% group_by(Network, Unit_Code, lat_rank) %>% 
  summarize(sap_ba_pct_NatCan = mean(sap_ba_pct_NatCan),
            sap_ba_pct_NatOth = mean(sap_ba_pct_NatOth),
            sap_ba_pct_Exotic = mean(sap_ba_pct_Exotic),
            sap_dens_pct_NatCan = mean(sap_dens_pct_NatCan),
            sap_dens_pct_NatOth = mean(sap_dens_pct_NatOth),
            sap_dens_pct_Exotic = mean(sap_dens_pct_Exotic),
            seed_dens_pct_NatCan = mean(seed_dens_pct_NatCan),
            seed_dens_pct_NatOth = mean(seed_dens_pct_NatOth),
            seed_dens_pct_Exotic = mean(seed_dens_pct_Exotic),
            .groups = 'drop') %>% data.frame()

# Rescale to range from 0 to 1
comp_park_rel <- comp_park %>% 
  mutate(sap_ba_tot = sap_ba_pct_NatCan + sap_ba_pct_NatOth + sap_ba_pct_Exotic,
         sap_dens_tot = sap_dens_pct_NatCan + sap_dens_pct_NatOth + sap_dens_pct_Exotic,
         seed_dens_tot = seed_dens_pct_NatCan + seed_dens_pct_NatOth + seed_dens_pct_Exotic,
         sap_ba_pct_NatCan_rel = sap_ba_pct_NatCan/sap_ba_tot,
         sap_ba_pct_NatOth_rel = sap_ba_pct_NatOth/sap_ba_tot,
         sap_ba_pct_Exotic_rel = sap_ba_pct_Exotic/sap_ba_tot,
         sap_dens_pct_NatCan_rel = sap_dens_pct_NatCan/sap_dens_tot,
         sap_dens_pct_NatOth_rel = sap_dens_pct_NatOth/sap_dens_tot,
         sap_dens_pct_Exotic_rel = sap_dens_pct_Exotic/sap_dens_tot,
         seed_dens_pct_NatCan_rel = seed_dens_pct_NatCan/seed_dens_tot,
         seed_dens_pct_NatOth_rel = seed_dens_pct_NatOth/seed_dens_tot,
         seed_dens_pct_Exotic_rel = seed_dens_pct_Exotic/seed_dens_tot,
         sap_ba_check = sap_ba_pct_NatCan_rel + sap_ba_pct_NatOth_rel + sap_ba_pct_Exotic_rel,
         sap_dens_check = sap_dens_pct_NatCan_rel + sap_dens_pct_NatOth_rel + sap_dens_pct_Exotic_rel,
         seed_dens_check = seed_dens_pct_NatCan_rel + seed_dens_pct_NatOth_rel + seed_dens_pct_Exotic_rel)

write.csv(comp_park_rel, "./data/EFWG_proportion_regen_20220325_ash_as_can.csv", row.names = F)

