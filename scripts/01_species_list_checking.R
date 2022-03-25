#-------------------------
# Species list checking for regen analysis
# Code written by Kate Miller on 3/25/22
#-------------------------
library(tidyverse)
list.files('./data/NCRN_data')


network = "NCRN"
data = "Trees"

import_data <- function(network, data){
  dat <- read.csv(paste0("./data/", network, "_data/", data, ".csv")) 
  if(data = 'Trees'){dat %>% filter()}
  
  dat2 <- dat %>% 
    mutate(network = network, present = 1, data = data) %>% 
    select(network, Latin_Name, present, Sample_Year, data) %>% 
    filter(between(Sample_Year, 2008, 2019)) %>% select(-Sample_Year) %>% 
    unique()
  
}

ermn_spp <- rbind(import_data("ERMN", "Trees"),
                  import_data("ERMN", "Saplings"),
                  import_data("ERMN", "Seedlings")) %>% unique()

midn_spp <- rbind(import_data("MIDN", "Trees"),
                  import_data("MIDN", "Saplings"),
                  import_data("MIDN", "Seedlings")) %>% unique()

ncrn_spp <- rbind(import_data("NCRN", "Trees"),
                  import_data("NCRN", "Saplings"),
                  import_data("NCRN", "Seedlings")) %>% unique()

netn_spp <- rbind(import_data("NETN", "Trees"),
                  import_data("NETN", "Saplings"),
                  import_data("NETN", "Seedlings")) %>% unique()

spp_list <- rbind(ermn_spp, midn_spp, ncrn_spp, netn_spp) %>% 
  pivot_wider(names_from = network, values_from = present) %>% arrange(Latin_Name)

spp_list # Full list of species in tree, sapling and seedling strata.
# Now compare with the list we're using.

spp_list_analysis <- read.csv("./data/NPS_tree_species_groups.csv")
spp_list_analysis$Species[grepl("Quercus illicifolia", spp_list_analysis$Species)] <- "Quercus ilicifolia"
spp_list_analysis$Native[grepl("Quercus illicifolia", spp_list_analysis$Species)] <- 1
spp_list_analysis$New <- NA_real_

missing_spp <- spp_list %>% filter(!Latin_Name %in% spp_list_analysis$Species) %>% 
  filter(!Latin_Name %in% c("No species recorded", "Unknown Tree - 03") & !is.na(Latin_Name))
missing_spp

write.csv(missing_spp, "./data/NPS_species_missing.csv", row.names = F)

spp_list_binder <- missing_spp %>% select(Species = Latin_Name) %>% 
                     mutate(Group = c("Other Native", 
                                      "Post Ag",
                                      "Other Native",
                                      "Other Native",
                                      "Other Native", 
                                      "Other Native", 
                                      "Exotic"), 
                            Canopy_Tree = c(1, 0, 0, 0, 1, 0, 0), 
                            Native = c(1, 0, 1, 1, 1, 1, 0), 
                            New = rep(1, 7))
spp_list_binder

species_list_fixed <- rbind(spp_list_analysis, spp_list_binder)
write.csv(species_list_fixed, "./data/NPS_tree_species_groups.csv", row.names = F)
