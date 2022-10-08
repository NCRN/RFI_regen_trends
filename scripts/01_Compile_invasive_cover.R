#----------------------
# Compile invasive quadrat cover for status analysis
#----------------------

library(tidyverse)

# NETN
forestNETN::importData()

inv_cov_netn1 <- forestNETN::sumSpeciesList(from = 2016, to = 2019, speciesType = "invasive") %>% 
  select(Plot_Name, quad_avg_cov)

inv_cov_netn <- inv_cov_netn1 %>% group_by(Plot_Name) %>% summarize(avg.cover = sum(quad_avg_cov, na.rm = T)) 

# MIDN
forestMIDN::importData()

inv_cov_midn1 <- forestMIDN::sumSpeciesList(from = 2016, to = 2019, speciesType = 'invasive') %>% 
  select(Plot_Name, quad_avg_cov)

inv_cov_midn <- inv_cov_midn1 %>% group_by(Plot_Name) %>% summarize(avg.cover = sum(quad_avg_cov, na.rm = T))

inv_cov2 <- rbind(inv_cov_netn, inv_cov_midn)

write.csv(inv_cov2, "./data/MIDN-NETN_2016-2019_invasive_cover.csv", row.names = FALSE)

# ERMN
inv_cov_ermn <- read.csv("./data/ERMN_Total_Invasives_2019.csv") %>% filter(between(year, 2016, 2019)) %>% 
 select(Plot_Name = plot_name, avg.cover)
head(inv_cov_ermn)

# NCRN 
inv_cov_ncrn <- read.csv("./data/NCRN_2016_2019_total_invasives.csv") %>% select(Plot_Name = plot_name, avg.cover)

inv_cov <- rbind(inv_cov_netn, inv_cov_midn, inv_cov_ermn, inv_cov_ncrn)

write.csv(inv_cov, "./data/EFWG_invasive_cover_2016-2019.csv", row.names = FALSE)
