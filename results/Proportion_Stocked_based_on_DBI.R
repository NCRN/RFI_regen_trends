library(tidyverse)

options("scipen" = 100, "digits" = 4) # keeps scientific notation from showing up

datapath <- "D:/NETN/R_Dev/NPSForVeg_Data/"

dens_df <- read.csv(paste0(datapath, "EFWG_full_dataset_20211101.csv"))
table(dens_df$Network)

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
  rename(park = Unit_Code)

dbi_stock_sum
write.csv(dbi_stock_sum, paste0(datapath, "/results/cycle_3_status_plots/cycle_3_DBI_prop_stock.csv"), row.names = FALSE)