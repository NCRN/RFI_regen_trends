#--------------------------------------------
# Compiling Tree, Sapling and Seedling Data for Eastern Forest Regen Project
#   Code written by Kate Miller 202010916 to check results using NPSForVeg package
#--------------------------------------------

#---- Dependencies and import data using NPSForVeg ----
#devtools::install_github("NCRN/NPSForVeg")
library(NPSForVeg)
library(tidyverse)
library(rlang)
library(vegan)

options(scipen = 100)

datapath <- "./data/" 

# 20211012: Update to NCRN stem data columns
# ncrn_saps <- read.csv(paste0(datapath, "NCRN_data/Saplings.csv")) 
# ncrn_saps$StemsLive <- ifelse(ncrn_saps$Equiv_Live_DBH_cm > 0, 1, 0)
# # write.csv(ncrn_saps, paste0(datapath, "NCRN_data/Saplings.csv"), row.names = FALSE)
# ncrn_trees <- read.csv(paste0(datapath, "NCRN_data/Trees.csv"))
# ncrn_trees$Stems <- ifelse(ncrn_trees$Equiv_Live_DBH_cm > 0 | ncrn_trees$Equiv_Dead_DBH_cm > 0, 1, 0)
# # write.csv(ncrn_trees, paste0(datapath, "NCRN_data/Trees.csv"), row.names = FALSE)

ncrn <- importNCRN(paste0(datapath,"NCRN_data"))
ermn <- importERMN(paste0(datapath, "ERMN_data"))                   
midn <- importMIDN(paste0(datapath, "MIDN_data"))
netn <- importNETN(paste0(datapath, "NETN_data"))

spp_list <- read.csv(paste0(datapath, "NPS_tree_species_groups.csv")) 
# Promote Fraxinus to not canopy tree
spp_list$Canopy_Tree[grepl("Fraxinus", spp_list$Species)] <- 1

#Fix Quercus illicifolia as native
# spp_list$Native[grepl("Quercus illicifolia", spp_list$Species)] <- 1
# # write.csv(spp_list, paste0(datapath, "NPS_tree_species_groups.csv"), row.names = FALSE)

#arglist <- list(years = 2008:2019, status = 'alive')
yrs_all <- 2008:2019
c1 <- 2008:2011; c2 <- c1+4; c3 <- c2+4
cycles <- list(c1, c2, c3)
cycle_names <- c("C1", "C2", "C3")

network_codes <- c("ERMN", "MIDN", "NCRN", "NETN")

sum_by_cycle <- function(network, network_name, group, year_span, value, units, value_name, cycle_name){
  df <- SiteXSpec(network, group = group, years = year_span, values = value, plot.type = 'active',
                  area = units, status = 'alive') %>% 
        pivot_longer(-Plot_Name, names_to = "Species", values_to = value_name) %>%
        mutate(cycle = cycle_name, Network = network_name)
return(data.frame(df))
}

sum_by_cycle_dbh <- function(network, network_name, group, year_span, value, units, 
                             value_name, cycle_name, size_min, size_max){
  df <- SiteXSpec(network, group = group, years = year_span, values = value, plot.type = 'active',
                  area = units, status = 'alive', size.min = size_min, size.max = size_max) %>% 
    pivot_longer(-Plot_Name, names_to = "Species", values_to = value_name) %>%
    mutate(cycle = cycle_name, Network = network_name)
  return(data.frame(df))
}
  
#---- Set up plot x visit df for left join ----
plot_visit_df1 <- rbind(getEvents(ermn, years = yrs_all, plot.type = 'active', output = 'dataframe') %>% 
                          select(Plot_Name, Unit_Code, Event_Year) %>% mutate(Network = "ERMN"),
                        getEvents(midn, years = yrs_all, plot.type = 'active', output = 'dataframe') %>% 
                          select(Plot_Name, Unit_Code, Event_Year) %>% mutate(Network = "MIDN"),
                        getEvents(ncrn, years = yrs_all, plot.type = 'active', output = 'dataframe') %>% 
                          select(Plot_Name, Unit_Code, Event_Year) %>% mutate(Network = "NCRN"),
                        getEvents(netn, years = yrs_all, plot.type = 'active', output = 'dataframe') %>% 
                          select(Plot_Name, Unit_Code, Event_Year) %>% mutate(Network = "NETN")) %>% 
#                  filter(!Plot_Name %in% plot_exclude) %>% 
                  rename(Year = Event_Year) %>% 
                  mutate(cycle = case_when(Year %in% 2008:2011 ~ 1,
                                           Year %in% 2012:2015 ~ 2,
                                           Year %in% 2016:2019 ~ 3,
                                           TRUE ~ NA_real_))

plot_sizes <- rbind(read.csv(paste0(datapath, "ERMN_data/MetaData.csv")) %>% 
                      select(Network, ParkCode, SapPlotNum, SapPlotSize, SeedPlotNum, SeedPlotSize,
                             ShrubPlotNum, ShrubPlotSize, HPlotNum, HPlotSize),
                    read.csv(paste0(datapath, "MIDN_data/MetaData.csv")) %>% 
                      select(Network, ParkCode, SapPlotNum, SapPlotSize, SeedPlotNum, SeedPlotSize,
                             ShrubPlotNum, ShrubPlotSize, HPlotNum, HPlotSize),
                    read.csv(paste0(datapath, "NCRN_data/MetaData.csv")) %>% 
                      select(Network, ParkCode, SapPlotNum, SapPlotSize, SeedPlotNum, SeedPlotSize,
                             ShrubPlotNum, ShrubPlotSize, HPlotNum, HPlotSize),
                    read.csv(paste0(datapath, "NETN_data/MetaData.csv")) %>% 
                      select(Network, ParkCode, SapPlotNum, SapPlotSize, SeedPlotNum, SeedPlotSize,
                             ShrubPlotNum, ShrubPlotSize, HPlotNum, HPlotSize)
                    )

plot_sizes$Network[plot_sizes$Network == "NCBN"] <- "MIDN"

# Relate subplot numbers and areas to each visit
  # Set up NCRN Events so same columns as rest of networks to ensure plots missing seeds/saps are included
  # and plots missing a subplot are also specified
ncrn_events <- left_join(read.csv(paste0(datapath, "NCRN_data/Events.csv")) %>% 
                 mutate(numHerbPlots = numSeedPlots), 
               read.csv(paste0(datapath, "NCRN_data/MetaData.csv")) %>% 
                 select(Network, ParkCode, SapPlotNum, SapPlotSize, SeedPlotNum, SeedPlotSize,
                        ShrubPlotNum, ShrubPlotSize, HPlotNum, HPlotSize), 
               by = c("Unit_Code" = "ParkCode")) %>% 
               mutate(numSapPlots = SapPlotNum, excludeEvent = 0) %>% 
               select(Plot_Name, Event_Year, numSapPlots, numSeedPlots, numHerbPlots, excludeEvent)


event_info <- rbind(read.csv(paste0(datapath, "ERMN_data/Events.csv")) %>% 
                      select(Plot_Name, Event_Year, numSapPlots, numSeedPlots, numHerbPlots, 
                             excludeEvent),
                    read.csv(paste0(datapath, "MIDN_data/Events.csv")) %>% 
                      select(Plot_Name, Event_Year, numSapPlots, numSeedPlots, numHerbPlots, 
                             excludeEvent),
                    ncrn_events,
                    read.csv(paste0(datapath, "NETN_data/Events.csv")) %>% 
                      select(Plot_Name, Event_Year, numSapPlots, numSeedPlots, numHerbPlots,
                             excludeEvent)
                    )

plot_visit_df2 <- left_join(plot_visit_df1, plot_sizes, by = c("Unit_Code" = "ParkCode", "Network"))
head(plot_visit_df2)
head(event_info)
# mutates take event-level number of subplots if not NA. Otherwise, take the park-level number of subplots
# from the metadata file. Use excludeEvent from Events.csv to drop ACAD-029-2010
plot_visit_df <- left_join(plot_visit_df2, event_info, by = c("Plot_Name", "Year" = "Event_Year")) %>% 
                 mutate(SapPlotNum = ifelse(!is.na(numSapPlots), numSapPlots, SapPlotNum),
                        SeedPlotNum = ifelse(!is.na(numSeedPlots), numSeedPlots, SeedPlotNum),
                        HPlotNum = ifelse(!is.na(numHerbPlots), numHerbPlots, HPlotNum)) %>% 
                 select(-starts_with("num")) #%>% 
                 #filter(excludeEvent != 1) #excludes ACAD-029-2010 and COLO-380-2018

## # write.csv(plot_visit_df, "EFWG_plot_visit_left_join.csv", row.names = F)

head(plot_visit_df)

# Check for duplicates
plot_visit_df %>% 
  group_by(Plot_Name, Network, Unit_Code, Year) %>% 
  summarize(num_events = n(), .groups = 'drop')%>% 
  filter(num_events > 1) #0- no duplicate events

table(plot_visit_df$Unit_Code, plot_visit_df$Year)
table(plot_visit_df$Unit_Code, plot_visit_df$cycle)
length(unique(plot_visit_df$Plot_Name)) #1515 unique plots
nrow(plot_visit_df) # 4464 plot x cycle rows
table(complete.cases(plot_visit_df)) # all 4462 T
head(plot_visit_df)

# Find max DBH of trees (partial check on ERMN's 999999 and for dbh dist.)
t(rbind(lapply(seq_along((ermn)), function(x){
  max(ermn[[x]]@Trees$Equiv_Live_DBH_cm, na.rm = T)})) %>% data.frame())  #137.7

t(rbind(lapply(seq_along((midn)), function(x){
  max(midn[[x]]@Trees$Equiv_Live_DBH_cm, na.rm = T)})) %>% data.frame())  #147.8

t(rbind(lapply(seq_along((ncrn)), function(x){
  max(ncrn[[x]]@Trees$Equiv_Live_DBH_cm, na.rm = T)})) %>% data.frame())  #172.5

t(rbind(lapply(seq_along((netn)), function(x){
  max(netn[[x]]@Trees$Equiv_Live_DBH_cm, na.rm = T)})) %>% data.frame())  #132.4

# Bring in deer browse index, so can order facets by Deer Browse Impacts in most recent cycle
dbi <- rbind(read.csv(paste0(datapath, "/ERMN_data/Events.csv")) %>% 
               select(Plot_Name, Event_Year, Deer_Browse_Line_ID) %>% 
               rename(DBI = Deer_Browse_Line_ID), 
             read.csv(paste0(datapath, "/MIDN_data/Events.csv")) %>% 
               select(Plot_Name, Event_Year, Deer_Browse_Line_ID) %>% 
               rename(DBI = Deer_Browse_Line_ID), 
             read.csv(paste0(datapath, "/NCRN_data/Plot_Visit_Data.csv")) %>% 
               select(Plot_Name, Sample_Year, Deer_Impact) %>% 
               rename(Event_Year = Sample_Year, DBI = Deer_Impact),
             read.csv(paste0(datapath, "/NETN_data/Events.csv")) %>% 
               select(Plot_Name, Event_Year, Deer_Browse_Line_ID) %>% 
               rename(DBI = Deer_Browse_Line_ID)) 

names(plot_visit_df)
pv_dbi <- left_join(plot_visit_df, dbi %>% select(Plot_Name, Event_Year, DBI), 
                    by = c("Plot_Name", "Year" = "Event_Year")) 

plots <- c(unique(pv_dbi$Plot_Name))
missing_plots <- plot_visit_df %>% filter(!Plot_Name %in% plots) %>% select(Plot_Name, Unit_Code)
missing_plots 

table(pv_dbi$Unit_Code, pv_dbi$Year)
table(pv_dbi$DBI, useNA = 'always') # There shouldn't be any 1s in the dataset (I checked), so converting to 2.
pv_dbi$DBI[pv_dbi$DBI == 1] <- 2
table(pv_dbi$excludeEvent)
length(unique(pv_dbi$Plot_Name)) #1515
pv_dbi$DBI[pv_dbi$DBI >= 9] <- NA
pv_dbi$DBI[pv_dbi$excludeEvent == 1] <- NA
# DBI rank wasn't a useful way to sort parks, so dropping this.
# dbi_sum <- dbi2 %>% group_by(Unit_Code) %>% 
#                     summarize(mean_DBI = mean(DBI, na.rm = T)) %>% 
#                     ungroup() %>% mutate(DBI_rank = rank(mean_DBI, ties.method = "first")) %>% 
#                     arrange(DBI_rank)
# 
# # write.csv(dbi_sum, paste0(datapath, "EFWG_park-level_DBI_rank.csv"), row.names = F)

## write.csv(pv_dbi, paste0(datapath, "EFWG_plot_visit_left_join.csv"), row.names = F)

#---- Live tree density by species group in stems/ha ----
live_tree_dens_ermn <- rbind(sum_by_cycle(ermn, network_name = "ERMN", 
                                          group = 'trees', year_span = 2008:2011,
                                          value = 'count', units = 'ha',
                                          value_name = "Dens", cycle_name = 1),
                             sum_by_cycle(ermn, network_name = "ERMN",  
                                          group = 'trees', year_span = 2012:2015,
                                          value = 'count', units = 'ha',
                                          value_name = "Dens", cycle_name = 2),
                             sum_by_cycle(ermn, network_name = "ERMN", 
                                          group = 'trees', year_span = 2016:2019,
                                          value = 'count', units = 'ha',
                                          value_name = "Dens", cycle_name = 3))

live_tree_dens_midn <- rbind(sum_by_cycle(midn, network_name = "MIDN", 
                                          group = 'trees', year_span = 2008:2011,
                                          value = 'count', units = 'ha',
                                          value_name = "Dens", cycle_name = 1),
                             sum_by_cycle(midn, network_name = "MIDN", 
                                          group = 'trees', year_span = 2012:2015,
                                          value = 'count', units = 'ha',
                                          value_name = "Dens", cycle_name = 2),
                             sum_by_cycle(midn, network_name = "MIDN", 
                                          group = 'trees', year_span = 2016:2019,
                                          value = 'count', units = 'ha',
                                          value_name = "Dens", cycle_name = 3))

live_tree_dens_ncrn <- rbind(sum_by_cycle(ncrn, network_name = "NCRN", 
                                          group = 'trees', year_span = 2008:2011,
                                          value = 'count', units = 'ha',
                                          value_name = "Dens", cycle_name = 1),
                             sum_by_cycle(ncrn, network_name = "NCRN", 
                                          group = 'trees', year_span = 2012:2015,
                                          value = 'count', units = 'ha',
                                          value_name = "Dens", cycle_name = 2),
                             sum_by_cycle(ncrn, network_name = "NCRN", 
                                          group = 'trees', year_span = 2016:2019,
                                          value = 'count', units = 'ha',
                                          value_name = "Dens", cycle_name = 3))

live_tree_dens_netn <- rbind(sum_by_cycle(netn, network_name = "NETN", 
                                          group = 'trees', year_span = 2008:2011,
                                          value = 'count', units = 'ha', 
                                          value_name = "Dens", cycle_name = 1),
                             sum_by_cycle(netn, network_name = "NETN", 
                                          group = 'trees', year_span = 2012:2015,
                                          value = 'count', units = 'ha', 
                                          value_name = "Dens", cycle_name = 2),
                             sum_by_cycle(netn, network_name = "NETN", 
                                          group = 'trees', year_span = 2016:2019,
                                          value = 'count', units = 'ha', 
                                          value_name = "Dens", cycle_name = 3))

live_tree_dens <- rbind(live_tree_dens_ermn, live_tree_dens_midn, live_tree_dens_ncrn, live_tree_dens_netn)
live_tree_dens_spp1 <- right_join(spp_list, live_tree_dens, by = "Species") %>% 
                       mutate(Group = ifelse(Species == "Total", "Total", Group))

live_tree_dens_spp <- live_tree_dens_spp1 %>% filter(!Species %in% c("No species recorded")) %>% 
                      left_join(plot_visit_df %>% select(Plot_Name, cycle), ., 
                                by = c("Plot_Name", "cycle"))

# write.csv(live_tree_dens_spp, paste0(datapath, "Live_tree_density_by_plot_spp.csv"), row.names = F)

spp_list2 <- unique(data.frame(live_tree_dens_spp$Species, live_tree_dens_spp$Group))

live_tree_dens_total <- live_tree_dens_spp %>% 
                          filter(Group == "Total") %>% 
                          select(Network, Plot_Name, cycle, Dens) %>% 
                          rename(Tree_Dens_Total = Dens)

live_tree_dens_native <- live_tree_dens_spp %>% 
                           filter(Group != "Total") %>% 
                           filter(Native == 1) %>% 
                           group_by(Network, Plot_Name, cycle) %>% 
                           summarize(Tree_Dens_Native = sum(Dens, na.rm = T), 
                                     .groups = 'drop')

live_tree_dens_natcan <- live_tree_dens_spp %>% 
                           filter(Group != "Total") %>% 
                           filter(Native == 1 & Canopy_Tree == 1) %>% 
                           group_by(Network, Plot_Name, cycle) %>% 
                           summarize(Tree_Dens_NatCan = sum(Dens, na.rm = T), 
                                     .groups = 'drop')

live_tree_dens_exotic <- live_tree_dens_spp %>% 
                           filter(Group != "Total") %>% 
                           filter(Native == 0) %>% 
                           group_by(Network, Plot_Name, cycle) %>% 
                           summarize(Tree_Dens_Exotic = sum(Dens, na.rm = T), 
                                    .groups = 'drop')

live_tree_dens_final <- list(plot_visit_df, live_tree_dens_total, live_tree_dens_native, 
                             live_tree_dens_natcan, live_tree_dens_exotic) %>% 
                        reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

# Checking work
length(unique(plot_visit_df$Plot_Name))  
length(unique(live_tree_dens_final$Plot_Name))
nrow(plot_visit_df) #4464
nrow(live_tree_dens_final)#4464
table(complete.cases(live_tree_dens_final))

#---- Live tree BA by species group in m2/ha ----
live_tree_ba_ermn <- rbind(sum_by_cycle(ermn, network_name = "ERMN", 
                                          group = 'trees', year_span = 2008:2011,
                                          value = 'size', units = 'ha',
                                          value_name = "BA", cycle_name = 1),
                             sum_by_cycle(ermn, network_name = "ERMN",  
                                          group = 'trees', year_span = 2012:2015,
                                          value = 'size', units = 'ha',
                                          value_name = "BA", cycle_name = 2),
                             sum_by_cycle(ermn, network_name = "ERMN", 
                                          group = 'trees', year_span = 2016:2019,
                                          value = 'size', units = 'ha',
                                          value_name = "BA", cycle_name = 3))

live_tree_ba_midn <- rbind(sum_by_cycle(midn, network_name = "MIDN", 
                                          group = 'trees', year_span = 2008:2011,
                                          value = 'size', units = 'ha',
                                          value_name = "BA", cycle_name = 1),
                             sum_by_cycle(midn, network_name = "MIDN", 
                                          group = 'trees', year_span = 2012:2015,
                                          value = 'size', units = 'ha',
                                          value_name = "BA", cycle_name = 2),
                             sum_by_cycle(midn, network_name = "MIDN", 
                                          group = 'trees', year_span = 2016:2019,
                                          value = 'size', units = 'ha',
                                          value_name = "BA", cycle_name = 3))

live_tree_ba_ncrn <- rbind(sum_by_cycle(ncrn, network_name = "NCRN", 
                                          group = 'trees', year_span = 2008:2011,
                                          value = 'size', units = 'ha',
                                          value_name = "BA", cycle_name = 1),
                             sum_by_cycle(ncrn, network_name = "NCRN", 
                                          group = 'trees', year_span = 2012:2015,
                                          value = 'size', units = 'ha',
                                          value_name = "BA", cycle_name = 2),
                             sum_by_cycle(ncrn, network_name = "NCRN", 
                                          group = 'trees', year_span = 2016:2019,
                                          value = 'size', units = 'ha',
                                          value_name = "BA", cycle_name = 3))

live_tree_ba_netn <- rbind(sum_by_cycle(netn, network_name = "NETN", 
                                          group = 'trees', year_span = 2008:2011,
                                          value = 'size', units = 'ha', 
                                          value_name = "BA", cycle_name = 1),
                             sum_by_cycle(netn, network_name = "NETN", 
                                          group = 'trees', year_span = 2012:2015,
                                          value = 'size', units = 'ha', 
                                          value_name = "BA", cycle_name = 2),
                             sum_by_cycle(netn, network_name = "NETN", 
                                          group = 'trees', year_span = 2016:2019,
                                          value = 'size', units = 'ha', 
                                          value_name = "BA", cycle_name = 3))

live_tree_ba <- rbind(live_tree_ba_ermn, live_tree_ba_midn, live_tree_ba_ncrn, live_tree_ba_netn)
live_tree_ba_spp1 <- right_join(spp_list, live_tree_ba, by = "Species") %>% 
  mutate(Group = ifelse(Species == "Total", "Total", Group))

live_tree_ba_spp <- live_tree_ba_spp1 %>% filter(!Species %in% c("No species recorded")) %>% 
  left_join(plot_visit_df %>% select(Plot_Name, cycle), ., 
            by = c("Plot_Name", "cycle"))

spp_list2 <- unique(data.frame(live_tree_ba_spp$Species, live_tree_ba_spp$Group))

live_tree_ba_total <- live_tree_ba_spp %>% 
  filter(Group == "Total") %>% 
  select(Network, Plot_Name, cycle, BA) %>% 
  rename(Tree_BA_Total = BA)

live_tree_ba_native <- live_tree_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Tree_BA_Native = sum(BA, na.rm = T), 
            .groups = 'drop')

live_tree_ba_natcan <- live_tree_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1 & Canopy_Tree == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Tree_BA_NatCan = sum(BA, na.rm = T), 
            .groups = 'drop')

live_tree_ba_exotic <- live_tree_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 0) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Tree_BA_Exotic = sum(BA, na.rm = T), 
            .groups = 'drop')

live_tree_ba_final <- list(plot_visit_df, live_tree_ba_total, live_tree_ba_native, 
                             live_tree_ba_natcan, live_tree_ba_exotic) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

# Checking work
length(unique(plot_visit_df$Plot_Name))  #1515
length(unique(live_tree_ba_final$Plot_Name)) #1515
nrow(plot_visit_df)#4464
nrow(live_tree_ba_final)#4464
table(complete.cases(live_tree_ba_final)) #4464 T

#---- Live tree density in pole-small (10-19.9 cm) size class by species group in stems/ha ----
live_tree_10cm_dens_ermn <- rbind(sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                     group = 'trees', year_span = 2008:2011,
                                                     value = 'count', units = 'ha', 
                                                     size_min = 10, size_max = 19.9,
                                                     value_name = "Dens", cycle_name = 1),
                                    sum_by_cycle_dbh(ermn, network_name = "ERMN",  
                                                     group = 'trees', year_span = 2012:2015,
                                                     value = 'count', units = 'ha',
                                                     size_min = 10, size_max = 19.9,
                                                     value_name = "Dens", cycle_name = 2),
                                    sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                     group = 'trees', year_span = 2016:2019,
                                                     value = 'count', units = 'ha',
                                                     size_min = 10, size_max = 19.9,
                                                     value_name = "Dens", cycle_name = 3))

live_tree_10cm_dens_midn <- rbind(sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                     group = 'trees', year_span = 2008:2011,
                                                     value = 'count', units = 'ha',
                                                     size_min = 10, size_max = 19.9,
                                                     value_name = "Dens", cycle_name = 1),
                                    sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                     group = 'trees', year_span = 2012:2015,
                                                     value = 'count', units = 'ha',
                                                     size_min = 10, size_max = 19.9,
                                                     value_name = "Dens", cycle_name = 2),
                                    sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                     group = 'trees', year_span = 2016:2019,
                                                     value = 'count', units = 'ha',
                                                     size_min = 10, size_max = 19.9,
                                                     value_name = "Dens", cycle_name = 3))

live_tree_10cm_dens_ncrn <- rbind(sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                     group = 'trees', year_span = 2008:2011,
                                                     value = 'count', units = 'ha',
                                                     size_min = 10, size_max = 19.9,
                                                     value_name = "Dens", cycle_name = 1),
                                    sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                     group = 'trees', year_span = 2012:2015,
                                                     value = 'count', units = 'ha',
                                                     size_min = 10, size_max = 19.9,
                                                     value_name = "Dens", cycle_name = 2),
                                    sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                     group = 'trees', year_span = 2016:2019,
                                                     value = 'count', units = 'ha',
                                                     size_min = 10, size_max = 19.9,
                                                     value_name = "Dens", cycle_name = 3))

live_tree_10cm_dens_netn <- rbind(sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                     group = 'trees', year_span = 2008:2011,
                                                     value = 'count', units = 'ha', 
                                                     size_min = 10, size_max = 19.9,
                                                     value_name = "Dens", cycle_name = 1),
                                    sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                     group = 'trees', year_span = 2012:2015,
                                                     value = 'count', units = 'ha', 
                                                     size_min = 10, size_max = 19.9,
                                                     value_name = "Dens", cycle_name = 2),
                                    sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                     group = 'trees', year_span = 2016:2019,
                                                     value = 'count', units = 'ha', 
                                                     size_min = 10, size_max = 19.9,
                                                     value_name = "Dens", cycle_name = 3))

live_tree_10cm_dens <- rbind(live_tree_10cm_dens_ermn, live_tree_10cm_dens_midn, 
                             live_tree_10cm_dens_ncrn, live_tree_10cm_dens_netn)

live_tree_10cm_dens_spp1 <- right_join(spp_list, live_tree_10cm_dens, by = "Species") %>% 
  mutate(Group = ifelse(Species == "Total", "Total", Group))

live_tree_10cm_dens_spp <- live_tree_10cm_dens_spp1 %>% filter(!Species %in% c("No species recorded")) %>% 
  left_join(plot_visit_df %>% select(Plot_Name, cycle), ., 
            by = c("Plot_Name", "cycle"))


spp_list2 <- unique(data.frame(live_tree_10cm_dens_spp$Species, live_tree_10cm_dens_spp$Group))

live_tree_10cm_dens_total <- live_tree_10cm_dens_spp %>% 
  filter(Group == "Total") %>% 
  select(Network, Plot_Name, cycle, Dens) %>% 
  rename(tree_10cm_Dens_Total = Dens)

live_tree_10cm_dens_native <- live_tree_10cm_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_10cm_Dens_Native = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_tree_10cm_dens_natcan <- live_tree_10cm_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1 & Canopy_Tree == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_10cm_Dens_NatCan = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_tree_10cm_dens_exotic <- live_tree_10cm_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 0) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_10cm_Dens_Exotic = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_tree_10cm_dens_final <- list(plot_visit_df, live_tree_10cm_dens_total, live_tree_10cm_dens_native, 
                                    live_tree_10cm_dens_natcan, live_tree_10cm_dens_exotic) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

# Checking work
length(unique(plot_visit_df$Plot_Name))  #1515
length(unique(live_tree_10cm_dens_final$Plot_Name)) #1515
nrow(plot_visit_df) #4464
nrow(live_tree_10cm_dens_final)#4464
table(complete.cases(live_tree_10cm_dens_final)) #4464 T

#---- Live tree BA in pole-small (10-19.9 cm) by species group in m2/ha ----
live_tree_10cm_ba_ermn <- rbind(sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                   group = 'trees', year_span = 2008:2011,
                                                   value = 'size', units = 'ha',
                                                   size_min = 10, size_max = 19.9,
                                                   value_name = "BA", cycle_name = 1),
                                  sum_by_cycle_dbh(ermn, network_name = "ERMN",    
                                                   group = 'trees', year_span = 2012:2015,
                                                   value = 'size', units = 'ha',
                                                   size_min = 10, size_max = 19.9,
                                                   value_name = "BA", cycle_name = 2),
                                  sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                   group = 'trees', year_span = 2016:2019,
                                                   value = 'size', units = 'ha',
                                                   size_min = 10, size_max = 19.9,
                                                   value_name = "BA", cycle_name = 3))

live_tree_10cm_ba_midn <- rbind(sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                   group = 'trees', year_span = 2008:2011,
                                                   value = 'size', units = 'ha',
                                                   size_min = 10, size_max = 19.9,
                                                   value_name = "BA", cycle_name = 1),
                                  sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                   group = 'trees', year_span = 2012:2015,
                                                   value = 'size', units = 'ha',
                                                   size_min = 10, size_max = 19.9,
                                                   value_name = "BA", cycle_name = 2),
                                  sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                   group = 'trees', year_span = 2016:2019,
                                                   value = 'size', units = 'ha',
                                                   size_min = 10, size_max = 19.9,
                                                   value_name = "BA", cycle_name = 3))

live_tree_10cm_ba_ncrn <- rbind(sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                   group = 'trees', year_span = 2008:2011,
                                                   value = 'size', units = 'ha',
                                                   size_min = 10, size_max = 19.9,
                                                   value_name = "BA", cycle_name = 1),
                                  sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                   group = 'trees', year_span = 2012:2015,
                                                   value = 'size', units = 'ha',
                                                   size_min = 10, size_max = 19.9,
                                                   value_name = "BA", cycle_name = 2),
                                  sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                   group = 'trees', year_span = 2016:2019,
                                                   value = 'size', units = 'ha',
                                                   size_min = 10, size_max = 19.9,
                                                   value_name = "BA", cycle_name = 3))

live_tree_10cm_ba_netn <- rbind(sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                   group = 'trees', year_span = 2008:2011,
                                                   value = 'size', units = 'ha', 
                                                   size_min = 10, size_max = 19.9,
                                                   value_name = "BA", cycle_name = 1),
                                  sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                   group = 'trees', year_span = 2012:2015,
                                                   value = 'size', units = 'ha', 
                                                   size_min = 10, size_max = 19.9,
                                                   value_name = "BA", cycle_name = 2),
                                  sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                   group = 'trees', year_span = 2016:2019,
                                                   value = 'size', units = 'ha', 
                                                   size_min = 10, size_max = 19.9,
                                                   value_name = "BA", cycle_name = 3))

live_tree_10cm_ba <- rbind(live_tree_10cm_ba_ermn, live_tree_10cm_ba_midn, live_tree_10cm_ba_ncrn, live_tree_10cm_ba_netn)
live_tree_10cm_ba_spp1 <- right_join(spp_list, live_tree_10cm_ba, by = "Species") %>% 
  mutate(Group = ifelse(Species == "Total", "Total", Group))

live_tree_10cm_ba_spp <- live_tree_10cm_ba_spp1 %>% filter(!Species %in% c("No species recorded")) %>% 
  left_join(plot_visit_df %>% select(Plot_Name, cycle), ., 
            by = c("Plot_Name", "cycle"))

spp_list2 <- unique(data.frame(live_tree_10cm_ba_spp$Species, live_tree_10cm_ba_spp$Group))

live_tree_10cm_ba_total <- live_tree_10cm_ba_spp %>% 
  filter(Group == "Total") %>% 
  select(Network, Plot_Name, cycle, BA) %>% 
  rename(tree_10cm_BA_Total = BA)

live_tree_10cm_ba_native <- live_tree_10cm_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_10cm_BA_Native = sum(BA, na.rm = T), 
            .groups = 'drop')

live_tree_10cm_ba_natcan <- live_tree_10cm_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1 & Canopy_Tree == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_10cm_BA_NatCan = sum(BA, na.rm = T), 
            .groups = 'drop')

live_tree_10cm_ba_exotic <- live_tree_10cm_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 0) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_10cm_BA_Exotic = sum(BA, na.rm = T), 
            .groups = 'drop')

head(live_tree_10cm_ba_natcan)
head(live_tree_ba_natcan)

live_tree_10cm_ba_final <- list(plot_visit_df, live_tree_10cm_ba_total, live_tree_10cm_ba_native, 
                                  live_tree_10cm_ba_natcan, live_tree_10cm_ba_exotic) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

# Checking work
length(unique(plot_visit_df$Plot_Name))  #1515
length(unique(live_tree_10cm_ba_final$Plot_Name)) #1515
nrow(plot_visit_df)#4464
nrow(live_tree_10cm_ba_final)#4464
table(complete.cases(live_tree_10cm_ba_final)) #4464 T

#---- Live tree density in pole-large (20.0-29.9 cm) size class by species group in stems/ha ----
live_tree_20cm_dens_ermn <- rbind(sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                     group = 'trees', year_span = 2008:2011,
                                                     value = 'count', units = 'ha', 
                                                     size_min = 20.0, size_max = 29.9,
                                                     value_name = "Dens", cycle_name = 1),
                                    sum_by_cycle_dbh(ermn, network_name = "ERMN",  
                                                     group = 'trees', year_span = 2012:2015,
                                                     value = 'count', units = 'ha',
                                                     size_min = 20.0, size_max = 29.9,
                                                     value_name = "Dens", cycle_name = 2),
                                    sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                     group = 'trees', year_span = 2016:2019,
                                                     value = 'count', units = 'ha',
                                                     size_min = 20.0, size_max = 29.9,
                                                     value_name = "Dens", cycle_name = 3))

live_tree_20cm_dens_midn <- rbind(sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                     group = 'trees', year_span = 2008:2011,
                                                     value = 'count', units = 'ha',
                                                     size_min = 20.0, size_max = 29.9,
                                                     value_name = "Dens", cycle_name = 1),
                                    sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                     group = 'trees', year_span = 2012:2015,
                                                     value = 'count', units = 'ha',
                                                     size_min = 20.0, size_max = 29.9,
                                                     value_name = "Dens", cycle_name = 2),
                                    sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                     group = 'trees', year_span = 2016:2019,
                                                     value = 'count', units = 'ha',
                                                     size_min = 20.0, size_max = 29.9,
                                                     value_name = "Dens", cycle_name = 3))

live_tree_20cm_dens_ncrn <- rbind(sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                     group = 'trees', year_span = 2008:2011,
                                                     value = 'count', units = 'ha',
                                                     size_min = 20.0, size_max = 29.9,
                                                     value_name = "Dens", cycle_name = 1),
                                    sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                     group = 'trees', year_span = 2012:2015,
                                                     value = 'count', units = 'ha',
                                                     size_min = 20.0, size_max = 29.9,
                                                     value_name = "Dens", cycle_name = 2),
                                    sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                     group = 'trees', year_span = 2016:2019,
                                                     value = 'count', units = 'ha',
                                                     size_min = 20.0, size_max = 29.9,
                                                     value_name = "Dens", cycle_name = 3))

live_tree_20cm_dens_netn <- rbind(sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                     group = 'trees', year_span = 2008:2011,
                                                     value = 'count', units = 'ha', 
                                                     size_min = 20.0, size_max = 29.9,
                                                     value_name = "Dens", cycle_name = 1),
                                    sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                     group = 'trees', year_span = 2012:2015,
                                                     value = 'count', units = 'ha', 
                                                     size_min = 20.0, size_max = 29.9,
                                                     value_name = "Dens", cycle_name = 2),
                                    sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                     group = 'trees', year_span = 2016:2019,
                                                     value = 'count', units = 'ha', 
                                                     size_min = 20.0, size_max = 29.9,
                                                     value_name = "Dens", cycle_name = 3))

live_tree_20cm_dens <- rbind(live_tree_20cm_dens_ermn, live_tree_20cm_dens_midn, 
                             live_tree_20cm_dens_ncrn, live_tree_20cm_dens_netn)
live_tree_20cm_dens_spp1 <- right_join(spp_list, live_tree_20cm_dens, by = "Species") %>% 
  mutate(Group = ifelse(Species == "Total", "Total", Group))

live_tree_20cm_dens_spp <- live_tree_20cm_dens_spp1 %>% filter(!Species %in% c("No species recorded")) %>% 
  left_join(plot_visit_df %>% select(Plot_Name, cycle), ., 
            by = c("Plot_Name", "cycle"))

spp_list2 <- unique(data.frame(live_tree_20cm_dens_spp$Species, live_tree_20cm_dens_spp$Group))

live_tree_20cm_dens_total <- live_tree_20cm_dens_spp %>% 
  filter(Group == "Total") %>% 
  select(Network, Plot_Name, cycle, Dens) %>% 
  rename(tree_20cm_Dens_Total = Dens)

live_tree_20cm_dens_native <- live_tree_20cm_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_20cm_Dens_Native = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_tree_20cm_dens_natcan <- live_tree_20cm_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1 & Canopy_Tree == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_20cm_Dens_NatCan = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_tree_20cm_dens_exotic <- live_tree_20cm_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 0) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_20cm_Dens_Exotic = sum(Dens, na.rm = T), 
            .groups = 'drop')

head(live_tree_20cm_dens_natcan)

live_tree_20cm_dens_final <- list(plot_visit_df, live_tree_20cm_dens_total, live_tree_20cm_dens_native, 
                                    live_tree_20cm_dens_natcan, live_tree_20cm_dens_exotic) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

# Checking work
length(unique(plot_visit_df$Plot_Name))  #1515
length(unique(live_tree_20cm_dens_final$Plot_Name)) #1515
nrow(plot_visit_df) #4464
nrow(live_tree_20cm_dens_final)#4464
table(complete.cases(live_tree_20cm_dens_final)) #4464 T

#---- Live tree BA in pole-large (20.0 - 29.9 cm) by species group in m2/ha ----
live_tree_20cm_ba_ermn <- rbind(sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                   group = 'trees', year_span = 2008:2011,
                                                   value = 'size', units = 'ha',
                                                   size_min = 20.0, size_max = 29.9,
                                                   value_name = "BA", cycle_name = 1),
                                  sum_by_cycle_dbh(ermn, network_name = "ERMN",    
                                                   group = 'trees', year_span = 2012:2015,
                                                   value = 'size', units = 'ha',
                                                   size_min = 20.0, size_max = 29.9,
                                                   value_name = "BA", cycle_name = 2),
                                  sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                   group = 'trees', year_span = 2016:2019,
                                                   value = 'size', units = 'ha',
                                                   size_min = 20.0, size_max = 29.9,
                                                   value_name = "BA", cycle_name = 3))

live_tree_20cm_ba_midn <- rbind(sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                   group = 'trees', year_span = 2008:2011,
                                                   value = 'size', units = 'ha',
                                                   size_min = 20.0, size_max = 29.9,
                                                   value_name = "BA", cycle_name = 1),
                                  sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                   group = 'trees', year_span = 2012:2015,
                                                   value = 'size', units = 'ha',
                                                   size_min = 20.0, size_max = 29.9,
                                                   value_name = "BA", cycle_name = 2),
                                  sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                   group = 'trees', year_span = 2016:2019,
                                                   value = 'size', units = 'ha',
                                                   size_min = 20.0, size_max = 29.9,
                                                   value_name = "BA", cycle_name = 3))

live_tree_20cm_ba_ncrn <- rbind(sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                   group = 'trees', year_span = 2008:2011,
                                                   value = 'size', units = 'ha',
                                                   size_min = 20.0, size_max = 29.9,
                                                   value_name = "BA", cycle_name = 1),
                                  sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                   group = 'trees', year_span = 2012:2015,
                                                   value = 'size', units = 'ha',
                                                   size_min = 20.0, size_max = 29.9,
                                                   value_name = "BA", cycle_name = 2),
                                  sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                   group = 'trees', year_span = 2016:2019,
                                                   value = 'size', units = 'ha',
                                                   size_min = 20.0, size_max = 29.9,
                                                   value_name = "BA", cycle_name = 3))

live_tree_20cm_ba_netn <- rbind(sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                   group = 'trees', year_span = 2008:2011,
                                                   value = 'size', units = 'ha', 
                                                   size_min = 20.0, size_max = 29.9,
                                                   value_name = "BA", cycle_name = 1),
                                  sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                   group = 'trees', year_span = 2012:2015,
                                                   value = 'size', units = 'ha', 
                                                   size_min = 20.0, size_max = 29.9,
                                                   value_name = "BA", cycle_name = 2),
                                  sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                   group = 'trees', year_span = 2016:2019,
                                                   value = 'size', units = 'ha', 
                                                   size_min = 20.0, size_max = 29.9,
                                                   value_name = "BA", cycle_name = 3))

live_tree_20cm_ba <- rbind(live_tree_20cm_ba_ermn, live_tree_20cm_ba_midn, live_tree_20cm_ba_ncrn, live_tree_20cm_ba_netn)
live_tree_20cm_ba_spp1 <- right_join(spp_list, live_tree_20cm_ba, by = "Species") %>% 
  mutate(Group = ifelse(Species == "Total", "Total", Group))

live_tree_20cm_ba_spp <- live_tree_20cm_ba_spp1 %>% filter(!Species %in% c("No species recorded")) %>% 
  left_join(plot_visit_df %>% select(Plot_Name, cycle), ., 
            by = c("Plot_Name", "cycle"))

spp_list2 <- unique(data.frame(live_tree_20cm_ba_spp$Species, live_tree_20cm_ba_spp$Group))

live_tree_20cm_ba_total <- live_tree_20cm_ba_spp %>% 
  filter(Group == "Total") %>% 
  select(Network, Plot_Name, cycle, BA) %>% 
  rename(tree_20cm_BA_Total = BA)

live_tree_20cm_ba_native <- live_tree_20cm_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_20cm_BA_Native = sum(BA, na.rm = T), 
            .groups = 'drop')

live_tree_20cm_ba_natcan <- live_tree_20cm_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1 & Canopy_Tree == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_20cm_BA_NatCan = sum(BA, na.rm = T), 
            .groups = 'drop')

live_tree_20cm_ba_exotic <- live_tree_20cm_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 0) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_20cm_BA_Exotic = sum(BA, na.rm = T), 
            .groups = 'drop')

head(live_tree_20cm_ba_natcan)
head(live_tree_ba_natcan)

live_tree_20cm_ba_final <- list(plot_visit_df, live_tree_20cm_ba_total, live_tree_20cm_ba_native, 
                                  live_tree_20cm_ba_natcan, live_tree_20cm_ba_exotic) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

# Checking work
length(unique(plot_visit_df$Plot_Name))  #1515
length(unique(live_tree_20cm_ba_final$Plot_Name)) #1515
nrow(plot_visit_df)#4464
nrow(live_tree_20cm_ba_final)#4464
table(complete.cases(live_tree_20cm_ba_final)) #4464 T

#---- Live tree density in sawtimber-small (30.0 - 39.9 cm) size class by species group in stems/ha ----
live_tree_30cm_dens_ermn <- rbind(sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                    group = 'trees', year_span = 2008:2011,
                                                    value = 'count', units = 'ha', 
                                                    size_min = 30.0, size_max = 39.9,
                                                    value_name = "Dens", cycle_name = 1),
                                   sum_by_cycle_dbh(ermn, network_name = "ERMN",  
                                                    group = 'trees', year_span = 2012:2015,
                                                    value = 'count', units = 'ha',
                                                    size_min = 30.0, size_max = 39.9,
                                                    value_name = "Dens", cycle_name = 2),
                                   sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                    group = 'trees', year_span = 2016:2019,
                                                    value = 'count', units = 'ha',
                                                    size_min = 30.0, size_max = 39.9,
                                                    value_name = "Dens", cycle_name = 3))

live_tree_30cm_dens_midn <- rbind(sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                    group = 'trees', year_span = 2008:2011,
                                                    value = 'count', units = 'ha',
                                                    size_min = 30.0, size_max = 39.9,
                                                    value_name = "Dens", cycle_name = 1),
                                   sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                    group = 'trees', year_span = 2012:2015,
                                                    value = 'count', units = 'ha',
                                                    size_min = 30.0, size_max = 39.9,
                                                    value_name = "Dens", cycle_name = 2),
                                   sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                    group = 'trees', year_span = 2016:2019,
                                                    value = 'count', units = 'ha',
                                                    size_min = 30.0, size_max = 39.9,
                                                    value_name = "Dens", cycle_name = 3))

live_tree_30cm_dens_ncrn <- rbind(sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                    group = 'trees', year_span = 2008:2011,
                                                    value = 'count', units = 'ha',
                                                    size_min = 30.0, size_max = 39.9,
                                                    value_name = "Dens", cycle_name = 1),
                                   sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                    group = 'trees', year_span = 2012:2015,
                                                    value = 'count', units = 'ha',
                                                    size_min = 30.0, size_max = 39.9,
                                                    value_name = "Dens", cycle_name = 2),
                                   sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                    group = 'trees', year_span = 2016:2019,
                                                    value = 'count', units = 'ha',
                                                    size_min = 30.0, size_max = 39.9,
                                                    value_name = "Dens", cycle_name = 3))

live_tree_30cm_dens_netn <- rbind(sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                    group = 'trees', year_span = 2008:2011,
                                                    value = 'count', units = 'ha', 
                                                    size_min = 30.0, size_max = 39.9,
                                                    value_name = "Dens", cycle_name = 1),
                                   sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                    group = 'trees', year_span = 2012:2015,
                                                    value = 'count', units = 'ha', 
                                                    size_min = 30.0, size_max = 39.9,
                                                    value_name = "Dens", cycle_name = 2),
                                   sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                    group = 'trees', year_span = 2016:2019,
                                                    value = 'count', units = 'ha', 
                                                    size_min = 30.0, size_max = 39.9,
                                                    value_name = "Dens", cycle_name = 3))

live_tree_30cm_dens <- rbind(live_tree_30cm_dens_ermn, live_tree_30cm_dens_midn, live_tree_30cm_dens_ncrn, live_tree_30cm_dens_netn)

live_tree_30cm_dens_spp1 <- right_join(spp_list, live_tree_30cm_dens, by = "Species") %>% 
  mutate(Group = ifelse(Species == "Total", "Total", Group))

live_tree_30cm_dens_spp <- live_tree_30cm_dens_spp1 %>% filter(!Species %in% c("No species recorded")) %>% 
  left_join(plot_visit_df %>% select(Plot_Name, cycle), ., 
            by = c("Plot_Name", "cycle"))

spp_list2 <- unique(data.frame(live_tree_30cm_dens_spp$Species, live_tree_30cm_dens_spp$Group))

live_tree_30cm_dens_total <- live_tree_30cm_dens_spp %>% 
  filter(Group == "Total") %>% 
  select(Network, Plot_Name, cycle, Dens) %>% 
  rename(tree_30cm_Dens_Total = Dens)

live_tree_30cm_dens_native <- live_tree_30cm_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_30cm_Dens_Native = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_tree_30cm_dens_natcan <- live_tree_30cm_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1 & Canopy_Tree == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_30cm_Dens_NatCan = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_tree_30cm_dens_exotic <- live_tree_30cm_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 0) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_30cm_Dens_Exotic = sum(Dens, na.rm = T), 
            .groups = 'drop')

head(live_tree_30cm_dens_natcan)

live_tree_30cm_dens_final <- list(plot_visit_df, live_tree_30cm_dens_total, live_tree_30cm_dens_native, 
                                   live_tree_30cm_dens_natcan, live_tree_30cm_dens_exotic) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

# Checking work
length(unique(plot_visit_df$Plot_Name))  #1515
length(unique(live_tree_30cm_dens_final$Plot_Name)) #1515
nrow(plot_visit_df) #4464
nrow(live_tree_30cm_dens_final)#4464
table(complete.cases(live_tree_30cm_dens_final)) #4464 T

#---- Live tree BA in sawtimber-small (30.0 - 39.9 cm) by species group in m2/ha ----
live_tree_30cm_ba_ermn <- rbind(sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                  group = 'trees', year_span = 2008:2011,
                                                  value = 'size', units = 'ha',
                                                  size_min = 30.0, size_max = 39.9,
                                                  value_name = "BA", cycle_name = 1),
                                 sum_by_cycle_dbh(ermn, network_name = "ERMN",    
                                                  group = 'trees', year_span = 2012:2015,
                                                  value = 'size', units = 'ha',
                                                  size_min = 30.0, size_max = 39.9,
                                                  value_name = "BA", cycle_name = 2),
                                 sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                  group = 'trees', year_span = 2016:2019,
                                                  value = 'size', units = 'ha',
                                                  size_min = 30.0, size_max = 39.9,
                                                  value_name = "BA", cycle_name = 3))

live_tree_30cm_ba_midn <- rbind(sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                  group = 'trees', year_span = 2008:2011,
                                                  value = 'size', units = 'ha',
                                                  size_min = 30.0, size_max = 39.9,
                                                  value_name = "BA", cycle_name = 1),
                                 sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                  group = 'trees', year_span = 2012:2015,
                                                  value = 'size', units = 'ha',
                                                  size_min = 30.0, size_max = 39.9,
                                                  value_name = "BA", cycle_name = 2),
                                 sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                  group = 'trees', year_span = 2016:2019,
                                                  value = 'size', units = 'ha',
                                                  size_min = 30.0, size_max = 39.9,
                                                  value_name = "BA", cycle_name = 3))

live_tree_30cm_ba_ncrn <- rbind(sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                  group = 'trees', year_span = 2008:2011,
                                                  value = 'size', units = 'ha',
                                                  size_min = 30.0, size_max = 39.9,
                                                  value_name = "BA", cycle_name = 1),
                                 sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                  group = 'trees', year_span = 2012:2015,
                                                  value = 'size', units = 'ha',
                                                  size_min = 30.0, size_max = 39.9,
                                                  value_name = "BA", cycle_name = 2),
                                 sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                  group = 'trees', year_span = 2016:2019,
                                                  value = 'size', units = 'ha',
                                                  size_min = 30.0, size_max = 39.9,
                                                  value_name = "BA", cycle_name = 3))

live_tree_30cm_ba_netn <- rbind(sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                  group = 'trees', year_span = 2008:2011,
                                                  value = 'size', units = 'ha', 
                                                  size_min = 30.0, size_max = 39.9,
                                                  value_name = "BA", cycle_name = 1),
                                 sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                  group = 'trees', year_span = 2012:2015,
                                                  value = 'size', units = 'ha', 
                                                  size_min = 30.0, size_max = 39.9,
                                                  value_name = "BA", cycle_name = 2),
                                 sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                  group = 'trees', year_span = 2016:2019,
                                                  value = 'size', units = 'ha', 
                                                  size_min = 30.0, size_max = 39.9,
                                                  value_name = "BA", cycle_name = 3))

live_tree_30cm_ba <- rbind(live_tree_30cm_ba_ermn, live_tree_30cm_ba_midn, live_tree_30cm_ba_ncrn, live_tree_30cm_ba_netn)
live_tree_30cm_ba_spp1 <- right_join(spp_list, live_tree_30cm_ba, by = "Species") %>% 
  mutate(Group = ifelse(Species == "Total", "Total", Group))

live_tree_30cm_ba_spp <- live_tree_30cm_ba_spp1 %>% filter(!Species %in% c("No species recorded")) %>% 
  left_join(plot_visit_df %>% select(Plot_Name, cycle), ., 
            by = c("Plot_Name", "cycle"))

spp_list2 <- unique(data.frame(live_tree_30cm_ba_spp$Species, live_tree_30cm_ba_spp$Group))

live_tree_30cm_ba_total <- live_tree_30cm_ba_spp %>% 
  filter(Group == "Total") %>% 
  select(Network, Plot_Name, cycle, BA) %>% 
  rename(tree_30cm_BA_Total = BA)

live_tree_30cm_ba_native <- live_tree_30cm_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_30cm_BA_Native = sum(BA, na.rm = T), 
            .groups = 'drop')

live_tree_30cm_ba_natcan <- live_tree_30cm_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1 & Canopy_Tree == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_30cm_BA_NatCan = sum(BA, na.rm = T), 
            .groups = 'drop')

live_tree_30cm_ba_exotic <- live_tree_30cm_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 0) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_30cm_BA_Exotic = sum(BA, na.rm = T), 
            .groups = 'drop')

head(live_tree_30cm_ba_natcan)
head(live_tree_ba_natcan)

live_tree_30cm_ba_final <- list(plot_visit_df, live_tree_30cm_ba_total, live_tree_30cm_ba_native, 
                                 live_tree_30cm_ba_natcan, live_tree_30cm_ba_exotic) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

# Checking work
length(unique(plot_visit_df$Plot_Name))  #1515
length(unique(live_tree_30cm_ba_final$Plot_Name)) #1515
nrow(plot_visit_df)#4464
nrow(live_tree_30cm_ba_final)#4464
table(complete.cases(live_tree_30cm_ba_final)) #4464 T

#---- Live tree density in sawtimber-lg (39.9+cm) size class by species group in stems/ha ----
live_tree_40cm_dens_ermn <- rbind(sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                    group = 'trees', year_span = 2008:2011,
                                                    value = 'count', units = 'ha', 
                                                    size_min = 39.9, size_max = 200.0,
                                                    value_name = "Dens", cycle_name = 1),
                                   sum_by_cycle_dbh(ermn, network_name = "ERMN",  
                                                    group = 'trees', year_span = 2012:2015,
                                                    value = 'count', units = 'ha',
                                                    size_min = 39.9, size_max = 200.0,
                                                    value_name = "Dens", cycle_name = 2),
                                   sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                    group = 'trees', year_span = 2016:2019,
                                                    value = 'count', units = 'ha',
                                                    size_min = 39.9, size_max = 200.0,
                                                    value_name = "Dens", cycle_name = 3))

live_tree_40cm_dens_midn <- rbind(sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                    group = 'trees', year_span = 2008:2011,
                                                    value = 'count', units = 'ha',
                                                    size_min = 39.9, size_max = 200.0,
                                                    value_name = "Dens", cycle_name = 1),
                                   sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                    group = 'trees', year_span = 2012:2015,
                                                    value = 'count', units = 'ha',
                                                    size_min = 39.9, size_max = 200.0,
                                                    value_name = "Dens", cycle_name = 2),
                                   sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                    group = 'trees', year_span = 2016:2019,
                                                    value = 'count', units = 'ha',
                                                    size_min = 39.9, size_max = 200.0,
                                                    value_name = "Dens", cycle_name = 3))

live_tree_40cm_dens_ncrn <- rbind(sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                    group = 'trees', year_span = 2008:2011,
                                                    value = 'count', units = 'ha',
                                                    size_min = 39.9, size_max = 200.0,
                                                    value_name = "Dens", cycle_name = 1),
                                   sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                    group = 'trees', year_span = 2012:2015,
                                                    value = 'count', units = 'ha',
                                                    size_min = 39.9, size_max = 200.0,
                                                    value_name = "Dens", cycle_name = 2),
                                   sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                    group = 'trees', year_span = 2016:2019,
                                                    value = 'count', units = 'ha',
                                                    size_min = 39.9, size_max = 200.0,
                                                    value_name = "Dens", cycle_name = 3))

live_tree_40cm_dens_netn <- rbind(sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                    group = 'trees', year_span = 2008:2011,
                                                    value = 'count', units = 'ha', 
                                                    size_min = 39.9, size_max = 200.0,
                                                    value_name = "Dens", cycle_name = 1),
                                   sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                    group = 'trees', year_span = 2012:2015,
                                                    value = 'count', units = 'ha', 
                                                    size_min = 39.9, size_max = 200.0,
                                                    value_name = "Dens", cycle_name = 2),
                                   sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                    group = 'trees', year_span = 2016:2019,
                                                    value = 'count', units = 'ha', 
                                                    size_min = 39.9, size_max = 200.0,
                                                    value_name = "Dens", cycle_name = 3))

live_tree_40cm_dens <- rbind(live_tree_40cm_dens_ermn, live_tree_40cm_dens_midn, 
                             live_tree_40cm_dens_ncrn, live_tree_40cm_dens_netn)

live_tree_40cm_dens_spp1 <- right_join(spp_list, live_tree_40cm_dens, by = "Species") %>% 
  mutate(Group = ifelse(Species == "Total", "Total", Group))

live_tree_40cm_dens_spp <- live_tree_40cm_dens_spp1 %>% filter(!Species %in% c("No species recorded")) %>% 
  left_join(plot_visit_df %>% select(Plot_Name, cycle), ., 
            by = c("Plot_Name", "cycle"))

spp_list2 <- unique(data.frame(live_tree_40cm_dens_spp$Species, live_tree_40cm_dens_spp$Group))

live_tree_40cm_dens_total <- live_tree_40cm_dens_spp %>% 
  filter(Group == "Total") %>% 
  select(Network, Plot_Name, cycle, Dens) %>% 
  rename(tree_40cm_Dens_Total = Dens)

live_tree_40cm_dens_native <- live_tree_40cm_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_40cm_Dens_Native = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_tree_40cm_dens_natcan <- live_tree_40cm_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1 & Canopy_Tree == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_40cm_Dens_NatCan = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_tree_40cm_dens_exotic <- live_tree_40cm_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 0) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_40cm_Dens_Exotic = sum(Dens, na.rm = T), 
            .groups = 'drop')

head(live_tree_40cm_dens_natcan)

live_tree_40cm_dens_final <- list(plot_visit_df, live_tree_40cm_dens_total, live_tree_40cm_dens_native, 
                                   live_tree_40cm_dens_natcan, live_tree_40cm_dens_exotic) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

# Checking work
length(unique(plot_visit_df$Plot_Name))  #1515
length(unique(live_tree_40cm_dens_final$Plot_Name)) #1515
nrow(plot_visit_df) #4464
nrow(live_tree_40cm_dens_final)#4464
table(complete.cases(live_tree_40cm_dens_final)) #4464 T

#---- Live tree BA in sawtimber-lg (39.9+ cm) by species group in m2/ha ----
live_tree_40cm_ba_ermn <- rbind(sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                  group = 'trees', year_span = 2008:2011,
                                                  value = 'size', units = 'ha',
                                                  size_min = 40., size_max = 200.0,
                                                  value_name = "BA", cycle_name = 1),
                                 sum_by_cycle_dbh(ermn, network_name = "ERMN",    
                                                  group = 'trees', year_span = 2012:2015,
                                                  value = 'size', units = 'ha',
                                                  size_min = 40., size_max = 200.0,
                                                  value_name = "BA", cycle_name = 2),
                                 sum_by_cycle_dbh(ermn, network_name = "ERMN", 
                                                  group = 'trees', year_span = 2016:2019,
                                                  value = 'size', units = 'ha',
                                                  size_min = 40., size_max = 200.0,
                                                  value_name = "BA", cycle_name = 3))

live_tree_40cm_ba_midn <- rbind(sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                  group = 'trees', year_span = 2008:2011,
                                                  value = 'size', units = 'ha',
                                                  size_min = 40., size_max = 200.0,
                                                  value_name = "BA", cycle_name = 1),
                                 sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                  group = 'trees', year_span = 2012:2015,
                                                  value = 'size', units = 'ha',
                                                  size_min = 40., size_max = 200.0,
                                                  value_name = "BA", cycle_name = 2),
                                 sum_by_cycle_dbh(midn, network_name = "MIDN", 
                                                  group = 'trees', year_span = 2016:2019,
                                                  value = 'size', units = 'ha',
                                                  size_min = 40., size_max = 200.0,
                                                  value_name = "BA", cycle_name = 3))

live_tree_40cm_ba_ncrn <- rbind(sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                  group = 'trees', year_span = 2008:2011,
                                                  value = 'size', units = 'ha',
                                                  size_min = 40., size_max = 200.0,
                                                  value_name = "BA", cycle_name = 1),
                                 sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                  group = 'trees', year_span = 2012:2015,
                                                  value = 'size', units = 'ha',
                                                  size_min = 40., size_max = 200.0,
                                                  value_name = "BA", cycle_name = 2),
                                 sum_by_cycle_dbh(ncrn, network_name = "NCRN", 
                                                  group = 'trees', year_span = 2016:2019,
                                                  value = 'size', units = 'ha',
                                                  size_min = 40., size_max = 200.0,
                                                  value_name = "BA", cycle_name = 3))

live_tree_40cm_ba_netn <- rbind(sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                  group = 'trees', year_span = 2008:2011,
                                                  value = 'size', units = 'ha', 
                                                  size_min = 40., size_max = 200.0,
                                                  value_name = "BA", cycle_name = 1),
                                 sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                  group = 'trees', year_span = 2012:2015,
                                                  value = 'size', units = 'ha', 
                                                  size_min = 40., size_max = 200.0,
                                                  value_name = "BA", cycle_name = 2),
                                 sum_by_cycle_dbh(netn, network_name = "NETN", 
                                                  group = 'trees', year_span = 2016:2019,
                                                  value = 'size', units = 'ha', 
                                                  size_min = 40., size_max = 200.0,
                                                  value_name = "BA", cycle_name = 3))

live_tree_40cm_ba <- rbind(live_tree_40cm_ba_ermn, live_tree_40cm_ba_midn, live_tree_40cm_ba_ncrn, live_tree_40cm_ba_netn)
live_tree_40cm_ba_spp1 <- right_join(spp_list, live_tree_40cm_ba, by = "Species") %>% 
  mutate(Group = ifelse(Species == "Total", "Total", Group))

live_tree_40cm_ba_spp <- live_tree_40cm_ba_spp1 %>% filter(!Species %in% c("No species recorded")) %>% 
  left_join(plot_visit_df %>% select(Plot_Name, cycle), ., 
            by = c("Plot_Name", "cycle"))

spp_list2 <- unique(data.frame(live_tree_40cm_ba_spp$Species, live_tree_40cm_ba_spp$Group))

live_tree_40cm_ba_total <- live_tree_40cm_ba_spp %>% 
  filter(Group == "Total") %>% 
  select(Network, Plot_Name, cycle, BA) %>% 
  rename(tree_40cm_BA_Total = BA)

live_tree_40cm_ba_native <- live_tree_40cm_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_40cm_BA_Native = sum(BA, na.rm = T), 
            .groups = 'drop')

live_tree_40cm_ba_natcan <- live_tree_40cm_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1 & Canopy_Tree == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_40cm_BA_NatCan = sum(BA, na.rm = T), 
            .groups = 'drop')

live_tree_40cm_ba_exotic <- live_tree_40cm_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 0) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(tree_40cm_BA_Exotic = sum(BA, na.rm = T), 
            .groups = 'drop')

head(live_tree_40cm_ba_natcan)
head(live_tree_ba_natcan)

live_tree_40cm_ba_final <- list(plot_visit_df, live_tree_40cm_ba_total, live_tree_40cm_ba_native, 
                                 live_tree_40cm_ba_natcan, live_tree_40cm_ba_exotic) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

# Checking work
length(unique(plot_visit_df$Plot_Name))  #1515
length(unique(live_tree_40cm_ba_final$Plot_Name)) #1515
nrow(plot_visit_df)#4464
nrow(live_tree_40cm_ba_final)#4464
table(complete.cases(live_tree_40cm_ba_final)) #4464 T

#---- Live sapling density by species group in stems/m2 ----
live_sap_dens_ermn <- rbind(sum_by_cycle(ermn, network_name = "ERMN", 
                                          group = 'saplings', year_span = 2008:2011,
                                          value = 'count', units = 'plot',
                                          value_name = "Dens", cycle_name = 1),
                             sum_by_cycle(ermn, network_name = "ERMN",  
                                          group = 'saplings', year_span = 2012:2015,
                                          value = 'count', units = 'plot',
                                          value_name = "Dens", cycle_name = 2),
                             sum_by_cycle(ermn, network_name = "ERMN", 
                                          group = 'saplings', year_span = 2016:2019,
                                          value = 'count', units = 'plot',
                                          value_name = "Dens", cycle_name = 3))

live_sap_dens_midn <- rbind(sum_by_cycle(midn, network_name = "MIDN", 
                                          group = 'saplings', year_span = 2008:2011,
                                          value = 'count', units = 'plot',
                                          value_name = "Dens", cycle_name = 1),
                             sum_by_cycle(midn, network_name = "MIDN", 
                                          group = 'saplings', year_span = 2012:2015,
                                          value = 'count', units = 'plot',
                                          value_name = "Dens", cycle_name = 2),
                             sum_by_cycle(midn, network_name = "MIDN", 
                                          group = 'saplings', year_span = 2016:2019,
                                          value = 'count', units = 'plot',
                                          value_name = "Dens", cycle_name = 3))

live_sap_dens_ncrn <- rbind(sum_by_cycle(ncrn, network_name = "NCRN", 
                                          group = 'saplings', year_span = 2008:2011,
                                          value = 'count', units = 'plot',
                                          value_name = "Dens", cycle_name = 1),
                             sum_by_cycle(ncrn, network_name = "NCRN", 
                                          group = 'saplings', year_span = 2012:2015,
                                          value = 'count', units = 'plot',
                                          value_name = "Dens", cycle_name = 2),
                             sum_by_cycle(ncrn, network_name = "NCRN", 
                                          group = 'saplings', year_span = 2016:2019,
                                          value = 'count', units = 'plot',
                                          value_name = "Dens", cycle_name = 3))

live_sap_dens_netn <- rbind(sum_by_cycle(netn, network_name = "NETN", 
                                          group = 'saplings', year_span = 2008:2011,
                                          value = 'count', units = 'plot', 
                                          value_name = "Dens", cycle_name = 1),
                             sum_by_cycle(netn, network_name = "NETN", 
                                          group = 'saplings', year_span = 2012:2015,
                                          value = 'count', units = 'plot', 
                                          value_name = "Dens", cycle_name = 2),
                             sum_by_cycle(netn, network_name = "NETN", 
                                          group = 'saplings', year_span = 2016:2019,
                                          value = 'count', units = 'plot', 
                                          value_name = "Dens", cycle_name = 3))

live_sap_dens <- rbind(live_sap_dens_ermn, live_sap_dens_midn, live_sap_dens_ncrn, live_sap_dens_netn)

live_sap_dens_spp1 <- right_join(spp_list, live_sap_dens, by = "Species") %>% 
  mutate(Group = ifelse(Species == "Total", "Total", Group)) 

live_sap_dens_spp <- live_sap_dens_spp1 %>% filter(!Species %in% c("No species recorded")) %>% 
  left_join(plot_visit_df %>% select(Plot_Name, cycle), ., 
            by = c("Plot_Name", "cycle"))

# write.csv(live_sap_dens_spp, paste0(datapath, "Live_sapling_dens_by_plot_spp.csv"), row.names = F)

head(live_sap_dens_spp)

spp_list2 <- unique(data.frame(live_sap_dens_spp$Species, live_sap_dens_spp$Group))

sort(unique(live_sap_dens_spp$Group))

live_sap_dens_total <- live_sap_dens_spp %>% 
  filter(Group == "Total") %>% 
  select(Network, Plot_Name, cycle, Dens) %>% 
  rename(Sap_Dens_Total = Dens)

live_sap_dens_native <- live_sap_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Sap_Dens_Native = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_sap_dens_natcan <- live_sap_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1 & Canopy_Tree == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Sap_Dens_NatCan = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_sap_dens_exotic <- live_sap_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 0) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Sap_Dens_Exotic = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_sap_dens_comb <- list(plot_visit_df, live_sap_dens_total, live_sap_dens_native, 
                             live_sap_dens_natcan, live_sap_dens_exotic) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

live_sap_dens_final <- live_sap_dens_comb %>% mutate(Sap_Dens_Total = Sap_Dens_Total/(SapPlotNum * SapPlotSize),
                                                     Sap_Dens_Native = Sap_Dens_Native/(SapPlotNum * SapPlotSize),
                                                     Sap_Dens_NatCan = Sap_Dens_NatCan/(SapPlotNum * SapPlotSize),
                                                     Sap_Dens_Exotic = Sap_Dens_Exotic/(SapPlotNum * SapPlotSize)) %>% 
                                              select(Plot_Name:cycle, Sap_Dens_Total, Sap_Dens_Native, 
                                                     Sap_Dens_NatCan, Sap_Dens_Exotic)


# Checking work
length(unique(live_sap_dens_final$Plot_Name)) #1515
nrow(live_sap_dens_final) #4464
table(complete.cases(live_sap_dens_final)) # 6F: b/c plots are missing regen data, which is correct.

#---- Live sapling BA by species group in m2/ha----
live_sap_ba_ermn <- rbind(sum_by_cycle(ermn, network_name = "ERMN", 
                                        group = 'saplings', year_span = 2008:2011,
                                        value = 'size', units = 'plot',
                                        value_name = "BA", cycle_name = 1),
                           sum_by_cycle(ermn, network_name = "ERMN",  
                                        group = 'saplings', year_span = 2012:2015,
                                        value = 'size', units = 'plot',
                                        value_name = "BA", cycle_name = 2),
                           sum_by_cycle(ermn, network_name = "ERMN", 
                                        group = 'saplings', year_span = 2016:2019,
                                        value = 'size', units = 'plot',
                                        value_name = "BA", cycle_name = 3))

live_sap_ba_midn <- rbind(sum_by_cycle(midn, network_name = "MIDN", 
                                        group = 'saplings', year_span = 2008:2011,
                                        value = 'size', units = 'plot',
                                        value_name = "BA", cycle_name = 1),
                           sum_by_cycle(midn, network_name = "MIDN", 
                                        group = 'saplings', year_span = 2012:2015,
                                        value = 'size', units = 'plot',
                                        value_name = "BA", cycle_name = 2),
                           sum_by_cycle(midn, network_name = "MIDN", 
                                        group = 'saplings', year_span = 2016:2019,
                                        value = 'size', units = 'plot',
                                        value_name = "BA", cycle_name = 3))

live_sap_ba_ncrn <- rbind(sum_by_cycle(ncrn, network_name = "NCRN", 
                                        group = 'saplings', year_span = 2008:2011,
                                        value = 'size', units = 'plot',
                                        value_name = "BA", cycle_name = 1),
                           sum_by_cycle(ncrn, network_name = "NCRN", 
                                        group = 'saplings', year_span = 2012:2015,
                                        value = 'size', units = 'plot',
                                        value_name = "BA", cycle_name = 2),
                           sum_by_cycle(ncrn, network_name = "NCRN", 
                                        group = 'saplings', year_span = 2016:2019,
                                        value = 'size', units = 'plot',
                                        value_name = "BA", cycle_name = 3))

live_sap_ba_netn <- rbind(sum_by_cycle(netn, network_name = "NETN", 
                                        group = 'saplings', year_span = 2008:2011,
                                        value = 'size', units = 'plot', 
                                        value_name = "BA", cycle_name = 1),
                           sum_by_cycle(netn, network_name = "NETN", 
                                        group = 'saplings', year_span = 2012:2015,
                                        value = 'size', units = 'plot', 
                                        value_name = "BA", cycle_name = 2),
                           sum_by_cycle(netn, network_name = "NETN", 
                                        group = 'saplings', year_span = 2016:2019,
                                        value = 'size', units = 'plot', 
                                        value_name = "BA", cycle_name = 3))

live_sap_ba <- rbind(live_sap_ba_ermn, live_sap_ba_midn, live_sap_ba_ncrn, live_sap_ba_netn)
live_sap_ba_spp1 <- right_join(spp_list, live_sap_ba, by = "Species") %>% 
  mutate(Group = ifelse(Species == "Total", "Total", Group))

live_sap_ba_spp <- live_sap_ba_spp1 %>% filter(!Species %in% c("No species recorded")) %>% 
  left_join(plot_visit_df %>% select(Plot_Name, cycle), ., 
            by = c("Plot_Name", "cycle"))

spp_list2 <- unique(data.frame(live_sap_ba_spp$Species, live_sap_ba_spp$Group))

live_sap_ba_total <- live_sap_ba_spp %>% 
  filter(Group == "Total") %>% 
  select(Network, Plot_Name, cycle, BA) %>% 
  rename(Sap_BA_Total = BA)

live_sap_ba_native <- live_sap_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Sap_BA_Native = sum(BA, na.rm = T), 
            .groups = 'drop')

live_sap_ba_natcan <- live_sap_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1 & Canopy_Tree == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Sap_BA_NatCan = sum(BA, na.rm = T), 
            .groups = 'drop')

live_sap_ba_exotic <- live_sap_ba_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 0) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Sap_BA_Exotic = sum(BA, na.rm = T), 
            .groups = 'drop')

head(live_sap_ba_natcan)

live_sap_ba_comb <- list(plot_visit_df, live_sap_ba_total, live_sap_ba_native, 
                         live_sap_ba_natcan, live_sap_ba_exotic) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

live_sap_ba_final <- live_sap_ba_comb %>% mutate(Sap_BA_Total = (10000*Sap_BA_Total)/(SapPlotNum * SapPlotSize),
                                                 Sap_BA_Native = (10000*Sap_BA_Native)/(SapPlotNum * SapPlotSize),
                                                 Sap_BA_NatCan = (10000*Sap_BA_NatCan)/(SapPlotNum * SapPlotSize),
                                                 Sap_BA_Exotic = (10000*Sap_BA_Exotic)/(SapPlotNum * SapPlotSize)) %>% 
  select(Plot_Name:cycle, Sap_BA_Total, Sap_BA_Native, 
         Sap_BA_NatCan, Sap_BA_Exotic)

# Checking work
length(unique(plot_visit_df$Plot_Name)) #1515 
length(unique(live_sap_ba_final$Plot_Name)) #1515
nrow(plot_visit_df) #4464
nrow(live_sap_ba_final)#4464
table(complete.cases(live_sap_ba_final)) # 6F: b/c plots are missing regen data, which is correct.

#---- Live seedling density by species group in stems/m2 ----
live_seed_dens_ermn <- rbind(sum_by_cycle(ermn, network_name = "ERMN", 
                                         group = 'seedlings', year_span = 2008:2011,
                                         value = 'count', units = 'plot',
                                         value_name = "Dens", cycle_name = 1),
                            sum_by_cycle(ermn, network_name = "ERMN",  
                                         group = 'seedlings', year_span = 2012:2015,
                                         value = 'count', units = 'plot',
                                         value_name = "Dens", cycle_name = 2),
                            sum_by_cycle(ermn, network_name = "ERMN", 
                                         group = 'seedlings', year_span = 2016:2019,
                                         value = 'count', units = 'plot',
                                         value_name = "Dens", cycle_name = 3))

live_seed_dens_midn <- rbind(sum_by_cycle(midn, network_name = "MIDN", 
                                         group = 'seedlings', year_span = 2008:2011,
                                         value = 'count', units = 'plot',
                                         value_name = "Dens", cycle_name = 1),
                            sum_by_cycle(midn, network_name = "MIDN", 
                                         group = 'seedlings', year_span = 2012:2015,
                                         value = 'count', units = 'plot',
                                         value_name = "Dens", cycle_name = 2),
                            sum_by_cycle(midn, network_name = "MIDN", 
                                         group = 'seedlings', year_span = 2016:2019,
                                         value = 'count', units = 'plot',
                                         value_name = "Dens", cycle_name = 3))

live_seed_dens_ncrn <- rbind(sum_by_cycle(ncrn, network_name = "NCRN", 
                                         group = 'seedlings', year_span = 2008:2011,
                                         value = 'count', units = 'plot',
                                         value_name = "Dens", cycle_name = 1),
                            sum_by_cycle(ncrn, network_name = "NCRN", 
                                         group = 'seedlings', year_span = 2012:2015,
                                         value = 'count', units = 'plot',
                                         value_name = "Dens", cycle_name = 2),
                            sum_by_cycle(ncrn, network_name = "NCRN", 
                                         group = 'seedlings', year_span = 2016:2019,
                                         value = 'count', units = 'plot',
                                         value_name = "Dens", cycle_name = 3))

live_seed_dens_netn <- rbind(sum_by_cycle(netn, network_name = "NETN", 
                                         group = 'seedlings', year_span = 2008:2011,
                                         value = 'count', units = 'plot', 
                                         value_name = "Dens", cycle_name = 1),
                            sum_by_cycle(netn, network_name = "NETN", 
                                         group = 'seedlings', year_span = 2012:2015,
                                         value = 'count', units = 'plot', 
                                         value_name = "Dens", cycle_name = 2),
                            sum_by_cycle(netn, network_name = "NETN", 
                                         group = 'seedlings', year_span = 2016:2019,
                                         value = 'count', units = 'plot', 
                                         value_name = "Dens", cycle_name = 3))

live_seed_dens <- rbind(live_seed_dens_ermn, live_seed_dens_midn, live_seed_dens_ncrn, live_seed_dens_netn)

live_seed_dens_spp1 <- right_join(spp_list, live_seed_dens, by = "Species") %>% 
  mutate(Group = ifelse(Species == "Total", "Total", Group))

live_seed_dens_spp <- live_seed_dens_spp1 %>% filter(!Species %in% c("No species recorded")) %>% 
  left_join(plot_visit_df %>% select(Plot_Name, cycle), ., 
            by = c("Plot_Name", "cycle"))

# write.csv(live_seed_dens_spp, paste0(datapath, "Live_seedling_dens_by_plot_spp.csv"), row.names = F)

spp_list2 <- unique(data.frame(live_seed_dens_spp$Species, live_seed_dens_spp$Group))

sort(unique(live_seed_dens_spp$Group))

live_seed_dens_total <- live_seed_dens_spp %>% 
  filter(Group == "Total") %>% 
  select(Network, Plot_Name, cycle, Dens) %>% 
  rename(Seed_Dens_Total = Dens)

live_seed_dens_native <- live_seed_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Seed_Dens_Native = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_seed_dens_natcan <- live_seed_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 1 & Canopy_Tree == 1) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Seed_Dens_NatCan = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_seed_dens_exotic <- live_seed_dens_spp %>% 
  filter(Group != "Total") %>% 
  filter(Native == 0) %>% 
  group_by(Network, Plot_Name, cycle) %>% 
  summarize(Seed_Dens_Exotic = sum(Dens, na.rm = T), 
            .groups = 'drop')

live_seed_dens_comb <- list(plot_visit_df, live_seed_dens_total, live_seed_dens_native, 
                           live_seed_dens_natcan, live_seed_dens_exotic) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

live_seed_dens_final <- live_seed_dens_comb %>% mutate(Seed_Dens_Total = Seed_Dens_Total/(SeedPlotNum * SeedPlotSize),
                                                       Seed_Dens_Native = Seed_Dens_Native/(SeedPlotNum * SeedPlotSize),
                                                       Seed_Dens_NatCan = Seed_Dens_NatCan/(SeedPlotNum * SeedPlotSize),
                                                       Seed_Dens_Exotic = Seed_Dens_Exotic/(SeedPlotNum * SeedPlotSize)) %>% 
  select(Plot_Name:cycle, Seed_Dens_Total, Seed_Dens_Native, 
         Seed_Dens_NatCan, Seed_Dens_Exotic)


# Checking work
length(unique(live_seed_dens_final$Plot_Name)) #1515
nrow(live_seed_dens_final) #4464
table(complete.cases(live_seed_dens_final)) #10 FALSE; correct
live_seed_dens_final[which(!complete.cases(live_seed_dens_final)), c("Plot_Name", "Year", "Seed_Dens_Total")]
# These are the 10 events that should have missing seedling data
# Plot_Name Year Seed_Dens_Total
# DEWA-159 2017             NaN
# DEWA-304 2017             NaN
# GARI-206 2018             NaN
# COLO-380 2018             NaN
# CATO-0331 2018             NaN
# CHOH-0015 2017             NaN
# CHOH-1191 2010             NaN
# NACE-0493 2017             NaN
# ACAD-029 2010             NaN
# SAGA-008 2010             NaN
names(plot_visit_df)

tree_sap_seed <- list(plot_visit_df %>% select(Plot_Name, Network, Unit_Code, Year, cycle, excludeEvent),
                      live_tree_ba_final %>% select(Network, Plot_Name, cycle, Tree_BA_Total:Tree_BA_Exotic), 
                      live_tree_dens_final %>% select(Network, Plot_Name, cycle, Tree_Dens_Total:Tree_Dens_Exotic),
                      
                      live_tree_10cm_ba_final %>% select(Network, Plot_Name, cycle, tree_10cm_BA_Total:tree_10cm_BA_Exotic),
                      live_tree_20cm_ba_final %>% select(Network, Plot_Name, cycle, tree_20cm_BA_Total:tree_20cm_BA_Exotic),
                      live_tree_30cm_ba_final %>% select(Network, Plot_Name, cycle, tree_30cm_BA_Total:tree_30cm_BA_Exotic),
                      live_tree_40cm_ba_final %>% select(Network, Plot_Name, cycle, tree_40cm_BA_Total:tree_40cm_BA_Exotic),
                      
                      live_tree_10cm_dens_final %>% select(Network, Plot_Name, cycle, tree_10cm_Dens_Total:tree_10cm_Dens_Exotic),
                      live_tree_20cm_dens_final %>% select(Network, Plot_Name, cycle, tree_20cm_Dens_Total:tree_20cm_Dens_Exotic),
                      live_tree_30cm_dens_final %>% select(Network, Plot_Name, cycle, tree_30cm_Dens_Total:tree_30cm_Dens_Exotic),
                      live_tree_40cm_dens_final %>% select(Network, Plot_Name, cycle, tree_40cm_Dens_Total:tree_40cm_Dens_Exotic),
                      
                      live_sap_dens_final %>% select(Network, Plot_Name, cycle, Sap_Dens_Total:Sap_Dens_Exotic), 
                      live_sap_ba_final %>% select(Network, Plot_Name, cycle, Sap_BA_Total:Sap_BA_Exotic),
                      live_seed_dens_final %>% select(Network, Plot_Name, cycle, Seed_Dens_Total:Seed_Dens_Exotic)) %>% 
                 reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

# Turn plots that were excluded at the full event level to NA
tree_sap_seed[tree_sap_seed$excludeEvent == 1,
              c("Tree_BA_Total", "Tree_BA_Native", "Tree_BA_NatCan", "Tree_BA_Exotic", 
                "Tree_Dens_Total", "Tree_Dens_Native", "Tree_Dens_NatCan", "Tree_Dens_Exotic",
                
                "tree_10cm_BA_Total", "tree_10cm_BA_Native", "tree_10cm_BA_NatCan", "tree_10cm_BA_Exotic", 
                "tree_10cm_Dens_Total", "tree_10cm_Dens_Native", "tree_10cm_Dens_NatCan", "tree_10cm_Dens_Exotic",
                "tree_20cm_BA_Total", "tree_20cm_BA_Native", "tree_20cm_BA_NatCan", "tree_20cm_BA_Exotic", 
                "tree_20cm_Dens_Total", "tree_20cm_Dens_Native", "tree_20cm_Dens_NatCan", "tree_20cm_Dens_Exotic",
                "tree_30cm_BA_Total", "tree_30cm_BA_Native", "tree_30cm_BA_NatCan", "tree_30cm_BA_Exotic", 
                "tree_30cm_Dens_Total", "tree_30cm_Dens_Native", "tree_30cm_Dens_NatCan", "tree_30cm_Dens_Exotic",
                "tree_40cm_BA_Total", "tree_40cm_BA_Native", "tree_40cm_BA_NatCan", "tree_40cm_BA_Exotic", 
                "tree_40cm_Dens_Total", "tree_40cm_Dens_Native", "tree_40cm_Dens_NatCan", "tree_40cm_Dens_Exotic",

                "Sap_Dens_Total", "Sap_Dens_Native", "Sap_Dens_NatCan", "Sap_Dens_Exotic",
                "Sap_BA_Total", "Sap_BA_Native", "Sap_BA_NatCan", "Sap_BA_Exotic", 
                "Seed_Dens_Total", "Seed_Dens_Native", "Seed_Dens_NatCan", "Seed_Dens_Exotic")] <- NA_real_

# write.csv(tree_sap_seed, paste0(datapath, "EFWG_tree_sapling_seedling_BA_dens_20220325.csv"), row.names = F)
names(tree_sap_seed)

#---- SiteXSpecies dfs for similarity ----
names(live_tree_dens_spp)
names(live_sap_dens_spp)
names(live_seed_dens_spp)

# Need to clean up species list a little
dens_comb1 <- rbind(live_tree_dens_spp %>% mutate(strata = "tree"),
                    live_sap_dens_spp %>% mutate(strata = "sapling"),
                    live_seed_dens_spp %>% mutate(strata = "seedling")) %>% 
              select(-Group, -Canopy_Tree, -Native) %>% 
              filter(!Species %in% c("No species recorded", "NA"))

sort(unique(dens_comb1$Species))

dens_comb1$Species <- gsub("- ", "", dens_comb1$Species)
dens_comb1$Species <- gsub(" ", "_", dens_comb1$Species)
dens_comb1$Species <- gsub("_sp\\.", "", dens_comb1$Species)
dens_comb1$Species[grepl("Unknown", dens_comb1$Species)] <- "Unknown"
dens_comb1$Species <- gsub("_spp\\.", "", dens_comb1$Species)
dens_comb1$Species <- gsub("sp\\.", "", dens_comb1$Species)
dens_comb1$Species <- gsub("spp\\.", "", dens_comb1$Species)

dens_comb <- dens_comb1 %>% group_by(Plot_Name, Network, cycle, Species, strata) %>% 
                            summarize(Dens = sum(Dens, na.rm = T), .groups = 'drop') %>% 
             left_join(plot_visit_df %>% select(Plot_Name, Network, Unit_Code, cycle), ., 
                       by = c("Plot_Name", "Network", "cycle")) # a bit slow but works

#sort(unique(dens_comb$Species))
length(unique(dens_comb$Plot_Name)) #1515

dens_wide <- dens_comb %>% arrange(Species) %>% 
                           pivot_wider(names_from = Species, 
                                       values_from = Dens, 
                                       values_fill = 0) %>% select(-Total) %>% 
                           arrange(Network, Plot_Name, cycle, strata)

length(unique(dens_wide$Plot_Name)) #1515
table(complete.cases(dens_wide)) #all T
head(dens_wide[,1:10])

# Create siteXspec matrices for each network and drop species that sum to 0
ERMN_siteXspec <- dens_wide %>% filter(Network == "ERMN") %>% select_if(~!is.numeric(.) || sum(.) != 0)
MIDN_siteXspec <- dens_wide %>% filter(Network == "MIDN") %>% select_if(~!is.numeric(.) || sum(.) != 0)
NCRN_siteXspec <- dens_wide %>% filter(Network == "NCRN") %>% select_if(~!is.numeric(.) || sum(.) != 0)
NETN_siteXspec <- dens_wide %>% filter(Network == "NETN") %>% select_if(~!is.numeric(.) || sum(.) != 0)

sor_fun <- function(df){
  df2 <- df %>% select_if(~is.numeric(.) && sum(.) != 0) # remove species that sum to 0
  sor <-  betadiver(df2, method = 'sor')
  return(sor)
}

horn_fun <- function(df){
  df2 <- df %>% select_if(~is.numeric(.) && sum(.) != 0)  #remove species that sum to 0
  hor <-  vegdist(df2, method = 'horn')
  return(hor)
}

ERMN_treesap_nest <- ERMN_siteXspec %>% filter(strata %in% c("tree", "sapling")) %>%  
  group_by(Plot_Name, Network, Unit_Code, cycle) %>% nest() %>% 
  mutate(Sor_sap = map(data, sor_fun),
         Hor_sap = map(data, horn_fun))%>% 
  unnest(cols = c(Sor_sap, Hor_sap)) %>% select(-data)

ERMN_treeseed_nest <- ERMN_siteXspec %>% filter(strata %in% c("tree", "seedling")) %>%  
  group_by(Plot_Name, Network, Unit_Code, cycle) %>% nest() %>% 
  mutate(Sor_seed = map(data, sor_fun),
         Hor_seed = map(data, horn_fun))%>% 
  unnest(cols = c(Sor_seed, Hor_seed)) %>% select(-data)

MIDN_treesap_nest <- MIDN_siteXspec %>% filter(strata %in% c("tree", "sapling")) %>%  
  group_by(Plot_Name, Network, Unit_Code, cycle) %>% nest() %>% 
  mutate(Sor_sap = map(data, sor_fun),
         Hor_sap = map(data, horn_fun))%>% 
  unnest(cols = c(Sor_sap, Hor_sap)) %>% select(-data)

MIDN_treeseed_nest <- MIDN_siteXspec %>% filter(strata %in% c("tree", "seedling")) %>%  
  group_by(Plot_Name, Network, Unit_Code, cycle) %>% nest() %>% 
  mutate(Sor_seed = map(data, sor_fun),
         Hor_seed = map(data, horn_fun))%>% 
  unnest(cols = c(Sor_seed, Hor_seed)) %>% select(-data)

NCRN_treesap_nest <- NCRN_siteXspec %>% filter(strata %in% c("tree", "sapling")) %>%  
  group_by(Plot_Name, Network, Unit_Code, cycle) %>% nest() %>% 
  mutate(Sor_sap = map(data, sor_fun),
         Hor_sap = map(data, horn_fun))%>% 
  unnest(cols = c(Sor_sap, Hor_sap)) %>% select(-data)

NCRN_treeseed_nest <- NCRN_siteXspec %>% filter(strata %in% c("tree", "seedling")) %>%  
  group_by(Plot_Name, Network, Unit_Code, cycle) %>% nest() %>% 
  mutate(Sor_seed = map(data, sor_fun),
         Hor_seed = map(data, horn_fun))%>% 
  unnest(cols = c(Sor_seed, Hor_seed)) %>% select(-data)

NETN_treesap_nest <- NETN_siteXspec %>% filter(strata %in% c("tree", "sapling")) %>%  
  group_by(Plot_Name, Network, Unit_Code, cycle) %>% nest() %>% 
  mutate(Sor_sap = map(data, sor_fun),
         Hor_sap = map(data, horn_fun))%>% 
  unnest(cols = c(Sor_sap, Hor_sap)) %>% select(-data)

NETN_treeseed_nest <- NETN_siteXspec %>% filter(strata %in% c("tree", "seedling")) %>%  
  group_by(Plot_Name, Network, Unit_Code, cycle) %>% nest() %>% 
  mutate(Sor_seed = map(data, sor_fun),
         Hor_seed = map(data, horn_fun))%>% 
  unnest(cols = c(Sor_seed, Hor_seed)) %>% select(-data)

sim_data_sap <- rbind(ERMN_treesap_nest,
                      MIDN_treesap_nest,
                      NCRN_treesap_nest, 
                      NETN_treesap_nest)

sim_data_seed <- rbind(ERMN_treeseed_nest,
                       MIDN_treeseed_nest,
                       NCRN_treeseed_nest, 
                       NETN_treeseed_nest)

head(sim_data_sap)
head(sim_data_seed)

sim_comb <- list(plot_visit_df %>% select(Plot_Name, Network, Unit_Code, cycle, excludeEvent,
                                          SapPlotNum, SeedPlotNum),
                 sim_data_sap, sim_data_seed) %>% 
            reduce(left_join, by = c("Plot_Name", "Network", "Unit_Code", "cycle")) %>% 
            data.frame()

sim_cols <- c("Sor_sap", "Sor_seed", "Hor_sap", "Hor_seed")

sim_comb[,sim_cols][is.na(sim_comb[,sim_cols])] <- 0 

# Plots missing partial or all visit data are converted to NA below
sim_comb[sim_comb$excludeEvent == 1,
              c("Sor_sap", "Hor_sap", "Sor_seed", "Hor_seed")] <- NA_real_

sim_comb[sim_comb$SapPlotNum == 0,
         c("Sor_sap", "Hor_sap")] <- NA_real_

sim_comb[sim_comb$SeedPlotNum == 0,
         c("Sor_seed", "Hor_seed")] <- NA_real_

table(complete.cases(sim_comb)) #4454T; 10F

# write.csv(sim_comb, paste0(datapath, "EFWG_Similarity_by_plot_cycle_20220325.csv"), row.names = F)

#----- Stocking Index Calculation -----
load_data <- function(network, file, path = datapath){
  read.csv(paste0(path, network, "_data/", file, ".csv")) %>% mutate(network = network)
}

# Load tree spp. list we decided on as a group 2/3/2021
tree_spp_list <- read.csv(paste0(datapath, "NPS_count_plots_w_species_by_park.csv"))[,1:2] 

#----- Combine plot and event data -----
# Load metadata about plot size and number
ermn_meta <- load_data("ERMN", "MetaData")
midn_meta <- load_data("MIDN", "MetaData") #NCBN data are always included with MIDN
ncrn_meta <- load_data("NCRN", "MetaData")
netn_meta <- load_data("NETN", "MetaData")

meta_comb <- rbind(ermn_meta, midn_meta, ncrn_meta, netn_meta) 
names(meta_comb)

# Load plot and event data
ermn_plots <- load_data("ERMN", "Plots")
ermn_events <- load_data("ERMN", "Events")
midn_plots <- load_data("MIDN", "Plots")
midn_events <- load_data("MIDN", "Events")
ncrn_plots <- load_data("NCRN", "Plots")
ncrn_events1 <- load_data("NCRN", "Events") 
ncrn_plotvis <- load_data("NCRN", "Plot_Visit_Data")
netn_plots <- load_data("NETN", "Plots")
netn_events <- load_data("NETN", "Events")

#add ncrn metaplot data
ncrn_events <- merge(ncrn_events1, ncrn_plotvis[, c("Plot_Name", "Event_Date_Txt", "Deer_Impact")], 
                     by = c("Plot_Name", "Event_Date_Txt"), all.x = T, all.y = T)
ncrn_events <- ncrn_events %>% rename("Deer_Browse_Line_ID" = "Deer_Impact") %>% 
  mutate(numHerbPlots = 12,
         numSapPlots = 3, 
         numSeedPlots = 12)

cols_ev <- intersect(names(ermn_events), names(ncrn_events))
cols_ev

events_comb <- rbind(ermn_events[, cols_ev],
                     midn_events[, cols_ev],
                     ncrn_events[, cols_ev],
                     netn_events[, cols_ev]
)

# short cut to get colnames in common
cols_pl1 <- intersect(names(ermn_plots), names(ncrn_plots))
cols_pl <- intersect(names(midn_plots), cols_pl1)

plots_comb <- rbind(ermn_plots[, cols_pl],
                    midn_plots[, cols_pl],
                    ncrn_plots[, cols_pl],
                    netn_plots[, cols_pl]
)

comb_df <- merge(plots_comb[ ,c("Plot_Name", "Latitude", "Longitude")],
                 events_comb[,c("Plot_Name", "Event_Date_Txt", "Event_Year", "network", "Unit_Code")],
                 by = "Plot_Name",
                 all.x = T, all.y = T) %>% 
  filter(Event_Year >= 2008) %>% 
  filter(Unit_Code != "ASIS") # only have 6 plots from 2019

# Check for duplicates
nrow(comb_df) - nrow(unique(comb_df[,c("Plot_Name", "Event_Year")])) #there's a duplicate in here
dup <- as.numeric(rownames(comb_df[duplicated(comb_df[,c("Plot_Name", "Event_Year")]),])) #CHOH-1191 2010 dup
plot_events <- comb_df[-dup,] 
nrow(plot_events) - nrow(unique(plot_events[,c("Plot_Name", "Event_Year")])) #no dups
table(plot_events$Unit_Code, plot_events$Event_Year) # Did ANTI not have plots until 2010?
# plot_events is the df of all unique plot and year combinations for left join with regen

#------ Load and combine seedling and sapling data -----
# Seedling data
ermn_seeds <- load_data("ERMN", "Seedlings")
midn_seeds <- load_data("MIDN", "Seedlings")
netn_seeds <- load_data("NETN", "Seedlings")
ncrn_seeds <- load_data("NCRN", "Seedlings")

seed_names <- c("Plot_Name", "network", "Unit_Code", "Sample_Year", "Latin_Name", "Height")

seed_comb <- rbind(ermn_seeds[, seed_names],
                   midn_seeds[, seed_names],
                   ncrn_seeds[, seed_names],
                   netn_seeds[, seed_names]) %>% 
  filter(Height >= 15.0) %>% # remove seedlings <15cm tall in ERMN 
  filter(Sample_Year >= 2008) # takes 3 most recent cycles

seed_comb <- seed_comb %>% mutate(size_class = case_when(Height <30 ~ paste("ht15.30"),
                                                         Height >=30 & Height <100 ~ paste("ht30.100"),
                                                         Height >=100 & Height <150 ~ paste("ht100.150"),
                                                         Height >=150 ~ paste("ht150p")),
                                  tally = ifelse(Height >=15, 1, 0)) %>% 
  select(Plot_Name:Latin_Name, tally, size_class)

# Sapling data
ermn_saps <- load_data("ERMN", "Saplings")
midn_saps <- load_data("MIDN", "Saplings")
netn_saps <- load_data("NETN", "Saplings")
ncrn_saps <- load_data("NCRN", "Saplings")
sap_names <- c("Plot_Name", "network", "Unit_Code", "Sample_Year", "Latin_Name", "StemsLive", "Equiv_Live_DBH_cm")
sap_comb <- rbind(ermn_saps[, sap_names],
                  midn_saps[, sap_names],
                  ncrn_saps[, sap_names],
                  netn_saps[, sap_names]) %>% 
  filter(Equiv_Live_DBH_cm >= 1 & Equiv_Live_DBH_cm <=2.5) %>% 
  filter(Sample_Year >= 2008) %>% 
  mutate(size_class = "sapling",
         tally = StemsLive) %>% 
  select(names(seed_comb)) #select names in seed_comb for rbind

# Combine seedling and sapling tally data
regen_comb <- rbind(sap_comb, seed_comb)

# Merge regen with tree species groupings
regen_spp <- merge(regen_comb, tree_spp_list, by.x = "Latin_Name", by.y = "Species", all.x = T, all.y = F)
head(regen_spp)

# Fixing missing group by hand
missing_spp <- unique(regen_spp$Latin_Name[is.na(regen_spp$Group)])
missing_spp

# > missing_spp

# [1] "Acer palmatum"          "Albizia julibrissin"    "Amelanchier canadensis" "Aralia elata"          
# [5] "Betulaceae"             "Carya spp."             "Castanea pumila"        "Celtis laevigata"      
# [9] "Cornus spp."            "Crataegus macrosperma"  "Crataegus sp. B"        "Fabaceae"              
# [13] "Fraxinus spp."          "Nyssa biflora"          "Persea"                 "Photinia villosa"      
# [17] "Populus alba"           "Populus sp."            "Prunus cerasus"         "Prunus spp."           
# [21] "Rosaceae"               "Ulmus spp."             "Unknown Conifer"        "Unknown Hardwood"      
# [25] "Unknown species"        "Unknown Tree"           "Unknown Tree - 01"      "Unknown Tree - 02"     
# [29] "Unknown Tree - 04" 

fixes <- data.frame(latin = missing_spp, 
                    group = c("Exotic", "Exotic", "Amelanchier sp.", "Exotic", 
                              "Other Native", "Carya sp.", "Understory Tree", "Celtis sp.",
                              "Understory Tree", "Post Ag", "Post Ag", "Unk",
                              "Fraxinus sp.", "Other Native", "Understory Tree", "Exotic",
                              "Exotic", "Other Native", "Exotic", "Prunus sp.",
                              "Unk", "Ulmus sp.", "Unk", "Unk",
                              "Unk", "Unk", "Unk", "Unk", 
                              "Unk"))

regen_spp2 <- merge(regen_spp, fixes, by.x = "Latin_Name", by.y = "latin", all.x = T) %>% 
  mutate(Group = ifelse(is.na(Group), group, Group)) %>% select(-group)

table(complete.cases(regen_spp2$Group)) # every spp. has a group

#------ Calculate stocking index ------
# Use Group to drop exotic and non-canopy trees
sort(unique(regen_spp2$Group))
drops <- c("Asimina triloba", "Ilex opaca", "Juniperus virginiana", 
           #"Fraxinus americana", "Fraxinus sp.",
           "Exotic",  "Post Ag", "Understory Tree", "Unk")

# Drop groups and summarize regen at plot/visit level
regen_sum <- regen_spp2 %>% filter(!Group %in% drops) %>%
  filter(!(network == "NETN" & Group == "Robinia pseudoacacia")) %>%  # exotic in NETN
  group_by(Plot_Name, network, Unit_Code, Sample_Year, size_class) %>% 
  summarize(num_stems = sum(tally), 
            .groups = 'drop')

# Make data wide so every plot event has a number for each size class
regen_wide <- regen_sum %>% pivot_wider(names_from = size_class, values_from = num_stems, values_fill = 0) %>% 
  select(Plot_Name, Sample_Year, ht15.30, ht30.100, ht100.150, ht150p, sapling)

# Use plot_events as left join to add plots with 0 seedlings/saplings
regen_full <- left_join(plot_events, regen_wide, by = c("Plot_Name", "Event_Year" = "Sample_Year"))

regen_cols <- c("ht15.30", "ht30.100", "ht100.150", "ht150p", "sapling")
regen_full[, regen_cols][is.na(regen_full[, regen_cols])] <- 0 #replace NAs with 0 for numeric fields
table(complete.cases(regen_full))

# Add meta plot data to df, so can calculate regen tallies on same area units
regen_meta <- right_join(meta_comb[,c("ParkCode", "network", "SapPlotSize", "SapPlotNum", 
                                      "SeedPlotSize", "SeedPlotNum")], 
                         regen_full, 
                         by = c("ParkCode" = "Unit_Code",
                                "network" = "network")
)

head(regen_meta)
nrow(unique(plot_events[,c("Plot_Name", "Event_Year")]))- 
  nrow(unique(regen_meta[,c("Plot_Name", "Event_Year")])) # no missing plot visits in regen_meta

# Now for the tricky part. 
# 1. Stocking index weights and thresholds were designed for 2m radius circles for both seedlings and saplings
#     a. NETN samples seedlings and saplings in 3 2m radius circles.
#     b. ERMN samples seedlings and saplings in 4 2m radius circles.
#     c. MIDN/NCRN sample seedlings in 12 1m2 quadrats, and saplings in 3 3m radius circles.
# 2. NETN/ERMN interpret stocking index on the 2m radius scale, with 25, 50, and 100 being important thresholds.
#    MIDN/NCRN interpret stocking index at the 1m square scale, with 1.99, 3.96, and 7.96 as thresholds 
# 3. To get networks on the same scale, we have to use the SeedPlotSize, SeedPlotNum, SapPlotSize and SapPlotNum fields,
#    and then convert MIDN/NCRN from /m2 to /2m radius circle, then we can apply the weights by size class.
#      a. MIDN/NCRN have to convert seedlings to number/m2 by dividing by SeedPlotSize*SeedPlotNum
#         and saplings/m2 by dividing by SapPlotSize*SapPlotNum. To put these on the 0-100 scale for
#         2m radius circles, these then need to be multiplied by pi*2^2.
#      b. ERMN and NETN can be handled in the same step because SeedPlotNum and SapPlotNum account for 3 vs 4 microplots.

rebels <- c("MIDN", "NCRN") # ;-) NCBN was combined into MIDN for simplicity 

stock <- regen_meta %>% mutate(seed_conv = ifelse(network %in% rebels,
                                                  SeedPlotSize*SeedPlotNum, # denom to convert to m2 
                                                  SeedPlotNum), #denom to ave 2m radius micros
                               sap_conv = ifelse(network %in% rebels,
                                                 SapPlotSize*SapPlotNum,
                                                 SapPlotNum),
                               stock = (1*ht15.30)/seed_conv + (2*ht30.100)/seed_conv +
                                 (20*ht100.150)/seed_conv + (50*ht150p)/seed_conv +
                                 (50*sapling)/sap_conv, # stock is MIDN/NCRN's original scale
                               stock_final = ifelse(network %in% rebels, 
                                                    stock*pi*4,
                                                    stock)) # stock_final is /2m radius microplot scale for all

table(complete.cases(stock$stock_final))

# Final data checks
nrow(stock) - nrow(unique(stock[,c("Plot_Name", "Event_Year")])) #0
table(stock$ParkCode, stock$Event_Year)

# Bring in plot visits with incomplete or excluded events
plot_ev_lj <- read.csv(paste0(datapath, "EFWG_plot_visit_left_join.csv"))

stock_final <- left_join(plot_ev_lj, stock %>% select(Plot_Name, Event_Year, stock_final), 
                         by = c("Plot_Name", "Year" = "Event_Year"))
stock_final$stock_final <- ifelse(stock_final$excludeEvent == 1 | stock_final$SeedPlotNum == 0, NA,
                                  stock_final$stock_final)

# write.csv(stock_final, paste(datapath, "EFWG_stocking_index_20220325_no_ash.csv", sep = "/"), row.names = F)

#---- Combine all plot-level datasets ----
tss <- read.csv(paste0(datapath, "EFWG_tree_sapling_seedling_BA_dens_20220325.csv"))
# Adding Native Other to the species group analysis 
natoth_fun <- function(met){
  new_col <- tss[, paste0(met, "_Native")] - tss[, paste0(met, "_NatCan")]
  return(new_col)
  }

met_list <- c("Tree_BA", "Tree_Dens", "tree_10cm_BA", "tree_10cm_Dens", "tree_20cm_BA", "tree_20cm_Dens",
              "tree_30cm_BA", "tree_30cm_Dens", "tree_40cm_BA", "tree_40cm_Dens",
              "Sap_BA", "Sap_Dens", "Seed_Dens")

tss2 <- cbind(tss, 
              natoth_fun("Tree_BA"), natoth_fun("Tree_Dens"), 
              natoth_fun("tree_10cm_BA"), natoth_fun("tree_10cm_Dens"),
              natoth_fun("tree_20cm_BA"), natoth_fun("tree_20cm_Dens"),
              natoth_fun("tree_30cm_BA"), natoth_fun("tree_30cm_Dens"),
              natoth_fun("tree_40cm_BA"), natoth_fun("tree_40cm_Dens"),
              natoth_fun("Sap_BA"), natoth_fun("Sap_Dens"), natoth_fun("Seed_Dens")) 
  
colnames(tss2) <- c(names(tss), c(paste0(met_list, "_NatOth")))

sim <- read.csv(paste0(datapath, "EFWG_Similarity_by_plot_cycle_20220325.csv"))
stock <- read.csv(paste0(datapath, "EFWG_stocking_index_20220325_no_ash.csv")) 
plot_lj <- read.csv(paste0(datapath, "EFWG_plot_visit_left_join.csv")) %>% unique()
head(plot_lj)

reg_comb <- list(plot_lj,
                  tss2 %>% select(-Year, -excludeEvent),
                  stock %>% select(Plot_Name, Network, cycle, stock_final),
                  sim %>% select(Plot_Name, Network, cycle, Sor_sap:Hor_seed)) %>% 
                  reduce(left_join, by = c("Network", "Plot_Name", "cycle")) %>% 
              rename(Unit_Code = Unit_Code.x) %>% select(-Unit_Code.y)

#dbi <- read.csv(paste0(datapath, "EFWG_park-level_DBI_rank.csv")) %>% select(Unit_Code, DBI_rank)
lat_order <- read.csv(paste0(datapath, "EFWG_lat_order.csv")) %>% select(-mean.lat) %>% 
  rename(lat_rank = lat.rank)
head(plot_lj)

reg_final <- left_join(reg_comb, lat_order, by = c("Network" = "network", "Unit_Code" = "park")) %>% 
             select(Plot_Name:cycle, DBI, lat_rank, everything()) %>% 
             mutate(Network = ifelse(Unit_Code %in% c("COLO", "GEWA", "SAHI", "THST"), "NCBN", Network))

nrow(reg_final)
head(reg_final)
plot_check <- data.frame(table(reg_final$Plot_Name)) %>% filter(Freq != 3)
plot_check

nrow(reg_final) #4465
length(unique(reg_final$Plot_Name)) #1515

#Ran this with a different date after running this script with a few changes to see if it works locally
write.csv(reg_final, paste0(datapath, "EFWG_full_dataset_20220325_ASH_AS_CAN.csv"), row.names = FALSE) 

#------- Composition Proportion Data Compile --------
#reg_final <- read.csv(paste0("./data/EFWG_full_dataset_20220325.csv"))
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


write.csv(comp_park_rel, "./data/EFWG_proportion_regen_20220325_ASH_AS_CAN.csv", row.names = F)
head(comp_park_rel)
