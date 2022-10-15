#--------------------------------------------
# Compiling Tree, Sapling and Seedling Data for Eastern Forest Regen Project
#   Code written by Kate Miller 202010916 to check results using NPSForVeg package
#--------------------------------------------

#---- Dependencies and import data using NPSForVeg ----
#devtools::install_github("NCRN/NPSForVeg")
library(NPSForVeg)
library(tidyverse)
#library(rlang)
#library(vegan)
library(boot)

options(scipen = 100)

datapath <- "./data/" 

#---- Compile Number of species per strata and group -----
ncrn <- importNCRN(paste0(datapath,"NCRN_data"))
ermn <- importERMN(paste0(datapath, "ERMN_data"))                   
midn <- importMIDN(paste0(datapath, "MIDN_data"))
netn <- importNETN(paste0(datapath, "NETN_data"))

spp_list <- read.csv(paste0(datapath, "NPS_tree_species_groups.csv")) 
# Demote Fraxinus to not canopy tree
spp_list$Canopy_Tree[grepl("Fraxinus", spp_list$Species)] <- 0


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

## write.csv(plot_visit_df, "EFWG_plot_visit_left_join.csv", row.names = F)

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

# write.csv(pv_dbi, paste0(datapath, "EFWG_plot_visit_left_join.csv"), row.names = F)

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
head(live_tree_dens_spp)


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

head(live_tree_dens_final)


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
head(live_tree_ba_final)

tree_dens_stats <- live_tree_dens_spp %>% filter(!Species %in% "Total") %>% select(-cycle) %>% 
  mutate(park = substr(Plot_Name, 1, 4)) %>% 
  group_by(park, Canopy_Tree, Native, Species) %>% 
  summarize(dens = sum(Dens, na.rm = T), 
            pres = ifelse(sum(dens > 0), 1, 0)) %>% filter(pres > 0) %>% 
  group_by(park, Canopy_Tree, Native) %>% summarize(numspp_tree = sum(pres > 0)) %>% 
  mutate(species_type = case_when(Canopy_Tree == 1 & Native == 1 ~ "NatCan",
                                  Canopy_Tree == 0 & Native == 1 ~ "NatSub",
                                  Canopy_Tree == 1 & Native == 0 ~ "Exotic", 
                                  Canopy_Tree == 0 & Native == 0 ~ "Exotic", 
                                  TRUE ~ "Unk")) %>% 
  group_by(park, species_type) %>% summarize(numspp_tree= sum(numspp_tree))

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
head(live_sap_dens_spp)

sap_dens_stats <- live_sap_dens_spp %>% filter(!Species %in% "Total") %>% select(-cycle) %>% 
  mutate(park = substr(Plot_Name, 1, 4)) %>% 
  group_by(park, Canopy_Tree, Native, Species) %>% 
  summarize(dens = sum(Dens, na.rm = T),
            pres = ifelse(sum(dens > 0), 1, 0)) %>% filter(pres > 0) %>% 
  group_by(park, Canopy_Tree, Native) %>% summarize(numspp_sap = sum(pres > 0)) %>% 
  mutate(species_type = case_when(Canopy_Tree == 1 & Native == 1 ~ "NatCan",
                                  Canopy_Tree == 0 & Native == 1 ~ "NatSub",
                                  Canopy_Tree == 1 & Native == 0 ~ "Exotic", 
                                  Canopy_Tree == 0 & Native == 0 ~ "Exotic", 
                                  TRUE ~ "Unk")) %>% 
  group_by(park, species_type) %>% summarize(numspp_sap = sum(numspp_sap))

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


seed_dens_stats <- live_seed_dens_spp %>% filter(!Species %in% "Total") %>% select(-cycle) %>% 
  mutate(park = substr(Plot_Name, 1, 4)) %>% 
  group_by(park, Canopy_Tree, Native, Species) %>% 
  summarize(dens = sum(Dens, na.rm = T),
            pres = ifelse(sum(dens > 0), 1, 0)) %>% filter(pres > 0) %>% 
  group_by(park, Canopy_Tree, Native) %>% summarize(numspp_seed = sum(pres > 0)) %>% 
  mutate(species_type = case_when(Canopy_Tree == 1 & Native == 1 ~ "NatCan",
                                  Canopy_Tree == 0 & Native == 1 ~ "NatSub",
                                  Canopy_Tree == 1 & Native == 0 ~ "Exotic", 
                                  Canopy_Tree == 0 & Native == 0 ~ "Exotic", 
                                  TRUE ~ "Unk")) %>% 
  group_by(park, species_type) %>% summarize(numspp_seed = sum(numspp_seed))

#----- Combine spp stats tables -----
numspp_table <- reduce(list(tree_dens_stats, sap_dens_stats, seed_dens_stats), left_join, by = c("park", "species_type")) %>% 
  mutate(Species_Group = case_when(species_type == "NatCan" ~ "Native Canopy",
                                   species_type == "NatSub" ~ "Subcanopy",
                                   species_type == "Exotic" ~ "Exotic"),
         sort_order = case_when(species_type == "NatCan" ~ 1,
                                species_type == "NatSub" ~ 2,
                                TRUE ~ 3)) %>% 
  arrange(park, sort_order) %>% 
  select(park, Species_Group, numspp_tree, numspp_sap, numspp_seed) 

write.csv(numspp_table, "./results/supptables/Number_species_per_strata_group.csv", row.names = F)

#----- Clean up status table -----
dens_df <- read.csv(paste0("./data/EFWG_full_dataset_20220325.csv"))
res_stat1 <- read.csv('./results/20220325/EFWG_cycle_3_status.csv') 
table(res_stat1$metric)

res_stat <- res_stat1 %>% mutate(metgrp = "Regen. Status",
                                 labels = case_when(metric == "Sor_sap" ~ "Sorensen Sapling",
                                                    metric == "Sor_seed" ~ "Sorensen Seedling",
                                                    metric == "Sap_Dens_NatCan" ~ "Sapling Density",
                                                    metric == "Seed_Dens_NatCan" ~ "Seedling Density",
                                                    metric == "stock_final" ~ "Stocking Index",
                                                    TRUE ~ "drop")) %>% 
  filter(!labels %in% "drop") %>% select(-Network)

comp <- read.csv("./data/EFWG_proportion_regen_20220325.csv") %>% 
  select(Network, Unit_Code, sap_dens_pct_NatCan, seed_dens_pct_NatCan) %>% 
  pivot_longer(cols = sap_dens_pct_NatCan:seed_dens_pct_NatCan, names_to = "metric", values_to = "mean") %>% 
  mutate(labels = ifelse(metric == "sap_dens_pct_NatCan", "Sapling Composition", "Seedling Composition"),
         metgrp = "Regen. Status", lower95 = NA, upper95 = NA) %>% 
  select(park = Unit_Code, metric, mean, lower95, upper95, metgrp, labels)

# Add DBI and proportion of stocked plots 
dens_c3 <- dens_df %>% filter(cycle == 3) 
head(dens_c3)

# Create mean function used in bootstrap
mean_fun <- function(data, i){
  d <- data[i, ]
  return(mean(d, na.rm = TRUE))
}

# Iterate over parks and metrics to calc. mean and 95% CI of mean 
parks <- sort(unique(dens_c3$Unit_Code))
c3_DBI <- map_dfr(parks, function(park){
  df <- dens_c3 %>% filter(Unit_Code == park) 
  results_df <- boot(df[, "DBI", drop = FALSE], mean_fun, R = 1000)
  df2 <- data.frame(park = park, 
                    Network = unique(df$Network),
                    metric = "DBI", 
                    mean = results_df$t0,
                    lower95 = quantile(results_df$t, probs = 0.025, na.rm = T),
                    upper95 = quantile(results_df$t, probs = 0.975, na.rm = T),
                    row.names = NULL)
  return(df2)
}) %>% mutate(labels = "Deer Browse Impacts",
              metgrp = "Regen. Status") %>% 
  select(park, metric, mean, lower95, upper95, metgrp, labels)

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
  rename(park = Unit_Code) %>% select(park, mean = prop_stocked) %>% 
  mutate(metric = "prop_stocked",
         labels = "% Stocked Plots",
         metgrp = "Regen. Status",
         lower95 = NA,
         upper95 = NA)

res_stat_comb <- rbind(res_stat, comp, c3_DBI, dbi_stock_sum)
head(res_stat_comb)

res_stat_cat <- read.csv('./results/results_for_fig2.csv') %>% filter(metgrp == "Regen. Status") %>% 
  filter(!labels %in% c("Flat Tree Diam. Dist."))
head(res_stat_cat)

results_table <- left_join(res_stat_cat, res_stat_comb, by = c("park", "metgrp", "labels")) %>% 
  mutate(deermgt_park = ifelse(substr(park_dm, 5, 5) == "*", 1, 0)) %>% 
  select(park, labels, sign, park_reggrp, mean, lower95, upper95, metric_code = metric, deermgt_park,
         order) %>% arrange(park, order)
head(results_table)

write.csv(results_table, "./results/supptables/Status_metric_statistics.csv", row.names = FALSE)

#----- Flat Tree diam. dist -----
tree_dbh_dist <- read.csv("./data/Tree_DBH_Dist_by_park.csv")

head(tree_dbh_dist)

park_list <- unique(tree_dbh_dist$Unit_Code)

AIC_test <- map_dfr(park_list, function(park){
  df <- tree_dbh_dist %>% filter(Unit_Code == park & cycle == 3)
  lin_mod <- lm(density ~ class, data = df)
  exp_mod <- lm(log(density + 1) ~ class, data = df)
  aic_check <- data.frame(Unit_Code = park, 
                          linear = AIC(lin_mod),
                          exp = AIC(exp_mod) + sum(2*log(df$density + 1)))
}) %>% mutate(best_mod = ifelse(linear - exp >=4, "exp", "linear"))

write.csv(AIC_test, "./results/supptables/Flat_Tree_Diameter_Dist_AIC.csv", row.names = F)

#---- Clean up trend results table -----
results1 <- read.csv("./results/20220325/all_metrics_randint_20220325.csv")
head(results1)

results <- results1 %>% 
  mutate(response_full = case_when(
    resp == "Hor_sap" ~ "Sapling vs. Tree Similarity: Horn", 
    resp == "Hor_seed" ~ "Seedling vs. Tree Similarity: Horn", 
    resp == "Sap_BA_Exotic" ~ "Sapling Basal Area: Exotic", 
    resp == "Sap_BA_NatCan" ~ "Sapling Basal Area: Native Canopy", 
    resp == "Sap_BA_NatOth" ~ "Sapling Basal Area: Subcanopy", 
    resp == "Sap_BA_Total" ~ "Sapling Basal Area: Total", 
    resp == "Sap_Dens_Exotic" ~ "Sapling Density: Exotic", 
    resp == "Sap_Dens_NatCan" ~ "Sapling Density: Native Canopy", 
    resp == "Sap_Dens_NatOth" ~ "Sapling Density: Subcanopy", 
    resp == "Sap_Dens_Total" ~ "Sapling Density: Total", 
    resp == "Seed_Dens_Exotic" ~ "Seedling Density: Exotic", 
    resp == "Seed_Dens_NatCan" ~ "Seedling Density; Native Canopy",
    resp == "Seed_Dens_NatOth" ~ "Seedling Density: Subcanopy", 
    resp == "Seed_Dens_Total" ~ "Seedling Density: Total", 
    resp == "Sor_sap" ~ "Sapling vs. Tree Similarity: Sorensen", 
    resp == "Sor_seed" ~ "Seedling vs. Tree Similarity: Sorensen", 
    resp == "stock_final" ~ "Stocking Index", 
    resp == "Tree_BA_Exotic" ~ "Tree Basal Area: Exotic",
    resp == "Tree_BA_NatCan" ~ "Tree Basal Area: Native Canopy", 
    resp == "Tree_BA_NatOth" ~ "Tree Basal Area: Subcanopy", 
    resp == "Tree_BA_Total" ~ "Tree Basal Area: Total", 
    resp == "Tree_Dens_Exotic" ~ "Tree Density: Exotic", 
    resp == "Tree_Dens_NatCan" ~ "Tree Density: Native Canopy", 
    resp == "Tree_Dens_NatOth" ~ "Tree Density: Subcanopy", 
    resp == "Tree_Dens_Total" ~ "Tree Density: Total"),
    sort_order = case_when(
      resp == "Hor_sap" ~ 24,
      resp == "Hor_seed" ~ 25, 
      resp == "Sap_BA_Exotic" ~ 16, 
      resp == "Sap_BA_NatCan" ~ 14,
      resp == "Sap_BA_NatOth" ~ 15,
      resp == "Sap_BA_Total" ~ 13, 
      resp == "Sap_Dens_Exotic" ~ 12, 
      resp == "Sap_Dens_NatCan" ~ 10, 
      resp == "Sap_Dens_NatOth" ~ 11, 
      resp == "Sap_Dens_Total" ~ 9, 
      resp == "Seed_Dens_Exotic" ~ 20, 
      resp == "Seed_Dens_NatCan" ~ 18,
      resp == "Seed_Dens_NatOth" ~ 19,
      resp == "Seed_Dens_Total" ~ 17,
      resp == "Sor_sap" ~ 22, 
      resp == "Sor_seed" ~ 23, 
      resp == "stock_final" ~ 21, 
      resp == "Tree_BA_Exotic" ~ 8,
      resp == "Tree_BA_NatCan" ~ 6, 
      resp == "Tree_BA_NatOth" ~ 7, 
      resp == "Tree_BA_Total" ~ 5, 
      resp == "Tree_Dens_Exotic" ~ 4, 
      resp == "Tree_Dens_NatCan" ~ 2, 
      resp == "Tree_Dens_NatOth" ~ 3, 
      resp == "Tree_Dens_Total" ~ 1) 
  ) %>% select(Network, park, response_full, term, estimate, lower95, upper95, num_boots, lat_rank, resp, sort_order) %>% 
  arrange(park, sort_order)

write.csv(results, "./results/supptables/Model_output.csv", row.names = F)

#----- Predictors in regression analysis -----
# Categorical variables first
reg_df <- read.csv("./data/RFI_data_for_reg.csv") %>% 
  select(Plot_Name, Park, hummod300m, DBI, Province = PROVINCE, Physiographic_Class, 
         cancov, tmax, precip, stage_fac, QMD, mDBH, Tree_Dens_Total,
         avg.cover)

summary(reg_df$cancov)

dbi <- reg_df %>% group_by(Park, DBI) %>% summarize(num_plots = n()) %>% 
  pivot_wider(names_from = DBI, values_from = num_plots, names_prefix = "DBI_", values_fill = 0)

write.csv(dbi, "./results/supptables/Regress_DBI.csv", row.names = F)

strstg <- reg_df %>% group_by(Park, stage_fac) %>% summarize(num_plots = n()) %>% 
  pivot_wider(names_from = stage_fac, values_from = num_plots, values_fill = 0)

write.csv(strstg, "./results/supptables/Regress_Structural_Stage.csv", row.names = F)

physio <- reg_df %>% group_by(Park, Physiographic_Class) %>% summarize(num_plots = n()) %>% 
  pivot_wider(names_from = Physiographic_Class, values_from = num_plots, values_fill = 0)

write.csv(physio, "./results/supptables/Regress_Physiographic_Class.csv", row.names = F)

prov <- reg_df %>% group_by(Park, Province) %>% summarize(num_plots = n()) %>% 
  pivot_wider(names_from = Province, values_from = num_plots, values_fill = 0)

write.csv(prov, "./results/supptables/Regress_Ecological_Provinces_by_park.csv", row.names = F)

# Predictor variables with min/max
reg_long <- reg_df %>% select(-DBI, -Physiographic_Class, -stage_fac, -Province, -mDBH) %>% 
  pivot_longer(cols = c(-Plot_Name, -Park), names_to = 'metric', values_to = 'value')

reg_sum <- reg_long %>% group_by(Park, metric) %>% 
  summarize(mean = mean(value, na.rm = T),
            min = min(value, na.rm = T),
            max = max(value, na.rm = T))

head(reg_sum)
write.csv(reg_sum, "./results/supptables/Regress_Predictors_by_park.csv", row.names = F)
