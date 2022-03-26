#--------------------------------------------
# Compiling Species Specific Tree, Sapling and Seedling Data for Eastern Forest Regen Project
#   Code written by Kate Miller and Stephanie Perles 20211028
#--------------------------------------------

#---- Dependencies and import data using NPSForVeg ----
#devtools::install_github("NCRN/NPSForVeg")
library(NPSForVeg)
library(tidyverse)
library(rlang)
library(vegan)

options(scipen = 100)

#datapath <- "D:/NETN/R_Dev/NPSForVeg_Data/" #Local dir for Kate
datapath <- "Data/" #Relative path


ncrn <- importNCRN(paste0(datapath,"NCRN_data"))
ermn <- importERMN(paste0(datapath, "ERMN_data"))                   
midn <- importMIDN(paste0(datapath, "MIDN_data"))
netn <- importNETN(paste0(datapath, "NETN_data"))

plot_visit_lj <- read.csv(paste0(datapath, "EFWG_plot_visit_left_join.csv")) %>% select(-DBI) %>% unique()

spp_list <- read.csv(paste0(datapath, "NPS_tree_species_groups.csv")) # Demoted Fraxinus to not canopy tree

# Functions to streamline compiling

sum_by_cycle_spp <- function(network, network_name, group, year_span, value, units, spp_list, spp_name, value_name, cycle_name){

  df <- SiteXSpec(network, group = group, years = year_span, values = value, plot.type = 'active',
                  area = units, status = 'alive') %>% 
        pivot_longer(-Plot_Name, names_to = "Species", values_to = value_name) %>%
        mutate(cycle = cycle_name, Network = network_name) %>% filter(Species %in% spp_list) %>% 
        group_by(Plot_Name, cycle, Network) %>% summarize(Species = spp_name,
                                                          metric = sum(.data[[value_name]], na.rm = T),                                                          
                                                          .groups = 'drop')
  return(data.frame(df))

}

#---- Live tree density by species group in stems/ha ----
#--- Fraxinus

frax_list = c("Fraxinus", "Fraxinus americana", "Fraxinus nigra", "Fraxinus pennsylvanica", 
                "Fraxinus profunda", "Fraxinus sp.", "Fraxinus spp.")

yr_start <- c(2008, 2012, 2016)
cname <- c(1, 2, 3)

frax_tree_dens <- rbind(map2_dfr(yr_start, cname, function(yr_start, cname){
  sum_by_cycle_spp(network = ermn, network_name = "ERMN",
                   year_span = yr_start:(yr_start+3), cycle_name = cname,
                   group = 'trees', value = 'count', value_name = 'Dens', units = 'ha',
                   spp_list = frax_list, spp_name = "Fraxinus spp.")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = midn, network_name = "MIDN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'trees', value = 'count', value_name = 'Dens', units = 'ha',
                     spp_list = frax_list, spp_name = "Fraxinus spp.")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = ncrn, network_name = "NCRN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'trees', value = 'count', value_name = 'Dens', units = 'ha',
                     spp_list = frax_list, spp_name = "Fraxinus spp.")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = netn, network_name = "NETN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'trees', value = 'count', value_name = 'Dens', units = 'ha',
                     spp_list = frax_list, spp_name = "Fraxinus spp.")})
) %>% select(-Species) %>% rename(Tree_Dens_FRAX = metric)

#--- FAGGRA
faggra_tree_dens <- rbind(map2_dfr(yr_start, cname, function(yr_start, cname){
  sum_by_cycle_spp(network = ermn, network_name = "ERMN",
                   year_span = yr_start:(yr_start+3), cycle_name = cname,
                   group = 'trees', value = 'count', value_name = 'Dens', units = 'ha',
                   spp_list = "Fagus grandifolia", spp_name = "Fagus grandifolia")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = midn, network_name = "MIDN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'trees', value = 'count', value_name = 'Dens', units = 'ha',
                     spp_list = "Fagus grandifolia", spp_name = "Fagus grandifolia")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = ncrn, network_name = "NCRN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'trees', value = 'count', value_name = 'Dens', units = 'ha',
                     spp_list = "Fagus grandifolia", spp_name = "Fagus grandifolia")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = netn, network_name = "NETN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'trees', value = 'count', value_name = 'Dens', units = 'ha',
                     spp_list = "Fagus grandifolia", spp_name = "Fagus grandifolia")})
) %>% select(-Species) %>% rename(Tree_Dens_FAGGRA = metric)


#--- TSUCAN
tsucan_tree_dens <- rbind(map2_dfr(yr_start, cname, function(yr_start, cname){
  sum_by_cycle_spp(network = ermn, network_name = "ERMN",
                   year_span = yr_start:(yr_start+3), cycle_name = cname,
                   group = 'trees', value = 'count', value_name = 'Dens', units = 'ha',
                   spp_list = "Tsuga canadensis", spp_name = "Tsuga canadensis")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = midn, network_name = "MIDN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'trees', value = 'count', value_name = 'Dens', units = 'ha',
                     spp_list = "Tsuga canadensis", spp_name = "Tsuga canadensis")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = ncrn, network_name = "NCRN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'trees', value = 'count', value_name = 'Dens', units = 'ha',
                     spp_list = "Tsuga canadensis", spp_name = "Tsuga canadensis")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = netn, network_name = "NETN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'trees', value = 'count', value_name = 'Dens', units = 'ha',
                     spp_list = "Tsuga canadensis", spp_name = "Tsuga canadensis")})
) %>% select(-Species) %>% rename(Tree_Dens_TSUCAN = metric)

#--- ASITRI
asitri_tree_dens <- rbind(map2_dfr(yr_start, cname, function(yr_start, cname){
  sum_by_cycle_spp(network = ermn, network_name = "ERMN",
                   year_span = yr_start:(yr_start+3), cycle_name = cname,
                   group = 'trees', value = 'count', value_name = 'Dens', units = 'ha',
                   spp_list = "Asimina triloba", spp_name = "Asimina triloba")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = midn, network_name = "MIDN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'trees', value = 'count', value_name = 'Dens', units = 'ha',
                     spp_list = "Asimina triloba", spp_name = "Asimina triloba")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = ncrn, network_name = "NCRN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'trees', value = 'count', value_name = 'Dens', units = 'ha',
                     spp_list = "Asimina triloba", spp_name = "Asimina triloba")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = netn, network_name = "NETN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'trees', value = 'count', value_name = 'Dens', units = 'ha',
                     spp_list = "Asimina triloba", spp_name = "Asimina triloba")})
) %>% select(-Species) %>% rename(Tree_Dens_ASITRI = metric)


# Combine tree density data

tree_dens_final <- list(plot_visit_lj, frax_tree_dens, faggra_tree_dens, tsucan_tree_dens, asitri_tree_dens) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

met_cols <- c("Tree_Dens_FRAX", "Tree_Dens_FAGGRA", "Tree_Dens_TSUCAN", "Tree_Dens_ASITRI")
tree_dens_final[,met_cols][is.na(tree_dens_final[,met_cols])]<-0

head(tree_dens_final)

# Checking work
length(unique(tree_dens_final$Plot_Name)) #1515
nrow(tree_dens_final) #4465
nrow(faggra_tree_dens) #4464
table(complete.cases(tree_dens_final)) #all T
#tree_dens_final[which(!complete.cases(tree_dens_final)), c("Plot_Name", "Year", "Tree_Dens_FRAX")]
#---- Live tree BA by species group in stems/ha ----

frax_list = c("Fraxinus", "Fraxinus americana", "Fraxinus nigra", "Fraxinus pennsylvanica", 
              "Fraxinus profunda", "Fraxinus sp.", "Fraxinus spp.")

yr_start <- c(2008, 2012, 2016)
cname <- c(1, 2, 3)

frax_tree_ba <- rbind(map2_dfr(yr_start, cname, function(yr_start, cname){
  sum_by_cycle_spp(network = ermn, network_name = "ERMN",
                   year_span = yr_start:(yr_start+3), cycle_name = cname,
                   group = 'trees', value = 'size', value_name = 'BA', units = 'ha',
                   spp_list = frax_list, spp_name = "Fraxinus spp.")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = midn, network_name = "MIDN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'trees', value = 'size', value_name = 'BA', units = 'ha',
                     spp_list = frax_list, spp_name = "Fraxinus spp.")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = ncrn, network_name = "NCRN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'trees', value = 'size', value_name = 'BA', units = 'ha',
                     spp_list = frax_list, spp_name = "Fraxinus spp.")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = netn, network_name = "NETN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'trees', value = 'size', value_name = 'BA', units = 'ha',
                     spp_list = frax_list, spp_name = "Fraxinus spp.")})
) %>% select(-Species) %>% rename(Tree_BA_FRAX = metric)

#--- FAGGRA
faggra_tree_ba <- rbind(map2_dfr(yr_start, cname, function(yr_start, cname){
  sum_by_cycle_spp(network = ermn, network_name = "ERMN",
                   year_span = yr_start:(yr_start+3), cycle_name = cname,
                   group = 'trees', value = 'size', value_name = 'BA', units = 'ha',
                   spp_list = "Fagus grandifolia", spp_name = "Fagus grandifolia")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = midn, network_name = "MIDN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'trees', value = 'size', value_name = 'BA', units = 'ha',
                     spp_list = "Fagus grandifolia", spp_name = "Fagus grandifolia")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = ncrn, network_name = "NCRN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'trees', value = 'size', value_name = 'BA', units = 'ha',
                     spp_list = "Fagus grandifolia", spp_name = "Fagus grandifolia")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = netn, network_name = "NETN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'trees', value = 'size', value_name = 'BA', units = 'ha',
                     spp_list = "Fagus grandifolia", spp_name = "Fagus grandifolia")})
) %>% select(-Species) %>% rename(Tree_BA_FAGGRA = metric)


#--- TSUCAN
tsucan_tree_ba <- rbind(map2_dfr(yr_start, cname, function(yr_start, cname){
  sum_by_cycle_spp(network = ermn, network_name = "ERMN",
                   year_span = yr_start:(yr_start+3), cycle_name = cname,
                   group = 'trees', value = 'size', value_name = 'BA', units = 'ha',
                   spp_list = "Tsuga canabais", spp_name = "Tsuga canabais")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = midn, network_name = "MIDN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'trees', value = 'size', value_name = 'BA', units = 'ha',
                     spp_list = "Tsuga canabais", spp_name = "Tsuga canabais")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = ncrn, network_name = "NCRN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'trees', value = 'size', value_name = 'BA', units = 'ha',
                     spp_list = "Tsuga canabais", spp_name = "Tsuga canabais")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = netn, network_name = "NETN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'trees', value = 'size', value_name = 'BA', units = 'ha',
                     spp_list = "Tsuga canabais", spp_name = "Tsuga canabais")})
) %>% select(-Species) %>% rename(Tree_BA_TSUCAN = metric)

#--- ASITRI
asitri_tree_ba <- rbind(map2_dfr(yr_start, cname, function(yr_start, cname){
  sum_by_cycle_spp(network = ermn, network_name = "ERMN",
                   year_span = yr_start:(yr_start+3), cycle_name = cname,
                   group = 'trees', value = 'size', value_name = 'BA', units = 'ha',
                   spp_list = "Asimina triloba", spp_name = "Asimina triloba")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = midn, network_name = "MIDN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'trees', value = 'size', value_name = 'BA', units = 'ha',
                     spp_list = "Asimina triloba", spp_name = "Asimina triloba")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = ncrn, network_name = "NCRN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'trees', value = 'size', value_name = 'BA', units = 'ha',
                     spp_list = "Asimina triloba", spp_name = "Asimina triloba")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = netn, network_name = "NETN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'trees', value = 'size', value_name = 'BA', units = 'ha',
                     spp_list = "Asimina triloba", spp_name = "Asimina triloba")})
) %>% select(-Species) %>% rename(Tree_BA_ASITRI = metric)


# Combine tree ba data
tree_ba_final <- list(plot_visit_lj, frax_tree_ba, faggra_tree_ba, tsucan_tree_ba, asitri_tree_ba) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

met_cols <- c("Tree_BA_FRAX", "Tree_BA_FAGGRA", "Tree_BA_TSUCAN", "Tree_BA_ASITRI")
tree_ba_final[,met_cols][is.na(tree_ba_final[,met_cols])]<-0

# Checking work
length(unique(plot_visit_lj$Plot_Name)) #1515 
length(unique(tree_ba_final$Plot_Name)) #1515
nrow(plot_visit_lj) #4464
nrow(tree_ba_final)#4464
table(complete.cases(tree_ba_final)) #0F
# OLD: 6F: b/c plots are missing regen data, which is correct.

#---- Live sap density by species group in stems/ha ----
#--- Fraxinus

frax_list = c("Fraxinus", "Fraxinus americana", "Fraxinus nigra", "Fraxinus pennsylvanica", 
              "Fraxinus profunda", "Fraxinus sp.", "Fraxinus spp.")

yr_start <- c(2008, 2012, 2016)
cname <- c(1, 2, 3)

frax_sap_dens <- rbind(map2_dfr(yr_start, cname, function(yr_start, cname){
  sum_by_cycle_spp(network = ermn, network_name = "ERMN",
                   year_span = yr_start:(yr_start+3), cycle_name = cname,
                   group = 'saplings', value = 'count', value_name = 'Dens', units = 'plot',
                   spp_list = frax_list, spp_name = "Fraxinus spp.")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = midn, network_name = "MIDN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'saplings', value = 'count', value_name = 'Dens', units = 'plot',
                     spp_list = frax_list, spp_name = "Fraxinus spp.")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = ncrn, network_name = "NCRN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'saplings', value = 'count', value_name = 'Dens', units = 'plot',
                     spp_list = frax_list, spp_name = "Fraxinus spp.")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = netn, network_name = "NETN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'saplings', value = 'count', value_name = 'Dens', units = 'plot',
                     spp_list = frax_list, spp_name = "Fraxinus spp.")})
) %>% select(-Species) %>% rename(sap_dens_FRAX = metric)

#--- FAGGRA
faggra_sap_dens <- rbind(map2_dfr(yr_start, cname, function(yr_start, cname){
  sum_by_cycle_spp(network = ermn, network_name = "ERMN",
                   year_span = yr_start:(yr_start+3), cycle_name = cname,
                   group = 'saplings', value = 'count', value_name = 'Dens', units = 'plot',
                   spp_list = "Fagus grandifolia", spp_name = "Fagus grandifolia")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = midn, network_name = "MIDN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'saplings', value = 'count', value_name = 'Dens', units = 'plot',
                     spp_list = "Fagus grandifolia", spp_name = "Fagus grandifolia")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = ncrn, network_name = "NCRN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'saplings', value = 'count', value_name = 'Dens', units = 'plot',
                     spp_list = "Fagus grandifolia", spp_name = "Fagus grandifolia")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = netn, network_name = "NETN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'saplings', value = 'count', value_name = 'Dens', units = 'plot',
                     spp_list = "Fagus grandifolia", spp_name = "Fagus grandifolia")})
) %>% select(-Species) %>% rename(sap_dens_FAGGRA = metric)


#--- TSUCAN
tsucan_sap_dens <- rbind(map2_dfr(yr_start, cname, function(yr_start, cname){
  sum_by_cycle_spp(network = ermn, network_name = "ERMN",
                   year_span = yr_start:(yr_start+3), cycle_name = cname,
                   group = 'saplings', value = 'count', value_name = 'Dens', units = 'plot',
                   spp_list = "Tsuga canadensis", spp_name = "Tsuga canadensis")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = midn, network_name = "MIDN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'saplings', value = 'count', value_name = 'Dens', units = 'plot',
                     spp_list = "Tsuga canadensis", spp_name = "Tsuga canadensis")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = ncrn, network_name = "NCRN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'saplings', value = 'count', value_name = 'Dens', units = 'plot',
                     spp_list = "Tsuga canadensis", spp_name = "Tsuga canadensis")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = netn, network_name = "NETN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'saplings', value = 'count', value_name = 'Dens', units = 'plot',
                     spp_list = "Tsuga canadensis", spp_name = "Tsuga canadensis")})
) %>% select(-Species) %>% rename(sap_dens_TSUCAN = metric)

#--- ASITRI
asitri_sap_dens <- rbind(map2_dfr(yr_start, cname, function(yr_start, cname){
  sum_by_cycle_spp(network = ermn, network_name = "ERMN",
                   year_span = yr_start:(yr_start+3), cycle_name = cname,
                   group = 'saplings', value = 'count', value_name = 'Dens', units = 'plot',
                   spp_list = "Asimina triloba", spp_name = "Asimina triloba")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = midn, network_name = "MIDN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'saplings', value = 'count', value_name = 'Dens', units = 'plot',
                     spp_list = "Asimina triloba", spp_name = "Asimina triloba")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = ncrn, network_name = "NCRN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'saplings', value = 'count', value_name = 'Dens', units = 'plot',
                     spp_list = "Asimina triloba", spp_name = "Asimina triloba")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = netn, network_name = "NETN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'saplings', value = 'count', value_name = 'Dens', units = 'plot',
                     spp_list = "Asimina triloba", spp_name = "Asimina triloba")})
) %>% select(-Species) %>% rename(sap_dens_ASITRI = metric)


# Combine sap density data
sap_dens_final <- list(plot_visit_lj, frax_sap_dens, faggra_sap_dens, tsucan_sap_dens, asitri_sap_dens) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

names(sap_dens_final)

met_cols <- c("sap_dens_FRAX", "sap_dens_FAGGRA", "sap_dens_TSUCAN", "sap_dens_ASITRI")
sap_dens_final[,met_cols][is.na(sap_dens_final[,met_cols])]<-0

sap_dens_final <- sap_dens_final %>% mutate(Sap_Dens_FRAX = sap_dens_FRAX/(SapPlotNum * SapPlotSize),
                                            Sap_Dens_FAGGRA = sap_dens_FAGGRA/(SapPlotNum * SapPlotSize),
                                            Sap_Dens_TSUCAN = sap_dens_TSUCAN/(SapPlotNum * SapPlotSize),
                                            Sap_Dens_ASITRI = sap_dens_ASITRI/(SapPlotNum * SapPlotSize)) %>% 
  select(Plot_Name:cycle, Sap_Dens_FRAX, Sap_Dens_FAGGRA, Sap_Dens_TSUCAN, Sap_Dens_ASITRI)


# Checking work
length(unique(sap_dens_final$Plot_Name)) #1515
nrow(sap_dens_final) #4464
table(complete.cases(sap_dens_final)) # 6F: b/c plots are missing regen data, which is correct.

#---- Live sap BA by species group in stems/ha ----
frax_list = c("Fraxinus", "Fraxinus americana", "Fraxinus nigra", "Fraxinus pennsylvanica", 
              "Fraxinus profunda", "Fraxinus sp.", "Fraxinus spp.")

yr_start <- c(2008, 2012, 2016)
cname <- c(1, 2, 3)

frax_sap_ba <- rbind(map2_dfr(yr_start, cname, function(yr_start, cname){
  sum_by_cycle_spp(network = ermn, network_name = "ERMN",
                   year_span = yr_start:(yr_start+3), cycle_name = cname,
                   group = 'saplings', value = 'size', value_name = 'BA', units = 'plot',
                   spp_list = frax_list, spp_name = "Fraxinus spp.")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = midn, network_name = "MIDN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'saplings', value = 'size', value_name = 'BA', units = 'plot',
                     spp_list = frax_list, spp_name = "Fraxinus spp.")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = ncrn, network_name = "NCRN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'saplings', value = 'size', value_name = 'BA', units = 'plot',
                     spp_list = frax_list, spp_name = "Fraxinus spp.")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = netn, network_name = "NETN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'saplings', value = 'size', value_name = 'BA', units = 'plot',
                     spp_list = frax_list, spp_name = "Fraxinus spp.")})
) %>% select(-Species) %>% rename(sap_ba_FRAX = metric)

#--- FAGGRA
faggra_sap_ba <- rbind(map2_dfr(yr_start, cname, function(yr_start, cname){
  sum_by_cycle_spp(network = ermn, network_name = "ERMN",
                   year_span = yr_start:(yr_start+3), cycle_name = cname,
                   group = 'saplings', value = 'size', value_name = 'BA', units = 'plot',
                   spp_list = "Fagus grandifolia", spp_name = "Fagus grandifolia")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = midn, network_name = "MIDN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'saplings', value = 'size', value_name = 'BA', units = 'plot',
                     spp_list = "Fagus grandifolia", spp_name = "Fagus grandifolia")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = ncrn, network_name = "NCRN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'saplings', value = 'size', value_name = 'BA', units = 'plot',
                     spp_list = "Fagus grandifolia", spp_name = "Fagus grandifolia")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = netn, network_name = "NETN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'saplings', value = 'size', value_name = 'BA', units = 'plot',
                     spp_list = "Fagus grandifolia", spp_name = "Fagus grandifolia")})
) %>% select(-Species) %>% rename(sap_ba_FAGGRA = metric)


#--- TSUCAN
tsucan_sap_ba <- rbind(map2_dfr(yr_start, cname, function(yr_start, cname){
  sum_by_cycle_spp(network = ermn, network_name = "ERMN",
                   year_span = yr_start:(yr_start+3), cycle_name = cname,
                   group = 'saplings', value = 'size', value_name = 'BA', units = 'plot',
                   spp_list = "Tsuga canabais", spp_name = "Tsuga canabais")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = midn, network_name = "MIDN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'saplings', value = 'size', value_name = 'BA', units = 'plot',
                     spp_list = "Tsuga canabais", spp_name = "Tsuga canabais")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = ncrn, network_name = "NCRN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'saplings', value = 'size', value_name = 'BA', units = 'plot',
                     spp_list = "Tsuga canabais", spp_name = "Tsuga canabais")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = netn, network_name = "NETN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'saplings', value = 'size', value_name = 'BA', units = 'plot',
                     spp_list = "Tsuga canabais", spp_name = "Tsuga canabais")})
) %>% select(-Species) %>% rename(sap_ba_TSUCAN = metric)

#--- ASITRI
asitri_sap_ba <- rbind(map2_dfr(yr_start, cname, function(yr_start, cname){
  sum_by_cycle_spp(network = ermn, network_name = "ERMN",
                   year_span = yr_start:(yr_start+3), cycle_name = cname,
                   group = 'saplings', value = 'size', value_name = 'BA', units = 'plot',
                   spp_list = "Asimina triloba", spp_name = "Asimina triloba")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = midn, network_name = "MIDN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'saplings', value = 'size', value_name = 'BA', units = 'plot',
                     spp_list = "Asimina triloba", spp_name = "Asimina triloba")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = ncrn, network_name = "NCRN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'saplings', value = 'size', value_name = 'BA', units = 'plot',
                     spp_list = "Asimina triloba", spp_name = "Asimina triloba")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = netn, network_name = "NETN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'saplings', value = 'size', value_name = 'BA', units = 'plot',
                     spp_list = "Asimina triloba", spp_name = "Asimina triloba")})
) %>% select(-Species) %>% rename(sap_ba_ASITRI = metric)


# Combine sap ba data
sap_ba_final <- list(plot_visit_lj, frax_sap_ba, faggra_sap_ba, tsucan_sap_ba, asitri_sap_ba) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

met_cols <- c("sap_ba_FRAX", "sap_ba_FAGGRA", "sap_ba_TSUCAN", "sap_ba_ASITRI")
sap_ba_final[,met_cols][is.na(sap_ba_final[,met_cols])]<-0

#---- Combine sapling BA
sap_ba_final <- sap_ba_final %>% mutate(Sap_BA_FRAX = (10000*sap_ba_FRAX)/(SapPlotNum * SapPlotSize),
                                       Sap_BA_FAGGRA = (10000*sap_ba_FAGGRA)/(SapPlotNum * SapPlotSize),
                                       Sap_BA_TSUCAN = (10000*sap_ba_TSUCAN)/(SapPlotNum * SapPlotSize),
                                       Sap_BA_ASITRI = (10000*sap_ba_ASITRI)/(SapPlotNum * SapPlotSize)) %>% 
  select(Plot_Name:cycle, Sap_BA_FRAX, Sap_BA_FAGGRA, Sap_BA_TSUCAN, Sap_BA_ASITRI)

# Checking work
length(unique(plot_visit_lj$Plot_Name)) #1515 
length(unique(sap_ba_final$Plot_Name)) #1515
nrow(plot_visit_lj) #4465
nrow(sap_ba_final)#4465
table(complete.cases(sap_ba_final)) # 6F: b/c plots are missing regen data, which is correct.

#----- Seedling density m2/ha ----
#--- Fraxinus

frax_list = c("Fraxinus", "Fraxinus americana", "Fraxinus nigra", "Fraxinus pennsylvanica", 
              "Fraxinus profunda", "Fraxinus sp.", "Fraxinus spp.")

yr_start <- c(2008, 2012, 2016)
cname <- c(1, 2, 3)

frax_seed_dens <- rbind(map2_dfr(yr_start, cname, function(yr_start, cname){
  sum_by_cycle_spp(network = ermn, network_name = "ERMN",
                   year_span = yr_start:(yr_start+3), cycle_name = cname,
                   group = 'seedlings', value = 'count', value_name = 'Dens', units = 'plot',
                   spp_list = frax_list, spp_name = "Fraxinus spp.")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = midn, network_name = "MIDN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'seedlings', value = 'count', value_name = 'Dens', units = 'plot',
                     spp_list = frax_list, spp_name = "Fraxinus spp.")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = ncrn, network_name = "NCRN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'seedlings', value = 'count', value_name = 'Dens', units = 'plot',
                     spp_list = frax_list, spp_name = "Fraxinus spp.")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = netn, network_name = "NETN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'seedlings', value = 'count', value_name = 'Dens', units = 'plot',
                     spp_list = frax_list, spp_name = "Fraxinus spp.")})
) %>% select(-Species) %>% rename(seed_dens_FRAX = metric)

#--- FAGGRA
faggra_seed_dens <- rbind(map2_dfr(yr_start, cname, function(yr_start, cname){
  sum_by_cycle_spp(network = ermn, network_name = "ERMN",
                   year_span = yr_start:(yr_start+3), cycle_name = cname,
                   group = 'seedlings', value = 'count', value_name = 'Dens', units = 'plot',
                   spp_list = "Fagus grandifolia", spp_name = "Fagus grandifolia")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = midn, network_name = "MIDN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'seedlings', value = 'count', value_name = 'Dens', units = 'plot',
                     spp_list = "Fagus grandifolia", spp_name = "Fagus grandifolia")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = ncrn, network_name = "NCRN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'seedlings', value = 'count', value_name = 'Dens', units = 'plot',
                     spp_list = "Fagus grandifolia", spp_name = "Fagus grandifolia")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = netn, network_name = "NETN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'seedlings', value = 'count', value_name = 'Dens', units = 'plot',
                     spp_list = "Fagus grandifolia", spp_name = "Fagus grandifolia")})
) %>% select(-Species) %>% rename(seed_dens_FAGGRA = metric)


#--- TSUCAN
tsucan_seed_dens <- rbind(map2_dfr(yr_start, cname, function(yr_start, cname){
  sum_by_cycle_spp(network = ermn, network_name = "ERMN",
                   year_span = yr_start:(yr_start+3), cycle_name = cname,
                   group = 'seedlings', value = 'count', value_name = 'Dens', units = 'plot',
                   spp_list = "Tsuga canadensis", spp_name = "Tsuga canadensis")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = midn, network_name = "MIDN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'seedlings', value = 'count', value_name = 'Dens', units = 'plot',
                     spp_list = "Tsuga canadensis", spp_name = "Tsuga canadensis")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = ncrn, network_name = "NCRN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'seedlings', value = 'count', value_name = 'Dens', units = 'plot',
                     spp_list = "Tsuga canadensis", spp_name = "Tsuga canadensis")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = netn, network_name = "NETN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'seedlings', value = 'count', value_name = 'Dens', units = 'plot',
                     spp_list = "Tsuga canadensis", spp_name = "Tsuga canadensis")})
) %>% select(-Species) %>% rename(seed_dens_TSUCAN = metric)

#--- ASITRI
asitri_seed_dens <- rbind(map2_dfr(yr_start, cname, function(yr_start, cname){
  sum_by_cycle_spp(network = ermn, network_name = "ERMN",
                   year_span = yr_start:(yr_start+3), cycle_name = cname,
                   group = 'seedlings', value = 'count', value_name = 'Dens', units = 'plot',
                   spp_list = "Asimina triloba", spp_name = "Asimina triloba")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = midn, network_name = "MIDN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'seedlings', value = 'count', value_name = 'Dens', units = 'plot',
                     spp_list = "Asimina triloba", spp_name = "Asimina triloba")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = ncrn, network_name = "NCRN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'seedlings', value = 'count', value_name = 'Dens', units = 'plot',
                     spp_list = "Asimina triloba", spp_name = "Asimina triloba")}),
  
  map2_dfr(yr_start, cname, function(yr_start, cname){
    sum_by_cycle_spp(network = netn, network_name = "NETN",
                     year_span = yr_start:(yr_start+3), cycle_name = cname,
                     group = 'seedlings', value = 'count', value_name = 'Dens', units = 'plot',
                     spp_list = "Asimina triloba", spp_name = "Asimina triloba")})
) %>% select(-Species) %>% rename(seed_dens_ASITRI = metric)


# Combine seed density data
seed_dens_final <- list(plot_visit_lj, frax_seed_dens, faggra_seed_dens, tsucan_seed_dens, asitri_seed_dens) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

met_cols <- c("seed_dens_FRAX", "seed_dens_FAGGRA", "seed_dens_TSUCAN", "seed_dens_ASITRI")
seed_dens_final[,met_cols][is.na(seed_dens_final[,met_cols])]<-0

head(seed_dens_final)

seed_dens_final <- seed_dens_final %>% mutate(Seed_Dens_FRAX = seed_dens_FRAX/(SeedPlotNum * SeedPlotSize),
                                              Seed_Dens_FAGGRA = seed_dens_FAGGRA/(SeedPlotNum * SeedPlotSize),
                                              Seed_Dens_TSUCAN = seed_dens_TSUCAN/(SeedPlotNum * SeedPlotSize),
                                              Seed_Dens_ASITRI = seed_dens_ASITRI/(SeedPlotNum * SeedPlotSize)) %>% 
  select(Plot_Name:cycle, Seed_Dens_FRAX, Seed_Dens_FAGGRA, Seed_Dens_TSUCAN, Seed_Dens_ASITRI)

# Checking work
length(unique(seed_dens_final$Plot_Name)) #1515
nrow(seed_dens_final) #4464
table(complete.cases(seed_dens_final)) # 11F: b/c plots are missing regen data, which is correct.

#----- Combine all data frames -----
names(tree_ba_final)
names(plot_visit_lj)

tree_sap_seed_spp <- list(plot_visit_lj %>% select(Plot_Name, Network, Unit_Code, Year, cycle, excludeEvent),
                          tree_ba_final %>% select(Network, Plot_Name, cycle, Tree_BA_FRAX:Tree_BA_ASITRI), 
                          tree_dens_final %>% select(Network, Plot_Name, cycle, Tree_Dens_FRAX:Tree_Dens_ASITRI),
                          sap_ba_final %>% select(Network, Plot_Name, cycle, Sap_BA_FRAX:Sap_BA_ASITRI),
                          sap_dens_final %>% select(Network, Plot_Name, cycle, Sap_Dens_FRAX:Sap_Dens_ASITRI), 
                          seed_dens_final %>% select(Network, Plot_Name, cycle, Seed_Dens_FRAX:Seed_Dens_ASITRI)) %>% 
  reduce(left_join, by = c("Network", "Plot_Name", "cycle"))

nrow(tree_sap_seed_spp)
nrow(plot_visit_lj)

names(tree_sap_seed_spp)

data_cols <- c("Tree_BA_FRAX", "Tree_BA_FAGGRA", "Tree_BA_TSUCAN", "Tree_BA_ASITRI", "Tree_Dens_FRAX", "Tree_Dens_FAGGRA",
               "Tree_Dens_TSUCAN", "Tree_Dens_ASITRI", "Sap_BA_FRAX", "Sap_BA_FAGGRA", "Sap_BA_TSUCAN", "Sap_BA_ASITRI",
               "Sap_Dens_FRAX", "Sap_Dens_FAGGRA", "Sap_Dens_TSUCAN", "Sap_Dens_ASITRI", "Seed_Dens_FRAX", "Seed_Dens_FAGGRA",
               "Seed_Dens_TSUCAN", "Seed_Dens_ASITRI")

tree_sap_seed_spp[,data_cols][tree_sap_seed_spp$excludeEvent == 1,] <- NA

lat_order <- read.csv(paste0(datapath, "EFWG_lat_order.csv")) %>% select(-mean.lat) %>% 
  rename(lat_rank = lat.rank)

reg_final <- left_join(tree_sap_seed_spp, lat_order, by = c("Network" = "network", "Unit_Code" = "park")) %>% 
  select(Plot_Name:cycle, lat_rank, everything()) %>% 
  mutate(Network = ifelse(Unit_Code %in% c("COLO", "GEWA", "SAHI", "THST"), "NCBN", Network))

nrow(reg_final) #4464

plot_check <- data.frame(table(reg_final$Plot_Name)) %>% filter(Freq!=3)
plot_check

write.csv(reg_final, paste0(datapath, "EFWG_species_dataset_20220325.csv"), row.names = FALSE)

#------- Composition Proportion Data Compile --------
#reg_final <- read.csv(paste0("./data/EFWG_species_dataset_20220325.csv"))
tss <- read.csv("./data/EFWG_full_dataset_20220325.csv") %>% 
  select(Plot_Name, Network, Unit_Code, Year, cycle, Sap_BA_Total, Sap_Dens_Total, Seed_Dens_Total)
head(tss)
head(reg_final)
comp_data <- reg_final %>% select(Plot_Name, Network, Unit_Code, Year, cycle, lat_rank, excludeEvent,
                                  Sap_BA_FRAX, Sap_BA_FAGGRA, Sap_BA_TSUCAN, Sap_BA_ASITRI, 
                                  Sap_Dens_FRAX, Sap_Dens_FAGGRA, Sap_Dens_TSUCAN, Sap_Dens_ASITRI, 
                                  Seed_Dens_FRAX, Seed_Dens_FAGGRA, Seed_Dens_TSUCAN, Seed_Dens_ASITRI) %>% 
                           left_join(tss, ., by = c("Plot_Name", "Network", "Unit_Code", "Year",
                                                    "cycle"))

comp_data_c3 <- comp_data %>% filter(cycle == 3 & excludeEvent == 0) 
table(complete.cases(comp_data_c3)) #4 F
comp_data_c3[!complete.cases(comp_data_c3),]

# DEWA-159-2017 NA Seedling and Sapling Data (Converted to 0)
# CATO-0331-2018 NA Seedling Data (Converted to 0)
# CHOH-0015-2017 NA Seedling Data (Converted to 0)
# NACE-0493-2017 NA Seedling Data (Converted to 0)
names(comp_data_c3)

comp_data_c3[, 5:22][is.na(comp_data_c3[,5:22])] <- 0

# This approach adds to 1- weights each stem equally, vs. 2nd process weights each plot equally.
comp_park <- comp_data_c3 %>% group_by(Network, Unit_Code) %>% 
  summarize(sap_ba_tot = sum(Sap_BA_Total),
            sap_dens_tot = sum(Sap_Dens_Total),
            seed_dens_tot = sum(Seed_Dens_Total),
            sap_ba_pct_FRAX = sum(Sap_BA_FRAX)/sap_ba_tot,
            sap_ba_pct_FAGGRA = sum(Sap_BA_FAGGRA)/sap_ba_tot,
            sap_ba_pct_ASITRI = sum(Sap_BA_ASITRI)/sap_ba_tot,
            sap_dens_pct_FRAX = sum(Sap_Dens_FRAX)/sap_dens_tot,
            sap_dens_pct_FAGGRA = sum(Sap_Dens_FAGGRA)/sap_dens_tot,
            sap_dens_pct_ASITRI = sum(Sap_Dens_ASITRI)/sap_dens_tot,
            seed_dens_pct_FRAX = sum(Seed_Dens_FRAX)/seed_dens_tot,
            seed_dens_pct_FAGGRA = sum(Seed_Dens_FAGGRA)/seed_dens_tot,
            seed_dens_pct_ASITRI = sum(Seed_Dens_ASITRI)/seed_dens_tot)


comp_park <- comp_park %>% mutate(sap_ba_check = sap_ba_pct_FRAX + sap_ba_pct_FAGGRA + sap_ba_pct_ASITRI,
                                  sap_dens_check = sap_dens_pct_FRAX + sap_dens_pct_FAGGRA + sap_dens_pct_ASITRI,
                                  seed_dens_check = seed_dens_pct_FRAX + seed_dens_pct_FAGGRA + seed_dens_pct_ASITRI)


write.csv(comp_park, "./data/EFWG_proportion_regen_species_20220325.csv", row.names = F)


