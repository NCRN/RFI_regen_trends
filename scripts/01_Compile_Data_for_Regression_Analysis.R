#----------------------------
# Regression Analysis
#----------------------------
library(tidyverse)
library(sf) 
#library(prism)
#library(raster)

datapath <- "./data/"

options(scipen = 100, digits = 4)

#----- Read in Analysis Datasets -----
reg_df <- read.csv("./data/EFWG_full_dataset_20220325.csv") %>% 
  select(Plot_Name, Park = Unit_Code, Network, Year, DBI, 
         Tree_BA_Total, Tree_Dens_Total, Sap_Dens_NatCan, Seed_Dens_NatCan,
         Sap_Dens_NatOth, Seed_Dens_NatOth, Sap_Dens_Exotic, Seed_Dens_Exotic,
         stock_final, Sap_Dens_Total, Seed_Dens_Total, Sor_seed, Sor_sap) %>% 
  filter(between(Year, 2016, 2019))

inv_df <- read.csv("./data/EFWG_invasive_cover_2016-2019.csv")# %>% 

inv_df$Plot_Name[duplicated(inv_df$Plot_Name)] # 0

comb <- left_join(reg_df, inv_df, by = c("Plot_Name")) 

#----- Import plot location data -----
# Load Lat/Long coordinates then convert to UTM Albers 5070
load_data <- function(network, file, path = datapath){
  read.csv(paste0(path, network, "_data/", file, ".csv")) %>% mutate(network = network)
}

# WGS84 is 4326
# UTM Albers is 5070
ermn_plots <- load_data('ERMN', "Plots") %>% 
  select(Plot_Name, Park = Unit_Code, Long = Longitude, Lat = Latitude) 

midn_plots <- load_data('MIDN', "Plots") %>% 
  select(Plot_Name, Park = Unit_Code, Long = Longitude, Lat = Latitude) 

ncrn_plots <- load_data('NCRN', "Plots") %>% 
  select(Plot_Name, Park = Unit_Code, Long = Longitude, Lat = Latitude) 

netn_plots <- load_data('NETN', "Plots") %>% 
  select(Plot_Name, Park = Unit_Code, Long = Longitude, Lat = Latitude) 


plots <- rbind(ermn_plots, midn_plots, ncrn_plots, netn_plots) %>% filter(Park != "ASIS")

plots_sf <- st_as_sf(plots, coords = c("Long", "Lat"), crs = 4326)
plots_utm <- st_transform(plots_sf, crs = 5070)
plots_utm$x <- st_coordinates(plots_utm)[,1]
plots_utm$y <- st_coordinates(plots_utm)[,2]
plots_df <- plots_utm %>% select(Plot_Name, Park, x, y) %>% data.frame()

#----- Spatially join plot locations and ecoregions -----
ecoprov1 <- st_read("./data/GIS/Baileys_ecoregions_sgca.shp")
ecoprov <- st_transform(ecoprov1, crs = 5070) %>% select(-AREA, - PERIMETER)

plots_ep1 <- st_join(plots_utm, ecoprov)

table(plots_ep1$Park, plots_ep1$DIVISION, useNA = 'always')
# ACAD, COLO, GEWA and SAHI have NAs, but should be all 1 group.

plots_ep <- plots_ep1 %>% group_by(Park) %>% 
  fill(DOMAIN) %>% 
  fill(DIVISION) %>% 
  fill(PROVINCE) %>% 
  fill(SECTION)

plots_ep$ECOCODE[plots_ep$Park == "GEWA"] <- "-232A"
plots_ep$DOMAIN[plots_ep$Park == "GEWA"] <- "HUMID TEMPERATE DOMAIN"
plots_ep$DIVISION[plots_ep$Park == "GEWA"] <- "Subtropical Division"
plots_ep$PROVINCE[plots_ep$Park == "GEWA"] <- "Outer Coastal Plain Mixed Forest Province"
plots_ep$SECTION[plots_ep$Park == "GEWA"] <- "Middle Atlantic Coastal Plain Section"

plots_ep$ECOCODE[plots_ep$Park == "SAHI"] <- "-221A"
plots_ep$DOMAIN[plots_ep$Park == "SAHI"] <- "HUMID TEMPERATE DOMAIN"
plots_ep$DIVISION[plots_ep$Park == "SAHI"] <- "Hot Continental Division"
plots_ep$PROVINCE[plots_ep$Park == "SAHI"] <- "PROVINCE	Eastern Broadleaf Forest (Oceanic) Province"
plots_ep$SECTION[plots_ep$Park == "SAHI"] <- "Lower New England Section"

veggrp <- read.csv("./data/EFWG_veggroups_20221004.csv")

plots_epveg <- left_join(plots_ep, veggrp, by = "Plot_Name")
table(complete.cases(plots_epveg$Network)) #1509 T 9 F
plots_shp <- plots_epveg %>% filter(!is.na(Network)) %>% 
  select(-Park)

table(complete.cases(plots_epveg$Unit_Code)) #1509 T 9 F

table(complete.cases(plots_shp$Unit_Code)) #1509

#st_write(plots_shp, paste0(datapath, "GIS/plots_utm_5070_final.shp"), append = F, delete_layer = T)

#---- Download/compile PRISM climate data -----
prismfile <- ("./data/GIS/prism")
setwd(prismfile)
options(prism.path = prismfile)

# # Download climate data (commented out, so doesn't run after first download)
# get_prism_normals('ppt', "4km", annual = TRUE, mon = 4:9, keepZip = FALSE) # monthly 30yr avg precip Apr-Sept
# get_prism_normals('tmax', '4km', annual = T, keepZip = FALSE) # 30 year average max temp
# get_prism_annual('tmax', years = 1911:1992, keepZip = FALSE) #annual max temp. Note: prism requires 1982 to be the end year
# get_prism_monthlys('ppt', years = 1911:1992, mon = 4:9, keepZip = F) #monthly historic precip 


# Calculate 30year average precip during Apr-Sept for 1911-1940 to compare with 30year normals for 1991-2020
years <- 1911:1940

import_prec <- function(year, month){
  filename <- paste0("PRISM_ppt_stable_4kmM2_", year, "0", month, "_bil")  
  ras <- raster::raster(paste0(filename, "/", filename, ".bil"))
}

walk(years, function(year){
  m4 <- import_prec(year, 4)
  m5 <- import_prec(year, 5)
  m6 <- import_prec(year, 6)
  m7 <- import_prec(year, 7)
  m8 <- import_prec(year, 8)
  m9 <- import_prec(year, 9)
  
  prec <- m4 + m5 + m6 + m7 + m8 + m9
  cat(paste(year, cellStats(prec, min), cellStats(prec, max)), "\n")
  #assign(paste0("ppt_", year), prec, envir = .GlobalEnv)
  writeRaster(prec, paste0("./derived_ppt/PRISM_ppt_", year, ".tif"), format = 'GTiff', overwrite = T)
  }
  )

# Stack output and average stack to come up with 30-year normal for growing season precip: 1911-1940
sum_files <- list.files('./derived_ppt/', "\\.tif$", full.names = T)
sum_stack <- stack(sum_files)

avg_ppt_init <- calc(sum_stack, mean, na.rm = T)
avg_ppt_init2 <- projectRaster(avg_ppt_init, crs = 5070)
writeRaster(avg_ppt_init2, "avg_growing_season_ppt_1911-40_utm.tif", format = "GTiff", overwrite = T)

# Create same stack for latest normals (1991 - 2010)
import_pnorms <- function(month){
  filename <- paste0("PRISM_ppt_30yr_normal_4kmM3_", "0", month, "_bil")  
  ras <- raster::raster(paste0(filename, "/", filename, ".bil"))
}

m4 <- import_pnorms(4)
m5 <- import_pnorms(5)
m6 <- import_pnorms(6)
m7 <- import_pnorms(7)
m8 <- import_pnorms(8)
m9 <- import_pnorms(9)

prec <- m4 + m5 + m6 + m7 + m8 + m9

sum_ppt_curr <- projectRaster(prec, crs = 5070) 
writeRaster(sum_ppt_curr, "avg_growing_season_ppt_30yr_utm.tif", format = "GTiff", overwrite = T)

# Calculate the percent change between the two 30year periods
pct_chg_precip <- (sum_ppt_curr - avg_ppt_init2)/(sum_ppt_curr)
writeRaster(pct_chg_precip, "Pct_Change_Apr_Sept_Precip_1140_9120.tif", format = "GTiff", overwrite = T)

# Calculate 30year average tmax for 1911-1940 to compare with 30year normals for 1991-2020
years <- 1911:1940

import_tmax <- function(year){
  filename <- paste0("PRISM_tmax_stable_4kmM3_", year, "_bil")  
  ras <- raster::raster(paste0(filename, "/", filename, ".bil"))
}

walk(years, function(year){
  ras = import_tmax(year)
  cat(paste(year, cellStats(ras, min), cellStats(ras, max)), "\n")
  #assign(paste0("ppt_", year), prec, envir = .GlobalEnv)
  writeRaster(ras, paste0("./derived_tmax/PRISM_tmax_", year, ".tif"), format = 'GTiff', overwrite = T)
})

# Stack output and average stack to come up with 30-year normal for tmax: 1911-1940
tmax_files <- list.files('./derived_tmax/', "\\.tif$", full.names = T)
tmax_stack <- stack(tmax_files)
tmax_stack

avg_tmax_init <- calc(tmax_stack, mean, na.rm = T)
avg_tmax_init2 <- projectRaster(avg_tmax_init, crs = 5070)
writeRaster(avg_tmax_init2, "avg_tmax_1911-40_utm.tif", format = "GTiff", overwrite = T)

tmax_curr <- raster("./PRISM_tmax_30yr_normal_4kmM3_annual_bil/PRISM_tmax_30yr_normal_4kmM3_annual_bil.bil")
tmax_curr2 <- projectRaster(tmax_curr, crs = 5070) 

# Calculate the percent change between the two 30year periods
pct_chg_tmax <- (tmax_curr2 - avg_tmax_init2)/(tmax_curr2)
writeRaster(pct_chg_tmax, "Pct_Change_tmax_1140_9120.tif", format = "GTiff", overwrite = T)

cellStats(pct_chg_tmax, min)
cellStats(pct_chg_tmax, max)

cellStats(pct_chg_precip, min)
cellStats(pct_chg_precip, max)

#----- Prepare NLCD Human Modified Land Cover -----
r <- raster('./nlcd_2016_land_cover_l48_20210604/nlcd_2016_land_cover_l48_20210604.img')

# Convert land cover codes to 0, 1 based whether human mod land use
lc.mat <- matrix(c(10, 13, 0, 
                   20, 25, 1,
                   30, 53, 0,
                   70, 83, 1,
                   89, 96, 0))

nlcd_bin <- reclassify(r, lc.mat)
nlcd_bin2 <- na.omit(nlcd_bin)

cellStats(nlcd_bin, max) #max is 1
cellStats(nlcd_bin, min) # min is 0
#writeRaster(nlcd_bin, 'nlcdbin.tif', format = "GTiff")
#nlcd_bin <- raster('./GIS/nlcdbin.tif')

# Aggregate to 300m resolution
nlcd300 <- raster::aggregate(nlcd_bin, fact = 10, fun = mean, na.rm = T)
nlcd300_5070 <- projectRaster(nlcd300, crs = 5070)
writeRaster(nlcd300_5070,'./nlcd300_hmod_2016.tif', format = "GTiff", overwrite = T)

# Extract values to points from rasters
pct_chg_precip <- raster::raster("./data/GIS/prism/Pct_Change_Apr_Sept_Precip_1140_9120.tif")
pct_chg_tmax <- raster::raster("./data/GIS/prism/Pct_Change_tmax_1140_9120.tif")
nlcd300_5070 <- raster::raster("./data/GIS/nlcd300_hmod_2016.tif")
treecan <- raster::raster("./data/GIS/nlcd_2016_treecanopy_2019_08_31/nlcd_2016_treecanopy_2019_08_31.img")


plots_shp_cov <- cbind(plots_shp, 
                       hummod300m = raster::extract(nlcd300_5070, plots_shp), 
                       precip = raster::extract(pct_chg_precip, plots_shp),
                       tmax = raster::extract(pct_chg_tmax, plots_shp),
                       cancov = raster::extract(treecan, plots_shp)
                       )

plots_df_cov <- st_drop_geometry(plots_shp_cov) %>% data.frame() %>% 
  dplyr::select(Plot_Name, Network, Unit_Code, x, y, PROVINCE, 
         Physiographic_Class, hummod300m, precip, tmax, cancov) 

table(complete.cases(plots_df_cov)) #1509 T
summary(plots_df_cov)

write.csv(plots_df_cov, "plots_covs_utm5070_points.csv", row.names = F)


# Other forest metrics
park_status <- read.csv("./results/results_for_fig2.csv") %>% 
  select(park, status = park_reggrp) %>% unique()


dens_df1 <- read.csv(paste0(datapath, "EFWG_full_dataset_20220325.csv")) %>% 
  left_join(., park_status, by = c("Unit_Code" = "park"))
spp_df <- read.csv(paste0(datapath, "EFWG_species_dataset_20220325.csv"))
dens_df <- left_join(dens_df1, spp_df, by = c("Plot_Name", "Unit_Code", "Year", "Network", 
                                              "cycle", "lat_rank", "excludeEvent")) %>% 
           filter(between(Year, 2016, 2019))

dens_df$park_ord <- reorder(dens_df$Unit_Code, desc(dens_df$lat_rank))
dens_df <- dens_df[,c(1:16, 108, 87, 17:86, 88:107)]
head(dens_df)

# Calculate quadratic mean diameter
load_data <- function(network, file, path = datapath){
  read.csv(paste0(path, network, "_data/", file, ".csv")) %>% mutate(network = network)
}

ermn_tr <- load_data('ERMN', "Trees") %>% 
  select(Plot_Name, park = Unit_Code, Sample_Year, Tag = Tree_Number, dbh = Equiv_Live_DBH_cm, 
         ba = SumLiveBasalArea_cm2, crown = Crown_Class_ID) %>% 
  filter(dbh < 999999)
midn_tr <- load_data('MIDN', "Trees") %>% 
  select(Plot_Name, park = Unit_Code, Sample_Year, Tag, dbh = Equiv_Live_DBH_cm, ba = SumLiveBasalArea_cm2,
         crown = Crown_Class)
ncrn_tr <- load_data('NCRN', "Trees") %>% 
  select(Plot_Name, park = Unit_Code, Sample_Year, Tag, dbh = Equiv_Live_DBH_cm, ba = SumLiveBasalArea_cm2, 
         crown = Crown_Class)
netn_tr <- load_data('NETN', "Trees") %>% 
  select(Plot_Name, park = Unit_Code, Sample_Year, Tag, dbh = Equiv_Live_DBH_cm, ba = SumLiveBasalArea_cm2,
         crown = Crown_Class)

trees <- rbind(ermn_tr, midn_tr, ncrn_tr, netn_tr) %>% 
  filter(between(Sample_Year, 2016, 2019)) %>% 
  filter(!is.na(dbh) & dbh > 0)


# Structural Stage (Miller et al. 2016)
tr_ss <- trees %>% filter(crown %in% c(2, 3, 4)) %>% 
  mutate(pole_size = ifelse(park == "ACAD", 20 , 26),
         mature_size = ifelse(park == "ACAD", 34.9, 45.9),
         BA_pole = ifelse(dbh < pole_size, ba, 0),
         BA_mature = ifelse(dbh >= pole_size & dbh < mature_size, ba, 0),
         BA_large = ifelse(dbh >= mature_size, ba, 0)
  )

stand_str <- tr_ss %>% group_by(Plot_Name, park) %>% 
  summarize(ba_tot = sum(ba),
            pctBA_pole = sum(BA_pole)/ba_tot * 100,
            pctBA_mature = sum(BA_mature)/ba_tot * 100,
            pctBA_large = sum(BA_large)/ba_tot * 100, 
            stage = case_when(
              pctBA_pole + pctBA_mature >= 67 & pctBA_pole > pctBA_mature ~ 'Pole',
              pctBA_pole + pctBA_mature >= 67 & pctBA_pole <= pctBA_mature | pctBA_mature >= 67 ~ 'Mature',
              pctBA_mature + pctBA_large >= 67 & pctBA_large > pctBA_mature ~ 'Late Succ.',
              TRUE ~ 'Mosaic'
            ),
            .groups = 'drop')

dens_dfb <- left_join(dens_df, stand_str, by = c("Unit_Code" = 'park', "Plot_Name")) %>% 
  select(Plot_Name, status,
         Sap_Dens_NatCan, Seed_Dens_NatCan, Tree_Dens_Total, Tree_BA_Total,
         stock_final, stage) %>% 
  filter(!is.na(stage))

dens_dfb$stage_fac <- factor(dens_dfb$stage, levels = c("Pole", "Mature", "Late Succ.", "Mosaic"))

# QMD
trees_psum <- trees %>% group_by(Plot_Name) %>% 
  summarize(n = n(),
            QMD = sqrt( sum(dbh^2)/n),
            mDBH = sum(dbh)/n
  ) %>% ungroup()

names(dens_dfb)
names(trees_psum)
head(plots_shp)
names(plots_df)
names(plots_df_cov)
plots_final <- reduce(list(plots_df_cov,
                           dens_dfb %>% select(Plot_Name, stage_fac),
                           trees_psum %>% select(Plot_Name, QMD, mDBH)),
                      left_join, by = c("Plot_Name")) %>% 
  select(Plot_Name, x, y, PROVINCE, Physiographic_Class,  hummod300m, cancov, 
         tmax, precip, stage_fac, QMD, mDBH) %>% data.frame()

table(complete.cases(plots_final)) #18 F 1491 T
names(plots_final)

comb_df <- left_join(data.frame(plots_final), comb, by = c("Plot_Name")) %>% 
  select(Plot_Name, Park, Network, Year, hummod300m, DBI, everything()) %>% 
  filter(!is.na(Network)) %>% filter(!is.na(DBI)) %>% data.frame()

table(complete.cases(comb_df[,17:25]))
comb_df$Seed_Dens_NatCan[is.na(comb_df$Seed_Dens_NatCan)] <- 0 #NCRN values that should be 0
comb_df$Seed_Dens_NatOth[is.na(comb_df$Seed_Dens_NatOth)] <- 0 #NCRN values that should be 0
comb_df$Seed_Dens_Exotic[is.na(comb_df$Seed_Dens_Exotic)] <- 0 #NCRN values that should be 0
comb_df$Seed_Dens_Total[is.na(comb_df$Seed_Dens_Total)] <- 0 #NCRN values that should be 0
comb_df$stock_final[is.na(comb_df$stock_final)] <- 0 #NCRN values that should be 0
comb_df$Sor_seed[is.na(comb_df$Sor_seed)] <- 0

comb_df$stage_fac[is.na(comb_df$stage_fac)] <- "Mosaic" 
comb_df$QMD[is.na(comb_df$QMD)] <- 0 
comb_df$mDBH[is.na(comb_df$mDBH)] <- 0 
names(comb_df)

dm_parks = c("ANTI", "CATO", "CHOH", "HAFE", "MANA", "MONO", "ROCR", "GETT", "VAFO") 
comb_df <- comb_df %>% mutate(dm_park = ifelse(comb_df$Park %in% dm_parks, 1, 0))

comb_df <- comb_df %>% filter(!is.na(avg.cover)) # Dropping the BLUE plots that didn't have quads in 2019

comb_df[which(!complete.cases(comb_df)),]
table(complete.cases(comb_df)) # 1496 T

write.csv(comb_df, "./data/RFI_data_for_reg.csv", row.names = F)

