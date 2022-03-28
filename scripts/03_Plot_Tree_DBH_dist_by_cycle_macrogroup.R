#--------------------------------------------
# Compiling Live tree data for dbh distribution plots by cycle and park
#   Code written by Kate Miller 20211012 
#--------------------------------------------

#---- Dependencies and import data using NPSForVeg ----
#devtools::install_github("NCRN/NPSForVeg")
library(NPSForVeg)
library(tidyverse)
library(grid) # grid.draw
library(gridExtra) #grid.arrange
library(gtable) #gtable extracting legends

options(scipen = 100)

datapath <- "./data/"

yrs_all <- 2008:2019
c1 <- 2008:2011; c2 <- c1+4; c3 <- c2+4
cycles <- list(c1, c2, c3)
cycle_names <- c("C1", "C2", "C3")

network_codes <- c("ERMN", "MIDN", "NCRN", "NETN")

plot_visit_df <- read.csv(paste0(datapath, "EFWG_full_dataset_20220325.csv"))[,c(1:5,7,16)] 
plot_visit_df$Network <- ifelse(plot_visit_df$Unit_Code %in% c("COLO", "GEWA", "SAHI", "THST"), "NCBN", 
                                plot_visit_df$Network)
length(unique(plot_visit_df$Plot_Name)) #1515 unique plots
nrow(plot_visit_df) # 4464 plot x cycle rows
table(complete.cases(plot_visit_df)) # all 4462 T
head(plot_visit_df)

#---- Live tree density by species group in stems/ha ----
network = c("ERMN", "MIDN", "NCRN", "NETN")

load_data <- function(network, file, path = datapath){
  read.csv(paste0(datapath, network, "_data/", file, ".csv")) %>% mutate(network = network)
}

#------ Load and combine seedling and sapling data -----
# Metadata
meta <- rbind( 
  load_data("ERMN", "MetaData") %>% select(ParkCode, TPlotSize),
  load_data("MIDN", "MetaData") %>% select(ParkCode, TPlotSize),
  load_data("NCRN", "MetaData") %>% select(ParkCode, TPlotSize),
  load_data("NETN", "MetaData") %>% select(ParkCode, TPlotSize)
)

# Tree data
ermn_trees <- load_data("ERMN", "Trees")
ermn_trees$Equiv_Dead_DBH_cm[ermn_trees$Equiv_Dead_DBH_cm == 999999] <- NA
ermn_trees$Equiv_Live_DBH_cm[ermn_trees$Equiv_Live_DBH_cm == 999999] <- NA
ermn_trees$SumDeadBasalArea_cm2[ermn_trees$SumDeadBasalArea_cm2 == 785398000000] <- NA
ermn_trees$SumLiveBasalArea_cm2[ermn_trees$SumLiveBasalArea_cm2 == 785398000000] <- NA

midn_trees <- load_data("MIDN", "Trees")
netn_trees <- load_data("NETN", "Trees")
ncrn_trees <- load_data("NCRN", "Trees")
names(ncrn_trees)

tree_names <- c("Plot_Name", "Unit_Code", "Sample_Year", "Equiv_Live_DBH_cm")

trees_comb <- rbind(ermn_trees[, tree_names],
                   midn_trees[, tree_names],
                   ncrn_trees[, tree_names],
                   netn_trees[, tree_names]) %>% 
  filter(Equiv_Live_DBH_cm >= 10.0) %>% # remove treeslings <15cm tall in ERMN 
  filter(Sample_Year >= 2008) # takes 3 most recent cycles

head(plot_visit_df)

trees_comb2 <- left_join(plot_visit_df, 
                         trees_comb, 
                         by = c("Plot_Name", "Unit_Code", "Year" = "Sample_Year")) %>% 
               left_join(., meta, by = c("Unit_Code" = "ParkCode"))

head(trees_comb2)
hist(trees_comb2$Equiv_Live_DBH_cm)
summary(trees_comb2$Equiv_Live_DBH_cm)
nrow(trees_comb2[trees_comb2$Equiv_Live_DBH_cm > 110,])/nrow(trees_comb2) * 100
#0.05% of trees >110 cm DBH

# Setting up diameter distribution
trees_comb2 <- trees_comb2 %>% mutate(dbh_10cm = ifelse(Equiv_Live_DBH_cm <20, 1, 0),
                                      dbh_20cm = ifelse(between(Equiv_Live_DBH_cm, 20, 29.9), 1, 0),
                                      dbh_30cm = ifelse(between(Equiv_Live_DBH_cm, 30, 39.9), 1, 0),
                                      dbh_40cm = ifelse(between(Equiv_Live_DBH_cm, 40, 49.9), 1, 0),
                                      dbh_50cm = ifelse(between(Equiv_Live_DBH_cm, 50, 59.9), 1, 0),
                                      dbh_60cm = ifelse(between(Equiv_Live_DBH_cm, 60, 69.9), 1, 0),
                                      dbh_70cm = ifelse(between(Equiv_Live_DBH_cm, 70, 79.9), 1, 0),
                                      dbh_80cm = ifelse(between(Equiv_Live_DBH_cm, 80, 89.9), 1, 0),
                                      dbh_90cm = ifelse(between(Equiv_Live_DBH_cm, 90, 99.9), 1, 0),
                                      dbh_100cm = ifelse(Equiv_Live_DBH_cm >=100, 1, 0),
                                      conv = 10000/TPlotSize) 
head(trees_comb2)
trees_comb2$check <- rowSums(trees_comb2[,9:18], na.rm = T)
table(trees_comb2$check) # none > 1, good

names(trees_comb2)

tree_dbh_dist_plot <- trees_comb2 %>% group_by(Plot_Name, Unit_Code, Year, Network, cycle, lat_rank, excludeEvent) %>% 
                                      summarize(across(starts_with("dbh_"), 
                                                ~sum(.x, na.rm = TRUE)*first(conv)),
                                                .groups = 'drop')

names(tree_dbh_dist_plot)
mg_df <- read.csv(paste0(datapath, "EFWG_macrogroup_plot_lat_rank.csv")) 
mg_df_simp <- mg_df %>% select(MACROGROUP_NAME, mg_short, lat_rank) %>% unique()

tree_dbh_dist_plot2 <- left_join(mg_df, tree_dbh_dist_plot %>% select(-lat_rank), by = "Plot_Name") %>% 
  filter(!is.na(cycle))

tree_dbh_dist_plot2$mg_ord <- reorder(tree_dbh_dist_plot2$mg_short, 
                                      desc(tree_dbh_dist_plot2$lat_rank))

tree_dbh_dist_plot2[tree_dbh_dist_plot2$excludeEvent == 1, 8:17] <- NA_real_

tree_dbh_dist_mg <- tree_dbh_dist_plot2 %>% group_by(mg_short, lat_rank, cycle) %>% 
                                             summarize(#num_plots = sum(!is.na(dbh_10cm)),
                                                       across(starts_with("dbh_"),
                                                             ~mean(.x, na.rm = TRUE)),
                                                       .groups = 'drop')
head(tree_dbh_dist_mg)

tree_dbh_dist <- tree_dbh_dist_mg %>% 
  pivot_longer(cols = starts_with("dbh"), names_to = "size_class", values_to = "density") %>% 
  filter(!is.na(cycle))


tree_dbh_dist$size_class <- ordered(tree_dbh_dist$size_class,
                                         levels = c("dbh_10cm", "dbh_20cm", "dbh_30cm",
                                                    "dbh_40cm", "dbh_50cm", 
                                                    "dbh_60cm", "dbh_70cm", "dbh_80cm",
                                                    "dbh_90cm", "dbh_100cm"))

tree_dbh_dist$class <- as.numeric(gsub("\\D", "", tree_dbh_dist$size_class))

tree_dbh_dist$mg_ord <- reorder(tree_dbh_dist$mg_short, desc(tree_dbh_dist$lat_rank))
head(tree_dbh_dist)

write.csv(tree_dbh_dist, paste0(datapath, "Tree_DBH_Dist_by_mg.csv"), row.names = FALSE)

p <- ggplot(data = tree_dbh_dist %>% arrange(mg_ord), aes(x = class, y = density, fill = as.factor(cycle),
                                 color = as.factor(cycle)))+
  geom_bar(stat = 'identity', position = "dodge")+
  facet_wrap(~mg_ord, scales = 'fixed', ncol = 4)+ forestNETN::theme_FHM()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0, size = 9))+
  scale_color_manual(values = c("#A9DFBF", "#27AE60", "#196F3D"), name = "cycle")+
  scale_fill_manual(values = c("#A9DFBF", "#27AE60", "#196F3D"), name = 'cycle')+
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
                     labels = c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100+"))+
  labs(x = "DBH size classes in 10cm increments", y = "Tree Density (stems/ha)")

svg("./results/Diam_Dist_bar_mg.avg", height = 7, width = 10) 
p
dev.off()

# Next section of code is to force the y axis to be from 0 to 400 for all but ACAD in the facets. It requires
# adding another size class that doesn't get plotted in the x axis, but includes 400 in the y for all bu ACAD.

y400 <- data.frame(tree_dbh_dist %>% filter(size_class == "dbh_10cm"))
y400$class <- 0
y400$size_class <- "dbh_0cm"
y400$density <- ifelse(y400$mg_short == "Acadian Mixed", 640, 400)

tree_dbh_dist2 <- rbind(tree_dbh_dist, y400) %>% arrange(mg_ord, cycle, size_class)

tree_dbh_dist2$size_class <- ordered(tree_dbh_dist2$size_class,
                                     levels = c("dbh_0cm", "dbh_10cm", "dbh_20cm", 
                                                "dbh_30cm", "dbh_40cm", "dbh_50cm", 
                                                "dbh_60cm", "dbh_70cm", "dbh_80cm",
                                                "dbh_90cm", "dbh_100cm"))

p <- ggplot(data = tree_dbh_dist2 %>% arrange(mg_ord), aes(x = class, y = density, fill = as.factor(cycle),
                                      color = as.factor(cycle)))+
  geom_line(aes(color = as.factor(cycle)), size = 0.5)+
  scale_color_manual(values = c("#A9DFBF", "#27AE60", "#196F3D"), name = "cycle")+
  scale_fill_manual(values = c("#A9DFBF", "#27AE60", "#196F3D", name = 'cycle'))+
  facet_wrap(~mg_ord, scales = 'free_y', ncol = 4)+ 
  scale_x_continuous(limits = c(10, 100),
                     breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
                     labels = c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100+"))+
  labs(x = "DBH size classes in 10 cm increments", y = "Tree Density (stems/ha)")+
  forestNETN::theme_FHM()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        legend.position = 'none')

p
# Fake plot for cycle legend
leg_cyc <- ggplot(data = tree_dbh_dist2 %>% arrange(mg_ord), aes(x = class, y = density, fill = as.factor(cycle),
                                                                        color = as.factor(cycle)))+
  geom_line(aes(color = as.factor(cycle)), size = 0.5)+
  scale_color_manual(values = c("#A9DFBF", "#27AE60", "#196F3D"), name = "Cycle:")+
  scale_fill_manual(values = c("#A9DFBF", "#27AE60", "#196F3D", name = 'Cycle:'))+
  forestNETN::theme_FHM()+
  theme(legend.position = 'bottom',
        legend.title = element_text(size = 8), 
        legend.text = element_text(size = 8),
        legend.background = element_rect(color = "#B9B9B9", size = 0.25))#, 

# Extract legend from fake plot
leg_cyc <- gtable_filter(ggplot_gtable(ggplot_build(leg_cyc)), "guide-box")

svg(paste0("./results/20220325/Diam_Dist_line_mg.svg"), width = 11, height = 8, family = 'sans')

grid.arrange(grobs = list(p, leg_cyc),
             heights = c(6.5, 0.5),
             widths = c(2.25, 6.5, 2.25),
             layout_matrix = rbind(c(1, 1, 1),
                               c(NA, 2, NA)))

dev.off()


