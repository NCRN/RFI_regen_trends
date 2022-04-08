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

tree_dbh_dist_plot <- trees_comb2 %>% group_by(Plot_Name, Unit_Code, Year, Network, cycle, lat_rank, excludeEvent) %>% 
                                      summarize(across(starts_with("dbh_"), 
                                                ~sum(.x, na.rm = TRUE)*first(conv)),
                                                .groups = 'drop')

names(tree_dbh_dist_plot)
tree_dbh_dist_plot[tree_dbh_dist_plot$excludeEvent == 1, 8:17] <- NA_real_

tree_dbh_dist_park <- tree_dbh_dist_plot %>% group_by(Unit_Code, Network, lat_rank, cycle) %>% 
                                             summarize(#num_plots = sum(!is.na(dbh_10cm)),
                                                       across(starts_with("dbh_"),
                                                             ~mean(.x, na.rm = TRUE)),
                                                       .groups = 'drop')
head(tree_dbh_dist_park)

tree_dbh_dist <- tree_dbh_dist_park %>% 
  pivot_longer(cols = starts_with("dbh"), names_to = "size_class", values_to = "density")


tree_dbh_dist$size_class <- ordered(tree_dbh_dist$size_class,
                                         levels = c("dbh_10cm", "dbh_20cm", "dbh_30cm",
                                                    "dbh_40cm", "dbh_50cm", 
                                                    "dbh_60cm", "dbh_70cm", "dbh_80cm",
                                                    "dbh_90cm", "dbh_100cm"))

tree_dbh_dist$class <- as.numeric(gsub("\\D", "", tree_dbh_dist$size_class))

tree_dbh_dist$park_ord <- reorder(tree_dbh_dist$Unit_Code, desc(tree_dbh_dist$lat_rank))
head(tree_dbh_dist)

# Matching networks to colors to color-code facet strips by network
tree_dbh_dist$strp_col <- case_when(tree_dbh_dist$Network == "ERMN" ~ "#A5BDCD",
                                    tree_dbh_dist$Network == "MIDN" ~ "#E7CDA4",
                                    tree_dbh_dist$Network == "NCBN" ~ "#CFB9D9",
                                    tree_dbh_dist$Network == "NCRN" ~ "#E1E59B",
                                    tree_dbh_dist$Network == "NETN" ~ "#AACCA7") 

write.csv(tree_dbh_dist, paste0("./results/Tree_DBH_Dist_by_park.csv"), row.names = FALSE)

park_cols <- rbind(data.frame(park_ord = "BLNK", strp_col = "white"), # placeholder for bottom right corner that's blank
                   unique(data.frame(park_ord = tree_dbh_dist$park_ord,
                                     strp_col = tree_dbh_dist$strp_col)) %>% arrange(desc(park_ord))
)

park_cols <- park_cols %>% mutate(facet_ord = c(rev(202:209), rev(210:217), rev(218:225), 
                                                rev(226:233), rev(234:241))) %>% 
  arrange(facet_ord)

fills <- c(park_cols$strp_col) 

p <- ggplot(data = tree_dbh_dist %>% arrange(park_ord), aes(x = class, y = density, fill = as.factor(cycle),
                                 color = as.factor(cycle)))+
  geom_bar(stat = 'identity', position = "dodge")+
  facet_wrap(~park_ord, scales = 'fixed', ncol = 8)+ forestNETN::theme_FHM()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0, size = 9))+
  scale_color_manual(values = c("#A9DFBF", "#27AE60", "#196F3D"), name = "cycle")+
  scale_fill_manual(values = c("#A9DFBF", "#27AE60", "#196F3D"), name = 'cycle')+
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
                     labels = c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100+"))+
  labs(x = "DBH size classes in 10cm increments", y = "Tree Density (stems/ha)")

# code for color-coding facet strips
g <- ggplot_gtable(ggplot_build(p))
strp_col <- which(grepl('strip-t', g$layout$name))

k <- 1
for (i in strp_col) {
  g$grobs[[i]]$grobs[[1]]$children[[1]]$gp$fill <- fills[k]
  k <- k+1
}

# fake plot to make network legend
leg_net <- ggplot(data = data.frame(network = c("ERMN", "MIDN", "NCBN", "NCRN", "NETN"),
                                    x = c(1, 2, 3, 4, 5)),
                  aes(x = x, fill = network))+
  geom_histogram()+
  scale_fill_manual(values = c("#A5BDCD", "#E7CDA4", "#CFB9D9", "#E1E59B", "#AACCA7"), 
                    name = "Network:")+ theme_bw()+
  theme(legend.position = 'bottom', 
        legend.title = element_text(size = 8), 
        legend.text=element_text(size = 8),
        #legend.background = element_blank()
        legend.background = element_rect(color = "#B9B9B9", size = 0.25))

# grab legend from fake plot and plot with grid.arrange
leg_gnet <- gtable_filter(ggplot_gtable(ggplot_build(leg_net)), "guide-box")
        
png("./results/Diam_Dist_bar.png", 
    height = 7, width = 10, units = 'in', res = 600) 

grid.arrange(grobs = list(g, leg_gnet),
             heights = c(6.5, 0.5))#,
             #widths = c(0.6, 5.1, 3.7, 0.6),
             #layout_matrix = rbind(c(1, 1, 1, 1),
             #                      c(NA, 2, 3, NA)))

dev.off()

# Next section of code is to force the y axis to be from 0 to 400 for all but ACAD in the facets. It requires
# adding another size class that doesn't get plotted in the x axis, but includes 400 in the y for all bu ACAD.

y400 <- data.frame(tree_dbh_dist %>% filter(size_class == "dbh_10cm"))
y400$class <- 0
y400$size_class <- "dbh_0cm"
y400$density <- ifelse(y400$Unit_Code == "ACAD", 640, 400)

tree_dbh_dist2 <- rbind(tree_dbh_dist, y400) %>% arrange(Unit_Code, cycle, size_class)

tree_dbh_dist2$size_class <- ordered(tree_dbh_dist2$size_class,
                                     levels = c("dbh_0cm", "dbh_10cm", "dbh_20cm", 
                                                "dbh_30cm", "dbh_40cm", "dbh_50cm", 
                                                "dbh_60cm", "dbh_70cm", "dbh_80cm",
                                                "dbh_90cm", "dbh_100cm"))

p <- ggplot(data = tree_dbh_dist2 %>% arrange(park_ord), aes(x = class, y = density, fill = as.factor(cycle),
                                      color = as.factor(cycle)))+
  geom_line(aes(color = as.factor(cycle)), size = 0.5)+
  scale_color_manual(values = c("#A9DFBF", "#27AE60", "#196F3D"), name = "cycle")+
  scale_fill_manual(values = c("#A9DFBF", "#27AE60", "#196F3D", name = 'cycle'))+
  facet_wrap(~park_ord, scales = 'free_y', ncol = 8)+ 
  scale_x_continuous(limits = c(10, 100),
                     breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
                     labels = c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100+"))+
  labs(x = "DBH size classes in 10 cm increments", y = "Tree Density (stems/ha)")+
  forestNETN::theme_FHM()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        legend.position = 'none')

# Fake plot for cycle legend
leg_cyc <- ggplot(data = tree_dbh_dist2 %>% arrange(park_ord), aes(x = class, y = density, fill = as.factor(cycle),
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

# Color code facets by network
g <- ggplot_gtable(ggplot_build(p))
strp_col <- which(grepl('strip-t', g$layout$name))

k <- 1
for (i in strp_col) {
  g$grobs[[i]]$grobs[[1]]$children[[1]]$gp$fill <- fills[k]
  k <- k+1
}

# Fake plot for network legend
leg_net <- ggplot(data = data.frame(network = c("ERMN", "MIDN", "NCBN", "NCRN", "NETN"),
                                    x = c(1, 2, 3, 4, 5)),
                  aes(x = x, fill = network))+
  geom_histogram()+
  scale_fill_manual(values = c("#A5BDCD", "#E7CDA4", "#CFB9D9", "#E1E59B", "#AACCA7"), 
                    name = "Network:")+ theme_bw()+
  theme(legend.position = 'bottom', 
        legend.title = element_text(size = 8), 
        legend.text = element_text(size = 8),
        legend.background = element_rect(color = "#B9B9B9", size = 0.25))

# Extract legend from fake plot
leg_gnet <- gtable_filter(ggplot_gtable(ggplot_build(leg_net)), "guide-box")

# png(paste0(datapath, "/results/Diam_Dist_line.png"), 
#     height = 7, width = 10, units = 'in', res = 600) 
# 

svg(paste0("./results/20220325/Diam_Dist_line.svg"), width = 11, height = 8, family = 'sans')

grid.arrange(grobs = list(g, leg_cyc, leg_gnet),
             heights = c(6.5, 0.5),
             widths = c(2, 3, 3.5, 2.5),
             layout_matrix = rbind(c(1, 1, 1, 1),
                               c(NA, 2, 3, NA)))

dev.off()


