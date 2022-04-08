library(forestTrends)
library(tidyverse)
library(grid) # for digging into str of ggplot grobs
library(gridExtra) # for ggarrange
library(gtable)

options("scipen" = 100, "digits" = 4) # keeps scientific notation from showing up

dens_df <- read.csv("./data/EFWG_full_dataset_20220325.csv")
dens_df$park_ord <- reorder(dens_df$Unit_Code, desc(dens_df$lat_rank))
names(dens_df)
dens_df <- dens_df[,c(1:16, 87, 17:86)]

tree_dbh_dist <- read.csv("./results/Tree_DBH_Dist_by_park.csv")

boot_results <- read.csv("./results/20220325/all_metrics_randint_20220325.csv")
boot_results$park_ord <- reorder(boot_results$park, desc(boot_results$lat_rank))
boot_results$Network[boot_results$park %in% c("COLO", "SAHI", "THST")] <- "NCBN" # in case this hasn't been changed
boot_results$network_ord <- factor(boot_results$Network, levels = c("NETN", "ERMN", "NCRN", "NCBN", "MIDN"))
table(boot_results$network)

#---- Set up lists to iterate over ----
# column names for response
names(dens_df)
#metrics <- c(names(dens_df[, c(18:ncol(dens_df))]))

metrics <- c(names(dens_df[, c(18:87)]))

# response titles for plotting
metric_names <- c(rep("Tree BA (sq.m/ha)", 4), 
                  rep("Tree Density (stems/ha)", 4),
                  rep("Tree BA 10 cm (sq.m/ha)", 4),  
                  rep("Tree BA 20 cm (sq.m/ha)", 4), 
                  rep("Tree BA 30 cm (sq.m/ha)", 4),
                  rep("Tree BA 40 cm+ (sq.m/ha)", 4),
                  rep("Tree Dens. 10 cm (stems/ha)", 4),  
                  rep("Tree Dens. 20 cm (stems/ha)", 4), 
                  rep("Tree Dens. 30 cm (stems/ha)", 4),
                  rep("Tree Dens. 40 cm+ (stems/ha)", 4),
                  rep("Sapling Density (stems/sq.m)", 4),
                  rep("Sapling BA (sq.m/ha)", 4),
                  rep("Seedling Density (stems/sq.m)", 4),
                  "Tree BA (sq.m/ha)", "Tree Density (stems/ha)", 
                  "Tree BA 10 cm (sq.m/ha)", "Tree Dens. 10 cm (stems/ha)", 
                  "Tree BA 20 cm (sq.m/ha)", "Tree Dens. 20 cm (stems/ha)",
                  "Tree BA 30 cm (sq.m/ha)", "Tree Dens. 30 cm (stems/ha)",
                  "Tree BA 40 cm (sq.m/ha)", "Tree Dens. 40 cm (stems/ha)",
                  "Sapling BA (sq.m/ha)", "Sapling Density (stems/sq.m)", 
                  "Seedling Density (stems/sq.m)",    
                  "Stocking Index", "Sorensen Sapling", 
                  "Horn Sapling", "Sorensen Seedling", 
                  "Horn Seedling")

# match metric columns and titles
met_df <- data.frame(metrics, metric_names)
met_df

met_df <- met_df %>% mutate(metgrp = factor(case_when(grepl("Total", metrics) ~ "Total",
                                                      grepl("NatCan|stock", metrics) ~ "Native Canopy",
                                                      grepl("NatOth", metrics) ~ "Other Native",
                                                      grepl("Exotic", metrics) ~ "Exotic",
                                                      grepl("Sor|Hor", metrics) ~ "Similarity", 
                                                      grepl("Native", metrics) ~ "Native",
                                                      TRUE ~ "Unk"), 
                                                      levels = c("Total", "Native Canopy", "Other Native",
                                                                 "Exotic", "Similarity", "Native", "Unk"))) %>% 
                    filter(!metgrp %in% c("Native", "Unk")) # split native to canopy & other. Don't care about total
head(met_df)

park_met_list <- data.frame(expand.grid(unique(dens_df$Unit_Code), metrics)) %>% 
  set_names("Unit_Code", "metrics") %>% 
  arrange(Unit_Code, metrics) %>% 
  mutate(across(where(is.factor), as.character)) %>% 
  left_join(., met_df, by = "metrics") %>% filter(!metgrp %in% "Native" & !is.na(metgrp))

head(park_met_list)
park_met_list$sppgrp <- gsub("^.*_", "", park_met_list$metrics)
sppgrp_list <- c(paste0("_", unique(park_met_list$sppgrp), collapse = "|"))
park_met_list$var <- gsub(sppgrp_list, "", park_met_list$metrics)

park_list <- c(park_met_list[,1])
met_list <- c(park_met_list[,2])
head(boot_results)

# boot_results already has these columns, so turning off for now.
# boot_results$strp_col <- case_when(boot_results$Network == "ERMN" ~ "#A5BDCD",
#                                    boot_results$Network == "MIDN" ~ "#E7CDA4",
#                                    boot_results$Network == "NCBN" ~ "#CFB9D9",
#                                    boot_results$Network == "NCRN" ~ "#E1E59B",
#                                    boot_results$Network == "NETN" ~ "#AACCA7") 

# boot_results <- boot_results %>% rename(Network = network)

#----- Total plots -----
# Fake plot to customize trend legend
sign_color = c("#979797", "#686868", "#6FAB6A", "#BB6363")
leg_line <- ggplot(data = data.frame(sign = c("notmod", "nonsign", "signinc", "signdec"),
                                     x = c(1, 2, 3, 4)), 
                   aes(x = x, y = x, color = sign, fill = sign, linetype = sign, shape = sign))+
  theme_bw()+
  geom_line(size = 0.5)+
  geom_point(size = 1, shape = 21, alpha = 0.8)+
  scale_fill_manual(values = c("notmod" = "white", "nonsign" =  sign_color[2],
                               "signinc" = sign_color[3], "signdec" = sign_color[4]), 
                    labels = c("Not Modeled", "Not Significant", "Sign. Increase", "Sign. Decrease"),
                    name = "Trends:",
                    drop = FALSE)+
  scale_color_manual(values = c("notmod" = "#CACACA", "nonsign" = "black",
                                "signinc" = sign_color[3], "signdec" = sign_color[4]), 
                     labels = c("Not Modeled", "Not Significant", "Sign. Increase", "Sign. Decrease"),
                     name = "Trends:",
                     drop = FALSE)+
  scale_linetype_manual(values = c("notmod" = 'dashed', "nonsign" = 'dashed',
                                   "signinc" = 'solid', "signdec" = 'solid'), 
                        labels = c("Not Modeled", "Not Significant", "Sign. Increase", "Sign. Decrease"),
                        name = "Trends:",
                        drop = FALSE)+
  theme(legend.position = 'bottom', 
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10),
        #legend.background = element_blank()
        legend.background = element_rect(colour = "#B9B9B9", size = 0.25)
  )

# Extracting custom legends from fake plots
leg_gline <- gtable_filter(ggplot_gtable(ggplot_build(leg_line)), "guide-box")

# Plot park-level trends in total species
head(park_met_list)

met_total <- park_met_list %>% filter(metgrp == "Total") %>% select(metrics)
park_total <- park_met_list %>% filter(metgrp == "Total") %>% select(Unit_Code)

# Plot 5 main metrics for tree, sapling, seedlings
# met_ls <- park_met_list %>% filter(metrics %in% c("Tree_BA_Total", "Tree_Dens_Total",
#                                                   "Sapl_BA_Total", "Sap_Dens_Total", 
#                                                   "Seed_Dens_Total"))
# table(met_ls$metrics)
# table(park_met_list$metrics)
# head(met_ls)

met_tot_list <- c("Tree_BA_Total", "Tree_Dens_Total",
                  "Sap_BA_Total", "Sap_Dens_Total", 
                  "Seed_Dens_Total")
park_ls <- unique(park_list)
park_ls

walk(park_ls, function(park){
  parkcode = as.character(park_met_list %>% filter(Unit_Code == park) %>% select(Unit_Code) %>% unique())
  cat(parkcode, "\n")
  
  plots <- map(seq_along(met_tot_list), function(met){
      
     df <- boot_results %>% filter(resp == met_tot_list[met]) %>%
                            filter(park == parkcode)

     title = as.character(park_met_list %>% filter(metrics == met_tot_list[met]) %>% 
                          select(metric_names) %>% unique())
     
     p <- plot_trend_response(df, xlab = "Cycle", ylab = title, model_type = 'lmer') +
          scale_x_continuous(breaks = c(1, 2, 3), labels = c("1", "2", "3"))
  
     }) %>% set_names(met_tot_list) # map
   
  svg(paste0("./results/20220325/park_plots/total/", parkcode, "_Total_metrics", ".svg"),
      height = 3, width = 12)

     grid.arrange(grobs = list(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], leg_gline),
               heights = c(4.85, 0.2, 0.4, 0.2),
               widths = c(0.1, 2.5, 2.5, 2.5, 2.5, 2.5, 0.1),
               layout_matrix = rbind(c(NA, 1, 2, 3, 4, 5, NA),
                                     c(NA, NA, NA, NA, NA, NA, NA),
                                     c(NA, NA, 6, 6, 6, NA, NA)),
                                     c(NA, NA, NA, NA, NA, NA, NA) #makes sure legend bottom isn't cut off
               )

  dev.off()

})


# Saves each individual park/metric plot
# walk2(1:nrow(met_total), 1:nrow(park_total), function(x, y){
#   parkcode = park_met_list %>% filter(Unit_Code == y) %>% select(Unit_Code) %>% unique()
#   metric = park_met_list %>% filter(metrics == x) %>% select(metrics) %>% unique()
#   
#   title = as.character(park_met_list %>% filter(metgrp == "Total") %>% 
#                                    filter(metrics == met_total[x,]) %>%  
#                                    filter(Unit_Code == park_total[y,]) %>% 
#                                    select(metric_names))
# 
#   df <- boot_results %>% filter(resp == met_total[x,]) %>%
#                          filter(park == park_total[y,])
# 
#   parkcode = as.character(unique(df$park))
#   metric = as.character(park_met_list %>% filter(metgrp == "Total") %>% 
#                           filter(metrics == met_total[x,]) %>%  
#                           filter(Unit_Code == park_total[y,]) %>% 
#                           select(metrics))
#   
#   p <- plot_trend_response(df, xlab = "Cycle", ylab = title, model_type = 'lmer') +
#        scale_x_continuous(breaks = c(1, 2, 3), labels = c("1", "2", "3"))
# 
#   svg("./results/20220325/park_plots_sing/", parkcode, "_", metric, ".svg"),
#       height = 6, width = 6)
# 
#   grid.arrange(grobs = list(p, leg_gline), 
#                heights = c(4.85, 0.1, 0.25, 0.1),
#                widths = c(0.5, 3, 0.25),
#                layout_matrix = rbind(c(1, 1, 1),
#                                      c(NA, NA, NA),
#                                      c(NA, 2, NA)),
#                                      c(NA, NA, NA) #makes sure legend bottom isn't cut off
#                )
# 
#   dev.off()
#   cat(parkcode, metric, "\n")
# })
head(boot_results)
head(met_df)

#----- By Species Group -----
boot_sppgrp1 <- left_join(boot_results, 
                          met_df %>% select(metrics, metgrp), 
                          by = c("resp" = "metrics")) %>% 
                filter(metgrp %in% c("Native Canopy", "Other Native", "Exotic")) 

boot_sign <- boot_sppgrp1 %>% filter(term == "Slope") %>% 
                             group_by(park, resp) %>% 
                             mutate(sign = case_when(lower95 > 0 | upper95 < 0 ~ "sign",
                                                     is.na(lower95) ~ "notmod",
                                                     TRUE ~ "nonsign")) %>% 
                             select(park, resp, sign)

boot_sppgrp <- left_join(boot_sppgrp1 %>% filter(!term %in% c("Slope", "Intercept")), 
                         boot_sign, 
                         by = c("park", "resp"))
boot_sppgrp$sppgrp <- gsub("^.*_", "", boot_sppgrp$resp)
sppgrp_list <- c(paste0("_", unique(boot_sppgrp$sppgrp), collapse = "|"))
boot_sppgrp$metric <- gsub(sppgrp_list, "", boot_sppgrp$resp)

# Plot function for park-level plot for tree metrics by spp type
plot_park_by_grp <- function(df, xlab, ylab, parkcode, var){ 
  #var_sym <- sym(var)
  
  # hacky way to plot groups that didn't get modeled and so don't have errorbars or ribbons
  df$upper95 <- ifelse(is.na(df$upper95), df$estimate, df$upper95)
  df$lower95 <- ifelse(is.na(df$lower95), df$estimate, df$lower95)
  df$time <- as.numeric(gsub("\\D", "", df$term))
  df1 <- df %>% filter(metric == var) 
  
  p <- 
  ggplot(df1 %>% filter(park == parkcode), aes(x = time, y = estimate))+ 
    geom_point(aes(color = sppgrp, shape = sppgrp, fill = sppgrp), 
               size = 2, na.rm = TRUE)+
    geom_errorbar(aes(ymin = lower95, ymax = upper95, x = time,
                      colour = sppgrp), width = 0.1, size = 0.5, na.rm = TRUE)+
    geom_line(aes(y = estimate, x = time, colour = sppgrp, linetype = sign), na.rm = TRUE)+
    scale_linetype_manual(values = c('notmod' = 'dotted', 'nonsign' = 'dashed', 
                                     'sign' = 'solid'))+
    scale_color_manual(values = c("NatCan" = "#2BA12E", "NatOth" = "#B9B9B9", "Exotic" = "#CD5C5C"))+
    scale_fill_manual(values = c("NatCan" = "#2BA12E", "NatOth" = "#B9B9B9", "Exotic" = "#CD5C5C"))+
    scale_shape_manual(values = c("NatCan" = 24, "NatOth" = 21, "Exotic" = 25))+
    theme_bw()+
    theme(axis.text = element_text(size = 11), 
          axis.title = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, size = 12, margin = margin(t = 10, b = -15)),
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.line = element_blank(), 
          legend.position = 'none') + 
    labs(x = xlab, y = ylab)#+
  return(p)
} 

# Fake plot for trend legend
leg_line2 <- ggplot(data = data.frame(sign = c("notmod", "nonsign", "sign"),
                                      x = c(1, 2, 3)), 
                    aes(x = x, y = x, color = sign, fill = sign, linetype = sign))+
  theme_bw()+ geom_line(size = 0.5)+ # geom_point(size = 1.5, shape = 21, alpha = 0.8)+
  scale_fill_manual(values = c("notmod" = "#686868", "nonsign" =  "#686868", "sign" = "#686868"), 
                    labels = c("Not Modeled", "Not Significant", "Significant"),
                    name = "Trends:",
                    drop = FALSE)+
  scale_color_manual(values = c("notmod" = "#686868", "nonsign" =  "#686868", "sign" = "#686868"), 
                     labels = c("Not Modeled", "Not Significant", "Significant"),
                     name = "Trends:",
                     drop = FALSE)+
  scale_linetype_manual(values = c("notmod" = 'dotted', "nonsign" = 'dashed', "sign" = 'solid'), 
                        labels = c("Not Modeled", "Not Significant", "Significant"),
                        name = "Trends:",
                        drop = FALSE)+
  theme(legend.position = 'bottom', legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10), #legend.background = element_blank()
        legend.background = element_rect(colour = "#B9B9B9", size = 0.25)
  )

# Fake plot for species group legend
leg_linesp <- ggplot(data = data.frame(spgrp = c("Exotic", "NatCan", "NatOth"),
                                       x = c(1, 2, 3)), 
                     aes(x = x, y = x, color = spgrp, fill = spgrp, shape = spgrp))+
  theme_bw()+ geom_line(size = 0.5)+  
  geom_point(size = 1.5)+
  scale_fill_manual(values = c("NatCan" = "#2BA12E", "NatOth" = "#B9B9B9", "Exotic" = "#CD5C5C"), 
                    labels = c("Native Canopy", "Native Other", "Exotic"),
                    name = "Groups:",
                    drop = FALSE)+
  scale_color_manual(values = c("NatCan" = "#2BA12E", "NatOth" = "#B9B9B9", "Exotic" = "#CD5C5C"), 
                     labels = c("Native Canopy", "Native Other", "Exotic"),
                     name = "Groups:",
                     drop = FALSE)+
  scale_shape_manual(values = c("NatCan" = 24, "NatOth" = 21, "Exotic" = 25),
                     labels = c("Native Canopy", "Native Other", "Exotic"),
                     name = "Groups:",
                     drop = FALSE)+
  theme(legend.position = 'bottom', legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10), #legend.background = element_blank()
        legend.background = element_rect(colour = "#B9B9B9", size = 0.25)
  )

leg_gline2 <- gtable_filter(ggplot_gtable(ggplot_build(leg_line2)), "guide-box")
leg_glinesp <- gtable_filter(ggplot_gtable(ggplot_build(leg_linesp)), "guide-box")

met_spp <- c("Tree_BA", "Tree_Dens", "Sap_BA", "Sap_Dens", "Seed_Dens")

df <- boot_sppgrp

walk(park_ls, function(park){
  parkcode = as.character(park_met_list %>% filter(Unit_Code == park) %>% select(Unit_Code) %>% unique())
  cat(parkcode, "\n")
  
  df_park <- boot_sppgrp %>% filter(park == parkcode) 

  plots <- map(seq_along(met_spp), function(met){
    
    var = met_spp[met]

    df_met <- df_park %>% filter(metric == met_spp[met])
    
    title = as.character(park_met_list %>% filter(var == met_spp[met]) %>% 
                         select(metric_names) %>% unique())
    
    p <- plot_park_by_grp(df_met, parkcode = parkcode,
                            xlab = "Cycle", ylab = title, var = var)+
         scale_x_continuous(breaks = c(1, 2, 3), labels = c("1", "2", "3"))
    
  }) %>% set_names(met_spp) # map
  
  svg(paste0("./results/20220325/park_plots/spp_grp/", parkcode, "_spp_grp", ".svg"),
      height = 3, width = 12)

  grid.arrange(grobs = list(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]],
                            leg_gline2, leg_glinesp),
               heights = c(4.85, 0.2, 0.4, 0.2),
               widths = c(0.1, 2.5, 2.5, 2.5, 2.5, 2.5, 0.1),
               layout_matrix = rbind(c(NA, 1, 2, 3, 4, 5, NA),
                                     c(NA, NA, NA, NA, NA, NA, NA),
                                     c(NA, NA, 6, NA, 7, NA, NA)),
               c(NA, NA, NA, NA, NA, NA, NA) #makes sure legend bottom isn't cut off
  )

  dev.off()
cat(parkcode, "\n")
})

#---- Tree DBH Dist plots by park ----
head(tree_dbh_dist)

tree_dbh_dist$size_class <- ordered(tree_dbh_dist$size_class,
                                     levels = c("dbh_10cm", "dbh_20cm", 
                                                "dbh_30cm", "dbh_40cm", "dbh_50cm", 
                                                "dbh_60cm", "dbh_70cm", "dbh_80cm",
                                                "dbh_90cm", "dbh_100cm"))
walk(park_ls, function(park){
  parkcode = as.character(park_met_list %>% filter(Unit_Code == park) %>% select(Unit_Code) %>% unique())
  
  df_park <- tree_dbh_dist %>% filter(Unit_Code == parkcode) 

  p <- 
    ggplot(data = df_park %>% arrange(park_ord), aes(x = class, y = density, fill = as.factor(cycle),
                                                         color = as.factor(cycle), group = as.factor(cycle)))+
         geom_bar(stat = 'identity', position = "dodge")+
         #geom_line(aes(color = as.factor(cycle)), size = 0.5)+
         scale_color_manual(values = c("#A9DFBF", "#27AE60", "#196F3D"), name = "Cycle")+
         scale_fill_manual(values = c("#A9DFBF", "#27AE60", "#196F3D"), name = 'Cycle')+
         theme_bw()+
         theme(axis.text = element_text(size = 11), 
               axis.title = element_text(size = 12),
               axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
               plot.title = element_text(hjust = 0.5, size = 12, margin = margin(t = 10, b = -15)),
               panel.background = element_blank(),
               panel.grid.major = element_blank(), 
               panel.grid.minor = element_blank(), 
               axis.line = element_blank(), legend.position = 'bottom')+    
         scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
                            labels = c("10 \U2013 19.9cm", "20 \U2013 29.9cm", "30 \U2013 39.9cm", 
                                       "40 \U2013 49.9cm", "50 \U2013 59.9cm", "60 \U2013 69.9cm", 
                                       "70 \U2013 79.9cm", "80 \U2013 89.9cm", "90 \U2013 99.9cm", "100+cm"))+
         labs(x = "DBH size classes", y = "Tree Density (stems/ha)")
  
  svg(paste0("./results/20220325/park_plots/tree_sizes/", parkcode, "_diam_dist", ".svg"),
      height = 6, width = 8)

  print(p)

  dev.off()
  cat(parkcode, "\n")
})


#----- Tree size classes (10-40) -----
# Fake plot to customize trend legend
sign_color = c("#979797", "#686868", "#6FAB6A", "#BB6363")
leg_line <- ggplot(data = data.frame(sign = c("notmod", "nonsign", "signinc", "signdec"),
                                     x = c(1, 2, 3, 4)), 
                   aes(x = x, y = x, color = sign, fill = sign, linetype = sign, shape = sign))+
  theme_bw()+
  geom_line(size = 0.5)+
  geom_point(size = 1, shape = 21, alpha = 0.8)+
  scale_fill_manual(values = c("notmod" = "white", "nonsign" =  sign_color[2],
                               "signinc" = sign_color[3], "signdec" = sign_color[4]), 
                    labels = c("Not Modeled", "Not Significant", "Sign. Increase", "Sign. Decrease"),
                    name = "Trends:",
                    drop = FALSE)+
  scale_color_manual(values = c("notmod" = "#CACACA", "nonsign" = "black",
                                "signinc" = sign_color[3], "signdec" = sign_color[4]), 
                     labels = c("Not Modeled", "Not Significant", "Sign. Increase", "Sign. Decrease"),
                     name = "Trends:",
                     drop = FALSE)+
  scale_linetype_manual(values = c("notmod" = 'dashed', "nonsign" = 'dashed',
                                   "signinc" = 'solid', "signdec" = 'solid'), 
                        labels = c("Not Modeled", "Not Significant", "Sign. Increase", "Sign. Decrease"),
                        name = "Trends:",
                        drop = FALSE)+
  theme(legend.position = 'bottom', 
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10),
        #legend.background = element_blank()
        legend.background = element_rect(colour = "#B9B9B9", size = 0.25)
  )

# Extracting custom legends from fake plots
leg_gline <- gtable_filter(ggplot_gtable(ggplot_build(leg_line)), "guide-box")

met_size_list <- c("tree_10cm_BA_Total", "tree_20cm_BA_Total", "tree_30cm_BA_Total", "tree_40cm_BA_Total",
                   "tree_10cm_Dens_Total", "tree_20cm_Dens_Total", "tree_30cm_Dens_Total", "tree_40cm_Dens_Total")
park_ls

#Text grobs for titles
#Text grobs for titles
t10 <- textGrob("   DBH Class: 10 \U2013 19.9cm") #spaces are hacky way to align text better
t20 <- textGrob("   DBH Class: 20 \U2013 29.9cm") 
t30 <- textGrob("   DBH Class: 30 \U2013 39.9cm") 
t40 <- textGrob("   DBH Class: 40 \U2013 49.9cm")

walk(park_ls, function(park){
  parkcode = as.character(park_met_list %>% filter(Unit_Code == park) %>% select(Unit_Code) %>% unique())
  cat(parkcode, "\n")
  
  park_df <- boot_results %>% filter(park == parkcode)
  park_ba <- park_df %>% filter(resp %in% c("tree_10cm_BA_Total", "tree_20cm_BA_Total", 
                                            "tree_30cm_BA_Total", "tree_40cm_BA_Total")) %>% 
             summarize(ba_max = round(max(upper95)+5,-1)) # y range rounded up to nearest 10
  
  park_dens <- park_df %>% filter(resp %in% c("tree_10cm_Dens_Total", "tree_20cm_Dens_Total", 
                                              "tree_30cm_Dens_Total", "tree_40cm_Dens_Total")) %>% 
               summarize(dens_max = round(max(upper95)+5,-1)) # y range rounded up to nearest 10
  
  plots <- map(seq_along(met_size_list), function(met){
    
    ymax <- as.numeric(ifelse(met < 5, park_ba, park_dens))
    
    df <- boot_results %>% filter(resp == met_size_list[met]) %>%
      filter(park == parkcode)
    
    title = as.character(park_met_list %>% filter(metrics == met_size_list[met]) %>% 
                           select(metric_names) %>% unique())
    
    p <- plot_trend_response(df, xlab = "Cycle", ylab = title, model_type = 'lmer') +
         scale_x_continuous(breaks = c(1, 2, 3), labels = c("1", "2", "3"))+
         ylim(0, ymax)

  }) %>% set_names(met_size_list) # map
  
  svg(paste0("./results/20220325/park_plots/tree_sizes/", parkcode, "_size_metrics", ".svg"),
      height = 6, width = 10)
  
  grid.arrange(grobs = list(t10, t20, t30, t40,
                            plots[[1]], plots[[2]], plots[[3]], plots[[4]],
                            plots[[5]], plots[[6]], plots[[7]], plots[[8]],
                            leg_gline),
               heights = c(0.5, 3, 3, 0.1, 0.3, 0.1),
               widths = c(0.1, 3, 3, 3, 3, 0.1),
               layout_matrix = rbind(c(NA, 1, 2, 3, 4, NA),
                                     c(NA, 5, 6, 7, 8, NA),
                                     c(NA, 9, 10, 11, 12, NA),
                                     c(NA, NA, NA, NA, NA, NA),
                                     c(NA, NA, 13, 13, NA, NA),
                                     c(NA, NA, NA, NA, NA, NA)) #makes sure legend bottom isn't cut off
  )
  
  dev.off()
  
})

#----- Similarity -----
# Use legend from leg_line in previous section
leg_gline <- gtable_filter(ggplot_gtable(ggplot_build(leg_line)), "guide-box")
metrics

met_sim_list <- c("Sor_sap", "Hor_sap", "Sor_seed", "Hor_seed")
met_sim_names <- c("Saplings vs. Canopy: Sorensen", "Saplings vs. Canopy: Horn",
                   "Seedlings vs. Canopy: Sorensen", "Seedlings vs. Canopy: Horn")
park_ls

walk(park_ls, function(park){
  parkcode = as.character(park_met_list %>% filter(Unit_Code == park) %>% select(Unit_Code) %>% unique())
  cat(parkcode, "\n")
  
  plots <- map(seq_along(met_sim_list), function(met){
    
    df <- boot_results %>% filter(resp == met_sim_list[met]) %>%
      filter(park == parkcode)
    
    title = as.character(park_met_list %>% filter(metrics == met_sim_list[met]) %>% 
                           select(metric_names) %>% unique())
    
    p <- plot_trend_response(df, xlab = "Cycle", ylab = title, model_type = 'lmer') +
      scale_x_continuous(breaks = c(1, 2, 3), labels = c("1", "2", "3"))+ ylim(0,1)
    
  }) %>% set_names(met_sim_list) # map
  
  svg(paste0("./results/20220325/park_plots/sim/", parkcode, "_sim", ".svg"),
      height = 3, width = 10)
  
  grid.arrange(grobs = list(plots[[1]], plots[[2]], plots[[3]], plots[[4]],
                            leg_gline),
               heights = c(3, 0.1, 0.3, 0.1),
               widths = c(0.1, 3, 3, 3, 3, 0.1),
               layout_matrix = rbind(c(NA, 1, 2, 3, 4, NA),
                                     c(NA, NA, NA, NA, NA, NA),
                                     c(NA, NA, 5, 5, NA, NA),
                                     c(NA, NA, NA, NA, NA, NA)) #makes sure legend bottom isn't cut off
  )
  
  dev.off()
  
})

#---- Deer Browse Index -----
dbi_sum <- dens_df %>% group_by(Unit_Code, Network, cycle, DBI) %>% 
  summarize(num_plots = sum(!is.na(DBI)), .groups = 'drop') %>% 
  pivot_wider(names_from = DBI, values_from = num_plots, 
              names_glue = "DBI_{.name}" ,values_fill = 0) %>% 
  select(-DBI_NA) %>% 
  pivot_longer(cols = DBI_2:DBI_5, names_to = "DBI", values_to = "num_plots")

walk(park_ls, function(park){
  parkcode = as.character(park_met_list %>% filter(Unit_Code == park) %>% select(Unit_Code) %>% unique())
  
  df_park <- dbi_sum %>% filter(Unit_Code == parkcode) 
  
  dbi_bar <- 
    ggplot(df_park, aes(x = cycle, y = num_plots, fill = as.factor(DBI), color = as.factor(DBI)))+ 
    geom_bar(position = 'fill', stat = 'identity', na.rm = TRUE)+
    scale_color_manual(values = c("#05e689", "#efdf00", "#f94b24", "#a60808"),
                       labels = c("Low", "Medium", "High", "Very High"), name = "Deer Browse Impact:")+
    scale_fill_manual(values = c("#05e689", "#efdf00", "#f94b24", "#a60808"),
                      labels = c("Low", "Medium", "High", "Very High"), name = "Deer Browse Impact:")+
    
    scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1.00), labels = c(0, 25, 50, 75, 100))+
    theme_bw()+
    theme(axis.text = element_text(size = 11), 
          axis.title = element_text(size = 11),
          plot.title = element_text(hjust = 0.5, size = 12, margin = margin(t = 10, b = -15)),
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.line = element_blank(),
          legend.background = element_rect(colour = "#B9B9B9", size = 0.25),
          legend.position = 'bottom') + 
    labs(x = "Cycle", y = "Proportion of Plots")#+
  
  svg(paste0("./results/20220325/park_plots/indices/", parkcode, "_dbi", ".svg"),
      height = 6, width = 8)
  
  print(dbi_bar)
  
  dev.off()
  cat(parkcode, "\n")
})


#---- Stocking Index -----
walk(park_ls, function(park){
  parkcode = as.character(park_met_list %>% filter(Unit_Code == park) %>% select(Unit_Code) %>% unique())

  df_park <- boot_results %>% filter(park == parkcode) %>% filter(resp == "stock_final") 

  p <- plot_trend_response(df_park, xlab = "Cycle", ylab = "Stocking Index", model_type = 'lmer') +
       scale_x_continuous(breaks = c(1, 2, 3), labels = c("1", "2", "3"))#+
       # geom_hline(yintercept = 25, linetype = 2, lwd = 0.8, color = 'dimgrey')+
       # geom_text(x = 3.2, y = 22.5, label = "Severely Understocked", hjust = 1, size = 3.5, color = 'dimgrey')+
       # geom_hline(yintercept = 100, linetype = 4, lwd = 0.8, color = "dimgrey")+
       # geom_text(x = 3.2, y = 104, hjust = 1, label = "Sufficiently Stocked", size = 3.5, color = 'dimgrey')
  
  svg(paste0("./results/20220325/park_plots/indices/", parkcode, "_stock_noline", ".svg"),
      height = 6, width = 8)

  grid.arrange(grobs = list(p, leg_gline),
               heights = c(4.85, 0.1, 0.25, 0.1),
               widths = c(0.5, 3, 0.25),
               layout_matrix = rbind(c(1, 1, 1),
                                     c(NA, NA, NA),
                                     c(NA, 2, NA)),
                                     c(NA, NA, NA) #makes sure legend bottom isn't cut off
               )

  dev.off()
  cat(parkcode, "\n")
})


