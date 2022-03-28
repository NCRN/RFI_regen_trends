#---------------------------------------------------------------------------------------------------
# Plotting trends from case bootstrap output 
#   Code written by Kate Miller 20211012 
#---------------------------------------------------------------------------------------------------
library(forestTrends)
library(tidyverse)
library(grid) # for digging into str of ggplot grobs
library(gridExtra) # for ggarrange
library(gtable)
library(stringr) #for word()

options("scipen" = 100, "digits" = 4) # keeps scientific notation from showing up

datapath <- "./data/"
dens_df1 <- read.csv(paste0(datapath, "EFWG_full_dataset_20220325.csv")) %>% select(-lat_rank)
spp_df <- read.csv(paste0(datapath, "EFWG_species_dataset_20220325.csv")) %>% select(-lat_rank)
dens_df2 <- left_join(dens_df1, spp_df, by =  c("Plot_Name", "Unit_Code", "Year", "Network", 
                                               "cycle", "excludeEvent"))
mg_df <- read.csv(paste0(datapath, "EFWG_macrogroup_plot_lat_rank.csv")) 
mg_df_simp <- mg_df %>% select(MACROGROUP_NAME, mg_short, lat_rank) %>% unique()

dens_df <- left_join(mg_df, dens_df2, by = "Plot_Name")
dens_df$mg_ord <- reorder(dens_df$mg_short, desc(dens_df$lat_rank))

names(dens_df)

dens_df <- dens_df[,c(1:18, 109, 19:108)]
dens_c3 <- dens_df %>% filter(cycle == 3) 

boot_results1 <- read.csv("./results/20220325/all_metrics_randint_macrogroup_20220325.csv")
head(mg_df)
head(boot_results1)

boot_results <- left_join(mg_df_simp, boot_results1, by = c("MACROGROUP_NAME" = "Macrogroup")) 
                          
boot_results$mg_ord <- reorder(boot_results$mg_short, desc(boot_results$lat_rank))

# Percent Composition
comp <- read.csv("./data/EFWG_proportion_regen_20220325_mg.csv")
names(comp)
names(dens_c3)
head(dens_c3)
comp_natcan <- left_join(comp, dens_c3 %>% select(mg_short, lat_rank) %>% unique(),
                         by = c("mg_short", 'lat_rank')) #%>% 
  # filter(Species == "Native Species") %>% # actually Native Canopy
  # filter(Metric %in% c("Sapling Density", "Seedling Density"))   
head(comp_natcan)
#---- Set up lists to iterate over ----
# column names for response
names(dens_df)
head(dens_df)
#metrics <- c(names(dens_df[, c(18:ncol(dens_df))]))

metrics <- c(names(dens_df[, c(20:109)]))
metrics
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
                  "Horn Seedling",
                  rep("Tree BA (sq.m/ha)", 4),
                  rep("Tree Dens. (stems/ha)", 4),
                  rep("Sapling BA (sq.m/ha)", 4),
                  rep("Sapling Density (stems/sq.m)", 4),
                  rep("Seedling Density (stems/sq.m)", 4)
)

# match metric columns and titles
met_df <- data.frame(metrics, metric_names)
met_df

mg_met_list <- data.frame(expand.grid(unique(dens_df$mg_short), metrics)) %>% 
  set_names("mg_short", "metrics") %>% 
  arrange(mg_short, metrics) %>% 
  mutate(across(where(is.factor), as.character)) %>% 
  left_join(., met_df, by = "metrics")

mg_list <- c(mg_met_list[,1])
met_list <- c(mg_met_list[,2])

boot_results$strp_col <- case_when(boot_results$mg_short == "Acadian Mixed" ~ "#9883CA", 
                                   boot_results$mg_short == "Appalachian Mesic" ~ "#92CA83",
                                   boot_results$mg_short == "Coastal Plain" ~ "#E7DA7E",
                                   boot_results$mg_short == "Floodplain" ~ "#83CAB2",
                                   boot_results$mg_short == "Northern Oak - Pine" ~ "#90B2EC", 
                                   boot_results$mg_short == "Northern Ruderal" ~ "#E4ACE1", 
                                   boot_results$mg_short == "Southern Oak - Pine" ~ "#F6CC78", 
                                   boot_results$mg_short == "Southern Ruderal" ~ "#FF6960") 

table(boot_results$mg_short)

#----- Setting up colors to color code facets by network 
# First matching networks to colors
# mg_cols <- #rbind(#data.frame(mg_ord = "BLNK", strp_col = "white"), # place holder for BR blank in facet
#             data.frame(unique(data.frame(mg_ord = boot_results$mg_ord,
#                                          strp_col = boot_results$strp_col))) %>% arrange(desc(mg_ord))


# mg_cols
# 
# mg_cols <- mg_cols %>% mutate(facet_ord = c(rev(42:45), rev(46:49))) %>%
#   arrange(facet_ord)
# 
# # fills <- c(mg_cols$strp_col) # for last strip that's blank b/c 39 not 40 parks
# # fills

# Fake plots to customize the network legend
# leg_net <- ggplot(data = data.frame(network = c("ERMN", "MIDN", "NCBN", "NCRN", "NETN"),
#                                     x = c(1, 2, 3, 4, 5)),
#                   aes(x = x, fill = network))+
#   geom_histogram()+
#   scale_fill_manual(values = 
#                       c("Acadian Mixed" = "#9883CA", 
#                         "Appalachian Mesic" = "#92CA83",
#                         "Coastal Plain" = "#E7DA7E",
#                         "Floodplain" = "#83CAB2",
#                         "Northern Oak - Pine" = "#90B2EC", 
#                         "Northern Ruderal" = "#E4ACE1", 
#                         "Southern Oak - Pine" = "#F6CC78", 
#                         "Southern Ruderal" = "#FF6960"), 
#                     name = "Groups:",
#                     drop = FALSE)+
#   theme_bw()+
#   theme(legend.position = 'bottom', 
#         legend.title = element_text(size = 8), 
#         legend.text=element_text(size = 8),
#         #legend.background = element_blank()
#         legend.background = element_rect(color = "#B9B9B9", size = 0.25)
#   )

sign_color = c("#979797", "#686868", "#6FAB6A", "#BB6363")

# Fake plot to customize trend legend
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
        legend.title = element_text(size = 8), 
        legend.text = element_text(size = 8),
        #legend.background = element_blank()
        legend.background = element_rect(colour = "#B9B9B9", size = 0.25)
  )

# Extracting custom legends from fake plots
leg_gline <- gtable_filter(ggplot_gtable(ggplot_build(leg_line)), "guide-box")
leg_gnet <- gtable_filter(ggplot_gtable(ggplot_build(leg_net)), "guide-box")

# Plot and save the results for each metric that facets on park with fixed axes

walk(metrics, function(metric){
  folder = ifelse(grepl("FRAX|FAGGRA|TSUCAN|ASITRI", metric), "TSS_by_species", "TSS_by_sppgrp")
  title = as.character(met_df$metric_names[met_df$metrics == metric])
  p <- plot_trend_response(df = boot_results %>% filter(resp == metric), 
                           xlab = "Cycle", ylab = title, group = "mg_ord",
                           model_type = "lmer", facet_cols = 4, 
                           sign_color = c("#979797", "#686868", "#6FAB6A", "#BB6363"))+
    scale_x_continuous(breaks = c(1, 2, 3), labels = c("1", "2", "3"))
  cat(metric, "\n")
  
  # color-coding facet strips
  # g <- ggplot_gtable(ggplot_build(p))
  # strp_col <- which(grepl('strip-t', g$layout$name))
  # 
  # k <- 1
  # for (i in strp_col) {
  #   g$grobs[[i]]$grobs[[1]]$children[[1]]$gp$fill <- fills[k]
  #   k <- k+1
  # }
  # 
  
  svg(paste0("./results/20220325/", folder, "/", metric, "_mg.svg"), 
      height = 8, width = 11)  
  
  grid.arrange(grobs = list(p, leg_gline),
               heights = c(6.5, 0.5, 0.01),
               widths = c(0.6, 5.1, 0.6),
               layout_matrix = rbind(c(1, 1, 1),
                                     c(NA, 2, NA),
                                     c(NA, NA, NA)
                                     ))
  
  dev.off()
})


# Plot and save the results for each metric that facets on park with free_y axes
walk(metrics, function(metric){
  title = as.character(met_df$metric_names[met_df$metrics == metric])
  p <- plot_trend_response(df = boot_results %>% filter(resp == metric), 
                           xlab = "Cycle", ylab = title, group = "mg_ord",
                           model_type = "lmer", facet_cols = 4, facet_scales = 'free_y',
                           sign_color = c("#D3D3D3", "#BABABA", "#AACCA7", "#CD9494"))+
    scale_x_continuous(breaks = c(1, 2, 3), labels = c("1", "2", "3"))
  cat(metric, "\n")
  
  # g <- ggplot_gtable(ggplot_build(p))
  # strp_col <- which(grepl('strip-t', g$layout$name))
  # 
  # k <- 1
  # for (i in strp_col) {
  #   g$grobs[[i]]$grobs[[1]]$children[[1]]$gp$fill <- fills[k]
  #   k <- k+1
  # }
  
  svg(paste0("./results/20220325/TSS_by_sppgrp/free_y/", metric, "_free_y_mg.svg"), 
      height = 8, width = 11) 
  
  grid.arrange(grobs = list(p, leg_gline),
               heights = c(6.5, 0.5, 0.01),
               widths = c(0.6, 5.1, 0.6),
               layout_matrix = rbind(c(1, 1, 1),
                                     c(NA, 2, NA),
                                     c(NA, NA, NA)
               ))
  
  
  dev.off()
})

#---- Coefficient plots for each metric -----
# metrics
# met_df
# 
# walk(metrics, function(metric){
#   
#   title = as.character(met_df$metric_names[met_df$metrics == metric])
#   met = as.character(met_df$metrics[met_df$metrics == metric])
#   
#   p <- plot_slopes(df = boot_results, 
#                    ylabel = title,
#                    metric = met,
#                    order = "park_ord",
#                    group = "Network",
#                    sign_only = TRUE, 
#                    legend_position = 'bottom')
#   
#   ggsave(paste0(datapath, "/results/20220325/coef_plots/", metric, "_sign_only.svg"), 
#          height = 8, width = 11, units = 'in')
# 
# 
#   p <- plot_slopes(df = boot_results, 
#                    ylabel = title,
#                    metric = met,
#                    order = "park_ord",
#                    group = "Network",
#                    sign_only = FALSE, 
#                    legend_position = 'bottom')
#   
#   ggsave(paste0(datapath, "/results/20220325/coef_plots/all/", metric, ".svg"), 
#          height = 8, width = 11, units = 'in')
#   
#   cat(metric, "\n")  
#   })

# Plot function for macrogroup-level plot for tree metrics by spp type
plot_trends_by_grp <- function(df, xlab, ylab, group, var, facet_scales = 'fixed'){ 
  
  if(!is.na(group)){group_sym <- sym(group)}
  var_sym <- sym(var)
  
  df$time <- as.numeric(gsub("\\D", "", df$term))
  
  df1 <- df %>% filter(metric == var) 
  
  df_sign <- df1 %>% filter(term == "Slope") %>% 
    mutate(sign = case_when(lower95 > 0 | upper95 < 0 ~ "sign",
                            is.na(lower95) ~ "notmod",
                            TRUE ~ "nonsign")) %>% select(mg_short, !!group_sym, metric, sign)
  
  df2 <- 
    left_join(df1 %>% filter(!term %in% c("Slope", "Intercept")), 
              df_sign, 
              by = c("mg_short", group, "metric")) %>%
    filter(!term %in% c("Intercept", "Slope"))
  
  # hacky way to plot groups that didn't get modeled and so don't have errorbars or ribbons
  df2$upper95 <- ifelse(is.na(df2$upper95), df2$estimate, df2$upper95)
  df2$lower95 <- ifelse(is.na(df2$lower95), df2$estimate, df2$lower95)
  
  p <- 
    ggplot(df2, aes(x = time, y = estimate))+ 
    facet_wrap(~mg_ord, scales = facet_scales, ncol = 4)+
    geom_point(aes(color = factor(!!group_sym), shape = !!group_sym, fill = !!group_sym), 
               size = 2, na.rm = TRUE)+
    geom_errorbar(aes(ymin = lower95, ymax = upper95, x = time,
                      colour = !!group_sym), width = 0.1, size = 0.5, na.rm = TRUE)+
    geom_line(aes(y = estimate, x = time, colour = !!group_sym, linetype = sign), na.rm = TRUE)+
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

#---- Combine most important species groups into 1 plot per park
# Set up lists to iterate over (exclude metrics that don't involve species groups) 
sort(unique(boot_results$resp))
boot_sppgrp <- boot_results %>% filter(!resp %in% c("Hor_sap", "Hor_seed", "Sor_sap", "Sor_seed", "stock_final"))

# Set up groups
boot_sppgrp$sppgrp <- gsub("^.*_", "", boot_sppgrp$resp)
sppgrp_list <- c(paste0("_", unique(boot_sppgrp$sppgrp), collapse = "|"))
boot_sppgrp$metric <- gsub(sppgrp_list, "", boot_sppgrp$resp)

boot_sppgrp_comb <- boot_sppgrp %>% filter(sppgrp %in% c("NatCan", "NatOth", "Exotic")) %>% arrange(lat_rank, resp, term)
boot_sppgrp_comb$mg_ord <- reorder(boot_sppgrp_comb$mg_short, desc(boot_sppgrp_comb$lat_rank))
boot_sppgrp_comb$estimate[boot_sppgrp_comb$estimate == 0] <- NA
boot_sppgrp_comb$lower95[is.na(boot_sppgrp_comb$estimate)] <- NA
boot_sppgrp_comb$upper95[is.na(boot_sppgrp_comb$estimate)] <- NA

metrics <- c(unique(boot_sppgrp_comb$metric))

# response titles for plotting
metric_names <- c("Sapling BA (sq.m/ha)",
                  "Sapling Density (stems/sq.m)",
                  "Seedling Density (stems/sq.m)",
                  "Tree BA 10 cm (sq.m/ha)",  
                  "Tree Dens. 10 cm (stems/ha)",  
                  "Tree BA 20 cm (sq.m/ha)", 
                  "Tree Dens. 20 cm (stems/ha)", 
                  "Tree BA 30 cm (sq.m/ha)",
                  "Tree Dens. 30 cm (stems/ha)",
                  "Tree BA 40 cm+ (sq.m/ha)",
                  "Tree Dens. 40 cm+ (stems/ha)",
                  "Tree BA (sq.m/ha)", 
                  "Tree Density (stems/ha)")

# Fake plot for trend legend
leg_line2 <- ggplot(data = data.frame(sign = c("notmod", "nonsign", "sign"),
                                      x = c(1, 2, 3)), 
                    aes(x = x, y = x, color = sign, fill = sign, linetype = sign))+
  theme_bw()+ geom_line(size = 0.5)+ # geom_point(size = 1.5, shape = 21, alpha = 0.8)+
  scale_fill_manual(values = c("notmod" = "#686868", "nonsign" =  "#686868", "sign" = "#686868"), 
                    labels = c("Not Modeled", "Not Sign.", "Sign."),
                    name = "Trends:",
                    drop = FALSE)+
  scale_color_manual(values = c("notmod" = "#686868", "nonsign" =  "#686868", "sign" = "#686868"), 
                     labels = c("Not Modeled", "Not Sign.", "Sign."),
                     name = "Trends:",
                     drop = FALSE)+
  scale_linetype_manual(values = c("notmod" = 'dotted', "nonsign" = 'dashed', "sign" = 'solid'), 
                        labels = c("Not Modeled", "Not Sign.", "Sign."),
                        name = "Trends:",
                        drop = FALSE)+
  theme(legend.position = 'bottom', legend.title = element_text(size = 7), 
        legend.text = element_text(size = 7), #legend.background = element_blank()
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
  theme(legend.position = 'bottom', legend.title = element_text(size = 7), 
        legend.text = element_text(size = 7), #legend.background = element_blank()
        legend.background = element_rect(colour = "#B9B9B9", size = 0.25)
  )

leg_gline2 <- gtable_filter(ggplot_gtable(ggplot_build(leg_line2)), "guide-box")
leg_glinesp <- gtable_filter(ggplot_gtable(ggplot_build(leg_linesp)), "guide-box")

# match metric columns and titles
met_df <- data.frame(metrics = metrics, metric_names)
met_df
#metric <- metrics[1]

walk(metrics, function(metric){
  title = as.character(met_df$metric_names[met_df$metrics == metric])
  var = as.character(met_df$metrics[met_df$metrics == metric])
  sppgrp = as.character(unique(boot_sppgrp_comb$sppgrp[boot_sppgrp_comb$metric == metric]))
  
  p <- plot_trends_by_grp(boot_sppgrp_comb, 
                          xlab = "Cycle", ylab = title,
                          group = "sppgrp", var = var, 
                          facet_scales = 'fixed')+
    scale_x_continuous(breaks = c(1, 2, 3), labels = c("1", "2", "3"))
  
  
  svg(paste0("./results/20220325/spp_grp_plots/", metric, "_mg.svg"), 
      height = 8, width = 11) 
  
  grid.arrange(grobs = list(p, leg_glinesp, leg_gline2),
               heights = c(6.5, 0.5),
               widths = c(1, 3.5, 0.01, 3.5, 1),
               layout_matrix = rbind(c(1, 1, 1, 1, 1),
                                     c(NA, 2, NA, 3, NA)))
  dev.off()
  cat(metric, "\n")  
})

walk(metrics, function(metric){
  title = as.character(met_df$metric_names[met_df$metrics == metric])
  var = as.character(met_df$metrics[met_df$metrics == metric])
  sppgrp = as.character(unique(boot_sppgrp_comb$sppgrp[boot_sppgrp_comb$metric == metric]))
  
  p <- plot_trends_by_grp(boot_sppgrp_comb, 
                          xlab = "Cycle", ylab = title,
                          group = "sppgrp", var = var, 
                          facet_scales = 'free_y')+
    scale_x_continuous(breaks = c(1, 2, 3), labels = c("1", "2", "3"))

  svg(paste0("./results/20220325/spp_grp_plots/free_y/", metric, "_free_y_mg.svg"), 
      height = 8, width = 11) 
  
  grid.arrange(grobs = list(p, leg_glinesp, leg_gline2),
               heights = c(6.5, 0.5),
               widths = c(1, 3.5, 0.01, 3.5, 1),
               layout_matrix = rbind(c(1, 1, 1, 1, 1),
                                     c(NA, 2, NA, 3, NA)))
  dev.off()
  cat(metric, "\n")  
})

table(dens_df$DBI)
#DBI_cols <- c("#05e689", "#efdf00", "#f94b24", "#a60808")

dbi_sum <- dens_df %>% group_by(mg_short, cycle, DBI) %>% 
  summarize(num_plots = sum(!is.na(DBI)), .groups = 'drop') %>% 
  pivot_wider(names_from = DBI, values_from = num_plots, 
              names_glue = "DBI_{.name}" ,values_fill = 0) %>% 
  select(-DBI_NA) %>% 
  pivot_longer(cols = DBI_2:DBI_5, names_to = "DBI", values_to = "num_plots")

head(dbi_sum)
head(dens_df)

dbi_bar <- 
  ggplot(dens_df, aes(x = cycle, y = DBI, fill = as.factor(DBI), color = as.factor(DBI)))+ 
  facet_wrap(~mg_ord, ncol = 4)+
  geom_bar(position = 'fill', stat = 'identity', na.rm = TRUE)+
  scale_color_manual(values = c("#05e689", "#efdf00", "#f94b24", "#a60808"),
                     labels = c("Low", "Medium", "High", "Very High"), name = "Deer Browse Impact")+
  scale_fill_manual(values = c("#05e689", "#efdf00", "#f94b24", "#a60808"),
                    labels = c("Low", "Medium", "High", "Very High"), name = "Deer Browse Impact")+
  
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1.00), labels = c(0, 25, 50, 75, 100))+
  theme_bw()+
  theme(axis.text = element_text(size = 11), 
        axis.title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 12, margin = margin(t = 10, b = -15)),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_blank(), 
        legend.position = 'none') + 
  labs(x = "Cycle", y = "Proportion of Plots")#+

dbi_leg <- ggplot(dens_df %>% filter(Unit_Code == "WEFA"), aes(x = cycle, y = DBI, fill = as.factor(DBI)))+ 
  geom_bar(position = 'fill', stat = 'identity', na.rm = TRUE)+
  scale_fill_manual(values = c("#05e689", "#efdf00", "#f94b24", "#a60808"),
                    labels = c("Low", "Medium", "High", "Very High"), name = "Deer Browse Impact")+
  
  theme_bw()+
  theme(legend.position = 'bottom', 
        legend.title = element_text(size = 9), 
        legend.text = element_text(size = 9),
        legend.background = element_rect(colour = "#B9B9B9", size = 0.25)) 

leg_gdbi <- gtable_filter(ggplot_gtable(ggplot_build(dbi_leg)), "guide-box")

svg("./results/DBI_by_cycle_mg.svg", 
    height = 8, width = 11) 

grid.arrange(grobs = list(dbi_bar, leg_gdbi),
             heights = c(6.5, 0.5),
             widths = c(0.5, 7, 0.5),
             layout_matrix = rbind(c(1, 1, 1),
                                   c(NA, 2, NA)))

dev.off()

head(boot_results)

#----- RESULTS GRID -----

# Trend metrics
tile_metrics <- data.frame(resp = c("Tree_BA_Total", "Tree_Dens_Total", "Sap_BA_Total", "Sap_Dens_Total", 
                                    "Seed_Dens_Total",
                                    "Tree_BA_NatCan", "Tree_Dens_NatCan", "Sap_BA_NatCan", "Sap_Dens_NatCan", 
                                    "Seed_Dens_NatCan",
                                    "Tree_BA_NatOth", "Tree_Dens_NatOth", "Sap_BA_NatOth", "Sap_Dens_NatOth", 
                                    "Seed_Dens_NatOth",
                                    "Tree_BA_Exotic", "Tree_Dens_Exotic", "Sap_BA_Exotic", "Sap_Dens_Exotic", 
                                    "Seed_Dens_Exotic",
                                    "stock_final", 
                                    "Sor_sap", "Hor_sap", "Sor_seed", "Hor_seed"),
                           labels = c("Tree BA", "Tree Density", "Sapling BA", 
                                      "Sapling Density", "Seedling Density", 
                                      "Tree BA", "Tree Density", "Sapling BA", 
                                      "Sapling Density", "Seedling Density", 
                                      "Tree BA", "Tree Density", "Sapling BA", 
                                      "Sapling Density", "Seedling Density", 
                                      "Tree BA", "Tree Density", "Sapling BA", 
                                      "Sapling Density", "Seedling Density", 
                                      "Stocking Index", 
                                      "Sorensen Sapling", "Horn Sapling",
                                      "Sorensen Seedling", "Horn Seedling"),
                           order = 10:34) # first 9 are status metrics
#1:15)

tile_metrics

result_sum <- boot_results %>% filter(term == "Slope") %>% 
  mutate(sign = case_when(lower95 > 0 & !grepl("Exotic", resp) ~ "signinc_good",
                          upper95 < 0 & !grepl("Exotic", resp) ~ "signdec_bad",
                          lower95 > 0 & grepl("Exotic", resp) ~ "signinc_bad",
                          upper95 < 0 & grepl("Exotic", resp) ~ "signdec_good",
                          is.na(lower95) ~ "notmod",
                          TRUE ~ "nonsign")) %>% 
  #select(park, park_ord, resp, estimate, sign, strp_col) %>% 
  left_join(tile_metrics, ., by = "resp") %>% arrange(mg_ord, order)

result_sum2 <- result_sum %>% group_by(mg_ord) %>% 
  mutate(num_sign_bad = sum(sign %in% c("signdec_bad", "signinc_bad"))) %>% ungroup()

#result_sum2$num_sign_bad[result_sum2$park %in% c("SAHI", "WOTR")] <- -1
#result_sum2$label_order <- reorder(result_sum2$labels, desc(result_sum2$order))
#result_sum2$park_order <- reorder(result_sum2$park, desc(result_sum2$num_sign_bad))
result_sum2 <- result_sum2 %>% mutate(metgrp = factor(case_when(grepl("Total", resp) ~ "Total",
                                                                grepl("NatCan|stock", resp) ~ "Native Canopy",
                                                                grepl("NatOth", resp) ~ "Other Native",
                                                                grepl("Exotic", resp) ~ "Exotic",
                                                                grepl("Sor|Hor", resp) ~ "Similarity", 
                                                                TRUE ~ "Unk"),
                                                      levels = c("Total", "Native Canopy", "Other Native",
                                                                 "Exotic", "Similarity")
)) %>% select(mg_short, order, labels, metgrp, sign)
head(result_sum2)
# decided to drop similarity trends because more confusing than helpful
result_sum3 <- result_sum2 %>% filter(metgrp != "Similarity") %>% droplevels()
levels(result_sum3$metgrp)
head(dens_df)

head(comp_natcan)
# 
# comp_natcan_wide <- comp_natcan %>% mutate(Metric = paste0("pctNatCan", word(Metric, 1))) %>% 
#   select(Network, Unit_Code, Metric, Mean) %>% 
#   pivot_wider(names_from = "Metric", values_from = "Mean") 

status_metrics_3a <- dens_df %>% filter(cycle == 3) %>% 
  group_by(mg_short) %>% 
  summarize(avg_sap_dens = mean(Sap_Dens_NatCan, na.rm = T), 
            avg_seed_dens = mean(Seed_Dens_NatCan, na.rm = T),
            avg_stock = mean(stock_final, na.rm = T),
            avg_sor_sap = mean(Sor_sap, na.rm = T),
            avg_sor_seed = mean(Sor_seed, na.rm = T),
            avg_dbi = mean(DBI, na.rm = T)) %>% ungroup() %>% 
  
  mutate(`Sapling Density` = case_when(avg_sap_dens < 0.1 ~ "critical", 
                                       between(avg_sap_dens, 0.1, 0.3) ~ "caution",
                                       avg_sap_dens > 0.3 ~ "acceptable",
                                       TRUE ~ "unknown"),
         `Seedling Density` = case_when(avg_seed_dens < 0.25 ~ "critical", 
                                        between(avg_seed_dens, 0.25, 1.99) ~ "caution",
                                        avg_seed_dens >= 2 ~ "acceptable", 
                                        TRUE ~ 'unknown'),
         `Stocking Index` = case_when(avg_stock < 25 ~ "critical",
                                      between(avg_stock, 25, 100) ~ "caution", 
                                      avg_stock >= 100 ~ "acceptable",
                                      TRUE ~ "unknown"),
         `Sorensen Sapling` = ifelse(avg_sor_sap < 0.2, "critical", "acceptable"),
         `Sorensen Seedling` = ifelse(avg_sor_seed < 0.2, "critical", "acceptable"),
         `Deer Browse Impacts` = case_when(avg_dbi >= 4 ~ 'critical', 
                                           between(avg_dbi, 3.01, 4) ~ 'caution', 
                                           avg_dbi <= 3 ~ 'acceptable',
                                           TRUE ~ "unknown"),
         `Flat Tree Diam. Dist.` = "acceptable") # None of the macrogroups had flat diam dist


status_metrics_3b <- left_join(status_metrics_3a, 
                               comp_natcan %>% select(mg_short, sap_dens_pct_NatCan, seed_dens_pct_NatCan), 
                               by = "mg_short") 

status_metrics_3 <- status_metrics_3b %>% mutate(
  `Sapling Composition` = case_when(sap_dens_pct_NatCan < 0.5 ~ "critical",
                                    between(sap_dens_pct_NatCan, 0.5, 0.7) ~ "caution",
                                    sap_dens_pct_NatCan > 0.7 ~ "acceptable"),
  `Seedling Composition` = case_when(seed_dens_pct_NatCan < 0.5 ~ "critical",
                                     between(seed_dens_pct_NatCan, 0.5, 0.7) ~ "caution",
  seed_dens_pct_NatCan > 0.7 ~ "acceptable")) %>% 
  select(-starts_with("pctNatCan"))

names(status_metrics_3)
# Reorder columns
status_metrics_3 <- status_metrics_3 %>% select(mg_short, avg_sap_dens:`Stocking Index`, 
                                                `Sapling Composition`, `Seedling Composition`,
                                                everything())

# Flat Tree Diam. Dist. was determined by modelling linear and exponential fit to diameter distribution.
# If linear had the lowest AIC, then the distribution has fewer small trees than expected.

status_met_long <- status_metrics_3 %>% select(mg_short, `Sapling Density`:`Flat Tree Diam. Dist.`) %>% 
  pivot_longer(-mg_short, names_to = "labels", values_to = "sign") %>% 
  mutate(metgrp = "Status", order = rep(c(1:2,4:10), 8)) %>%
  select(mg_short, order, labels, metgrp, sign)

head(status_met_long)

# Add proportion of stocked plots based on park average DBI
dens_c3 <- dens_df %>% filter(cycle == 3) 

dbi_stock_c3 <- dens_c3 %>% group_by(mg_short) %>% 
  mutate(avg_DBI = mean(DBI, na.rm = T)) %>% ungroup() %>% 
  mutate(stocked = case_when(avg_DBI <= 3 & stock_final >= 50 ~ 1,
                             avg_DBI > 3 & stock_final >= 100 ~ 1,
                             TRUE ~ 0))

dbi_stock_sum <- dbi_stock_c3 %>% group_by(mg_short) %>% 
  summarize(avg_stock = mean(stock_final, na.rm = TRUE),
            avg_DBI = first(avg_DBI),
            num_stocked = sum(stocked, na.rm = TRUE),
            num_plots = sum(!is.na(stocked)),
            prop_stocked = num_stocked/num_plots) %>% 
  mutate(order = 3)

table(dbi_stock_sum$avg_DBI)

prop_stock <- dbi_stock_sum %>% mutate(labels = "% Stocked Plots",
                                       metgrp = "Status", 
                                       sign = case_when(prop_stocked < 0.33 ~ "critical", 
                                                        between(prop_stocked, 0.33, 0.669) ~ 'caution', 
                                                        prop_stocked > 0.67 ~ "acceptable",
                                                        TRUE ~ 'unknown')) %>% 
  select(mg_short, order, labels, metgrp, sign)

head(prop_stock)
head(status_met_long)

#results_comb <- rbind(status_met_long, prop_stock, result_sum3) %>% arrange(park, order) 

# 4 groups instead of 5

# reg_stat_1 <- c("ANTI", "CATO", "CHOH", "GEWA", "HAFE", "MANA", "MIMA", "MORR", "ROVA", "SAHI", "THST", "VAFO", "WEFA")
# reg_stat_2 <- c("COLO", "FRSP", "HOFU", "NACE", "PRWI", "ROCR", "SARA")
# reg_stat_3 <- c("ALPO", "GWMP", "JOFL", "MONO", "RICH", "WOTR")
# reg_stat_4 <- c("APCO", "BLUE", "BOWA", "DEWA", "FONE", "FRHI", "GARI", "MABI", "NERI", "PETE", "SAGA")
# reg_stat_5 <- c("ACAD", "GETT")

results_comb <- rbind(status_met_long, prop_stock, result_sum3) %>% arrange(mg_short, order) 

# results_comb <- results_comb %>% 
#   mutate(park_reggrp = case_when(park %in% reg_stat_1 ~ "Imminent Failure",
#                                  park %in% c(reg_stat_2, reg_stat_3) ~ "Probable Failure",
#                                  park %in% reg_stat_4 ~ "Insecure", 
#                                  park %in% reg_stat_5 ~ "Secure",
#                                  TRUE ~ NA_character_))

head(results_comb)

results_comb$label_order <- factor(results_comb$labels, 
                                   levels = c("Tree BA", "Tree Density", "Sapling BA", "Sapling Density", "Seedling Density", 
                                              "% Stocked Plots", "Stocking Index","Deer Browse Impacts", "Flat Tree Diam. Dist.",
                                              "Sapling Composition", "Seedling Composition", 
                                              "Sorensen Sapling", "Sorensen Seedling"))

results_comb$metgrp <- factor(results_comb$metgrp, levels = c("Status", "Total", "Native Canopy", "Other Native",
                                                              "Exotic"))

# results_comb$park_reggrp <- factor(results_comb$park_reggrp, levels = c("Imminent Failure", "Probable Failure", "Insecure", "Secure"))
results_final <- left_join(mg_df_simp %>% select(-MACROGROUP_NAME), 
                           results_comb, by = "mg_short") %>% 
  arrange(lat_rank, metgrp, label_order)

results_final$mg_ord <- reorder(results_final$mg_short, desc(results_final$lat_rank))

results_plot <- 
  ggplot(results_final, aes(x = mg_ord, y = label_order))+
  geom_tile(aes(fill = sign), color = 'grey')+
  geom_text(aes(label = case_when(sign == "critical" ~ "●",
                                  sign == "caution" ~ "○",
                                  sign == "signdec_bad" ~ "-",
                                  sign == "signinc_bad" ~ "+", 
                                  sign == "signdec_good" ~ "-",
                                  sign == "signinc_good" ~ "+", 
                                  TRUE ~ "")))+
  facet_grid(metgrp ~ mg_ord, scales = 'free', space = 'free', switch = "y")+
  scale_fill_manual(values = c('critical' = "#FF5B5B", 
                               'caution' = "#FFFF79",
                               'acceptable' = '#BDEBA7',
                               "signdec_bad" = "#CD5C5C", 
                               "signinc_good" = "#7DD66D", #"#228B22",
                               "signdec_good" = "#6DA6D6", 
                               "signinc_bad" = "#FF8C00",
                               "nonsign" = "#E3E3E3", "notmod" = "white"),
                    labels = c("●          Status: Critical",
                               "○          Status: Caution",
                               "Status: Acceptable",
                               " -         Sign. Decline (Native)",
                               "+          Sign. Increase (Native)",
                               " -         Sign. Decline (Exotic)",
                               "+          Sign. Increase (Exotic)",
                               "Not Significant",
                               "Not Modeled"),
                    name = NULL)+
  scale_x_discrete(position = 'top')+
  scale_y_discrete(limits = rev)+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        #axis.text.x = element_text(angle = 45, hjust = 0, size = 10),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 9),
        strip.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 8.5),
        text = element_text(size = 9),
        #strip.placement = 'outside',
        #strip.background.x = element_blank(), 
        #strip.text.x = element_blank(),
        legend.spacing.x = unit(0.5, 'cm'),
        legend.position = 'bottom',
        legend.title = element_text(size = 9), 
        legend.text = element_text(size = 9, margin = margin(l = -0.9, unit = 'cm')),
        legend.background = element_rect(color = "#B9B9B9", size = 0.25))+
  labs(x = NULL, y = NULL)+ 
  guides(fill = guide_legend(nrow = 3, byrow = FALSE, hjust = 1))

results_plot

ggsave("./results/20220325/results_grid_symbols_mg.svg", 
       height = 8, width = 11, units = 'in')

# Had to open the svg in notepad and tweak the legend by hand to get symbols and
# font to line up correctly.

