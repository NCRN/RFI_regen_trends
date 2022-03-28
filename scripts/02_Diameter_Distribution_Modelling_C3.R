library(tidyverse)
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
})

AIC_test$best_mod <- ifelse(AIC_test$linear < AIC_test$exp, "linear", "log-linear")
AIC_test

#---- Macrogroup
tree_dbh_dist_mg <- read.csv("./data/Tree_DBH_Dist_by_mg.csv")

head(tree_dbh_dist_mg)

mg_list <- unique(tree_dbh_dist_mg$mg_short)

AIC_test_mg <- map_dfr(mg_list, function(mg){
  df <- tree_dbh_dist_mg %>% filter(mg_list == mg & cycle == 3)
  lin_mod <- lm(density ~ class, data = df)
  exp_mod <- lm(log(density + 1) ~ class, data = df)
  aic_check <- data.frame(mg_short = mg, 
                          linear = AIC(lin_mod),
                          exp = AIC(exp_mod) + sum(2*log(df$density + 1)))
})

AIC_test_mg$best_mod <- ifelse(AIC_test_mg$linear < AIC_test_mg$exp, "linear", "log-linear")
AIC_test_mg
