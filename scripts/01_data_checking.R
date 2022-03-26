
df_orig <- read.csv("./data/EFWG_full_dataset_20211101.csv")
df_new <- read.csv("./data/EFWG_full_dataset_20220325.csv")
names(df_orig)
names(df_new)

join_data <- function(df_new, df_old, ...){
  if(nrow(df_new) != nrow(df_old)){cat(
    paste0("Different row counts:", nrow(df_new), " new vs.", nrow(df_old), " old", "\n"))
  } else {cat("Same row counts", "\n")}
  
  diffcols1 <- setdiff(names(df_new), names(df_old))
  diffcols2 <- setdiff(names(df_old), names(df_new))
  
  if(length(diffcols1) > 0){cat("Column names only found in first df:", diffcols1, "\n")
  } else {cat("Same column names in both outputs", "\n")}
  if(length(diffcols2) > 0){cat("Column names only found in second df:", diffcols2, "\n")
  }
  
  if(is_tibble(df_new)){cat("New function returned tibble instead of dataframe.", "\n")}
  
  df_comb <- full_join(df_new, df_old, by = c("Plot_Name", "Unit_Code", "Year", "Network", "cycle", 
                                              ...),
                       suffix = c(".new", ".old"))
  
  if(nrow(df_comb) != nrow(df_new)){cat("Merged dataframe has", nrow(df_comb) - nrow(df_new),
                                        "more rows than new dataframe", "\n") }
  if(nrow(df_comb) != nrow(df_old)){cat("Merged dataframe has", nrow(df_comb) - nrow(df_old),
                                        "more rows than old dataframe", "\n") }
  
  return(df_comb)
  
}

check_data <- function(df, col, returndf = F){
  col1 <- paste0(col, ".new")
  col2 <- paste0(col, ".old")
  
  if(!col1 %in% names(df))stop("Column doesn't exist or have new and old versions.")
  
  if(is.numeric(df[,col1])){
    df[, col1] <- round(df[, col1], 1)
    df[, col2] <- round(df[, col2], 1)
  }
  
  compareNA <- function(col1, col2) {
    same <- (col1 %in% col2) | (is.na(col1) & is.na(col2))
    same[is.na(same)] <- FALSE
    return(same)
  }
  
  df2 <- df[!compareNA(df[,col1], df[,col2]), c("Plot_Name", "Year", col1, col2)]
  if(nrow(df2) > 0){
    print(df2)
    print(nrow(df2))
  }
  
  if(returndf == T){return(df2)}
}

df_comb <- join_data(df_new, df_orig)

names(df_comb)
names(df_orig)
names(df_new)
columns <- c(names(df_new))[6:86]
columns

lapply(seq_along(columns), function(x){
  check_data(df_comb, columns[[x]])
}
)

check_data(df_comb, "Tree_BA_Total", returndf = T)
check_data(df_comb, "excludeEvent", returndf = T)
