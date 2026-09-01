################################################################################
###
### AUTHOR:       Filippo Felice Boggetti
### DATE:         Spring 2024
### DESCRIPTION:  This script creates the global variables. 
###               for the replication of Favero (2013).
### OUTPUT:       Two Database Dataset.rds and Debt.rds. Note that differently
###               from Eviews those are intermediate results.
################################################################################


## -----------------------------------------------------------------------------

rm(list=ls())

Dataset <- read_rds("Database_RDS/Dataset_monthly.rds")

# Parameters
cutoff <- 50
cutoff1 <- 50

# List of countries
countries <- c("bg", "es", "fn", "fr", "ir", "nl", "oe", "pt", "gr")

# Initial setting of global variables to zero
Dataset$glob_sp1_it <- 0
Dataset$glob_fun1_it <- 0
Dataset$glob_sp2_it <- 0
Dataset$glob_fun2_it <- 0

# Initial denominators
denom <- 0
denom1 <- 0

# Calculating distances and weights for Italy as reference
for(idc in countries) {
  Dataset[[paste0("dist1_it_", idc)]] <- abs((Dataset$deficit_gdp_it - Dataset[[paste0("deficit_gdp_", idc)]])/3) + 0.001
  Dataset[[paste0("dist2_it_", idc)]] <- abs((Dataset$debt_gdp_it - Dataset[[paste0("debt_gdp_", idc)]])/60) + 0.001
  
  Dataset[[paste0("W1_IT_t_", idc)]] <- 1 / Dataset[[paste0("dist1_it_", idc)]]
  Dataset[[paste0("W2_IT_t_", idc)]] <- 1 / Dataset[[paste0("dist2_it_", idc)]]
  
  # Applying cutoff
  Dataset[[paste0("W1_it_t_", idc)]] <- Dataset[[paste0("W1_IT_t_", idc)]] * (Dataset[[paste0("dist1_it_", idc)]] < cutoff1)
  Dataset[[paste0("W2_it_t_", idc)]] <- Dataset[[paste0("W2_IT_t_", idc)]] * (Dataset[[paste0("dist2_it_", idc)]] < cutoff)
  
  # Summing denominators
  denom <- denom + Dataset[[paste0("W1_it_t_", idc)]]
  denom1 <- denom1 + Dataset[[paste0("W2_it_t_", idc)]]
}

# Normalizing weights and calculating global variables
for(idc in countries) {
  # Conditional generation of new variables using vectorized operations
  Dataset[[paste0("W1_IT_", idc)]] <- ifelse(denom > 0, Dataset[[paste0("W1_it_t_", idc)]] / denom, 0)
  Dataset[[paste0("W2_IT_", idc)]] <- ifelse(denom1 > 0, Dataset[[paste0("W2_it_t_", idc)]] / denom1, 0)
  
  # Updating global aggregates
  Dataset$glob_sp1_it <- Dataset$glob_sp1_it + Dataset[[paste0("W1_IT_", idc)]] * Dataset[[paste0("yield_s_", idc)]]
  Dataset$glob_sp2_it <- Dataset$glob_sp2_it + Dataset[[paste0("W2_IT_", idc)]] * Dataset[[paste0("yield_s_", idc)]]
  Dataset$glob_fun1_it <- Dataset$glob_fun1_it + Dataset[[paste0("W1_IT_", idc)]] * abs((Dataset$deficit_gdp_bd - Dataset[[paste0("deficit_gdp_", idc)]])/3)
  Dataset$glob_fun2_it <- Dataset$glob_fun2_it + Dataset[[paste0("W2_IT_", idc)]] * abs((Dataset$debt_gdp_bd - Dataset[[paste0("debt_gdp_", idc)]])/60)
}
# Cleanup
to_remove <- c(paste0("W1_IT_t_", countries), paste0("W2_IT_t_", countries), "denom", "denom1")
Dataset <- Dataset[ , !(names(Dataset) %in% to_remove)]

## -----------------------------------------------------------------------------
write.xlsx(Dataset, file = "Backup_excel/Global_it_dataset.xlsx")
rm(list=setdiff(ls(),"Dataset"))

## -----------------------------------------------------------------------------

source("Clean1.R")

col_types_vector <- c("date", rep("numeric", 308))
Global_it_2 <- read_excel("Global_it_EW.xlsx", col_types = col_types_vector)         
colnames(Global_it_2)[1] = "Date"
Global_it_2$Date<-as.Date(Global_it_2$Date)


Global_it<-Dataset

Global_it$Month_Year<-NULL

names(Global_it) <- tolower(names(Global_it))
names(Global_it_2) <- tolower(names(Global_it_2))

sorted_names_Global_it <- sort(names(Global_it))
sorted_names_Global_it_2 <- sort(names(Global_it_2))

if (all(sorted_names_Global_it == sorted_names_Global_it_2)) {
  message("All column names match.")
} else {
  not_in_Global_it_2 <- setdiff(sorted_names_Global_it, sorted_names_Global_it_2)
  not_in_Global_it<- setdiff(sorted_names_Global_it_2, sorted_names_Global_it)
  if (length(not_in_Global_it_2) > 0) {
    message("Columns in Global_it but not in : Global_it_2", paste(not_in_Global_it_2, collapse = ", "))
  }
  if (length(not_in_Global_it) > 0) {
    message("Columns in Global_it_2 but not in Global_it: ", paste(not_in_Global_it, collapse = ", "))
  }
}

Global_it_2_reordered <- Global_it_2[names(Global_it)]
Global_it_2_reordered<-clean_data_frame1(Global_it_2_reordered)

write.xlsx(Global_it_2_reordered, file = "Global_it_EW_ordered.xlsx")

# rm(list=ls())
# gc()

## -----------------------------------------------------------------------------











