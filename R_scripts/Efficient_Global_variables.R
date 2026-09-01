################################################################################
###
### AUTHOR:       Filippo Felice Boggetti
### DATE:         Spring 2024
### DESCRIPTION:  This script creates the global variables. 
###               for the replication of Favero (2013).
### OUTPUT:       One Database: "Dataset_monthly_global.rds"
###               
################################################################################


## -----------------------------------------------------------------------------

rm(list=ls())

Dataset<-read_rds("Database.RDS/Dataset_monthly.rds")

cutoff <- 50
cutoff1 <- 50

# List of all countries
all_countries <- c("it", "bg", "es", "fn", "fr", "ir", "nl", "oe", "pt", "gr")

# Loop over each country as the reference country
for (ref_country in all_countries) {
  countries <- setdiff(all_countries, ref_country) 
  
  Dataset[[paste0("glob_sp1_",ref_country)]] <- 0
  Dataset[[paste0("glob_sp2_",ref_country)]] <- 0
  Dataset[[paste0("glob_fun1_",ref_country)]] <- 0
  Dataset[[paste0("glob_fun2_",ref_country)]] <- 0

  # Initial denominators
  denom <- 0
  denom1 <- 0
  
  # Calculating distances and weights for Italy as reference
  for(idc in countries) {
    Dataset[[paste0("dist1_",ref_country,"_", idc)]] <- abs((Dataset[[paste0("deficit_gdp_",ref_country)]] - Dataset[[paste0("deficit_gdp_", idc)]])/3) + 0.001
    Dataset[[paste0("dist2_",ref_country,"_", idc)]] <- abs((Dataset[[paste0("debt_gdp_",ref_country)]] - Dataset[[paste0("debt_gdp_", idc)]])/60) + 0.001
    
    Dataset[[paste0("W1_",toupper(ref_country),"_t_", idc)]] <- 1 / Dataset[[paste0("dist1_",ref_country,"_", idc)]]
    Dataset[[paste0("W2_",toupper(ref_country),"_t_", idc)]] <- 1 / Dataset[[paste0("dist2_",ref_country,"_", idc)]]
    
    # Applying cutoff
    Dataset[[paste0("W1_",ref_country,"_t_", idc)]] <- Dataset[[paste0("W1_",toupper(ref_country),"_t_", idc)]] * (Dataset[[paste0("dist1_",ref_country,"_", idc)]] < cutoff1)
    Dataset[[paste0("W2_",ref_country,"_t_", idc)]] <- Dataset[[paste0("W2_",toupper(ref_country),"_t_", idc)]] * (Dataset[[paste0("dist2_",ref_country,"_", idc)]] < cutoff)
    
    # Summing denominators
    denom <- denom + Dataset[[paste0("W1_",ref_country,"_t_", idc)]]
    denom1 <- denom1 + Dataset[[paste0("W2_",ref_country,"_t_", idc)]]
  }
  
  # Normalizing weights and calculating global variables
  for(idc in countries) {
    # Conditional generation of new variables using vectorized operations
    Dataset[[paste0("W1_",toupper(ref_country),"_", idc)]] <- ifelse(denom > 0, Dataset[[paste0("W1_",ref_country,"_t_", idc)]] / denom, 0)
    Dataset[[paste0("W2_",toupper(ref_country),"_", idc)]] <- ifelse(denom1 > 0, Dataset[[paste0("W2_",ref_country,"_t_", idc)]] / denom1, 0)
    
    # Updating global aggregates
    Dataset[[paste0("glob_sp1_",ref_country)]] <- Dataset[[paste0("glob_sp1_",ref_country)]] + Dataset[[paste0("W1_",toupper(ref_country),"_", idc)]] * Dataset[[paste0("yield_s_", idc)]]
    Dataset[[paste0("glob_sp2_",ref_country)]] <- Dataset[[paste0("glob_sp2_",ref_country)]] + Dataset[[paste0("W2_",toupper(ref_country),"_", idc)]] * Dataset[[paste0("yield_s_", idc)]]
    Dataset[[paste0("glob_fun1_",ref_country)]] <- Dataset[[paste0("glob_fun1_",ref_country)]] + Dataset[[paste0("W1_",toupper(ref_country),"_", idc)]] * abs((Dataset$deficit_gdp_bd - Dataset[[paste0("deficit_gdp_", idc)]])/3)
    Dataset[[paste0("glob_fun2_",ref_country)]] <- Dataset[[paste0("glob_fun2_",ref_country)]] + Dataset[[paste0("W2_",toupper(ref_country),"_", idc)]] * abs((Dataset$debt_gdp_bd - Dataset[[paste0("debt_gdp_", idc)]])/60)
  }

  # Cleanup
  to_remove <- c(paste0("W1_",toupper(ref_country),"_t_", countries), paste0("W2_",toupper(ref_country),"_t_", countries), "denom", "denom1")
  Dataset <- Dataset[ , !(names(Dataset) %in% to_remove)]
  
}

saveRDS(Dataset, file = "Database.RDS/Dataset_monthly_global.rds")
## -----------------------------------------------------------------------------
write.xlsx(Dataset, file = "backup_excel/Global_dataset.xlsx")
rm(list=setdiff(ls(),"Dataset"))

## -----------------------------------------------------------------------------
rm(list=ls())
gc()

## -----------------------------------------------------------------------------















