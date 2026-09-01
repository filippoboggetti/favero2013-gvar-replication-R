################################################################################
###
### AUTHOR:       Filippo Felice Boggetti
### DATE:         Spring 2024
### DESCRIPTION:  This script the GIRFs for a 200 basis point shock to greek spreads 
###               and it hence replicates Fig.8 (b) lower part.
### OUTPUT:       one image: stored in "plots"
###                
################################################################################

## -----------------------------------------------------------------------------
rm(list=ls())                                                                   # Clear the environment 

## -----------------------------------------------------------------------------
Dataset<-read_rds("Database.RDS/dataset_monthly_global.rds")

countries <- c("bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", "pt")
horz <- 6
reps <- 5000

my_IRF <- as.data.frame(matrix(NA, nrow = horz, ncol = reps))

counter_irf=1
for (z in 1:reps){
  
  for (IDC in countries) {
    assign(paste0("all_boot_res_", IDC), matrix(NA, nrow = horz, ncol = reps))
  } 
  
## -----------------------------------------------------------------------------                                                                 
deltaNumericColumns <- as.data.frame(lapply(Dataset[, -c(1, 2)], function(x) c(NA, diff(x))))
names(deltaNumericColumns) <- paste0(names(Dataset[, -c(1, 2)]), "_diff")
extendedDataset <- cbind(Dataset, deltaNumericColumns)

laggedNumericColumns <- lapply(Dataset[, -c(1, 2)], function(x) c(NA, x[-length(x)]))
laggedNumericColumns <- as.data.frame(laggedNumericColumns)
names(laggedNumericColumns) <- paste0(names(Dataset[, -c(1, 2)]), "_lag")
finalDataset <- cbind(extendedDataset, laggedNumericColumns)

filteredDataset <- finalDataset[finalDataset$Date >= as.Date("2000-02-01") & finalDataset$Date <= as.Date("2009-12-01"), ]

equations <- list()

countries <- c("bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", "pt")

for (country in countries){
  y1_var <- paste0("yield_s_", country, "_diff")
  x1_var <- paste0("yield_s_", country, "_lag")
  x2_var <-paste0("glob_sp1_",country,"_lag")
  x3_var <-paste0("glob_sp2_",country,"_lag")
  x4_var <- "us_corp_spread_lag"
  x5_var <- paste0("debt_gdp_s_", country)
  x6_var <- paste0("deficit_gdp_s_", country)
  x7_var <- "us_corp_spread_diff" 
  
  formula_str <- sprintf("%s ~ %s +%s + %s + %s + I(%s/60) + I(%s/3) + %s",
                         y1_var, x1_var, x2_var, x3_var, x4_var, x5_var, x6_var ,x7_var)
  
  equations[[country]] <- as.formula(formula_str)
}

results <- systemfit(equations, data = filteredDataset, method = "SUR")

summary(results)
## ----------------------------------------------------------------------------- 

sys_gvar_coeff <- coef(results)

sys_gvar_coeff_matrix <- matrix(sys_gvar_coeff, ncol = 1)

residuals_df <- data.frame(Date = filteredDataset$Date)

for (i in seq_along(countries)) {
  country_residuals <- residuals(results)[[i]]
  adjusted_residuals <- c(NA, country_residuals)
  num_nas_to_add <- nrow(residuals_df) - length(adjusted_residuals)
  if (num_nas_to_add > 0) {
    adjusted_residuals <- c(adjusted_residuals, rep(NA, num_nas_to_add))
  }
  residuals_df[[paste0("res_", countries[i])]] <- head(adjusted_residuals, nrow(residuals_df))
}

residuals_only <- residuals_df[,-1]
cov_matrix <- cov(residuals_only, use="complete.obs")

## ----------------------------------------------------------------------------- 

## ----------------------------------------------------------------------------- 

countries_gr <- c("bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", "pt")
rm(list=("forecasting_dataset"))
first_column <- Dataset[, 1, drop = FALSE]
forecasting_dataset <- data.frame(First_Column = first_column)

forecasting_dataset <- forecasting_dataset %>%
  filter(Date >= ymd("2000-02-01"), Date <= ymd("2009-12-01"))



for (country in countries){
  forecasting_dataset[[paste0("diff_",country)]]<-0
  forecasting_dataset[[paste0("yield_s_",country,"_0")]]<-0
  forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]]<-0
  forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]]<-0
}


for (country in countries){
  x1_var <- paste0("yield_s_", country, "_lag")
  x2_var <- "us_corp_spread_lag"
  x3_var <- paste0("debt_gdp_s_", country)
  x4_var <- paste0("deficit_gdp_s_", country)
  x5_var <- "us_corp_spread_diff"
  x6_var <-paste0("glob_sp1_",country,"_lag")
  x7_var <-paste0("glob_sp2_",country,"_lag")
  i=1
  
  date <- forecasting_dataset$Date[i]
  date_indices <- which(forecasting_dataset$Date == date)
  date_indices_d<-which(finalDataset$Date==date)
  date_indices_dl<-which(finalDataset$Date=="2000-01-01")
  forecasting_dataset[[paste0("diff_",country)]][date_indices]<-results[["coefficients"]][[paste0(country,"_(Intercept)")]]+results[["coefficients"]][[paste0(country,"_yield_s_", country, "_lag")]]*finalDataset[[x1_var]][date_indices_d]+
    results[["coefficients"]][[paste0(country,"_us_corp_spread_lag")]]*finalDataset[[x2_var]][date_indices_d]+
    results[["coefficients"]][[paste0(country,"_I(debt_gdp_s_",country,"/60)")]]*finalDataset[[x3_var]][date_indices_d]/60+
    results[["coefficients"]][[paste0(country,"_I(deficit_gdp_s_",country,"/3)")]]*finalDataset[[x4_var]][date_indices_d]/3+
    results[["coefficients"]][[paste0(country,"_us_corp_spread_diff")]]*finalDataset[[x5_var]][date_indices_d]+results[["coefficients"]][[paste0(country,"_glob_sp1_",country,"_lag")]]*finalDataset[[x6_var]][date_indices_d]+
    results[["coefficients"]][[paste0(country,"_glob_sp2_",country,"_lag")]]*finalDataset[[x7_var]][date_indices_d]
  
  date_indices_D<-which(Dataset$Date=="2000-01-01")
  forecasting_dataset[[paste0("yield_s_",country,"_0")]][date_indices]<-forecasting_dataset[[paste0("diff_",country)]][date_indices]+Dataset[[paste0("yield_s_",country)]][date_indices_D]
}

date_indices_D<-which(Dataset$Date=="2000-02-01")
for (country in countries){
  for (countri in countries_gr){
    if (countri!=country){
        print(paste0("W1_",toupper(country),"_",countri))
        forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]<-Dataset[[paste0("W1_",toupper(country),"_",countri)]][[date_indices_D]]*forecasting_dataset[[paste0("yield_s_",countri, "_0")]][date_indices]+forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]
      }
}}

date_indices_D<-which(Dataset$Date=="2000-02-01")
for (country in countries){
  for (countri in countries_gr){
    if (countri!=country){
      print(paste0("W1_",toupper(country),"_",countri))
      forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]<-Dataset[[paste0("W1_",toupper(country),"_",countri)]][[date_indices_D]]*forecasting_dataset[[paste0("yield_s_",countri, "_0")]][date_indices]+forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]
    }
  }}
## -----------------------------------------------------------------------------
#(1) Start t+1 to T forcecast

for (i in 2:length(forecasting_dataset$Date)){
  
  date <- forecasting_dataset$Date[i]
  date_l <- forecasting_dataset$Date[i - 1]
  date_indices <- which(forecasting_dataset$Date == date)
  date_indices_d<-which(finalDataset$Date==date)
  date_indices_l<-which(forecasting_dataset$Date==date_l)
  date_indices_D<-which(Dataset$Date==date)
  date_indices_dl<-which(finalDataset$Date==date_l)
  
  for (country in countries){
    x1_var <- paste0("yield_s_", country, "_lag")
    x2_var <- "us_corp_spread_lag"
    x3_var <- paste0("debt_gdp_s_", country)
    x4_var <- paste0("deficit_gdp_s_", country)
    x5_var <- "us_corp_spread_diff"
    x6_var <-paste0("glob_sp1_",country,"_lag")
    x7_var <-paste0("glob_sp2_",country,"_lag")
    
    
    forecasting_dataset[[paste0("diff_",country)]][date_indices]<-results[["coefficients"]][[paste0(country,"_(Intercept)")]]+results[["coefficients"]][[paste0(country,"_yield_s_", country, "_lag")]]*forecasting_dataset[[paste0("yield_s_",country,"_0")]][date_indices_l]+
      results[["coefficients"]][[paste0(country,"_us_corp_spread_lag")]]*finalDataset[[x2_var]][date_indices_d]+
      results[["coefficients"]][[paste0(country,"_I(debt_gdp_s_",country,"/60)")]]*finalDataset[[x3_var]][date_indices_d]/60+
      results[["coefficients"]][[paste0(country,"_I(deficit_gdp_s_",country,"/3)")]]*finalDataset[[x4_var]][date_indices_d]/3+
      results[["coefficients"]][[paste0(country,"_us_corp_spread_diff")]]*finalDataset[[x5_var]][date_indices_d]+results[["coefficients"]][[paste0(country,"_glob_sp1_",country,"_lag")]]*forecasting_dataset[[x6_var]][date_indices_l]+
      results[["coefficients"]][[paste0(country,"_glob_sp2_",country,"_lag")]]*forecasting_dataset[[x7_var]][date_indices_l]
    
    forecasting_dataset[[paste0("yield_s_",country,"_0")]][date_indices]<-forecasting_dataset[[paste0("diff_",country)]][date_indices]+forecasting_dataset[[paste0("yield_s_",country,"_0")]][date_indices_l]
  }
  
  
  for (country in countries){
    for (countri in countries_gr){
      if (countri!=country){
          forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]<-Dataset[[paste0("W1_",toupper(country),"_",countri)]][[date_indices_D]]*forecasting_dataset[[paste0("yield_s_",countri, "_0")]][date_indices]+forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]
}}}
  
  for (country in countries){
    for (countri in countries_gr){
      if (countri!=country){
          forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]<-Dataset[[paste0("W2_",toupper(country),"_",countri)]][[date_indices_D]]*forecasting_dataset[[paste0("yield_s_",countri, "_0")]][date_indices]+forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]
        
}}}}



################################################################################
################################################################################
################################################################################
################################################################################


reshuffled_df <- residuals_df
reshuffled_df$Date <- as.Date(reshuffled_df$Date)

reshuffled_df[-1] <- lapply(reshuffled_df[-1], function(column) {
  non_na_values <- column[!is.na(column)]
  sampled_values <- sample(non_na_values, length(column), replace = TRUE)
  return(sampled_values)
})

filtered_reshuffled_df <- reshuffled_df[reshuffled_df$Date >= as.Date("2000-02-01") & reshuffled_df$Date <= as.Date("2009-12-01"), ]

## ----------------------------------------------------------------------------- 
dataframe_resh <- data.frame(Date = filtered_reshuffled_df$Date)

for (country in countries) {
  res_col_name <- paste0("res_", country)
  forecast_col_name <- paste0("yield_s_", country, "_0")
  
  dataframe_resh[[paste0("yield_r_", country)]] <- filtered_reshuffled_df[[res_col_name]] + forecasting_dataset[[forecast_col_name]]
}
## ----------------------------------------------------------------------------- 

for (country in countries) {
  glob_sp1_col_name <- paste0("glob_sp1_", country, "_lag")
  glob_sp2_col_name <- paste0("glob_sp2_", country, "_lag")
  
  dataframe_resh[[glob_sp1_col_name]] <- forecasting_dataset[[glob_sp1_col_name]]
  dataframe_resh[[glob_sp2_col_name]] <- forecasting_dataset[[glob_sp2_col_name]]
}

# for (country in countries) {
#   glob_sp1_col_name <- paste0("glob_sp1_", country, "_lag")
#   glob_sp2_col_name <- paste0("glob_sp2_", country, "_lag")
#   
#   dataframe_resh[[glob_sp1_col_name]] <- c(NA, dataframe_resh[[glob_sp1_col_name]][-nrow(dataframe_resh)])
# 
#   dataframe_resh[[glob_sp2_col_name]] <- c(NA, dataframe_resh[[glob_sp2_col_name]][-nrow(dataframe_resh)])
# }
## ----------------------------------------------------------------------------- 

for (country in countries) {
  glob_sp1_col_name <- paste0("glob_sp1_", country, "_lag")
  glob_sp2_col_name <- paste0("glob_sp2_", country, "_lag")
  
  dataframe_resh[[glob_sp1_col_name]] <- c(NA, dataframe_resh[[glob_sp1_col_name]][-nrow(dataframe_resh)])
  dataframe_resh[[glob_sp2_col_name]] <- c(NA, dataframe_resh[[glob_sp2_col_name]][-nrow(dataframe_resh)])
  
  if(is.na(dataframe_resh[[glob_sp1_col_name]][1])) {
    dataframe_resh[[glob_sp1_col_name]][1] <- forecasting_dataset[[glob_sp1_col_name]][1]
  }
  
  if(is.na(dataframe_resh[[glob_sp2_col_name]][1])) {
    dataframe_resh[[glob_sp2_col_name]][1] <- forecasting_dataset[[glob_sp2_col_name]][1]
  }
}
## ----------------------------------------------------------------------------- 
# Loop over each country to create a lagged version of yield_r_{country} and fill the NA values
for (country in countries) {
  # Define the column name for yield_r_{country} and its lagged version
  yield_r_col_name <- paste0("yield_r_", country)
  yield_r_lag_col_name <- paste0("yield_r_", country, "_lag")
  
  # Create the lagged column by shifting yield_r_{country} values down by one row
  dataframe_resh[[yield_r_lag_col_name]] <- c(NA, dataframe_resh[[yield_r_col_name]][-nrow(dataframe_resh)])
  
  # Fill the NA in the first row of the lagged column with the first value from yield_s_{country} in forecasting_dataset
  yield_s_col_name <- paste0("yield_s_", country)
  dataframe_resh[[yield_r_lag_col_name]][1] <- Dataset[[yield_s_col_name]][1]
}
## ----------------------------------------------------------------------------- 
dataframe_resh$us_corp_spread_lag <- finalDataset$us_corp_spread_lag[match(dataframe_resh$Date, Dataset$Date)]
dataframe_resh$us_corp_spread_diff <- finalDataset$us_corp_spread_diff[match(dataframe_resh$Date, Dataset$Date)]

# Loop over each country to add country-specific columns
for (country in countries) {
  # Define the column names for debt_gdp_s_ and deficit_gdp_s_
  debt_gdp_s_col_name <- paste0("debt_gdp_s_", country)
  deficit_gdp_s_col_name <- paste0("deficit_gdp_s_", country)
  
  # Add these columns to dataframe_resh based on matching dates
  dataframe_resh[[debt_gdp_s_col_name]] <- Dataset[[debt_gdp_s_col_name]][match(dataframe_resh$Date, Dataset$Date)]
  dataframe_resh[[deficit_gdp_s_col_name]] <- Dataset[[deficit_gdp_s_col_name]][match(dataframe_resh$Date, Dataset$Date)]
}
## ----------------------------------------------------------------------------- 

for (country in countries) {
  # Define the column names
  yield_r_col_name <- paste0("yield_r_", country)
  yield_s_col_name <- paste0("yield_s_", country)
  yield_r_diff_col_name <- paste0("yield_r_diff", country)
  
  # Initialize the new column with NA values to later fill in
  dataframe_resh[[yield_r_diff_col_name]] <- NA
  
  # Calculate the difference for the first row as specified
  dataframe_resh[[yield_r_diff_col_name]][1] <- dataframe_resh[[yield_r_col_name]][1] - Dataset[[yield_s_col_name]][1]
  
  # For the rest of the rows, calculate the difference between each value and its preceding value
  dataframe_resh[[yield_r_diff_col_name]][-1] <- diff(dataframe_resh[[yield_r_col_name]])
}
## ----------------------------------------------------------------------------- 
countries <- c("bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", "pt")

for (country in countries){
  y1_var <- paste0("yield_r_diff", country)
  x1_var <- paste0("yield_r_", country, "_lag")
  x2_var <-paste0("glob_sp1_",country,"_lag")
  x3_var <-paste0("glob_sp2_",country,"_lag")
  x4_var <- "us_corp_spread_lag"
  x5_var <- paste0("debt_gdp_s_", country)
  x6_var <- paste0("deficit_gdp_s_", country)
  x7_var <- "us_corp_spread_diff" 
  
  formula_str <- sprintf("%s ~ %s +%s + %s + %s + I(%s/60) + I(%s/3) + %s",
                         y1_var, x1_var, x2_var, x3_var, x4_var, x5_var, x6_var ,x7_var)
  
  equations[[country]] <- as.formula(formula_str)
}

results <- systemfit(equations, data = dataframe_resh, method = "SUR")

summary(results)
## ----------------------------------------------------------------------------- 

countries_gr <- c("bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", "pt")

first_column <- Dataset[, 1, drop = FALSE]
forecasting_dataset <- data.frame(First_Column = first_column)

forecasting_dataset <- forecasting_dataset %>%
  filter(Date >= ymd("2005-05-01"), Date <= ymd("2005-11-01"))



for (country in countries){
  forecasting_dataset[[paste0("diff_",country)]]<-0
  forecasting_dataset[[paste0("yield_r_",country,"_0")]]<-0
  forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]]<-0
  forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]]<-0
}



for (country in countries){
  counter=1
  x1_var <- paste0("yield_r_", country, "_lag")
  x2_var <-paste0("glob_sp1_",country,"_lag")
  x3_var <-paste0("glob_sp2_",country,"_lag")
  x4_var <- "us_corp_spread_lag"
  x5_var <- paste0("debt_gdp_s_", country)
  x6_var <- paste0("deficit_gdp_s_", country)
  x7_var <- "us_corp_spread_diff" 
  i=1
  
  date <- forecasting_dataset$Date[i]
  date_indices <- which(forecasting_dataset$Date == date)
  date_indices_d<-which(finalDataset$Date==date)
  date_indices_dl<-which(finalDataset$Date=="2005-04-01")
  forecasting_dataset[[paste0("diff_",country)]][date_indices]<-results[["coefficients"]][[paste0(country,"_(Intercept)")]]+results[["coefficients"]][[paste0(country,"_yield_r_", country, "_lag")]]*finalDataset[[paste0("yield_s_",country)]][date_indices_d]+
    results[["coefficients"]][[paste0(country,"_us_corp_spread_lag")]]*finalDataset[[x4_var]][date_indices_d]+
    results[["coefficients"]][[paste0(country,"_I(debt_gdp_s_",country,"/60)")]]*finalDataset[[x5_var]][date_indices_d]/60+
    results[["coefficients"]][[paste0(country,"_I(deficit_gdp_s_",country,"/3)")]]*finalDataset[[x6_var]][date_indices_d]/3+
    results[["coefficients"]][[paste0(country,"_us_corp_spread_diff")]]*finalDataset[[x7_var]][date_indices_d]+results[["coefficients"]][[paste0(country,"_glob_sp1_",country,"_lag")]]*finalDataset[[x2_var]][date_indices_d]+
    results[["coefficients"]][[paste0(country,"_glob_sp2_",country,"_lag")]]*finalDataset[[x3_var]][date_indices_d]+2*cov_matrix[5,counter]/cov_matrix[5,5]
  
  date_indices_D<-which(Dataset$Date=="2005-04-01")
  forecasting_dataset[[paste0("yield_r_",country,"_0")]][date_indices]<-forecasting_dataset[[paste0("diff_",country)]][date_indices]+Dataset[[paste0("yield_s_",country)]][date_indices_D]
  counter=counter+1
}

date_indices_D<-which(Dataset$Date=="2005-05-01")
for (country in countries){
  for (countri in countries_gr){
    if (countri!=country){
        print(paste0("W1_",toupper(country),"_",countri))
        forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]<-Dataset[[paste0("W1_",toupper(country),"_",countri)]][[date_indices_D]]*forecasting_dataset[[paste0("yield_r_",countri, "_0")]][date_indices]+forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]
      }}}

for (country in countries){
  for (countri in countries_gr){
    if (countri!=country){
        forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]<-Dataset[[paste0("W2_",toupper(country),"_",countri)]][[date_indices_D]]*forecasting_dataset[[paste0("yield_r_",countri, "_0")]][date_indices]+forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]
}}}



################################################################################
for (i in 2:length(forecasting_dataset$Date)){
  
  date <- forecasting_dataset$Date[i]
  date_l <- forecasting_dataset$Date[i - 1]
  date_indices <- which(forecasting_dataset$Date == date)
  date_indices_d<-which(finalDataset$Date==date)
  date_indices_l<-which(forecasting_dataset$Date==date_l)
  date_indices_D<-which(Dataset$Date==date)
  date_indices_dl<-which(finalDataset$Date==date_l)
  
  for (country in countries){
    x1_var <- paste0("yield_r_", country, "_lag")
    x2_var <-paste0("glob_sp1_",country,"_lag")
    x3_var <-paste0("glob_sp2_",country,"_lag")
    x4_var <- "us_corp_spread_lag"
    x5_var <- paste0("debt_gdp_s_", country)
    x6_var <- paste0("deficit_gdp_s_", country)
    x7_var <- "us_corp_spread_diff" 
    
    
    forecasting_dataset[[paste0("diff_",country)]][date_indices]<-results[["coefficients"]][[paste0(country,"_(Intercept)")]]+results[["coefficients"]][[paste0(country,"_yield_r_", country, "_lag")]]*forecasting_dataset[[paste0("yield_r_",country,"_0")]][date_indices_l]+
      results[["coefficients"]][[paste0(country,"_us_corp_spread_lag")]]*finalDataset[[x4_var]][date_indices_d]+
      results[["coefficients"]][[paste0(country,"_I(debt_gdp_s_",country,"/60)")]]*finalDataset[[x5_var]][date_indices_d]/60+
      results[["coefficients"]][[paste0(country,"_I(deficit_gdp_s_",country,"/3)")]]*finalDataset[[x6_var]][date_indices_d]/3+
      results[["coefficients"]][[paste0(country,"_us_corp_spread_diff")]]*finalDataset[[x7_var]][date_indices_d]+results[["coefficients"]][[paste0(country,"_glob_sp1_",country,"_lag")]]*forecasting_dataset[[x2_var]][date_indices_l]+
      results[["coefficients"]][[paste0(country,"_glob_sp2_",country,"_lag")]]*forecasting_dataset[[x3_var]][date_indices_l]
    
    forecasting_dataset[[paste0("yield_r_",country,"_0")]][date_indices]<-forecasting_dataset[[paste0("diff_",country)]][date_indices]+forecasting_dataset[[paste0("yield_r_",country,"_0")]][date_indices_l]
  }
  
  
  for (country in countries){
    for (countri in countries_gr){
      if (countri!=country){
        forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]<-Dataset[[paste0("W1_",toupper(country),"_",countri)]][[date_indices_D]]*forecasting_dataset[[paste0("yield_r_",countri, "_0")]][date_indices]+forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]
      }}}
  
  for (country in countries){
    for (countri in countries_gr){
      if (countri!=country){
        forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]<-Dataset[[paste0("W2_",toupper(country),"_",countri)]][[date_indices_D]]*forecasting_dataset[[paste0("yield_r_",countri, "_0")]][date_indices]+forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]
        
      }}}
  
}

IRF <- data.frame(
  Column1 = forecasting_dataset$yield_r_pt_0
)
################################# Construct Baseline ###########################

countries_gr <- c("bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", "pt")

first_column <- Dataset[, 1, drop = FALSE]
forecasting_dataset <- data.frame(First_Column = first_column)

forecasting_dataset <- forecasting_dataset %>%
  filter(Date >= ymd("2005-05-01"), Date <= ymd("2005-11-01"))



for (country in countries){
  forecasting_dataset[[paste0("diff_",country)]]<-0
  forecasting_dataset[[paste0("yield_r_",country,"_0")]]<-0
  forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]]<-0
  forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]]<-0
}



for (country in countries){
  counter=1
  x1_var <- paste0("yield_r_", country, "_lag")
  x2_var <-paste0("glob_sp1_",country,"_lag")
  x3_var <-paste0("glob_sp2_",country,"_lag")
  x4_var <- "us_corp_spread_lag"
  x5_var <- paste0("debt_gdp_s_", country)
  x6_var <- paste0("deficit_gdp_s_", country)
  x7_var <- "us_corp_spread_diff" 
  i=1
  
  date <- forecasting_dataset$Date[i]
  date_indices <- which(forecasting_dataset$Date == date)
  date_indices_d<-which(finalDataset$Date==date)
  date_indices_dl<-which(finalDataset$Date=="2005-04-01")
  forecasting_dataset[[paste0("diff_",country)]][date_indices]<-results[["coefficients"]][[paste0(country,"_(Intercept)")]]+results[["coefficients"]][[paste0(country,"_yield_r_", country, "_lag")]]*finalDataset[[paste0("yield_s_",country)]][date_indices_d]+
    results[["coefficients"]][[paste0(country,"_us_corp_spread_lag")]]*finalDataset[[x4_var]][date_indices_d]+
    results[["coefficients"]][[paste0(country,"_I(debt_gdp_s_",country,"/60)")]]*finalDataset[[x5_var]][date_indices_d]/60+
    results[["coefficients"]][[paste0(country,"_I(deficit_gdp_s_",country,"/3)")]]*finalDataset[[x6_var]][date_indices_d]/3+
    results[["coefficients"]][[paste0(country,"_us_corp_spread_diff")]]*finalDataset[[x7_var]][date_indices_d]+results[["coefficients"]][[paste0(country,"_glob_sp1_",country,"_lag")]]*finalDataset[[x2_var]][date_indices_d]+
    results[["coefficients"]][[paste0(country,"_glob_sp2_",country,"_lag")]]*finalDataset[[x3_var]][date_indices_d]
  
  date_indices_D<-which(Dataset$Date=="2005-04-01")
  forecasting_dataset[[paste0("yield_r_",country,"_0")]][date_indices]<-forecasting_dataset[[paste0("diff_",country)]][date_indices]+Dataset[[paste0("yield_s_",country)]][date_indices_D]
  counter=counter+1
}

date_indices_D<-which(Dataset$Date=="2005-05-01")
for (country in countries){
  for (countri in countries_gr){
    if (countri!=country){
      print(paste0("W1_",toupper(country),"_",countri))
      forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]<-Dataset[[paste0("W1_",toupper(country),"_",countri)]][[date_indices_D]]*forecasting_dataset[[paste0("yield_r_",countri, "_0")]][date_indices]+forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]
    }}}

for (country in countries){
  for (countri in countries_gr){
    if (countri!=country){
      forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]<-Dataset[[paste0("W2_",toupper(country),"_",countri)]][[date_indices_D]]*forecasting_dataset[[paste0("yield_r_",countri, "_0")]][date_indices]+forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]
    }}}



################################################################################
for (i in 2:length(forecasting_dataset$Date)){
  
  date <- forecasting_dataset$Date[i]
  date_l <- forecasting_dataset$Date[i - 1]
  date_indices <- which(forecasting_dataset$Date == date)
  date_indices_d<-which(finalDataset$Date==date)
  date_indices_l<-which(forecasting_dataset$Date==date_l)
  date_indices_D<-which(Dataset$Date==date)
  date_indices_dl<-which(finalDataset$Date==date_l)
  
  for (country in countries){
    x1_var <- paste0("yield_r_", country, "_lag")
    x2_var <-paste0("glob_sp1_",country,"_lag")
    x3_var <-paste0("glob_sp2_",country,"_lag")
    x4_var <- "us_corp_spread_lag"
    x5_var <- paste0("debt_gdp_s_", country)
    x6_var <- paste0("deficit_gdp_s_", country)
    x7_var <- "us_corp_spread_diff" 
    
    
    forecasting_dataset[[paste0("diff_",country)]][date_indices]<-results[["coefficients"]][[paste0(country,"_(Intercept)")]]+results[["coefficients"]][[paste0(country,"_yield_r_", country, "_lag")]]*forecasting_dataset[[paste0("yield_r_",country,"_0")]][date_indices_l]+
      results[["coefficients"]][[paste0(country,"_us_corp_spread_lag")]]*finalDataset[[x4_var]][date_indices_d]+
      results[["coefficients"]][[paste0(country,"_I(debt_gdp_s_",country,"/60)")]]*finalDataset[[x5_var]][date_indices_d]/60+
      results[["coefficients"]][[paste0(country,"_I(deficit_gdp_s_",country,"/3)")]]*finalDataset[[x6_var]][date_indices_d]/3+
      results[["coefficients"]][[paste0(country,"_us_corp_spread_diff")]]*finalDataset[[x7_var]][date_indices_d]+results[["coefficients"]][[paste0(country,"_glob_sp1_",country,"_lag")]]*forecasting_dataset[[x2_var]][date_indices_l]+
      results[["coefficients"]][[paste0(country,"_glob_sp2_",country,"_lag")]]*forecasting_dataset[[x3_var]][date_indices_l]
    
    forecasting_dataset[[paste0("yield_r_",country,"_0")]][date_indices]<-forecasting_dataset[[paste0("diff_",country)]][date_indices]+forecasting_dataset[[paste0("yield_r_",country,"_0")]][date_indices_l]
  }
  
  
  for (country in countries){
    for (countri in countries_gr){
      if (countri!=country){
        forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]<-Dataset[[paste0("W1_",toupper(country),"_",countri)]][[date_indices_D]]*forecasting_dataset[[paste0("yield_r_",countri, "_0")]][date_indices]+forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]
      }}}
  
  for (country in countries){
    for (countri in countries_gr){
      if (countri!=country){
        forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]<-Dataset[[paste0("W2_",toupper(country),"_",countri)]][[date_indices_D]]*forecasting_dataset[[paste0("yield_r_",countri, "_0")]][date_indices]+forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]
        
      }}}
  
}


IRF$Column2 = forecasting_dataset$yield_r_pt_0

IRF<-as.data.frame(IRF)
IRF$diff<-IRF$Column1 - IRF$Column2
my_IRF[[counter_irf]] <- head(IRF$diff, 6) 

counter_irf=counter_irf+1
}
################################################################################
################################################################################
########################### 2002 BASED IRF - POINT #############################
################################################################################

rm(list=(setdiff(ls(),c("IRF_conf", "my_IRF"))))
Dataset<-read_rds("dataset_monthly_global.rds")

countries <- c("bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", "pt")
horz <- 6
reps <- 1

my_IRF1 <- as.data.frame(matrix(NA, nrow = horz, ncol = reps))

counter_irf=1
for (z in 1:reps){
  
  for (IDC in countries) {
    assign(paste0("all_boot_res_", IDC), matrix(NA, nrow = horz, ncol = reps))
  } 
  
  ## -----------------------------------------------------------------------------                                                                 
  deltaNumericColumns <- as.data.frame(lapply(Dataset[, -c(1, 2)], function(x) c(NA, diff(x))))
  names(deltaNumericColumns) <- paste0(names(Dataset[, -c(1, 2)]), "_diff")
  extendedDataset <- cbind(Dataset, deltaNumericColumns)
  
  laggedNumericColumns <- lapply(Dataset[, -c(1, 2)], function(x) c(NA, x[-length(x)]))
  laggedNumericColumns <- as.data.frame(laggedNumericColumns)
  names(laggedNumericColumns) <- paste0(names(Dataset[, -c(1, 2)]), "_lag")
  finalDataset <- cbind(extendedDataset, laggedNumericColumns)
  
  filteredDataset <- finalDataset[finalDataset$Date >= as.Date("2000-02-01") & finalDataset$Date <= as.Date("2009-12-01"), ]
  
  equations <- list()
  
  countries <- c("bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", "pt")
  
  for (country in countries){
    y1_var <- paste0("yield_s_", country, "_diff")
    x1_var <- paste0("yield_s_", country, "_lag")
    x2_var <-paste0("glob_sp1_",country,"_lag")
    x3_var <-paste0("glob_sp2_",country,"_lag")
    x4_var <- "us_corp_spread_lag"
    x5_var <- paste0("debt_gdp_s_", country)
    x6_var <- paste0("deficit_gdp_s_", country)
    x7_var <- "us_corp_spread_diff" 
    
    formula_str <- sprintf("%s ~ %s +%s + %s + %s + I(%s/60) + I(%s/3) + %s",
                           y1_var, x1_var, x2_var, x3_var, x4_var, x5_var, x6_var ,x7_var)
    
    equations[[country]] <- as.formula(formula_str)
  }
  
  results <- systemfit(equations, data = filteredDataset, method = "SUR")
  
  summary(results)
  ## ----------------------------------------------------------------------------- 
  
  sys_gvar_coeff <- coef(results)
  
  sys_gvar_coeff_matrix <- matrix(sys_gvar_coeff, ncol = 1)
  
  residuals_df <- data.frame(Date = filteredDataset$Date)
  
  for (i in seq_along(countries)) {
    country_residuals <- residuals(results)[[i]]
    adjusted_residuals <- c(NA, country_residuals)
    num_nas_to_add <- nrow(residuals_df) - length(adjusted_residuals)
    if (num_nas_to_add > 0) {
      adjusted_residuals <- c(adjusted_residuals, rep(NA, num_nas_to_add))
    }
    residuals_df[[paste0("res_", countries[i])]] <- head(adjusted_residuals, nrow(residuals_df))
  }
  
  residuals_only <- residuals_df[,-1]
  cov_matrix <- cov(residuals_only, use="complete.obs")
  
  ## ----------------------------------------------------------------------------- 
  
  ## ----------------------------------------------------------------------------- 
  
  countries_gr <- c("bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", "pt")
  rm(list=("forecasting_dataset"))
  first_column <- Dataset[, 1, drop = FALSE]
  forecasting_dataset <- data.frame(First_Column = first_column)
  
  forecasting_dataset <- forecasting_dataset %>%
    filter(Date >= ymd("2000-02-01"), Date <= ymd("2009-12-01"))
  
  
  
  for (country in countries){
    forecasting_dataset[[paste0("diff_",country)]]<-0
    forecasting_dataset[[paste0("yield_s_",country,"_0")]]<-0
    forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]]<-0
    forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]]<-0
  }
  
  
  for (country in countries){
    x1_var <- paste0("yield_s_", country, "_lag")
    x2_var <- "us_corp_spread_lag"
    x3_var <- paste0("debt_gdp_s_", country)
    x4_var <- paste0("deficit_gdp_s_", country)
    x5_var <- "us_corp_spread_diff"
    x6_var <-paste0("glob_sp1_",country,"_lag")
    x7_var <-paste0("glob_sp2_",country,"_lag")
    i=1
    
    date <- forecasting_dataset$Date[i]
    date_indices <- which(forecasting_dataset$Date == date)
    date_indices_d<-which(finalDataset$Date==date)
    date_indices_dl<-which(finalDataset$Date=="2000-01-01")
    forecasting_dataset[[paste0("diff_",country)]][date_indices]<-results[["coefficients"]][[paste0(country,"_(Intercept)")]]+results[["coefficients"]][[paste0(country,"_yield_s_", country, "_lag")]]*finalDataset[[x1_var]][date_indices_d]+
      results[["coefficients"]][[paste0(country,"_us_corp_spread_lag")]]*finalDataset[[x2_var]][date_indices_d]+
      results[["coefficients"]][[paste0(country,"_I(debt_gdp_s_",country,"/60)")]]*finalDataset[[x3_var]][date_indices_d]/60+
      results[["coefficients"]][[paste0(country,"_I(deficit_gdp_s_",country,"/3)")]]*finalDataset[[x4_var]][date_indices_d]/3+
      results[["coefficients"]][[paste0(country,"_us_corp_spread_diff")]]*finalDataset[[x5_var]][date_indices_d]+results[["coefficients"]][[paste0(country,"_glob_sp1_",country,"_lag")]]*finalDataset[[x6_var]][date_indices_d]+
      results[["coefficients"]][[paste0(country,"_glob_sp2_",country,"_lag")]]*finalDataset[[x7_var]][date_indices_d]
    
    date_indices_D<-which(Dataset$Date=="2000-01-01")
    forecasting_dataset[[paste0("yield_s_",country,"_0")]][date_indices]<-forecasting_dataset[[paste0("diff_",country)]][date_indices]+Dataset[[paste0("yield_s_",country)]][date_indices_D]
  }
  
  date_indices_D<-which(Dataset$Date=="2000-02-01")
  for (country in countries){
    for (countri in countries_gr){
      if (countri!=country){
        print(paste0("W1_",toupper(country),"_",countri))
        forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]<-Dataset[[paste0("W1_",toupper(country),"_",countri)]][[date_indices_D]]*forecasting_dataset[[paste0("yield_s_",countri, "_0")]][date_indices]+forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]
      }
    }}
  
  date_indices_D<-which(Dataset$Date=="2000-02-01")
  for (country in countries){
    for (countri in countries_gr){
      if (countri!=country){
        print(paste0("W1_",toupper(country),"_",countri))
        forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]<-Dataset[[paste0("W1_",toupper(country),"_",countri)]][[date_indices_D]]*forecasting_dataset[[paste0("yield_s_",countri, "_0")]][date_indices]+forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]
      }
    }}
  ## -----------------------------------------------------------------------------
  #(1) Start t+1 to T forcecast
  
  for (i in 2:length(forecasting_dataset$Date)){
    
    date <- forecasting_dataset$Date[i]
    date_l <- forecasting_dataset$Date[i - 1]
    date_indices <- which(forecasting_dataset$Date == date)
    date_indices_d<-which(finalDataset$Date==date)
    date_indices_l<-which(forecasting_dataset$Date==date_l)
    date_indices_D<-which(Dataset$Date==date)
    date_indices_dl<-which(finalDataset$Date==date_l)
    
    for (country in countries){
      x1_var <- paste0("yield_s_", country, "_lag")
      x2_var <- "us_corp_spread_lag"
      x3_var <- paste0("debt_gdp_s_", country)
      x4_var <- paste0("deficit_gdp_s_", country)
      x5_var <- "us_corp_spread_diff"
      x6_var <-paste0("glob_sp1_",country,"_lag")
      x7_var <-paste0("glob_sp2_",country,"_lag")
      
      
      forecasting_dataset[[paste0("diff_",country)]][date_indices]<-results[["coefficients"]][[paste0(country,"_(Intercept)")]]+results[["coefficients"]][[paste0(country,"_yield_s_", country, "_lag")]]*forecasting_dataset[[paste0("yield_s_",country,"_0")]][date_indices_l]+
        results[["coefficients"]][[paste0(country,"_us_corp_spread_lag")]]*finalDataset[[x2_var]][date_indices_d]+
        results[["coefficients"]][[paste0(country,"_I(debt_gdp_s_",country,"/60)")]]*finalDataset[[x3_var]][date_indices_d]/60+
        results[["coefficients"]][[paste0(country,"_I(deficit_gdp_s_",country,"/3)")]]*finalDataset[[x4_var]][date_indices_d]/3+
        results[["coefficients"]][[paste0(country,"_us_corp_spread_diff")]]*finalDataset[[x5_var]][date_indices_d]+results[["coefficients"]][[paste0(country,"_glob_sp1_",country,"_lag")]]*forecasting_dataset[[x6_var]][date_indices_l]+
        results[["coefficients"]][[paste0(country,"_glob_sp2_",country,"_lag")]]*forecasting_dataset[[x7_var]][date_indices_l]
      
      forecasting_dataset[[paste0("yield_s_",country,"_0")]][date_indices]<-forecasting_dataset[[paste0("diff_",country)]][date_indices]+forecasting_dataset[[paste0("yield_s_",country,"_0")]][date_indices_l]
    }
    
    
    for (country in countries){
      for (countri in countries_gr){
        if (countri!=country){
          forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]<-Dataset[[paste0("W1_",toupper(country),"_",countri)]][[date_indices_D]]*forecasting_dataset[[paste0("yield_s_",countri, "_0")]][date_indices]+forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]
        }}}
    
    for (country in countries){
      for (countri in countries_gr){
        if (countri!=country){
          forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]<-Dataset[[paste0("W2_",toupper(country),"_",countri)]][[date_indices_D]]*forecasting_dataset[[paste0("yield_s_",countri, "_0")]][date_indices]+forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]
          
        }}}}
  
  
  
  ################################################################################
  ################################################################################
  ################################################################################
  ################################################################################
  
  
  reshuffled_df <- residuals_df
  reshuffled_df$Date <- as.Date(reshuffled_df$Date)
  # 
  # reshuffled_df[-1] <- lapply(reshuffled_df[-1], function(column) {
  #   non_na_values <- column[!is.na(column)]
  #   sampled_values <- sample(non_na_values, length(column), replace = TRUE)
  #   return(sampled_values)
  # })
  
  filtered_reshuffled_df <- reshuffled_df[reshuffled_df$Date >= as.Date("2000-02-01") & reshuffled_df$Date <= as.Date("2009-12-01"), ]
  
  ## ----------------------------------------------------------------------------- 
  dataframe_resh <- data.frame(Date = filtered_reshuffled_df$Date)
  
  for (country in countries) {
    res_col_name <- paste0("res_", country)
    forecast_col_name <- paste0("yield_s_", country, "_0")
    
    dataframe_resh[[paste0("yield_r_", country)]] <- filtered_reshuffled_df[[res_col_name]] + forecasting_dataset[[forecast_col_name]]
  }
  ## ----------------------------------------------------------------------------- 
  
  for (country in countries) {
    glob_sp1_col_name <- paste0("glob_sp1_", country, "_lag")
    glob_sp2_col_name <- paste0("glob_sp2_", country, "_lag")
    
    dataframe_resh[[glob_sp1_col_name]] <- forecasting_dataset[[glob_sp1_col_name]]
    dataframe_resh[[glob_sp2_col_name]] <- forecasting_dataset[[glob_sp2_col_name]]
  }
  
  # for (country in countries) {
  #   glob_sp1_col_name <- paste0("glob_sp1_", country, "_lag")
  #   glob_sp2_col_name <- paste0("glob_sp2_", country, "_lag")
  #   
  #   dataframe_resh[[glob_sp1_col_name]] <- c(NA, dataframe_resh[[glob_sp1_col_name]][-nrow(dataframe_resh)])
  # 
  #   dataframe_resh[[glob_sp2_col_name]] <- c(NA, dataframe_resh[[glob_sp2_col_name]][-nrow(dataframe_resh)])
  # }
  ## ----------------------------------------------------------------------------- 
  
  for (country in countries) {
    glob_sp1_col_name <- paste0("glob_sp1_", country, "_lag")
    glob_sp2_col_name <- paste0("glob_sp2_", country, "_lag")
    
    dataframe_resh[[glob_sp1_col_name]] <- c(NA, dataframe_resh[[glob_sp1_col_name]][-nrow(dataframe_resh)])
    dataframe_resh[[glob_sp2_col_name]] <- c(NA, dataframe_resh[[glob_sp2_col_name]][-nrow(dataframe_resh)])
    
    if(is.na(dataframe_resh[[glob_sp1_col_name]][1])) {
      dataframe_resh[[glob_sp1_col_name]][1] <- forecasting_dataset[[glob_sp1_col_name]][1]
    }
    
    if(is.na(dataframe_resh[[glob_sp2_col_name]][1])) {
      dataframe_resh[[glob_sp2_col_name]][1] <- forecasting_dataset[[glob_sp2_col_name]][1]
    }
  }
  ## ----------------------------------------------------------------------------- 
  # Loop over each country to create a lagged version of yield_r_{country} and fill the NA values
  for (country in countries) {
    # Define the column name for yield_r_{country} and its lagged version
    yield_r_col_name <- paste0("yield_r_", country)
    yield_r_lag_col_name <- paste0("yield_r_", country, "_lag")
    
    # Create the lagged column by shifting yield_r_{country} values down by one row
    dataframe_resh[[yield_r_lag_col_name]] <- c(NA, dataframe_resh[[yield_r_col_name]][-nrow(dataframe_resh)])
    
    # Fill the NA in the first row of the lagged column with the first value from yield_s_{country} in forecasting_dataset
    yield_s_col_name <- paste0("yield_s_", country)
    dataframe_resh[[yield_r_lag_col_name]][1] <- Dataset[[yield_s_col_name]][1]
  }
  ## ----------------------------------------------------------------------------- 
  dataframe_resh$us_corp_spread_lag <- finalDataset$us_corp_spread_lag[match(dataframe_resh$Date, Dataset$Date)]
  dataframe_resh$us_corp_spread_diff <- finalDataset$us_corp_spread_diff[match(dataframe_resh$Date, Dataset$Date)]
  
  # Loop over each country to add country-specific columns
  for (country in countries) {
    # Define the column names for debt_gdp_s_ and deficit_gdp_s_
    debt_gdp_s_col_name <- paste0("debt_gdp_s_", country)
    deficit_gdp_s_col_name <- paste0("deficit_gdp_s_", country)
    
    # Add these columns to dataframe_resh based on matching dates
    dataframe_resh[[debt_gdp_s_col_name]] <- Dataset[[debt_gdp_s_col_name]][match(dataframe_resh$Date, Dataset$Date)]
    dataframe_resh[[deficit_gdp_s_col_name]] <- Dataset[[deficit_gdp_s_col_name]][match(dataframe_resh$Date, Dataset$Date)]
  }
  ## ----------------------------------------------------------------------------- 
  
  for (country in countries) {
    # Define the column names
    yield_r_col_name <- paste0("yield_r_", country)
    yield_s_col_name <- paste0("yield_s_", country)
    yield_r_diff_col_name <- paste0("yield_r_diff", country)
    
    # Initialize the new column with NA values to later fill in
    dataframe_resh[[yield_r_diff_col_name]] <- NA
    
    # Calculate the difference for the first row as specified
    dataframe_resh[[yield_r_diff_col_name]][1] <- dataframe_resh[[yield_r_col_name]][1] - Dataset[[yield_s_col_name]][1]
    
    # For the rest of the rows, calculate the difference between each value and its preceding value
    dataframe_resh[[yield_r_diff_col_name]][-1] <- diff(dataframe_resh[[yield_r_col_name]])
  }
  ## ----------------------------------------------------------------------------- 
  countries <- c("bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", "pt")
  
  for (country in countries){
    y1_var <- paste0("yield_r_diff", country)
    x1_var <- paste0("yield_r_", country, "_lag")
    x2_var <-paste0("glob_sp1_",country,"_lag")
    x3_var <-paste0("glob_sp2_",country,"_lag")
    x4_var <- "us_corp_spread_lag"
    x5_var <- paste0("debt_gdp_s_", country)
    x6_var <- paste0("deficit_gdp_s_", country)
    x7_var <- "us_corp_spread_diff" 
    
    formula_str <- sprintf("%s ~ %s +%s + %s + %s + I(%s/60) + I(%s/3) + %s",
                           y1_var, x1_var, x2_var, x3_var, x4_var, x5_var, x6_var ,x7_var)
    
    equations[[country]] <- as.formula(formula_str)
  }
  
  results <- systemfit(equations, data = dataframe_resh, method = "SUR")
  
  summary(results)
  ## ----------------------------------------------------------------------------- 
  
  countries_gr <- c("bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", "pt")
  
  first_column <- Dataset[, 1, drop = FALSE]
  forecasting_dataset <- data.frame(First_Column = first_column)
  
  forecasting_dataset <- forecasting_dataset %>%
    filter(Date >= ymd("2002-03-01"), Date <= ymd("2002-08-01"))
  
  
  
  for (country in countries){
    forecasting_dataset[[paste0("diff_",country)]]<-0
    forecasting_dataset[[paste0("yield_r_",country,"_0")]]<-0
    forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]]<-0
    forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]]<-0
  }
  
  
  
  for (country in countries){
    counter=1
    x1_var <- paste0("yield_r_", country, "_lag")
    x2_var <-paste0("glob_sp1_",country,"_lag")
    x3_var <-paste0("glob_sp2_",country,"_lag")
    x4_var <- "us_corp_spread_lag"
    x5_var <- paste0("debt_gdp_s_", country)
    x6_var <- paste0("deficit_gdp_s_", country)
    x7_var <- "us_corp_spread_diff" 
    i=1
    
    date <- forecasting_dataset$Date[i]
    date_indices <- which(forecasting_dataset$Date == date)
    date_indices_d<-which(finalDataset$Date==date)
    date_indices_dl<-which(finalDataset$Date=="2002-02-01")
    forecasting_dataset[[paste0("diff_",country)]][date_indices]<-results[["coefficients"]][[paste0(country,"_(Intercept)")]]+results[["coefficients"]][[paste0(country,"_yield_r_", country, "_lag")]]*finalDataset[[paste0("yield_s_",country)]][date_indices_d]+
      results[["coefficients"]][[paste0(country,"_us_corp_spread_lag")]]*finalDataset[[x4_var]][date_indices_d]+
      results[["coefficients"]][[paste0(country,"_I(debt_gdp_s_",country,"/60)")]]*finalDataset[[x5_var]][date_indices_d]/60+
      results[["coefficients"]][[paste0(country,"_I(deficit_gdp_s_",country,"/3)")]]*finalDataset[[x6_var]][date_indices_d]/3+
      results[["coefficients"]][[paste0(country,"_us_corp_spread_diff")]]*finalDataset[[x7_var]][date_indices_d]+results[["coefficients"]][[paste0(country,"_glob_sp1_",country,"_lag")]]*finalDataset[[x2_var]][date_indices_d]+
      results[["coefficients"]][[paste0(country,"_glob_sp2_",country,"_lag")]]*finalDataset[[x3_var]][date_indices_d]+2*cov_matrix[5,counter]/cov_matrix[5,5]
    
    date_indices_D<-which(Dataset$Date=="2002-02-01")
    forecasting_dataset[[paste0("yield_r_",country,"_0")]][date_indices]<-forecasting_dataset[[paste0("diff_",country)]][date_indices]+Dataset[[paste0("yield_s_",country)]][date_indices_D]
    counter=counter+1
  }
  
  date_indices_D<-which(Dataset$Date=="2002-03-01")
  for (country in countries){
    for (countri in countries_gr){
      if (countri!=country){
        print(paste0("W1_",toupper(country),"_",countri))
        forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]<-Dataset[[paste0("W1_",toupper(country),"_",countri)]][[date_indices_D]]*forecasting_dataset[[paste0("yield_r_",countri, "_0")]][date_indices]+forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]
      }}}
  
  for (country in countries){
    for (countri in countries_gr){
      if (countri!=country){
        forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]<-Dataset[[paste0("W2_",toupper(country),"_",countri)]][[date_indices_D]]*forecasting_dataset[[paste0("yield_r_",countri, "_0")]][date_indices]+forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]
      }}}
  
  
  
  ################################################################################
  for (i in 2:length(forecasting_dataset$Date)){
    
    date <- forecasting_dataset$Date[i]
    date_l <- forecasting_dataset$Date[i - 1]
    date_indices <- which(forecasting_dataset$Date == date)
    date_indices_d<-which(finalDataset$Date==date)
    date_indices_l<-which(forecasting_dataset$Date==date_l)
    date_indices_D<-which(Dataset$Date==date)
    date_indices_dl<-which(finalDataset$Date==date_l)
    
    for (country in countries){
      x1_var <- paste0("yield_r_", country, "_lag")
      x2_var <-paste0("glob_sp1_",country,"_lag")
      x3_var <-paste0("glob_sp2_",country,"_lag")
      x4_var <- "us_corp_spread_lag"
      x5_var <- paste0("debt_gdp_s_", country)
      x6_var <- paste0("deficit_gdp_s_", country)
      x7_var <- "us_corp_spread_diff" 
      
      
      forecasting_dataset[[paste0("diff_",country)]][date_indices]<-results[["coefficients"]][[paste0(country,"_(Intercept)")]]+results[["coefficients"]][[paste0(country,"_yield_r_", country, "_lag")]]*forecasting_dataset[[paste0("yield_r_",country,"_0")]][date_indices_l]+
        results[["coefficients"]][[paste0(country,"_us_corp_spread_lag")]]*finalDataset[[x4_var]][date_indices_d]+
        results[["coefficients"]][[paste0(country,"_I(debt_gdp_s_",country,"/60)")]]*finalDataset[[x5_var]][date_indices_d]/60+
        results[["coefficients"]][[paste0(country,"_I(deficit_gdp_s_",country,"/3)")]]*finalDataset[[x6_var]][date_indices_d]/3+
        results[["coefficients"]][[paste0(country,"_us_corp_spread_diff")]]*finalDataset[[x7_var]][date_indices_d]+results[["coefficients"]][[paste0(country,"_glob_sp1_",country,"_lag")]]*forecasting_dataset[[x2_var]][date_indices_l]+
        results[["coefficients"]][[paste0(country,"_glob_sp2_",country,"_lag")]]*forecasting_dataset[[x3_var]][date_indices_l]
      
      forecasting_dataset[[paste0("yield_r_",country,"_0")]][date_indices]<-forecasting_dataset[[paste0("diff_",country)]][date_indices]+forecasting_dataset[[paste0("yield_r_",country,"_0")]][date_indices_l]
    }
    
    
    for (country in countries){
      for (countri in countries_gr){
        if (countri!=country){
          forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]<-Dataset[[paste0("W1_",toupper(country),"_",countri)]][[date_indices_D]]*forecasting_dataset[[paste0("yield_r_",countri, "_0")]][date_indices]+forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]
        }}}
    
    for (country in countries){
      for (countri in countries_gr){
        if (countri!=country){
          forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]<-Dataset[[paste0("W2_",toupper(country),"_",countri)]][[date_indices_D]]*forecasting_dataset[[paste0("yield_r_",countri, "_0")]][date_indices]+forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]
          
        }}}
    
  }
  
  IRF <- data.frame(
    Column1 = forecasting_dataset$yield_r_pt_0
  )
  ################################# Construct Baseline ###########################
  
  countries_gr <- c("bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", "pt")
  
  first_column <- Dataset[, 1, drop = FALSE]
  forecasting_dataset <- data.frame(First_Column = first_column)
  
  forecasting_dataset <- forecasting_dataset %>%
    filter(Date >= ymd("2002-03-01"), Date <= ymd("2002-08-01"))
  
  
  
  for (country in countries){
    forecasting_dataset[[paste0("diff_",country)]]<-0
    forecasting_dataset[[paste0("yield_r_",country,"_0")]]<-0
    forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]]<-0
    forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]]<-0
  }
  
  
  
  for (country in countries){
    counter=1
    x1_var <- paste0("yield_r_", country, "_lag")
    x2_var <-paste0("glob_sp1_",country,"_lag")
    x3_var <-paste0("glob_sp2_",country,"_lag")
    x4_var <- "us_corp_spread_lag"
    x5_var <- paste0("debt_gdp_s_", country)
    x6_var <- paste0("deficit_gdp_s_", country)
    x7_var <- "us_corp_spread_diff" 
    i=1
    
    date <- forecasting_dataset$Date[i]
    date_indices <- which(forecasting_dataset$Date == date)
    date_indices_d<-which(finalDataset$Date==date)
    date_indices_dl<-which(finalDataset$Date=="2002-02-01")
    forecasting_dataset[[paste0("diff_",country)]][date_indices]<-results[["coefficients"]][[paste0(country,"_(Intercept)")]]+results[["coefficients"]][[paste0(country,"_yield_r_", country, "_lag")]]*finalDataset[[paste0("yield_s_",country)]][date_indices_d]+
      results[["coefficients"]][[paste0(country,"_us_corp_spread_lag")]]*finalDataset[[x4_var]][date_indices_d]+
      results[["coefficients"]][[paste0(country,"_I(debt_gdp_s_",country,"/60)")]]*finalDataset[[x5_var]][date_indices_d]/60+
      results[["coefficients"]][[paste0(country,"_I(deficit_gdp_s_",country,"/3)")]]*finalDataset[[x6_var]][date_indices_d]/3+
      results[["coefficients"]][[paste0(country,"_us_corp_spread_diff")]]*finalDataset[[x7_var]][date_indices_d]+results[["coefficients"]][[paste0(country,"_glob_sp1_",country,"_lag")]]*finalDataset[[x2_var]][date_indices_d]+
      results[["coefficients"]][[paste0(country,"_glob_sp2_",country,"_lag")]]*finalDataset[[x3_var]][date_indices_d]
    
    date_indices_D<-which(Dataset$Date=="2002-02-01")
    forecasting_dataset[[paste0("yield_r_",country,"_0")]][date_indices]<-forecasting_dataset[[paste0("diff_",country)]][date_indices]+Dataset[[paste0("yield_s_",country)]][date_indices_D]
    counter=counter+1
  }
  
  date_indices_D<-which(Dataset$Date=="2002-03-01")
  for (country in countries){
    for (countri in countries_gr){
      if (countri!=country){
        print(paste0("W1_",toupper(country),"_",countri))
        forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]<-Dataset[[paste0("W1_",toupper(country),"_",countri)]][[date_indices_D]]*forecasting_dataset[[paste0("yield_r_",countri, "_0")]][date_indices]+forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]
      }}}
  
  for (country in countries){
    for (countri in countries_gr){
      if (countri!=country){
        forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]<-Dataset[[paste0("W2_",toupper(country),"_",countri)]][[date_indices_D]]*forecasting_dataset[[paste0("yield_r_",countri, "_0")]][date_indices]+forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]
      }}}
  
  
  
  ################################################################################
  for (i in 2:length(forecasting_dataset$Date)){
    
    date <- forecasting_dataset$Date[i]
    date_l <- forecasting_dataset$Date[i - 1]
    date_indices <- which(forecasting_dataset$Date == date)
    date_indices_d<-which(finalDataset$Date==date)
    date_indices_l<-which(forecasting_dataset$Date==date_l)
    date_indices_D<-which(Dataset$Date==date)
    date_indices_dl<-which(finalDataset$Date==date_l)
    
    for (country in countries){
      x1_var <- paste0("yield_r_", country, "_lag")
      x2_var <-paste0("glob_sp1_",country,"_lag")
      x3_var <-paste0("glob_sp2_",country,"_lag")
      x4_var <- "us_corp_spread_lag"
      x5_var <- paste0("debt_gdp_s_", country)
      x6_var <- paste0("deficit_gdp_s_", country)
      x7_var <- "us_corp_spread_diff" 
      
      
      forecasting_dataset[[paste0("diff_",country)]][date_indices]<-results[["coefficients"]][[paste0(country,"_(Intercept)")]]+results[["coefficients"]][[paste0(country,"_yield_r_", country, "_lag")]]*forecasting_dataset[[paste0("yield_r_",country,"_0")]][date_indices_l]+
        results[["coefficients"]][[paste0(country,"_us_corp_spread_lag")]]*finalDataset[[x4_var]][date_indices_d]+
        results[["coefficients"]][[paste0(country,"_I(debt_gdp_s_",country,"/60)")]]*finalDataset[[x5_var]][date_indices_d]/60+
        results[["coefficients"]][[paste0(country,"_I(deficit_gdp_s_",country,"/3)")]]*finalDataset[[x6_var]][date_indices_d]/3+
        results[["coefficients"]][[paste0(country,"_us_corp_spread_diff")]]*finalDataset[[x7_var]][date_indices_d]+results[["coefficients"]][[paste0(country,"_glob_sp1_",country,"_lag")]]*forecasting_dataset[[x2_var]][date_indices_l]+
        results[["coefficients"]][[paste0(country,"_glob_sp2_",country,"_lag")]]*forecasting_dataset[[x3_var]][date_indices_l]
      
      forecasting_dataset[[paste0("yield_r_",country,"_0")]][date_indices]<-forecasting_dataset[[paste0("diff_",country)]][date_indices]+forecasting_dataset[[paste0("yield_r_",country,"_0")]][date_indices_l]
    }
    
    
    for (country in countries){
      for (countri in countries_gr){
        if (countri!=country){
          forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]<-Dataset[[paste0("W1_",toupper(country),"_",countri)]][[date_indices_D]]*forecasting_dataset[[paste0("yield_r_",countri, "_0")]][date_indices]+forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]
        }}}
    
    for (country in countries){
      for (countri in countries_gr){
        if (countri!=country){
          forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]<-Dataset[[paste0("W2_",toupper(country),"_",countri)]][[date_indices_D]]*forecasting_dataset[[paste0("yield_r_",countri, "_0")]][date_indices]+forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]
          
        }}}
    
  }
  
  
  IRF$Column2 = forecasting_dataset$yield_r_pt_0
  
  IRF<-as.data.frame(IRF)
  IRF$diff<-IRF$Column1 - IRF$Column2
  my_IRF1[[counter_irf]] <- head(IRF$diff, 6) 
  
  counter_irf=counter_irf+1
}

################################################################################ Graph the IRF

percentiles_per_row <- apply(my_IRF, 1, function(x) {
  c(
    `95th Percentile` = quantile(x, probs = 0.95, na.rm = TRUE),
    `Median` = median(x, na.rm = TRUE),
    `5th Percentile` = quantile(x, probs = 0.05, na.rm = TRUE)
  )
})

my_IRF_conf <- t(percentiles_per_row)
my_IRF_conf <- as.data.frame(my_IRF_conf)

colnames(my_IRF1) <- "low int."
colnames(my_IRF_conf) <- c("95_per", "m_perc", "5_perc")

# # my_IRF_conf$Time <- 1:nrow(my_IRF_conf)
# # 
# # # Melt 'my_IRF_conf' to long format
# # my_IRF_conf_long <- melt(my_IRF_conf, id.vars = "Time", variable.name = "Type", value.name = "Value")
# # 
# # # Make sure 'my_IRF1' has a 'Time' column as well
# # my_IRF1$Time <- 1:nrow(my_IRF1)
# # 
# # # Create the plot
# # p <- ggplot() +
# #   geom_line(data = my_IRF_conf_long, aes(x = Time, y = Value, linetype = Type, color = Type)) +
# #   geom_line(data = my_IRF1, aes(x = Time, y = second), color = "red") +
# #   scale_color_manual(values = c("95_per" = "blue", "m_perc" = "blue", "5_perc" = "blue", "second" = "red")) +
# #   scale_linetype_manual(values = c("95_per" = "solid", "m_perc" = "longdash", "5_perc" = "solid")) +
# #   labs(title = "The response (GIRF) of Portuguese spreads to a 200 bp shock to Greek spreads",
# #        x = "", y = "") +
# #   theme_minimal() +
# #   theme(legend.title = element_blank(),
# #         legend.position = "bottom")
# # 
# # # Print the plot
# # print(p)
# 
# 
# 
# my_IRF_conf$Time <- 1:nrow(my_IRF_conf)
# 
# # Melt 'my_IRF_conf' to long format
# my_IRF_conf_long <- melt(my_IRF_conf, id.vars = "Time", variable.name = "Type", value.name = "Value")
# 
# # Make sure 'my_IRF1' has a 'Time' column as well
# my_IRF1$Time <- 1:nrow(my_IRF1)
# 
# # Define legend labels
# legend_labels <- c("95_per" = "Upper high interdependence (2005)",
#                    "m_perc" = "Point high interdependence (2005)",
#                    "5_perc" = "Lower high interdependence (2005)",
#                    "low int." = "Point low interdependence (2002)")
# 
# # Create the plot
# p <- ggplot() +
#   geom_line(data = my_IRF_conf_long, aes(x = Time, y = Value, linetype = Type, color = Type)) +
#   geom_line(data = my_IRF1, aes(x = Time, y = `low int.`, linetype = "low int.", color = "low int.")) +
#   scale_color_manual(values = c("95_per" = "blue", "m_perc" = "blue", "5_perc" = "blue", "low int." = "red"),
#                      labels = legend_labels) +
#   scale_linetype_manual(values = c("95_per" = "solid", "m_perc" = "longdash", "5_perc" = "solid", "low int." = "solid"),
#                         labels = legend_labels) +
#   scale_y_continuous(limits = c(0, 0.9)) + # Set y-axis limits
#   labs(title = "The response (GIRF) of Portuguese spreads to a 200 bp shock to Greek spreads",
#        x = "", y = "") +
#   theme_minimal() +
#   theme(legend.title = element_blank(),
#         legend.position = "bottom",
#         legend.text = element_text(size = 12)) +
#   guides(color = guide_legend(override.aes = list(linetype = c("solid", "longdash", "solid", "solid"))))
# 
# # Print the plot
# print(p)
# 
# # Check if the "plots" directory exists and create it if it doesn't
# if (!dir.exists("plots")) {
#   dir.create("plots")
# }
# 
# # Now save the plot as a PDF
# ggsave("plots/fig_8_GIRF.pdf", plot = p, device = "pdf", width = 11, height = 8.5, units = "in")
# 
# 
# 
# 
# # # Add a sequence column for the x-axis
# # my_IRF_conf$Time <- 1:nrow(my_IRF_conf)
# # 
# # # Melt 'my_IRF_conf' to long format
# # my_IRF_conf_long <- melt(my_IRF_conf, id.vars = "Time", variable.name = "Type", value.name = "Value")
# # 
# # # Make sure 'my_IRF1' has a 'Time' column as well
# # my_IRF1$Time <- 1:nrow(my_IRF1)
# # 
# # # Create the plot with correct legend labels
# # p <- ggplot() +
# #   geom_line(data = my_IRF_conf_long, aes(x = Time, y = Value, linetype = Type, color = Type), size=1) +
# #   geom_line(data = my_IRF1, aes(x = Time, y = `low int.`, linetype = "Point Estimate low interdependence (2002)", color = "Point Estimate low interdependence (2002)"), size=1) +
# #   scale_color_manual(values = c("Upper bound high interdependence (2005)" = "blue", 
# #                                 "Point Estimate high interdependence (2005)" = "blue", 
# #                                 "Lower bound high interdependence (2005)" = "blue", 
# #                                 "Point Estimate low interdependence (2002)" = "red")) +
# #   scale_linetype_manual(values = c("Upper bound high interdependence (2005)" = "solid", 
# #                                    "Point Estimate high interdependence (2005)" = "longdash", 
# #                                    "Lower bound high interdependence (2005)" = "solid", 
# #                                    "Point Estimate low interdependence (2002)" = "solid")) +
# #   scale_y_continuous(limits = c(0, 0.9)) +
# #   labs(title = "The response (GIRF) of Portuguese spreads to a 200 bp shock to Greek spreads",
# #        x = "", y = "") +
# #   theme_minimal() +
# #   theme(legend.title = element_blank(),
# #         legend.position = "bottom",
# #         legend.text = element_text(size = 12)) +
# #   guides(colour = guide_legend(override.aes = list(size=3)))
# # 
# # # Print the plot
# # print(p)
# # 
# # # Check if the "plots" directory exists and create it if it doesn't
# # if (!dir.exists("plots")) {
# #   dir.create("plots")
# # }
# # 
# # # Now save the plot as a PDF
# # ggsave("plots/fig_8_GIRF.pdf", plot = p, device = "pdf", width = 11, height = 8.5, units = "in")
# 
# 
# 
# 
# # # Melt 'my_IRF_conf' to long format
# # my_IRF_conf_long <- melt(my_IRF_conf, id.vars = "Time", variable.name = "Type", value.name = "Value")
# # 
# # # Ensure the column names are correct and there are no syntax errors
# # # 'second' must be renamed to 'low int.' in 'my_IRF1' if that has not been done yet
# # 
# # # Create the plot with correct legend labels
# # p <- ggplot() +
# #   geom_line(data = my_IRF_conf_long, aes(x = Time, y = Value, color = Type, linetype = Type), size=1) +
# #   geom_line(data = my_IRF1, aes(x = Time, y = `low int.`, color = "Point Estimate low interdependence (2002)", linetype = "Point Estimate low interdependence (2002)"), size=1) +
# #   scale_color_manual(values = c("95_per" = "blue", "m_perc" = "blue", "5_perc" = "blue", "low int." = "red"),
# #                      labels = c("95_per" = "Upper bound high interdependence (2005)", 
# #                                 "m_perc" = "Point Estimate high interdependence (2005)", 
# #                                 "5_perc" = "Lower bound high interdependence (2005)", 
# #                                 "low int." = "Point Estimate low interdependence (2002)")) +
# #   scale_linetype_manual(values = c("95_per" = "solid", "m_perc" = "longdash", "5_perc" = "solid", "low int." = "solid"),
# #                         labels = c("95_per" = "Upper bound high interdependence (2005)", 
# #                                    "m_perc" = "Point Estimate high interdependence (2005)", 
# #                                    "5_perc" = "Lower bound high interdependence (2005)", 
# #                                    "low int." = "Point Estimate low interdependence (2002)")) +
# #   scale_y_continuous(limits = c(0, 0.9)) +
# #   labs(title = "The response (GIRF) of Portuguese spreads to a 200 bp shock to Greek spreads",
# #        x = "", y = "") +
# #   theme_minimal() +
# #   theme(legend.title = element_blank(),
# #         legend.position = "bottom",
# #         legend.text = element_text(size = 12)) +
# #   guides(colour = guide_legend(override.aes = list(size=3)))
# # 
# # # Print the plot
# # print(p)
# # 
# # # Check if the "plots" directory exists and create it if it doesn't
# # if (!dir.exists("plots")) {
# #   dir.create("plots")
# # }
# # 
# # # Save the plot as a PDF
# # ggsave("plots/fig_8_GIRF.pdf", plot = p, device = "pdf", width = 11, height = 8.5, units = "in")
# 
# 
# # Assuming 'my_IRF1' and 'my_IRF_conf' dataframes are already loaded into R
# # and 'Time' column is added to both of them
# 
# # Melt 'my_IRF_conf' to long format
# my_IRF_conf_long <- melt(my_IRF_conf, id.vars = "Time", variable.name = "Type", value.name = "Value")
# 
# # Create the plot with smooth lines
# p <- ggplot() +
#   geom_smooth(data = my_IRF_conf_long, aes(x = Time, y = Value, color = Type, linetype = Type), method = "loess", se = FALSE, size = 1) +
#   geom_smooth(data = my_IRF1, aes(x = Time, y = `low int.`, color = "Point Estimate low interdependence (2002)", linetype = "Point Estimate low interdependence (2002)"), method = "loess", se = FALSE, size = 1) +
#   scale_color_manual(values = c("95_per" = "blue", "m_perc" = "blue", "5_perc" = "blue", "low int." = "red"),
#                      labels = c("95_per" = "Upper bound high interdependence (2005)", 
#                                 "m_perc" = "Point Estimate high interdependence (2005)", 
#                                 "5_perc" = "Lower bound high interdependence (2005)", 
#                                 "low int." = "Point Estimate low interdependence (2002)")) +
#   scale_linetype_manual(values = c("95_per" = "solid", "m_perc" = "longdash", "5_perc" = "solid", "low int." = "solid"),
#                         labels = c("95_per" = "Upper bound high interdependence (2005)", 
#                                    "m_perc" = "Point Estimate high interdependence (2005)", 
#                                    "5_perc" = "Lower bound high interdependence (2005)", 
#                                    "low int." = "Point Estimate low interdependence (2002)")) +
#   scale_y_continuous(limits = c(0, 0.9)) +
#   labs(title = "The response (GIRF) of Portuguese spreads to a 200 bp shock to Greek spreads",
#        x = "", y = "") +
#   theme_minimal() +
#   theme(legend.title = element_blank(),
#         legend.position = "bottom",
#         legend.text = element_text(size = 12)) +
#   guides(colour = guide_legend(override.aes = list(size=3)))
# 
# # Print the plot
# print(p)


my_IRF_conf_long$Type <- factor(my_IRF_conf_long$Type, levels = c("95_per", "m_perc", "5_perc"))

# Melt 'my_IRF_conf' to long format
my_IRF_conf_long <- melt(my_IRF_conf, id.vars = "Time", variable.name = "Type", value.name = "Value")

# Create the plot with smoothed lines for all series
p <- ggplot() +
  geom_smooth(data = my_IRF_conf_long, aes(x = Time, y = Value, color = Type, linetype = Type), method = "loess", se = FALSE, size = 1) +
  geom_smooth(data = my_IRF1, aes(x = Time, y = `low int.`, color = "Point Estimate low interdependence (2002)", linetype = "Point Estimate low interdependence (2002)"), method = "loess", se = FALSE, size = 1) +
  scale_color_manual(values = c("95_per" = "blue", "m_perc" = "blue", "5_perc" = "blue", "Point Estimate low interdependence (2002)" = "red"),
                     labels = c("Upper bound high interdependence (2005)", 
                                "Point Estimate high interdependence (2005)", 
                                "Lower bound high interdependence (2005)", 
                                "Point Estimate low interdependence (2002)")) +
  scale_linetype_manual(values = c("95_per" = "solid", "m_perc" = "longdash", "5_perc" = "solid", "Point Estimate low interdependence (2002)" = "solid"),
                        labels = c("Upper bound high interdependence (2005)", 
                                   "Point Estimate high interdependence (2005)", 
                                   "Lower bound high interdependence (2005)", 
                                   "Point Estimate low interdependence (2002)")) +
  scale_y_continuous(limits = c(0, 0.9)) +
  labs(title = "The response (GIRF) of Portuguese spreads to a 200 bp shock to Greek spreads",
       x = "", y = "") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 12)) +
  guides(color = guide_legend(override.aes = list(size = 3)))

# Print the plot
print(p)

# Check if the "plots" directory exists and create it if it doesn't
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Save the plot as a PDF
ggsave("plots/fig_8_GIRF.pdf", plot = p, device = "pdf", width = 11, height = 8.5, units = "in")









