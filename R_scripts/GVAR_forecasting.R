################################################################################
###
### AUTHOR:       Filippo Felice Boggetti
### DATE:         Spring 2024
### DESCRIPTION:  This script use the GVAR to forecast O.O.S. please note that  
###               the conditining is equal to the T.M.
### OUTPUT:       Two Graph: 7.3 and 7.4 saved in plots
###               
################################################################################

## -----------------------------------------------------------------------------
rm(list=ls())                                                                   # Clear the environment 
## -----------------------------------------------------------------------------
Dataset<-read_rds("Database.RDS/Dataset_monthly_global.rds")

deltaNumericColumns <- as.data.frame(lapply(Dataset[, -c(1, 2)], function(x) c(NA, diff(x))))
names(deltaNumericColumns) <- paste0(names(Dataset[, -c(1, 2)]), "_diff")
extendedDataset <- cbind(Dataset, deltaNumericColumns)

laggedNumericColumns <- lapply(Dataset[, -c(1, 2)], function(x) c(NA, x[-length(x)]))
laggedNumericColumns <- as.data.frame(laggedNumericColumns)
names(laggedNumericColumns) <- paste0(names(Dataset[, -c(1, 2)]), "_lag")
finalDataset <- cbind(extendedDataset, laggedNumericColumns)

filteredDataset <- finalDataset[finalDataset$Date >= as.Date("2000-01-01") & finalDataset$Date <= as.Date("2009-12-01"),  ]

equations <- list()

countries <- c("bg", "es", "fn", "fr", "ir", "it", "nl", "oe", "pt")

for (country in countries){
  y1_var <- paste0("yield_s_", country, "_diff")
  x1_var <- paste0("yield_s_", country, "_lag")
  x2_var <- "us_corp_spread_lag"
  x3_var <- paste0("debt_gdp_s_", country)
  x4_var <- paste0("deficit_gdp_s_", country)
  x5_var <- "us_corp_spread_diff" 
  x6_var <-paste0("glob_sp1_",country,"_lag")
  x7_var <-paste0("glob_sp2_",country,"_lag")
  
  formula_str <- sprintf("%s ~ %s +%s + I(%s/60) + I(%s/3)+%s + %s + %s",
                         y1_var, x1_var, x2_var, x3_var, x4_var, x5_var, x6_var ,x7_var)
  
  equations[[country]] <- as.formula(formula_str)
}

results <- systemfit(equations, data = filteredDataset, method = "SUR")

summary(results)
## -----------------------------------------------------------------------------
#Traditional Model Forecasting (dynamic, no stochastic)
## -----------------------------------------------------------------------------
#(1) correctly initialize the forecasting Dataset

countries_gr <- c("bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", "pt")

first_column <- Dataset[, 1, drop = FALSE]
forecasting_dataset <- data.frame(First_Column = first_column)

forecasting_dataset <- forecasting_dataset %>%
  filter(Date >= ymd("2010-01-01"), Date <= ymd("2012-12-01"))



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
      date_indices_dl<-which(finalDataset$Date=="2019-12-01")
      forecasting_dataset[[paste0("diff_",country)]][date_indices]<-results[["coefficients"]][[paste0(country,"_(Intercept)")]]+results[["coefficients"]][[paste0(country,"_yield_s_", country, "_lag")]]*finalDataset[[x1_var]][date_indices_d]+
        results[["coefficients"]][[paste0(country,"_us_corp_spread_lag")]]*finalDataset[[x2_var]][date_indices_d]+
        results[["coefficients"]][[paste0(country,"_I(debt_gdp_s_",country,"/60)")]]*finalDataset[[x3_var]][date_indices_d]/60+
        results[["coefficients"]][[paste0(country,"_I(deficit_gdp_s_",country,"/3)")]]*finalDataset[[x4_var]][date_indices_d]/3+
        results[["coefficients"]][[paste0(country,"_us_corp_spread_diff")]]*finalDataset[[x5_var]][date_indices_d]+results[["coefficients"]][[paste0(country,"_glob_sp1_",country,"_lag")]]*finalDataset[[x6_var]][date_indices_d]+
        results[["coefficients"]][[paste0(country,"_glob_sp2_",country,"_lag")]]*finalDataset[[x7_var]][date_indices_d]
 
        date_indices_D<-which(Dataset$Date=="2009-12-01")
        forecasting_dataset[[paste0("yield_s_",country,"_0")]][date_indices]<-forecasting_dataset[[paste0("diff_",country)]][date_indices]+Dataset[[paste0("yield_s_",country)]][date_indices_D]
  }

date_indices_D<-which(Dataset$Date=="2010-01-01")
for (country in countries){
  for (countri in countries_gr){
  if (countri!=country){
          if (countri !="gr"){
            print(paste0("W1_",toupper(country),"_",countri))
        forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]<-Dataset[[paste0("W1_",toupper(country),"_",countri)]][[date_indices_D]]*forecasting_dataset[[paste0("yield_s_",countri, "_0")]][date_indices]+forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]
          }
          else{print(paste0("W1_",toupper(country),"_",countri))
            forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]<-Dataset[[paste0("W1_",toupper(country),"_",countri)]][[date_indices_D]]*finalDataset[[paste0("yield_s_",countri)]][date_indices_d]+forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]
          }}}}
        
for (country in countries){
  for (countri in countries_gr){
    if (countri!=country){
      if (countri !="gr"){
        forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]<-Dataset[[paste0("W2_",toupper(country),"_",countri)]][[date_indices_D]]*forecasting_dataset[[paste0("yield_s_",countri, "_0")]][date_indices]+forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]
      }
      else{
        print(countri)
        forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]<-Dataset[[paste0("W2_",toupper(country),"_",countri)]][[date_indices_D]]*finalDataset[[paste0("yield_s_",countri)]][date_indices_d]+forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]
      }}}}      
      
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
        if (countri !="gr"){
          forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]<-Dataset[[paste0("W1_",toupper(country),"_",countri)]][[date_indices_D]]*forecasting_dataset[[paste0("yield_s_",countri, "_0")]][date_indices]+forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]
        }
        else{
          forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]<-Dataset[[paste0("W1_",toupper(country),"_",countri)]][[date_indices_D]]*finalDataset[[paste0("yield_s_",countri)]][date_indices_d]+forecasting_dataset[[paste0("glob_sp1_",country, "_lag")]][date_indices]
        }}}}
  
  for (country in countries){
    for (countri in countries_gr){
      if (countri!=country){
        if (countri !="gr"){
          forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]<-Dataset[[paste0("W2_",toupper(country),"_",countri)]][[date_indices_D]]*forecasting_dataset[[paste0("yield_s_",countri, "_0")]][date_indices]+forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]
        }
        else{
          forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]<-Dataset[[paste0("W2_",toupper(country),"_",countri)]][[date_indices_D]]*finalDataset[[paste0("yield_s_",countri)]][date_indices_d]+forecasting_dataset[[paste0("glob_sp2_",country, "_lag")]][date_indices]
        }}}} 

}
## -----------------------------------------------------------------------------






