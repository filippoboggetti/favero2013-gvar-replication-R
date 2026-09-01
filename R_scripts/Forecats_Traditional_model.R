################################################################################
###
### AUTHOR:       Filippo Felice Boggetti
### DATE:         Spring 2024
### DESCRIPTION:  This script creates a frozen version of the database to be used 
###               for the replication of Favero (2013).
### OUTPUT:       one database: one for the macro variables and one for the term
###               structure    
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

filteredDataset <- finalDataset[finalDataset$Date >= as.Date("2000-01-01") & finalDataset$Date <= as.Date("2009-12-01"), ]

equations <- list()

countries <- c("bg", "es", "fn", "fr", "ir", "it", "nl", "oe", "pt")

for (country in countries){
  y1_var <- paste0("yield_s_", country, "_diff")
  x1_var <- paste0("yield_s_", country, "_lag")
  x2_var <- "us_corp_spread_lag"
  x3_var <- paste0("debt_gdp_s_", country)
  x4_var <- paste0("deficit_gdp_s_", country)
  x5_var <- "us_corp_spread_diff"  
  
  formula_str <- sprintf("%s ~ %s + %s + I(%s/60) + I(%s/3) + %s",
                         y1_var, x1_var, x2_var, x3_var, x4_var, x5_var)
  
  equations[[country]] <- as.formula(formula_str)
}

results <- systemfit(equations, data = filteredDataset, method = "SUR")

summary(results)
## -----------------------------------------------------------------------------
#Traditional Model Forecasting (dynamic, no stochastic)

first_column <- Dataset[, 1, drop = FALSE]
forecasting_dataset <- data.frame(First_Column = first_column)

forecasting_dataset <- forecasting_dataset %>%
  filter(Date >= ymd("2010-01-01"), Date <= ymd("2012-12-01"))

for (country in countries){
  x1_var <- paste0("yield_s_", country, "_lag")
  x2_var <- "us_corp_spread_lag"
  x3_var <- paste0("debt_gdp_s_", country)
  x4_var <- paste0("deficit_gdp_s_", country)
  x5_var <- "us_corp_spread_diff"
  forecasting_dataset[[paste0("diff_",country)]]<-0
  forecasting_dataset[[paste0("yield_s_",country,"_0")]]<-0
  for (i in 1:length(forecasting_dataset$Date)){
    if (i==1){
    date <- forecasting_dataset$Date[i]
    date_indices <- which(forecasting_dataset$Date == date)
    date_indices_d<-which(finalDataset$Date==date)
    forecasting_dataset[[paste0("diff_",country)]][date_indices]<-results[["coefficients"]][[paste0(country,"_(Intercept)")]]+results[["coefficients"]][[paste0(country,"_yield_s_", country, "_lag")]]*finalDataset[[x1_var]][date_indices_d]+
    results[["coefficients"]][[paste0(country,"_us_corp_spread_lag")]]*finalDataset[[x2_var]][date_indices_d]+
    results[["coefficients"]][[paste0(country,"_I(debt_gdp_s_",country,"/60)")]]*finalDataset[[x3_var]][date_indices_d]/60+
    results[["coefficients"]][[paste0(country,"_I(deficit_gdp_s_",country,"/3)")]]*finalDataset[[x4_var]][date_indices_d]/3+
    results[["coefficients"]][[paste0(country,"_us_corp_spread_diff")]]*finalDataset[[x5_var]][date_indices_d]
    if (date == as.Date("2010-01-01")) {
      date_indices_D<-which(Dataset$Date=="2009-12-01")
      forecasting_dataset[[paste0("yield_s_",country,"_0")]][date_indices]<-forecasting_dataset[[paste0("diff_",country)]][date_indices]+Dataset[[paste0("yield_s_",country)]][date_indices_D]
    } else {
      date_l <- forecasting_dataset$Date[i - 1]
      date_indices_l<-which(forecasting_dataset$Date==date_l)
      forecasting_dataset[[paste0("yield_s_",country,"_0")]][date_indices]<-forecasting_dataset[[paste0("diff_",country)]][date_indices]+forecasting_dataset[[paste0("yield_s_",country,"_0")]][date_indices_l]
    }
    } else{
      date <- forecasting_dataset$Date[i]
      date_l <- forecasting_dataset$Date[i - 1]
      date_indices <- which(forecasting_dataset$Date == date)
      date_indices_d<-which(finalDataset$Date==date)
      date_indices_l<-which(forecasting_dataset$Date==date_l)
      forecasting_dataset[[paste0("diff_",country)]][date_indices]<-results[["coefficients"]][[paste0(country,"_(Intercept)")]]+results[["coefficients"]][[paste0(country,"_yield_s_", country, "_lag")]]*forecasting_dataset[[paste0("yield_s_",country,"_0")]][date_indices_l]+
        results[["coefficients"]][[paste0(country,"_us_corp_spread_lag")]]*finalDataset[[x2_var]][date_indices_d]+
        results[["coefficients"]][[paste0(country,"_I(debt_gdp_s_",country,"/60)")]]*finalDataset[[x3_var]][date_indices_d]/60+
        results[["coefficients"]][[paste0(country,"_I(deficit_gdp_s_",country,"/3)")]]*finalDataset[[x4_var]][date_indices_d]/3+
        results[["coefficients"]][[paste0(country,"_us_corp_spread_diff")]]*finalDataset[[x5_var]][date_indices_d] 
      forecasting_dataset[[paste0("yield_s_",country,"_0")]][date_indices]<-forecasting_dataset[[paste0("diff_",country)]][date_indices]+forecasting_dataset[[paste0("yield_s_",country,"_0")]][date_indices_l]
}}}
## -----------------------------------------------------------------------------
#Construct a printable dataframe

countries <- c("bg", "es", "fn", "fr", "ir", "it", "nl", "oe", "pt")

yield_columns <- paste0("yield_s_", countries)

columns_to_select <- c("Date", yield_columns)
columns_to_select <- columns_to_select[columns_to_select %in% names(Dataset)]

printable_dataframe <- Dataset[, columns_to_select]

for(country in countries) {
  column_name <- paste0("yield_s_", country,"_0")
  temp_forecast <- forecasting_dataset[c("Date", column_name)]
  printable_dataframe <- merge(printable_dataframe, temp_forecast, by="Date", all.x=TRUE)
}
## -----------------------------------------------------------------------------

printable_dataframe$Date <- as.Date(printable_dataframe$Date)

countries_to_plot <- c("es", "ir", "it", "pt")
plots_list <- list()

# Open the PDF device
pdf(file.path("plots", "Fig7.1.pdf"), width = 11, height = 8)

for (country in countries_to_plot) {
  country_data <- printable_dataframe %>%
    filter(Date >= as.Date("2000-01-01"), Date <= as.Date("2012-12-01"))
  
  yield_s <- paste0("yield_s_", country)
  yield_0_s <- paste0("yield_s_", country, "_0")
  
  p <- ggplot(country_data, aes_string(x = "Date")) +
    geom_line(aes_string(y = yield_s, colour = "'Actual Spread'"), size = 1) +
    geom_line(aes_string(y = yield_0_s, colour = "'Simulated Spread'"), size = 1) +
    geom_vline(xintercept = as.numeric(as.Date("2010-01-01")), colour = "black", linetype = "dashed") +
    scale_colour_manual(values = c("Actual Spread" = "green", "Simulated Spread" = "red")) +
    scale_y_continuous(limits = c(0, 12)) +
    theme_minimal() +
    labs(title = paste("Yield Data for", toupper(country))) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y")
  
  print(p)
}

dev.off()


countries_to_plot <- c("bg", "fr", "fn", "nl", "oe")

pdf(file.path("plots", "Fig7.2.pdf"), width = 11, height = 8)

for (country in countries_to_plot) {
  country_data <- printable_dataframe %>%
    filter(Date >= as.Date("2000-01-01"), Date <= as.Date("2012-12-01"))
  
  yield_s <- paste0("yield_s_", country)
  yield_0_s <- paste0("yield_s_", country, "_0")
  
  p <- ggplot(country_data, aes_string(x = "Date")) +
    geom_line(aes_string(y = yield_s, colour = "'Actual Spread'"), size = 1) +
    geom_line(aes_string(y = yield_0_s, colour = "'Simulated Spread'"), size = 1) +
    geom_vline(xintercept = as.numeric(as.Date("2010-01-01")), colour = "black", linetype = "dashed") +
    scale_colour_manual(values = c("Actual Spread" = "green", "Simulated Spread" = "red")) +
    scale_y_continuous(limits = c(NA, NA)) + 
    theme_minimal() +
    labs(title = paste("Yield Data for", toupper(country))) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    theme(legend.position = "bottom") 
  
  print(p)
}

dev.off()




























