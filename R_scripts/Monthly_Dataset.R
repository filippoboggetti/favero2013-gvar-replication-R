################################################################################
###
### AUTHOR:       Filippo Felice Boggetti
### DATE:         Spring 2024
### DESCRIPTION:  This script creates the monthly Dataset to be used for the  
###               for the replication of Favero (2013).
###
### OUTPUT:       One Dataframe: Dataset.rds 
###
################################################################################

rm(list=ls())
## -----------------------------------------------------------------------------
Dataset <- read_rds("Database.RDS/Dataset.rds")
Debt <- read_rds("Database.RDS/Debt.rds")

dataset_xts <- xts(Dataset[-1], order.by = as.Date(Dataset$Date))
monthly_avg_xts <- apply.monthly(dataset_xts, colMeans, na.rm = TRUE)
monthly_avg_df <- as.data.frame(monthly_avg_xts)

monthly_avg_df$Date <- as.Date(format(index(monthly_avg_xts), "%Y-%m-01"))

monthly_avg_df$Month_Year <- format(index(monthly_avg_xts), "%Y M%m")
monthly_avg_df$Month_Year <- sub(" M0", " M", monthly_avg_df$Month_Year) 

monthly_avg_df <- monthly_avg_df[, c("Date", "Month_Year", setdiff(names(monthly_avg_df), c("Date", "Month_Year")))]

Dataset <- monthly_avg_df

rm(list=setdiff(ls(), c("Dataset", "Debt")))
## -----------------------------------------------------------------------------
Debt<-read_rds("Database.RDS/Debt.rds")
debt_xts <- xts(Debt[-1], order.by = as.Date(Debt$Date))

monthly_last_xts <- apply.monthly(debt_xts, last)
monthly_last_df <- data.frame(as.data.frame(monthly_last_xts), row.names = NULL)
monthly_last_df$Date <- as.Date(paste0(substr(index(monthly_last_xts), 1, 4), "-", 
                                       sprintf("%02d", month(index(monthly_last_xts))), "-01"))

formatted_dates <- format(index(monthly_last_xts), "%Y M%m")
formatted_dates <- sub(" M0", " M", formatted_dates) 
monthly_last_df$Month_Year <- formatted_dates

monthly_last_df <- monthly_last_df[, c("Date", "Month_Year", names(monthly_last_df)[!names(monthly_last_df) %in% c("Date", "Month_Year")])]

Debt<-monthly_last_df

rm(list=setdiff(ls(), c("Dataset", "Debt")))

## -----------------------------------------------------------------------------
merged_df <- merge(Dataset, Debt, by = c("Date", "Month_Year"))                 #Merge the two dataframe to create  complete Dataset
Dataset<-merged_df

Dataset <- Dataset %>%
  filter(Date <= as.Date("2013-04-01"))

write.xlsx(Dataset, file = "backup_excel/Dataset_monthly.xlsx")
dir.create("Dataset", showWarnings = FALSE)
saveRDS(Dataset, file = "Database.RDS/Dataset_monthly.rds")

rm(list=setdiff(ls(),"Dataset"))

## -----------------------------------------------------------------------------
rm(list=ls())
gc()

