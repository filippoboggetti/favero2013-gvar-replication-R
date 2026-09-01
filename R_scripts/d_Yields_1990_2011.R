################################################################################
###
### AUTHOR:       Filippo Felice Boggetti
### DATE:         Spring 2024
### DESCRIPTION:  This script creates d_Yields_1990_2011  to match Eviews 
###               for the replication of Favero (2013).
### OUTPUT:       One Database: d_Yields_1990_2011.rds
###               
################################################################################


## -----------------------------------------------------------------------------
source("R_scripts/Clean1.R")                                                    #Import Yields_1990_2011
d_yields_1990_2011 <- read_excel("Database_cf1.xls",                               
                             sheet="d_Yields_1990_2011", range = "A2:N5938")
d_yields_1990_2011<-clean_data_frame1(d_yields_1990_2011)
colnames(d_yields_1990_2011)[1] = "Date"
d_yields_1990_2011$Date<-as.Date(d_yields_1990_2011$Date)

countries_1 <- c("bd", "bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", 
                 "pt", "us", "uk")

for(code in countries_1) {
  col_name <- paste0("BM", toupper(code), "10Y(RY)")
  new_col_name <- paste0("yield_", code) 
  d_yields_1990_2011[[new_col_name]] <- d_yields_1990_2011[[col_name]]
  d_yields_1990_2011[[col_name]] <- NULL
}

countries_2 <- c("bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", "pt", 
                 "us", "uk")

for(code in countries_2) {
  new_col_name <- paste0("yield_s_", code) 
  yield_col_name <- paste0("yield_", code) 
  d_yields_1990_2011[[new_col_name]] <- d_yields_1990_2011[[yield_col_name]] - 
    d_yields_1990_2011[["yield_bd"]]
}

write.xlsx(d_yields_1990_2011, file = "backup_excel/d_Yields_1990_2011.xlsx")
saveRDS(d_yields_1990_2011, file = "Database.RDS/d_Yields_1990_2011.rds")

rm(list=setdiff(ls(),"d_yields_1990_2011"))

## -----------------------------------------------------------------------------
rm(list=ls())
gc()

## -----------------------------------------------------------------------------






















