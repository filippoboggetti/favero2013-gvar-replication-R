################################################################################
###
### AUTHOR:       Filippo Felice Boggetti
### DATE:         Spring 2024
### DESCRIPTION:  This script creates a frozen version of the database to be used 
###               for the replication of Favero (2013).
### OUTPUT:       Two Database Dataset.rds and Debt.rds. Note that differently
###               from Eviews those are intermediate results (need aggregation)
################################################################################

## -----------------------------------------------------------------------------
trade_weights <- read_excel("Database_cf1.xls", sheet="d_trade_weights",        #Import trade weights
                            range = "A2:DR3180")
colnames(trade_weights) <- tolower(colnames(trade_weights))
colnames(trade_weights)[1] = "Date"
trade_weights$Date<-as.Date(trade_weights$Date)

countries <- c("bd", "bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", "pt")

for (country in countries) {
  var_name <- paste0("w_", country, "_", country, "_tr")
  if (var_name %in% names(trade_weights)) {
    trade_weights[[var_name]] <- NULL
  }
}
rm(list = setdiff(ls(), "trade_weights"))

## -----------------------------------------------------------------------------
d_ratings <- read_excel("Database_cf1.xls", sheet="d_Ratings",                  #Import ratings
                        range = "A2:N3123")
colnames(d_ratings) <- tolower(colnames(d_ratings))
colnames(d_ratings)[1] = "Date"
d_ratings$Date<-as.Date(d_ratings$Date)

d_ratings <- d_ratings %>%
  rename(
    rating_bd=germany,
    rating_bg=belgium,
    rating_es=spain,
    rating_fn=finland,
    rating_fr=france,
    rating_gr=greece,
    rating_ir=ireland,
    rating_it=italy,
    rating_nl=netherlands,
    rating_oe=austria,
    rating_pt=portugal,
    rating_us=us,
    rating_uk=uk
  )

rm(list = setdiff(ls(), c("trade_weights", "d_ratings")))

## -----------------------------------------------------------------------------
cds_5Y <- read_excel("Database_cf1.xls", sheet="d_CDS_Premia_10Y_&_5Y",         #Import cds 5 years
                     range = "A2:AA3205")
colnames(cds_5Y) <- tolower(colnames(cds_5Y))
colnames(cds_5Y)[1] = "Date"
cds_5Y$Date<-as.Date(cds_5Y$Date)
source("R_scripts/Clean1.R")
cds_5Y<-clean_data_frame1(cds_5Y)

country_codes <- c("bd", "bg", "es", "fr", "gr", "ir", "it", "nl", 
                   "oe", "pt", "us", "uk")
cols_to_remove <- c()

for(code in country_codes) {
  pattern <- paste0(code, "gvtsx")
  cols_to_remove <- c(cols_to_remove, grep(pattern, names(cds_5Y), value = TRUE))
}
cols_to_remove <- unique(cols_to_remove)
cds_5Y <- cds_5Y[, !names(cds_5Y) %in% cols_to_remove]

for(code in country_codes) {
  col_name_original <- paste0(code, "gvts5")
  col_name_cds <- paste0("cds_", code)
  names(cds_5Y)[names(cds_5Y) == col_name_original] <- col_name_cds
  cds_5Y[[paste0("cds1_", code)]] <- cds_5Y[[col_name_cds]] / 100
  cds_5Y[[col_name_cds]] <- NULL
  names(cds_5Y)[names(cds_5Y) == paste0("cds1_", code)] <- paste0("cds_5Y_", code)
}

cds_5Y$cds_5Y_fn <- cds_5Y$finlds5 / 100
cds_5Y$finlds5 <- NULL
cds_5Y$finldsx<-NULL

rm(list = setdiff(ls(), c("trade_weights", "d_ratings", "cds_5Y")))

## -----------------------------------------------------------------------------
source("R_scripts/Clean1.R")                                                    #Import swap Rates
swap_rates<-read_excel("Database_cf1.xls", sheet="d_Benchmark_10Y_RY_&_Swaps",       
                       range = "A2:AC3402")
swap_rates<-clean_data_frame1(swap_rates)
colnames(swap_rates)[1] = "Date"
swap_rates$Date<-as.Date(swap_rates$Date)

names(swap_rates)[names(swap_rates) == "ICDEM10"] <- "swap_10Y_EU"
names(swap_rates)[names(swap_rates) == "ICUSD10"] <- "swap_10Y_US"
names(swap_rates)[names(swap_rates) == "ICGBP10"] <- "swap_10Y_UK"
columns_to_keep <- c("Date","swap_10Y_EU", "swap_10Y_US", "swap_10Y_UK")
swap_rates <- swap_rates[, names(swap_rates) %in% columns_to_keep]

rm(list = setdiff(ls(), c("trade_weights", "d_ratings", "cds_5Y", "swap_rates")))

## -----------------------------------------------------------------------------
source("R_scripts/Clean1.R")                                                              #Import Exchange Rates
exchange_rates<-read_excel("Database_cf1.xls", sheet="d_Exchange_rates",       
                           range = "A2:C3328")
exchange_rates<-clean_data_frame1(exchange_rates)
colnames(exchange_rates)[1] = "Date"
exchange_rates$Date<-as.Date(exchange_rates$Date)

exchange_rates$exchange_eu_us <- 1 / exchange_rates$TEUSDSP
exchange_rates$exchange_eu_uk <- 1 / exchange_rates$TSEURSP

exchange_rates$TEUSDSP <- NULL
exchange_rates$TSEURSP <- NULL

rm(list = setdiff(ls(), c("trade_weights", "d_ratings", "cds_5Y", "swap_rates", 
                          "exchange_rates")))

## -----------------------------------------------------------------------------
source("R_scripts/Clean1.R")                                                              #Import US Corporate AAA e BAA
Us_corporate_spread<-read_excel("Database_cf1.xls", sheet="d_US_corporate_AAA_BAA",       
                                range = "A2:C3400")
Us_corporate_spread<-clean_data_frame1(Us_corporate_spread)
colnames(Us_corporate_spread)[1] = "Date"
Us_corporate_spread$Date<-as.Date(Us_corporate_spread$Date)

names(Us_corporate_spread)[names(Us_corporate_spread) == "DAAA"] <- "us_corp_AAA"
names(Us_corporate_spread)[names(Us_corporate_spread) == "DBAA"] <- "us_corp_BAA"
Us_corporate_spread$us_corp_spread <- Us_corporate_spread$us_corp_BAA - 
  Us_corporate_spread$us_corp_AAA

rm(list = setdiff(ls(), c("trade_weights", "d_ratings", "cds_5Y", "swap_rates", 
                          "exchange_rates", "Us_corporate_spread")))

## -----------------------------------------------------------------------------
source("R_scripts/Clean1.R")                                                              #Import EIB bonds
EIB_10Y<-read_excel("Database_cf1.xls", sheet="d_EIB_10Y",       
                    range = "A2:B3328")
EIB_10Y<-clean_data_frame1(EIB_10Y)
colnames(EIB_10Y)[1] = "Date"
EIB_10Y$Date<-as.Date(EIB_10Y$Date)
names(EIB_10Y)[names(EIB_10Y) == "EIB_10Y"] <- "eib_10y"

rm(list = setdiff(ls(), c("trade_weights", "d_ratings", "cds_5Y", "swap_rates", 
                          "exchange_rates", "Us_corporate_spread", "EIB_10Y")))

## -----------------------------------------------------------------------------
source("R_scripts/Clean1.R")                                                              #Import yields
yields<-read_excel("Database_cf1.xls", sheet="d_Benchmark_10Y_RY_&_Swaps",       
                   range = "A2:N3402")
yields<-clean_data_frame1(yields)
colnames(yields)[1] = "Date"
yields$Date<-as.Date(yields$Date)

country_codes <- c("bd", "bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", 
                   "pt", "us", "uk")

for(code in country_codes) {
  original_name <- paste0("BM", toupper(code), "10Y(RY)")
  final_name <- paste0("yield_", code)
  if(original_name %in% names(yields)) {
    names(yields)[names(yields) == original_name] <- final_name
  }
}

rm(list = setdiff(ls(), c("trade_weights", "d_ratings", "cds_5Y", "swap_rates", 
                          "exchange_rates", "Us_corporate_spread", "EIB_10Y",
                          "yields")))

## -----------------------------------------------------------------------------
source("R_scripts/Clean1.R")                                                              #Import CDS 10 year
cds_10Y<-read_excel("Database_cf1.xls", sheet="d_CDS_Premia_10Y_&_5Y",       
                    range = "A2:N3205")
cds_10Y<-clean_data_frame1(cds_10Y)
colnames(cds_10Y)[1] = "Date"
cds_10Y$Date<-as.Date(cds_10Y$Date)

country_codes <- c("bd", "bg", "es", "fr", "gr", "ir", "it", "nl", "oe", 
                   "pt", "us", "uk")

for (code in country_codes) {
  original_col <- paste0(toupper(code), "GVTSX")
  new_col <- paste0("cds_", code)
  names(cds_10Y)[names(cds_10Y) == original_col] <- new_col
  cds_10Y[[paste0("cds1_", code)]] <- cds_10Y[[new_col]] / 100
  cds_10Y[[new_col]] <- NULL
  names(cds_10Y)[names(cds_10Y) == paste0("cds1_", code)] <- paste0("cds_10Y_",code)
}

cds_10Y$cds_10Y_fn <- cds_10Y$FINLDSX / 100
cds_10Y$FINLDSX <- NULL

rm(list = setdiff(ls(), c("trade_weights", "d_ratings", "cds_5Y", "swap_rates", 
                          "exchange_rates", "Us_corporate_spread", "EIB_10Y",
                          "yields", "cds_10Y")))

## -----------------------------------------------------------------------------
source("R_scripts/Clean1.R")                                                              #Import CDS 10 year Newdata
cds_10Y_newdata <- read_excel("Database_cf1.xls",                               
                              sheet="d_CDS_Premia_10Y_Newdata", range = "A2:N3402")
cds_10Y_newdata<-clean_data_frame1(cds_10Y_newdata)
colnames(cds_10Y_newdata)[1] = "Date"
cds_10Y_newdata$Date<-as.Date(cds_10Y_newdata$Date)

country_codes <- c("bd" = "DEGA", "bg" = "BEGA", "es" = "ESGA", "fn" = "FIGA", "fr" = "FRGA", 
                   "gr" = "GRGA", "ir" = "IEGA", "it" = "ITGA", "nl" = "NLGA", "oe" = "ATGA", 
                   "pt" = "PTGA", "uk" = "GBGA")

for (code in names(country_codes)) {
  original_col <- paste0(country_codes[code], "$AC") 
  new_col <- paste0("cds_10Y_", code)       
  cds_10Y_newdata[[new_col]] <- cds_10Y_newdata[[original_col]] / 100
  cds_10Y_newdata[[original_col]] <- NULL
}

cds_10Y_newdata <- cds_10Y_newdata %>%
  rename(mmm_cds_usd_sr_10y_corp = `MMM CDS USD SR 10Y Corp`)

rm(list = setdiff(ls(), c("trade_weights", "d_ratings", "cds_5Y", "swap_rates", 
                          "exchange_rates", "Us_corporate_spread", "EIB_10Y",
                          "yields", "cds_10Y", "cds_10Y_newdata")))

## -----------------------------------------------------------------------------
shared_cols <- intersect(names(cds_10Y), names(cds_10Y_newdata))                #Merge CDS10Y and the newdata
unique_cols_cds_10Y <- setdiff(names(cds_10Y), shared_cols)
unique_cols_cds_10Y_newdata <- setdiff(names(cds_10Y_newdata), shared_cols)
shared_cols <- setdiff(shared_cols, "Date")

new_df <- data.frame(Date = cds_10Y_newdata$Date)

for(col in unique_cols_cds_10Y) {
  new_df[[col]] <- cds_10Y[[col]][match(new_df$Date, cds_10Y$Date)]
}

for(col in unique_cols_cds_10Y_newdata) {
  new_df[[col]] <- cds_10Y_newdata[[col]][match(new_df$Date, cds_10Y_newdata$Date)]
}

cutoff_date <- as.Date("2010-01-10")

for(col in shared_cols) {
  before_cutoff <- cds_10Y$Date < cutoff_date
  after_cutoff <- cds_10Y_newdata$Date >= cutoff_date
  new_df[[col]] <- NA
  new_df[match(cds_10Y$Date[before_cutoff], new_df$Date), col] <- cds_10Y[before_cutoff, col]
  new_df[match(cds_10Y_newdata$Date[after_cutoff], new_df$Date), col] <- cds_10Y_newdata[after_cutoff, col]
}

rm(list=c("cds_10Y", "cds_10Y_newdata"))
cds_10Y<-new_df

rm(list = setdiff(ls(), c("trade_weights", "d_ratings", "cds_5Y", "swap_rates", 
                          "exchange_rates", "Us_corporate_spread", "EIB_10Y",
                          "yields", "cds_10Y")))

## -----------------------------------------------------------------------------
start_date <- as.Date("2000-01-03")                                             #Merge everything in a unique dataframe
end_date <- as.Date("2013-12-31")                                               #with 5 days week spanning from 2000-03-01
dates <- seq(start_date, end_date, by="day")                                    #to 2013-12-31
dates <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")] 
Dataset <- data.frame(Date = dates)

dataframes_to_merge <- list(cds_10Y, cds_5Y, d_ratings, EIB_10Y, 
                            exchange_rates, swap_rates, trade_weights, 
                            Us_corporate_spread, yields)

source("R_scripts/Safe_merge.R")

for(df in dataframes_to_merge) {
  Dataset <- safe_merge_by_date(Dataset, df)
}

cutoff_date <- as.Date("2012-03-07")

columns_from_trade_weights <- names(trade_weights)

for (col in columns_from_trade_weights) {
  if(col %in% names(Dataset)) {
    indices_before_cutoff <- which(Dataset$Date <= cutoff_date)
    Dataset[indices_before_cutoff, col] <- na.locf(Dataset[indices_before_cutoff, col], na.rm = FALSE)
  }
}

rm(list = setdiff(ls(), "Dataset"))

## -----------------------------------------------------------------------------
country_codes <- c("bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", "pt")  #Differences with respect to Germany

for (code in country_codes) {
  Dataset[[paste0("cds_10Y_s_", code)]] <- Dataset[[paste0("cds_10Y_", code)]] - Dataset[["cds_10Y_bd"]]
  Dataset[[paste0("cds_5Y_s_", code)]] <- Dataset[[paste0("cds_5Y_", code)]] - Dataset[["cds_5Y_bd"]]
  Dataset[[paste0("yield_s_", code)]] <- Dataset[[paste0("yield_", code)]] - Dataset[["yield_bd"]]
}

Dataset$yield_s_us <- Dataset$yield_us - Dataset$yield_bd
Dataset$cds_10Y_s_us <- Dataset$cds_10Y_us - Dataset$cds_10Y_bd
Dataset$cds_5Y_s_us <- Dataset$cds_5Y_us - Dataset$cds_5Y_bd
Dataset$yield_s_uk <- Dataset$yield_uk - Dataset$yield_bd
Dataset$cds_10Y_s_uk <- Dataset$cds_10Y_uk - Dataset$cds_10Y_bd
Dataset$cds_5Y_s_uk <- Dataset$cds_5Y_uk - Dataset$cds_5Y_bd

rm(list = setdiff(ls(), "Dataset"))

#Check Against Eviews output
write.xlsx(Dataset, file = "backup_excel/Dataset.xlsx")
dir.create("Database.RDS")
saveRDS(Dataset, file = "Database.RDS/Dataset.rds")
## -----------------------------------------------------------------------------
source("R_scripts/Clean1.R")                                                              #Import deficit to gdp and debt to gdp
deficit_gdp_ex <- read_excel("Database_cf1.xls",                               
                             sheet="d_deficit_gdp_exp", range = "A2:L3478")
deficit_gdp_ex<-clean_data_frame1(deficit_gdp_ex)
colnames(deficit_gdp_ex)[1] = "Date"
deficit_gdp_ex$Date<-as.Date(deficit_gdp_ex$Date)

debt_gdp_ex <- read_excel("Database_cf1.xls",                               
                          sheet="d_debt_gdp_exp", range = "A2:L3478")
debt_gdp_ex<-clean_data_frame1(debt_gdp_ex)
colnames(debt_gdp_ex)[1] = "Date"
debt_gdp_ex$Date<-as.Date(debt_gdp_ex$Date)

Debt <- merge(deficit_gdp_ex, debt_gdp_ex, by = "Date")
rm(deficit_gdp_ex, debt_gdp_ex)

colnames(Debt)<-tolower(colnames(Debt))

country_codes <- c("bd", "bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", "pt")
for (code in country_codes) {
  Debt[[paste0("deficit_gdp_1_", code)]] <- Debt[[paste0("deficit_gdp_", code)]] * -1
  Debt[[paste0("deficit_gdp_", code)]] <- NULL
  names(Debt)[names(Debt) == paste0("deficit_gdp_1_", code)] <- paste0("deficit_gdp_", code)
}

country_codes <- setdiff(country_codes, "bd")
for (code in country_codes) {
  Debt[[paste0("deficit_gdp_s_", code)]] <- Debt[[paste0("deficit_gdp_", code)]] - Debt[["deficit_gdp_bd"]]
  Debt[[paste0("debt_gdp_s_", code)]] <- Debt[[paste0("debt_gdp_", code)]] - Debt[["debt_gdp_bd"]]
}
colnames(Debt)[1] = "Date"

rm(list = setdiff(ls(), c("Dataset", "Debt")))

new_dates <- setdiff(Dataset$Date, Debt$Date)
new_data <- data.frame(Date = new_dates)
columns_to_add <- setdiff(names(Debt), names(new_data))
for(col in columns_to_add) {
  new_data[[col]] <- NA
}

Debt <- rbind(Debt, new_data) %>% arrange(Date)

row_to_fill <- which(Debt$Date == "2012-10-31")
Debt[row_to_fill, -1] <- na.locf(Debt[row_to_fill - 1, -1])


rm(list = setdiff(ls(), c("Dataset", "Debt")))

###----------------------------------------------------------------------------- #Save the results
write.xlsx(Debt, file = "backup_excel/Debt.xlsx")
dir.create("Database.RDS")
saveRDS(Debt, file = "Database.RDS/Debt.rds")
