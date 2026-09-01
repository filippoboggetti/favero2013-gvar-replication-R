################################################################################
###
### AUTHOR:       Filippo Felice Boggetti
### DATE:         Spring 2024
### DESCRIPTION:  This script replicates the first part of the exploratory Data  
###               analysis for the replication of Favero (2013).
### OUTPUT:       "plots" folder: Figure 1 (pag. 345) and Figure 2 (pag.345)
### 
################################################################################

## -----------------------------------------------------------------------------
population_data <- read_excel('Database_cf1.xls', sheet = "y_pop_raw",          #Import population data and transform it in thousands, 
                              range = "A2:L16")                                 #Store in a new data frame called "population", eliminate
                                                                                #"population_data"
colnames(population_data)[1] <- "date"

colnames(population_data) <- tolower(gsub("[ ,]", "_", 
                                          colnames(population_data)))

codes <- c("bd", "bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", "pt")

population <- population_data %>%dplyr:: select(date)

for (code in codes) {
  population_code_column <- paste0(code, "espopu")
  if (population_code_column %in% colnames(population_data)) {
    population[[paste0(code, "_population")]] <- as.numeric(gsub(",", ".", 
                            population_data[[population_code_column]])) / 1000
  }
}
rm(population_data)
#print(population)
rm(list = setdiff(ls(), c("population", "gdp_df")))

## -----------------------------------------------------------------------------
source("R_scripts/Clean.R")
#source("ConvertQ.R")
gdp_data <- read_excel("Database_cf1.xls", sheet = "q_gdp_with_spain",          #Import data on gdp, compute the pro capita gdp 
                       range = "A2:L53")                                        #Log the procapita gdp. Save on gdp_df eliminate 
                                                                                #temporary variables. Note that when converting from
colnames(gdp_data)[1] <- "date"                                                 #low frequency (quarter) to high we use linear interpolation
colnames(gdp_data) <- tolower(gsub("[ ,.]", "_", colnames(gdp_data)))           #from the last data e.g. the annual value of 1999 and 2000
gdp_data<-as.data.frame(gdp_data)                                               #will be brought to 1999:Q4 and 2000:Q4 and intermediate values
                                                                                #are linearly interpolated
gdp_data<- clean_data_frame(gdp_data)

gdp_data[, startsWith(names(gdp_data), "bd")] <- gdp_data[, 
                                      startsWith(names(gdp_data), "bd")] * 1000

country_codes <- c("bd", "bg", "es", "fn", "fr", "gr", "ir", "it", "nl",
                   "oe", "pt")

gdp_df <- gdp_data

for (code in country_codes) {
  population_column_name <- paste0(code, "_population")
  
  annual_population <- population[[population_column_name]]

  annual_time_series <- zoo(annual_population, as.yearqtr(seq(from = 1999, 
        by = 1, length.out = length(annual_population)), format = "%Y") + 0.75)
  

  quarterly_seq <- seq(start(annual_time_series), 
                       end(annual_time_series) + 3/4, by = 1/4)
  

  quarterly_population <- na.approx(annual_time_series, xout = quarterly_seq)
  quarterly_population<-quarterly_population[-1]
  
  if (length(quarterly_population) > nrow(gdp_df)) {
    quarterly_population <- head(quarterly_population, nrow(gdp_df))
  }
  
  if (length(quarterly_population) != nrow(gdp_df)) {
    stop(paste("Length mismatch for", code, ": expected", nrow(gdp_df), 
               "got", length(quarterly_population)))
  }
  
  gdp_df[[paste0(code, "_gdp_pc")]] <- gdp_df[[code]] / quarterly_population
  gdp_df[[paste0(code, "_gdp_pc_log")]] <- log(gdp_df[[paste0(code, "_gdp_pc")]])
}


folderName <- "backup_excel"
fileName <- "Check1.xlsx"
                                                                                #Through the code this command will be pervasive, 
if (!dir.exists(folderName)) {                                                  #it has no other functionality than providing and Excel                                                
  dir.create(folderName)                                                        #for sanity checks
}

filePath <- file.path(folderName, fileName)

write.xlsx(gdp_df, file = filePath)
rm(list = setdiff(ls(), c("population","gdp_df", "country_codes")))

## -----------------------------------------------------------------------------
date_column <- "date"                                                           #Construct the plot 1 (a) of the paper, pag. 345

demeaned_df <- data.frame(date = gdp_df[[date_column]])

for (code in country_codes) {
  gdp_column <- paste0(code, "_gdp_pc_log")
  mean_value <- mean(gdp_df[[gdp_column]], na.rm = TRUE) 
  demeaned_df[[code]] <- gdp_df[[gdp_column]] - mean_value
}

demeaned_df$date <- as.character(demeaned_df$date)

demeaned_df$date <- sub("(Q\\d) (\\d{4})", "\\2 \\1", demeaned_df$date)

demeaned_df$date <- as.yearqtr(demeaned_df$date, format = "%Y Q%q")

demeaned_df <- demeaned_df %>%
  mutate(
    year = as.integer(sub(" Q[1-4]", "", date)),  
    quarter = as.integer(sub(".*Q", "", date)),   
    month = case_when(
      quarter == 1 ~ 1,
      quarter == 2 ~ 4,
      quarter == 3 ~ 7,
      quarter == 4 ~ 10
    ),
    day = 1,
    date = as.Date(paste(year, month, day, sep = "-")) 
  ) %>%
  dplyr::select(-c(year, quarter, month, day)) 

demeaned_df$date<-as.Date(demeaned_df$date)

#print(demeaned_df)
demeaned_df_filtered <- demeaned_df %>%
  filter(as.Date(date) <= as.Date("2011-06-30"))

folderName <- "backup_excel"
fileName2 <- "Check2.xlsx"
filePath2 <- file.path(folderName, fileName2)
write.xlsx(demeaned_df_filtered, file = filePath2)



#Plotting
country_colors <- c("Italy" = "black", "Spain" = "blue", 
                    "Portugal" = "red", "France" = "green4", "Finland" = "cyan",
                    "Austria" = "yellow","Netherland" = "darkorange", 
                    "Belgium" = "orange", "Greece" = "green",
                    "Ireland" = "brown")

country_linetypes <- c("Italy" = "solid", "Spain" = "longdash", 
                       "Portugal" = "dashed", "France" = "dotted", 
                       "Finland" = "twodash", "Austria" = "dotdash",
                       "Netherland" = "longdash", "Belgium" = "dashed", 
                       "Greece" = "solid","Ireland" = "solid")

plot1<-ggplot(data = demeaned_df_filtered, aes(x = date)) +
  geom_line(aes(y = it, color = "Italy", linetype = "Italy"), size = 1) +
  geom_line(aes(y = es, color = "Spain", linetype = "Spain"), size = 1) +
  geom_line(aes(y = pt, color = "Portugal", linetype = "Portugal"), size = 1) +
  geom_line(aes(y = fr, color = "France", linetype = "France"), size = 1) +
  geom_line(aes(y = fn, color = "Finland", linetype = "Finland"), size = 1) +
  geom_line(aes(y = oe, color = "Austria", linetype = "Austria"), size = 1) +
  geom_line(aes(y = nl, color = "Netherland", linetype = "Netherland"), size = 1) +
  geom_line(aes(y = bg, color = "Belgium", linetype = "Belgium"), size = 1) +
  geom_line(aes(y = gr, color = "Greece", linetype = "Greece"), size = 1) +
  geom_line(aes(y = ir, color = "Ireland", linetype = "Ireland"), size = 1) +
  scale_color_manual(values = country_colors) +
  scale_linetype_manual(values = country_linetypes, guide = FALSE) + 
  theme_minimal() +
  labs(title = "(log-demeaned) GDP", x = "", y = "") +
  theme(legend.position = "right")+
  ylim(-0.20, 0.15)


# folder_name <- "plots"
# folder_path <- file.path(getwd(), folder_name)
# dir.create(folder_path)
# file_name <- file.path(folder_path, "demeaned_gdp_per_capita.pdf")
# ggsave(filename = file_name, plot = plot1, width = 10, height = 6, dpi = 300)

rm(list = setdiff(ls(), c("plot1", "gdp_df")))
## -----------------------------------------------------------------------------
source("R_scripts/Clean1.R")                                                              #This is a simple function that allows to clean data from NA read improperly

data <- read_excel("Database_cf1.xls", sheet = "d_Benchmark_10Y_RY_&_Swaps",
                   range = "A2:N3402")
colnames(data)[1] <- "date"
data<- clean_data_frame1(data)
country_codes <- c("bd", "bg", "es", "fn", "fr", "gr", "ir", "it", "nl",
                   "oe", "pt", "us", "uk")

for (code in country_codes) {
  old_col_name <- paste0("BM", toupper(code), "10Y(RY)")
  new_col_name <- paste0("yield_", code)
  if (old_col_name %in% names(data)) {
    names(data)[names(data) == old_col_name] <- new_col_name
  }
}

for (code in country_codes[-1]) {
  print(code)
  yield_col_name <- paste0("yield_", code)
  new_series_name <- paste0("yield_s_", code)
  if (yield_col_name %in% names(data)) {
    data[[new_series_name]] <- data[[yield_col_name]] - data[["yield_bd"]]
  }
}

data$date <- as.Date(data$date)

data_xts <- xts(data[, -which(names(data) == "date")], order.by = data$date)

dir.create("backup_excel")
write.xlsx(gdp_df, file = "backup_excel/Check1.xlsx")
#write.xlsx(demeaned_df_filtered, file = "backup_excel/Check2.xlsx")
write.xlsx(data_xts, file = "backup_excel/Check3.xlsx")

data_f<-as.data.frame(data_xts)
df <- rownames_to_column(data_f, var = "date")
rm("data_f")
df$date<-as.Date(df$date)


country_codes <- c("bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", "pt")

columns_to_average <- paste0("yield_s_", country_codes)

monthly <- df %>%
  mutate(month_year = floor_date(date, "month")) %>%
  group_by(month_year) %>%
  summarise(across(all_of(columns_to_average), 
                   ~mean(.x, na.rm = TRUE), 
                   .names = "{.col}"), 
            .groups = "drop") %>%
  ungroup()

monthly$month_year<-as.Date(monthly$month_year)
monthly_filtered<-monthly %>%
  filter(month_year>=as.Date("2000-01-01") & month_year<=as.Date("2013-04-30"))


monthly_filtered_xts <- xts(monthly_filtered[, -which(names(monthly_filtered)
                      == "month_year")], order.by = monthly_filtered$month_year)



convert_to_quarterly <- function(x) {
  to_quarterly <- apply.quarterly(x, last)
  return(to_quarterly)
}

quarterly_data <- list()
for (code in country_codes) {
  yield_col_name <- paste0("yield_s_", code)
  if (yield_col_name %in% names(monthly_filtered_xts)) {
    quarterly_data[[yield_col_name]] <- convert_to_quarterly(monthly_filtered_xts[,
                                                              yield_col_name])
  }
}

standardize_quarter_dates <- function(xts_data) {
  qtr_dates <- as.yearqtr(index(xts_data))
  new_index <- as.Date(paste(year(qtr_dates),
            (quarter(qtr_dates) - 1) * 3 + 1, "01", sep="-"), format="%Y-%m-%d")
  index(xts_data) <- new_index
  return(xts_data)
}

quarterly_data_adjusted <- lapply(quarterly_data, standardize_quarter_dates)
rm("quarterly_data")
quarterly_xts <- do.call(merge, quarterly_data_adjusted)
rm("quarterly_data_adjusted")
quarterly_df <- as.data.frame(quarterly_xts)
rm("quarterly_xts")
quarterly_df$date <- as.Date(row.names(quarterly_df))


#Sanity Check
write.xlsx(quarterly_df, file = "backup_excel/Check5.xlsx")

#Up to end of 2012 
quarterly_df<-quarterly_df %>%
  filter(date>=as.Date("2000-01-01") & date<=as.Date("2012-12-31"))

quarterly_melted_filtered <- quarterly_df %>%
pivot_longer(-date, names_to = "Country", values_to = "Spread")
quarterly_melted_filtered$Country <- gsub("yield_s_", "", 
                                          quarterly_melted_filtered$Country)

quarterly_melted_filtered$date<-as.Date(quarterly_melted_filtered$date)

line_colors <- c("bg" = "orange",          # Belgium
                 "es" = "blue",            # Spain
                 "fn" = "cyan",            # Finland
                 "fr" = "green4",          # France
                 "gr" = "green",           # Greece
                 "ir" = "brown",           # Ireland
                 "it" = "black",           # Italy
                 "nl" = "darkorange",      # Netherland
                 "oe" = "yellow",          # Austria 
                 "pt" = "red")             # Portugal

line_types <- c("bg" = "dashed",           # Belgium
                "es" = "longdash",         # Spain
                "fn" = "twodash",          # Finland
                "fr" = "dotted",           # France
                "gr" = "solid",            # Greece
                "ir" = "solid",            # Ireland
                "it" = "solid",            # Italy
                "nl" = "longdash",         # Netherland
                "oe" = "dotdash",          # Austria 
                "pt" = "dashed")           # Portugal


plot2<-ggplot(quarterly_melted_filtered, aes(x = date,y = Spread, 
                                      color = Country, linetype = Country)) +
  geom_line(size = 1.25) +
  scale_color_manual(values = line_colors) +
  scale_linetype_manual(values = line_types) +
  labs(title = "Spreads on German Bunds", x = "", y = "Spread") +
  theme_minimal() +
  theme(legend.position = "right", legend.title = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 2)),
         linetype = guide_legend(override.aes = list(size = 2)))
# folder_name <- "plots"
# folder_path <- file.path(getwd(), folder_name)
# file_name <- file.path(folder_path, "Spreads.pdf")
# ggsave(filename = file_name, plot = plot2, width = 10, height = 6, dpi = 300)

## -----------------------------------------------------------------------------
figure1 <- plot1 + plot2                                                        #Combine plot1 and plot2 to obtain figure 1
folder_name <- "plots"
folder_path <- file.path(getwd(), folder_name)

file_name <- file.path(folder_path, "Figure1.pdf")

if (!dir.exists(folder_path)) {
  dir.create(folder_path, showWarnings = FALSE, recursive = TRUE)
}

ggsave(filename = file_name, plot = figure1, width = 10, height = 6, dpi = 300)

## -----------------------------------------------------------------------------
quarterly_df$mean_yi_s<-0                                                       #Construct figure 2 of the paper
quarterly_df$ten_sigma_square_yi_s<-0
columnames<- paste0("yield_s_", country_codes)
 for (col in columnames){
   quarterly_df$mean_yi_s<-quarterly_df[[col]]*0.1+quarterly_df$mean_yi_s
 }
for (col in columnames){
  quarterly_df$ten_sigma_square_yi_s<-quarterly_df$ten_sigma_square_yi_s+
    (quarterly_df[[col]]-quarterly_df$mean_yi_s)^2
}
quarterly_df$sigma_yi_s<-(0.1*quarterly_df$ten_sigma_square_yi_s)^0.5
quarterly_df$ten_sigma_square_yi_s<-NULL

quarterly_df$date <- as.Date(quarterly_df$date, "%Y-%m-%d")

long_df <- gather(quarterly_df, key = "type", value = "value",
                  mean_yi_s, sigma_yi_s)

long_df$type <- dplyr::recode(long_df$type, 
                       'mean_yi_s' = 'Cross Sectional Mean', 
                       'sigma_yi_s' = 'Cross Sectional Dispersion')

yield_spread_plot <- ggplot(long_df, aes(x = date, y = value,
                                         colour = type, group = type)) + 
  geom_line(aes(linetype = type), size = 1) +
  scale_colour_manual("", 
                      values = c("Cross Sectional Mean" = "blue",
                                 "Cross Sectional Dispersion" = "black")) +
  scale_linetype_manual("", 
                        values = c("Cross Sectional Mean" = "dashed",
                                   "Cross Sectional Dispersion" = "solid")) +
  theme_minimal() +
  labs(title = "Yield Spreads on German Bund",
       y = "Yield Spread on German Bund", 
       x = "") +
  scale_y_continuous(limits = c(-2, 10)) +
  theme(legend.position = "bottom",
        legend.title = element_blank())

#print(yield_spread_plot)

## -----------------------------------------------------------------------------
gdp_df$mean_ly_s<-0
gdp_df$ten_sigma_square_ly_s<-0
columnames<- paste0(country_codes, "_gdp_pc_log")
for (col in columnames){
  gdp_df$mean_ly_s<-(gdp_df[[col]]-gdp_df$bd_gdp_pc_log)*0.1+gdp_df$mean_ly_s
}
for (col in columnames){
  gdp_df$ten_sigma_square_ly_s<-gdp_df$ten_sigma_square_ly_s+
    (gdp_df[[col]]-gdp_df$mean_ly_s-gdp_df$bd_gdp_pc_log)^2
}
gdp_df$sigma_ly_s<-(0.1*gdp_df$ten_sigma_square_ly_s)^0.5
gdp_df$ten_sigma_square_ly_s<-NULL

gdp_df <- gdp_df %>%
  mutate(
    Quarter = sub("Q([1-4]) .*", "\\1", date),  
    Year = sub("Q[1-4] (\\d{4})", "\\1", date),  
    Month = case_when(
      Quarter == "1" ~ "01",
      Quarter == "2" ~ "04",
      Quarter == "3" ~ "07",
      Quarter == "4" ~ "10"
    ),
    DateString = paste(Year, Month, "01", sep = "-"),  
    NewDate = as.Date(DateString, format = "%Y-%m-%d")  
  ) %>%
  dplyr::select(-c(Quarter, Year, Month, DateString))

gdp_df$NewDate <- as.Date(gdp_df$NewDate, "%Y-%m-%d")

gdp_df_graph<-dplyr::select(gdp_df, NewDate, mean_ly_s, sigma_ly_s)

gdp_df_graph$NewDate <- as.Date(gdp_df_graph$NewDate, format = "%Y-%m-%d")

differential_germany_plot<-ggplot(data = gdp_df_graph, aes(x = NewDate)) +
  geom_line(aes(y = mean_ly_s, color = "Cross Sectional mean"),
            linetype = "dashed") +
  geom_line(aes(y = sigma_ly_s, color = "Cross Sectional dispersion")) +
  scale_color_manual(values = c("Cross Sectional mean" = "blue", 
                                "Cross Sectional dispersion" = "black"),
                     labels = c("Cross Sectional dispersion ", 
                                "Cross Sectional mean ")) +
  theme_minimal() +
  labs(
    x = "",
    y = "(log of) per capita output differential with Germany",
    color = "Series",
    title = "Per Capita Output Differential with Germany"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
  ) +
  ylim(-0.3, 0.4) 

## -----------------------------------------------------------------------------
figure2 <- yield_spread_plot + differential_germany_plot                        #Combine plot1 and plot2 to obtain figure 2
folder_name <- "plots"
folder_path <- file.path(getwd(), folder_name)

file_name <- file.path(folder_path, "Figure2.pdf")

ggsave(filename = file_name, plot = figure2, width = 10, height = 6, dpi = 300)

rm(list = setdiff(ls(), c("monthly", "gdp_df")))

## -----------------------------------------------------------------------------
source("R_scripts/Clean.R")
df_cds_old<-read_excel("Database_cf1.xls", sheet="d_CDS_Premia_10Y_&_5Y", 
                       range="A2:N3205")
df_cds_old<-clean_data_frame(df_cds_old)
colnames(df_cds_old)[1] = "Date"

df_cds_old_monthly <- df_cds_old %>%
  mutate(Year = year(Date), Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), .groups = 'drop')

df_cds_old_monthly$Date <- as.Date(paste(df_cds_old_monthly$Year, 
                                      df_cds_old_monthly$Month, "01", sep="-"))
df_cds_old_monthly <- subset(df_cds_old_monthly, 
                             select = -c(Year, Month))
df_cds_old_monthly <- df_cds_old_monthly[c("Date", 
                                    setdiff(names(df_cds_old_monthly), "Date"))]

country_codes <- c("bd", "bg", "es", "fr", "gr", 
                   "ir", "it", "nl", "oe", "pt", "us", "uk")

df_cds_old_transformed <- df_cds_old_monthly %>%
  dplyr::mutate(across(all_of(paste0(toupper(country_codes), "GVTSX")), 
                list(cds = ~ .x / 100), 
                .names = "cds_{.col}")) %>%
  dplyr::select(-all_of(paste0(toupper(country_codes), "GVTSX"))) %>%
  dplyr::rename_with(.cols = dplyr::starts_with("cds_"), 
              .fn = ~ str_replace(.x, "cds_", "cds_10Y_") %>%
                str_replace_all(toupper(country_codes), 
                                tolower(country_codes)) %>%
                str_replace_all("GVTSX", ""))

df_cds_old_transformed <- df_cds_old_transformed %>%
  mutate(cds_10Y_fn = FINLDSX / 100) %>%
  dplyr::select(-FINLDSX)

#print(head(df_cds_transformed))

df_cds <- read_excel("Database_cf1.xls", sheet="d_CDS_Premia_10Y_Newdata",      #Import CDS Swaps
                     range = "A2:N3402")                                 
df_cds_d<-clean_data_frame(df_cds)
rm(df_cds)

colnames(df_cds_d)[1] = "Date"

df_cds_d$Date <- ymd(df_cds_d$Date)

df_cds_d_monthly <- df_cds_d %>%
  mutate(Year = year(Date), Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), .groups = 'drop')

df_cds_d_monthly$Date <- as.Date(paste(df_cds_d_monthly$Year, 
                                       df_cds_d_monthly$Month, "01", sep="-"))
df_cds_d_monthly <- subset(df_cds_d_monthly, select = -c(Year, Month))
df_cds_monthly <- df_cds_d_monthly[c("Date", 
                                     setdiff(names(df_cds_d_monthly), "Date"))]

df_filtered<-df_cds_d_monthly
#df_filtered <- df_cds_d_monthly %>% filter(Date >= as.Date("2010-01-10"))

country_codes <- c("bd" = "DEGA$AC", "es" = "ESGA$AC", "fn" = "FIGA$AC", 
                   "fr" = "FRGA$AC", "uk" = "GBGA$AC", "gr" = "GRGA$AC", 
                   "ir" = "IEGA$AC", "it" = "ITGA$AC", "nl" = "NLGA$AC", 
                   "pt" = "PTGA$AC", "oe" = "ATGA$AC", 
                   "bg" = "BEGA$AC", "us"="MMM CDS USD SR 10Y Corp")


df_transformed <- df_filtered %>%
  mutate(date_column = Date) %>%
  dplyr::select(-Date) %>%                
  mutate(across(all_of(country_codes), ~ as.numeric(.x) / 100, .names =
                  "cds_10Y_{.col}")) %>%
  rename_with(~ str_replace(., "^(cds_10Y_)(.*)$", "\\1\\2"), .cols = 
                starts_with("cds_10Y_")) %>%
  bind_cols(df_filtered %>% dplyr::select(Date)) %>% 
  dplyr::select(Date, everything()) 

columns_to_remove <- unname(country_codes)
df_transformed <- df_transformed %>% dplyr::select(-date_column, -all_of(columns_to_remove))

df_cds_old_transformed_filtered <- df_cds_old_transformed %>%
  filter(Date < as.Date("2010-01-01"))

df_transformed_filtered <- df_transformed %>%
  filter(Date > max(df_cds_old_transformed_filtered$Date))

df_combined <- rbind(df_cds_old_transformed_filtered, df_transformed_filtered)

df_combined <- df_combined %>%
  arrange(Date)

df_combined <- df_combined %>%
  distinct(Date, .keep_all = TRUE)


countries <- c("bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", "pt")

for (country in countries) {
  df_combined[[paste0("cds_10Y_s_", country)]] <- 
    df_combined[[paste0("cds_10Y_", country)]] - df_combined[["cds_10Y_bd"]]
}

write.xlsx(df_combined, file = "backup_excel/Check6.xlsx")


#monthly_filtered<-monthly %>% filter(month_year>=as.Date("2010-01-10"))
monthly_filtered<-monthly

df_transformed<-cbind(df_combined, monthly_filtered)
df_transformed$month_year<-NULL

plot_list <- list()

for (country in unique(countries)) {
  yield_column <- sprintf("yield_s_%s", country)
  cds_column <- sprintf("cds_10Y_s_%s", country)
  non_default_column <- sprintf("non_default_s_%s", country)
  
  df_transformed[[non_default_column]] <- df_transformed[[yield_column]] - df_transformed[[cds_column]]
  
  p <- ggplot(df_transformed, aes(x = Date)) +
    geom_line(aes(y = !!sym(yield_column), color = "Yield Spread vs GER"), size = 1.25) +
    geom_line(aes(y = !!sym(cds_column), color = "CDS Spread vs GER"), linetype = "dashed", size = 1.25) +
    geom_line(aes(y = !!sym(non_default_column), color = "Non-Default Component vs GER"), size = 1.25) +
    scale_color_manual(values = c("red", "black", "blue")) +
    labs(title = country) +
    theme_minimal() +
    theme(axis.line = element_line(color = "grey"), panel.grid.major = element_line(color = "grey"), panel.grid.minor = element_blank()) +
    geom_vline(xintercept = as.numeric(ymd("2007-01-01")), color = "grey") +
    geom_vline(xintercept = as.numeric(ymd("2009-01-01")), color = "grey", linetype = "dashed")+
    theme(legend.position = "none")
  
  plot_list[[country]] <- p
  #ggsave(sprintf("fig_comp_%s.pdf", country), plot = p, width = 10, height = 8)
}

country_subset <- c("es", "it", "ir", "es", "pt")

aggregate_plots_subset <- list()

for (country in country_subset) {
  if (!is.null(plot_list[[country]])) {
    aggregate_plots_subset[[length(aggregate_plots_subset) + 1]] <- plot_list[[country]]
  }
}

p <- ggplot(df_transformed, aes(x = Date)) +
  geom_line(aes(y = !!sym(yield_column), color = "Yield Spread vs GER"), size = 1.25) +
  geom_line(aes(y = !!sym(cds_column), color = "CDS Spread vs GER"), linetype = "dashed", size = 1.25) +
  geom_line(aes(y = !!sym(non_default_column), color = "Non-Default Component vs GER"), size = 1.25) +
  scale_color_manual(values = c("blue", "red", "black")) +
  labs(title = country) +
  theme_minimal() +
  geom_vline(xintercept = as.numeric(ymd("2007-01-01")), color = "grey") +
  geom_vline(xintercept = as.numeric(ymd("2009-01-01")), color = "grey", linetype = "dashed")

# pp <- ggplotGrob(p)                                                           #This would create the aggregate legend, but, in my tries 
#                                                                               #Is unstable among versions. Newer versions hold with difficulty. 
# leg <- pp$grobs[[which(pp$layout$name == "guide-box")]]                       #Anyway the colour are kept equal to the Favero (2013) paper. 
#            
# is.gTree <- function(x) {
#   inherits(x, "gTree")
# }
# 
# #leg <- if (is.gTree(leg)) leg$children[[1]] else leg
# 
# grid.newpage()
# grid.draw(leg)
# 
# # saveRDS(leg, file = "legend.rds")
# # 
# # legend <- readRDS("legend.rds")
# 
# legend_plot <- ggplot() + 
#   annotation_custom(grob = leg) + 
#   theme_void() +
#   theme(plot.margin = unit(c(0,0,0,0), "null"))
# 
# aggregate_plots_subset[[length(aggregate_plots_subset) + 1]] <- legend_plot

combined_plot <- wrap_plots(aggregate_plots_subset, ncol = 3)

folder_name <- "plots"
folder_path <- file.path(getwd(), folder_name)

file_name <- file.path(folder_path, "Figure3.pdf")

ggsave(filename = file_name, plot = combined_plot, width = 15, height = 6, dpi = 300)

## -----------------------------------------------------------------------------
remaining_countries <- setdiff(unique(countries), c(unique(country_subset), "bg","gr"))
aggregate_plots_remaining <- list()

for (country in remaining_countries) {
  if (!is.null(plot_list[[country]])) {
    aggregate_plots_remaining[[length(aggregate_plots_remaining) + 1]] <- plot_list[[country]]
  }
}

#aggregate_plots_remaining[[length(aggregate_plots_remaining) + 1]] <- legend_plot Uncomment only if previous liens are uncommented (grop). Incompatibility among versions

combined_plot <- wrap_plots(aggregate_plots_remaining, ncol = 3)

folder_name <- "plots"
folder_path <- file.path(getwd(), folder_name)

file_name <- file.path(folder_path, "Figure4.pdf")

ggsave(filename = file_name, plot = combined_plot, width = 15, height = 6, dpi = 300)
rm(list=ls())









