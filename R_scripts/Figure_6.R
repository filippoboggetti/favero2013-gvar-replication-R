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
results<-read_rds("Database.RDS/model1_results.rds")
Dataset<-read_rds("Database.RDS/Dataset_monthly_global.rds")

countries <- c("bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", "pt")

first_column <- Dataset[, 1, drop = FALSE]
fitting_dataset <- data.frame(First_Column = first_column)

for (country in countries){
  name<-paste0("yield_s_",country,"_eqtm")
  x1_var <- paste0("yield_s_", country, "_lag")
  x2_var <- "us_corp_spread"
  x3_var <- paste0("debt_gdp_s_", country)
  x4_var <- paste0("deficit_gdp_s_", country)
  x5_var <- "us_corp_spread_diff"
  fitting_dataset[[name]]<-(
    -results[["coefficients"]][[paste0(country,"_us_corp_spread_lag")]]*Dataset[[x2_var]]-
    results[["coefficients"]][[paste0(country,"_I(debt_gdp_s_",country,"/60)")]]*Dataset[[x3_var]]/60-
    results[["coefficients"]][[paste0(country,"_I(deficit_gdp_s_",country,"/3)")]]*Dataset[[x4_var]]/3)/results[["coefficients"]][[paste0(country,"_yield_s_", country, "_lag")]]
}

rm(list=setdiff(ls(),c("Dataset", "fitting_dataset")))
## -----------------------------------------------------------------------------

results<-read_rds("Database.RDS/model2_results.rds")

countries <- c("bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", "pt")

for (country in countries){
  name<-paste0("yield_s_",country,"_eqgv")
  x1_var <- paste0("yield_s_", country, "_lag")
  x2_var <- "us_corp_spread"
  x3_var <- paste0("debt_gdp_s_", country)
  x4_var <- paste0("deficit_gdp_s_", country)
  x5_var <- "us_corp_spread_diff"
  x6_var <-paste0("glob_sp1_",country)
  x7_var <-paste0("glob_sp2_",country)
  fitting_dataset[[name]]<-(
    -results[["coefficients"]][[paste0(country,"_us_corp_spread_lag")]]*Dataset[[x2_var]]-
      results[["coefficients"]][[paste0(country,"_I(debt_gdp_s_",country,"/60)")]]*Dataset[[x3_var]]/60-
      results[["coefficients"]][[paste0(country,"_I(deficit_gdp_s_",country,"/3)")]]*Dataset[[x4_var]]/3-
      results[["coefficients"]][[paste0(country,"_glob_sp1_",country,"_lag")]]*Dataset[[x6_var]]-
      results[["coefficients"]][[paste0(country,"_glob_sp2_",country,"_lag")]]*Dataset[[x7_var]]
      )/results[["coefficients"]][[paste0(country,"_yield_s_", country, "_lag")]]
}

rm(list=setdiff(ls(),c("Dataset", "fitting_dataset")))

## -----------------------------------------------------------------------------
write.xlsx(fitting_dataset, file = "backup_excel/Fitting_Dataset.xlsx")
saveRDS(fitting_dataset, file = "Database.RDS/Fitting_dataset.rds")

countries <- c("bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", "pt")

for (country in countries){
  fitting_dataset[[paste0("yield_s_",country)]]<-Dataset[[paste0("yield_s_",country)]]
}

fitting_dataset$Date <- ymd(fitting_dataset$Date)

fitting_dataset <- fitting_dataset %>%
  filter(Date >= ymd("2000-01-01"), Date <= ymd("2012-12-31"))

country_codes <- c("bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", "pt")

for (country_code in country_codes) {
  
  yield_spread_var <- paste0("yield_s_", country_code)
  eqtm_var <- paste0("yield_s_", country_code, "_eqtm")
  eqgv_var <- paste0("yield_s_", country_code, "_eqgv")
  
  p <- ggplot(fitting_dataset, aes(x = Date)) +
    geom_line(aes_string(y = yield_spread_var, color = "'Spread'"), size = 1.25) +
    geom_line(aes_string(y = eqtm_var, color = "'EQTM'"), size = 1.25) +
    geom_line(aes_string(y = eqgv_var, color = "'EQGV'"), size = 1.25) +
    scale_color_manual(values = c("Spread" = "blue", "EQTM" = "red", "EQGV" = "black")) +
    labs(title = paste0("Yield Spread - ", toupper(country_code)), y = "Yield Spread", color = "Legend") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_vline(xintercept = as.numeric(ymd("2007-07-01")), linetype = "solid") +
    geom_vline(xintercept = as.numeric(ymd("2009-09-01")), linetype = "dashed")
  
  assign(paste0("p_", country_code), p)
}


fig_6_1_countries <- c("es", "it", "ir", "pt", "bg", "gr")
fig_6_2_countries <- c("fn", "fr", "nl", "oe")

plots_fig_6_1 <- lapply(fig_6_1_countries, function(code) get(paste0("p_", code)))
pdf("plots/fig_6_1.pdf", width = 11, height = 8.5)
do.call(grid.arrange, c(plots_fig_6_1, ncol = 2, nrow = 3))
dev.off()

plots_fig_6_2 <- lapply(fig_6_2_countries, function(code) get(paste0("p_", code)))
pdf("plots/fig_6_2.pdf", width = 11, height = 8.5)
do.call(grid.arrange, c(plots_fig_6_2, ncol = 2, nrow = 2))
dev.off()
## -----------------------------------------------------------------------------

rm(list=setdiff(ls(),"Dataset"))
fitting_dataset<-read_rds("Database.RDS/Fitting_dataset.rds")

countries <- c("bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", "pt")

for (country in countries){
  fitting_dataset[[paste0("yield_s_",country)]]<-Dataset[[paste0("yield_s_",country)]]
  fitting_dataset[[paste0("cds_10y_s_",country)]]<-Dataset[[paste0("cds_10Y_s_",country)]]
}

# Assuming fitting_dataset is already loaded into your R session...

# Convert the date column to Date type
fitting_dataset$Date <- ymd(fitting_dataset$Date)

# Assuming fitting_dataset is already loaded into your R session...

# Convert the date column to Date type
fitting_dataset$Date <- ymd(fitting_dataset$Date)

# Filter dataset for "es" and the date range
country_data <- fitting_dataset %>%
  filter(Date >= ymd("2009-12-01") & Date <= ymd("2012-10-31")) %>%
  mutate(GVAR_TM_diff = yield_s_es_eqgv - yield_s_es_eqtm,
         Bond_CDS_diff = yield_s_es - cds_10y_s_es)

# Calculate the scaling factor and offset for the secondary axis
primary_range <- 3.5 - 0
secondary_range <- 2 - (-0.8)
scale_factor <- primary_range / secondary_range
offset <- -0.8 * scale_factor

# Create the plot for "es"
p_es <- ggplot(country_data, aes(x = Date)) +
  geom_line(aes(y = GVAR_TM_diff), color = "blue", size = 1.25) +
  geom_line(aes(y = (Bond_CDS_diff + 0.8) * scale_factor), color = "red", linetype = "dashed", size = 1.25) +
  scale_y_continuous(
    name = "GVAR equilibrium spreads - TM equilibrium spreads",
    limits = c(0, 3.5),
    sec.axis = sec_axis(~(. / scale_factor) - 0.8, name = "Bond Spreads - CDS Spreads")
  ) +
  labs(title = "Exchange Rate Premia - ES") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )


country_data_ir <- fitting_dataset %>%
  filter(Date >= ymd("2009-12-01") & Date <= ymd("2012-10-31")) %>%
  mutate(GVAR_TM_diff = yield_s_ir_eqgv - yield_s_ir_eqtm,
         Bond_CDS_diff = yield_s_ir - cds_10y_s_ir)

primary_range_ir <- 12 - 0
secondary_range_ir <- 2.4 - 0
scale_factor_ir <- primary_range_ir / secondary_range_ir

p_ir <- ggplot(country_data_ir, aes(x = Date)) +
  geom_line(aes(y = GVAR_TM_diff), color = "blue", size = 1.25) +
  geom_line(aes(y = Bond_CDS_diff * scale_factor_ir), color = "red", linetype = "dashed", size = 1.25) +
  scale_y_continuous(
    name = "GVAR equilibrium spreads - TM equilibrium spreads",
    limits = c(0, 12),
    sec.axis = sec_axis(~ . / scale_factor_ir, name = "Bond Spreads - CDS Spreads")
  ) +
  labs(title = "Exchange Rate Premia - IR") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )

country_data_it <- fitting_dataset %>%
  filter(Date >= ymd("2009-12-01") & Date <= ymd("2012-10-31")) %>%
  mutate(GVAR_TM_diff = yield_s_it_eqgv - yield_s_it_eqtm,
         Bond_CDS_diff = yield_s_it - cds_10y_s_it)

primary_range_it <- 4.5 - 0
secondary_range_it <- 1.6 - (-0.2)
scale_factor_it <- primary_range_it / secondary_range_it
offset_it <- -0.2 * scale_factor_it

p_it <- ggplot(country_data_it, aes(x = Date)) +
  geom_line(aes(y = GVAR_TM_diff), color = "blue", size = 1.25) +
  geom_line(aes(y = (Bond_CDS_diff + 0.2) * scale_factor_it), color = "red", linetype = "dashed", size = 1.25) +
  scale_y_continuous(
    name = "GVAR equilibrium spreads - TM equilibrium spreads",
    limits = c(0, 4.5),
    sec.axis = sec_axis(~(. / scale_factor_it) - 0.2, name = "Bond Spreads - CDS Spreads")
  ) +
  labs(title = "Exchange Rate Premia - IT") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )

country_data_pt <- fitting_dataset %>%
  filter(Date >= ymd("2009-12-01") & Date <= ymd("2012-10-31")) %>%
  mutate(GVAR_TM_diff = yield_s_pt_eqgv - yield_s_pt_eqtm,
         Bond_CDS_diff = yield_s_pt - cds_10y_s_pt)

primary_range_pt <- 4 - 0
secondary_range_pt <- 3.6 - (-0.4)
scale_factor_pt <- primary_range_pt / secondary_range_pt
offset_pt <- -0.4 * scale_factor_pt

p_pt <- ggplot(country_data_pt, aes(x = Date)) +
  geom_line(aes(y = GVAR_TM_diff), color = "blue", size = 1.25) +
  geom_line(aes(y = (Bond_CDS_diff + 0.4) * scale_factor_pt), color = "red", linetype = "dashed", size = 1.25) +
  scale_y_continuous(
    name = "GVAR equilibrium spreads - TM equilibrium spreads",
    limits = c(0, 4),
    sec.axis = sec_axis(~(. / scale_factor_pt) - 0.4, name = "Bond Spreads - CDS Spreads")
  ) +
  labs(title = "Exchange Rate Premia - PT") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )


country_data_bg <- fitting_dataset %>%
  filter(Date >= ymd("2009-12-01") & Date <= ymd("2012-10-31")) %>%
  mutate(GVAR_TM_diff = yield_s_bg_eqgv - yield_s_bg_eqtm,
         Bond_CDS_diff = yield_s_bg - cds_10y_s_bg)

primary_min <- -0.3
primary_max <- 0.4
secondary_min <- -0.4
secondary_max <- 1

scale_factor_bg <- (primary_max - primary_min) / (secondary_max - secondary_min)

p_bg <- ggplot(country_data_bg, aes(x = Date)) +
  geom_line(aes(y = GVAR_TM_diff), color = "blue", size = 1.25) +
  geom_line(aes(y = (Bond_CDS_diff - secondary_min) * scale_factor_bg + primary_min), color = "red", linetype = "dashed", size = 1.25) +
  scale_y_continuous(
    name = "GVAR equilibrium spreads - TM equilibrium spreads",
    limits = c(primary_min, primary_max),
    sec.axis = sec_axis(~ (. - primary_min) / scale_factor_bg + secondary_min, name = "Bond Spreads - CDS Spreads")
  ) +
  labs(title = "Exchange Rate Premia - BG") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )

pdf("plots/Figure6.3.pdf", width = 11, height = 8.5) # Adjust size as needed
grid.arrange(
  p_es, p_it, p_ir,
  p_pt, p_bg, nrow = 2,
  ncol = 3,
  layout_matrix = rbind(c(1, 3, 2),
                        c(4, 5, NA))
)
dev.off()










