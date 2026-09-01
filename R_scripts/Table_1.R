################################################################################
###
### AUTHOR:       Filippo Felice Boggetti
### DATE:         Spring 2024
### DESCRIPTION:  This script estimate the global VAR as proposed by Favero (2013).
###                
### OUTPUT:       Two Folders: containing pdf and .tex for the estimation with 
###               and without panel restrictions.   
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

filteredDataset <- finalDataset[finalDataset$Date >= as.Date("2000-02-01") & finalDataset$Date <= as.Date("2009-12-01"), ]

equations <- list()

countries <- c("bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", "pt")

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

saveRDS(results, file = "Database.RDS/model1_results.rds")

## -----------------------------------------------------------------------------
summ <- summary(results)
coef_names <- rownames(summ$coefficients)
results_df <- data.frame(
  Country = rep(NA, length(coef_names)),
  Term = rep(NA, length(coef_names)),
  Estimate = rep(NA, length(coef_names)),
  Std.Error = rep(NA, length(coef_names)),
  t.value = rep(NA, length(coef_names)),
  Pr = rep(NA, length(coef_names)),
  stringsAsFactors = FALSE
)

for (i in 1:length(coef_names)) {
  split_name <- unlist(strsplit(coef_names[i], "_", fixed = TRUE))
  country <- split_name[1]
  term <- paste(split_name[-1], collapse = "_")
  
  results_df[i, "Country"] <- country
  results_df[i, "Term"] <- term
  results_df[i, "Estimate"] <- summ$coefficients[i, "Estimate"]
  results_df[i, "Std.Error"] <- summ$coefficients[i, "Std. Error"]
  results_df[i, "t.value"] <- summ$coefficients[i, "t value"]
  results_df[i, "Pr"] <- summ$coefficients[i, "Pr(>|t|)"]
}

adjusted_r_squared <- numeric(length(countries))

for (i in 1:length(countries)) {
  country <- countries[i]
  adjusted_r_squared[i]<-summ[["eq"]][[i]][["adj.r.squared"]]
}

results_df$Adjusted.R.Squared <- rep(NA, nrow(results_df))

for (i in 1:length(countries)) {
  country <- countries[i]
  results_df$Adjusted.R.Squared[results_df$Country == country] <- adjusted_r_squared[i]
}

## -----------------------------------------------------------------------------
results_df$Estimate <- round(results_df$Estimate, 3)
results_df$Std.Error <- round(results_df$Std.Error, 3)
results_df$Adjusted.R.Squared <- round(results_df$Adjusted.R.Squared,2)
results_df$Pr <- as.numeric(as.character(results_df$Pr))
results_df$Pr <- round(results_df$Pr, digits = 2)

countries <- sort(c("bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", "pt"))

col_names <- c(paste0("beta_", 0:5), "adj.R^2")

printable_results <- data.frame(matrix(NA, nrow = 30, ncol = 7, dimnames = list(NULL, col_names)))

row_names <- vector("character", length = 30)

for (i in 1:length(countries)) {
  country <- countries[i]
  row_names[(i-1)*3+1] <- paste0("coefficients_", country)
  row_names[(i-1)*3+2] <- paste0("s.e._", country)
  row_names[(i-1)*3+3] <- paste0("p_value_", country)
}

rownames(printable_results) <- row_names

for (i in 1:length(countries)) {
  country <- countries[i]
  start_index <- (i - 1) * 6 + 1 
  coeffs_row_index <- which(rownames(printable_results) == paste0("coefficients_", country))
  printable_results[coeffs_row_index, 1:6] <- results_df$Estimate[start_index:(start_index + 5)]
}

for (i in 1:length(countries)) {
  country <- countries[i]
  start_index <- (i - 1) * 6 + 1 
  se_row_index <- which(rownames(printable_results) == paste0("s.e._", country))
  printable_results[se_row_index, 1:6] <- results_df$Std.Error[start_index:(start_index + 5)]
}

for (i in 1:length(countries)) {
  country <- countries[i]
  start_index <- (i - 1) * 6 + 1 
  pvalue_row_index <- which(rownames(printable_results) == paste0("p_value_", country))
  printable_results[pvalue_row_index, 1:6] <- results_df$Pr[start_index:(start_index + 5)]
}

for (i in 1:length(countries)) {
  country <- countries[i]
  country_index_in_results <- which(results_df$Country == country)[1]
  adjusted_r_squared_value <- results_df$Adjusted.R.Squared[country_index_in_results]
  se_row_index <- which(rownames(printable_results) == paste0("s.e._", country))
  printable_results[se_row_index, 7] <- adjusted_r_squared_value
}
## -----------------------------------------------------------------------------
######################### PRINT THE RESULTS IN LATEX ###########################

colnames(printable_results) <- c("$\\beta_0$", "$\\beta_1$", "$\\beta_2$","$\\beta_3$","$\\beta_4$","$\\beta_5$","$R^2$")

sanitize_for_latex <- function(str) {
  str <- gsub("%", "\\\\%", str, fixed = TRUE)
  str <- gsub("#", "\\\\#", str, fixed = TRUE)  
  str <- gsub("&", "\\\\&", str, fixed = TRUE)  
  return(str)
}

unique_rownames <- unlist(lapply(countries, function(country) {
  paste("$",country, c("_1", "_2", "_3"),"$", sep = "")
}))

rownames(printable_results) <- unique_rownames

add_parentheses <- function(df) {
  df_with_parentheses <- df
  for (i in seq_len(nrow(df))) {
    if (i %% 3 == 2) { 
      
      df_with_parentheses[i, -ncol(df)] <- paste0("(", df[i, -ncol(df)], ")")
    }
  }
  return(df_with_parentheses)
}

add_brackets <- function(df) {
  df_with_brackets <- df
  for (i in seq_len(nrow(df))) {
    
    if (i %% 3 == 0) { 
      
      df_with_brackets[i, c(-ncol(df))] <- sprintf("\\ensuremath{[%.2f]}", df[i, c(-ncol(df))])
    }
  }
  return(df_with_brackets)
}

printable_results <- add_parentheses(printable_results)
printable_results <- add_brackets(printable_results)

latexTable <- xtable(printable_results, caption="Detailed Regression Results", label="tab:detailed_regression_results")

latexCode <- print(latexTable, include.rownames=TRUE, floating=FALSE,
                   hline.after=NULL, booktabs=TRUE, print.results=FALSE,
                   sanitize.text.function=sanitize_for_latex)

latexHeader <- "
\\documentclass{article}
\\usepackage[utf8]{inputenc}
\\usepackage{booktabs}
\\usepackage{amsmath}
\\title{Regression Results}
\\author{}
\\begin{document}
\\maketitle
"

latexFooter <- "\\end{document}"

# fullLatexDoc <- paste0(latexHeader, latexCode, latexFooter)
# 
# writeLines(fullLatexDoc, "Table1nopanelrestriction.tex")
# 
# texFileName <- "Table1nopanelrestriction.tex"
# tools::texi2pdf(texFileName, clean = TRUE)

baseFolder <- "Tables"
subFolder1 <- "Table1"
subFolder2 <- " No Panel restrictions"
nestedFolderPath <- file.path(baseFolder, subFolder1, subFolder2)

texFileName <- "Table1nopanelrestriction.tex"
fullFilePath <- file.path(nestedFolderPath, texFileName)

if (!dir.exists(nestedFolderPath)) {
  dir.create(nestedFolderPath, recursive = TRUE)
}

fullLatexDoc <- paste0(latexHeader, latexCode, latexFooter)

writeLines(fullLatexDoc, fullFilePath)

cmd <- "pdflatex"
args <- c("-output-directory", shQuote(nestedFolderPath), shQuote(fullFilePath))

system2(cmd, args)





###############################################################################
############################ ADD PANEL RESTRICTIONS ###########################
###############################################################################
rm(list=ls())

Dataset<-read_rds("Database.RDS/Dataset_monthly_global.rds")

countries <- c("bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", "pt")

deltaNumericColumns <- as.data.frame(lapply(Dataset[, -c(1, 2)], function(x) c(NA, diff(x))))
names(deltaNumericColumns) <- paste0(names(Dataset[, -c(1, 2)]), "_diff")
extendedDataset <- cbind(Dataset, deltaNumericColumns)

laggedNumericColumns <- lapply(Dataset[, -c(1, 2)], function(x) c(NA, x[-length(x)]))
laggedNumericColumns <- as.data.frame(laggedNumericColumns)
names(laggedNumericColumns) <- paste0(names(Dataset[, -c(1, 2)]), "_lag")
finalDataset <- cbind(extendedDataset, laggedNumericColumns)

filteredDataset <- finalDataset[finalDataset$Date >= as.Date("2000-02-01") & finalDataset$Date <= as.Date("2009-12-01"), ]

restrict<-c("bg_yield_s_bg_lag-es_yield_s_es_lag=0","bg_yield_s_bg_lag-fn_yield_s_fn_lag=0", "bg_yield_s_bg_lag-fr_yield_s_fr_lag=0",
            "bg_yield_s_bg_lag-gr_yield_s_gr_lag=0", "bg_yield_s_bg_lag-ir_yield_s_ir_lag=0", "bg_yield_s_bg_lag-it_yield_s_it_lag=0",
            "bg_yield_s_bg_lag-nl_yield_s_nl_lag=0", "bg_yield_s_bg_lag-oe_yield_s_oe_lag=0", "bg_yield_s_bg_lag-pt_yield_s_pt_lag=0","bg_debt_gdp_s_bg_ratio-es_debt_gdp_s_es_ratio=0", "bg_debt_gdp_s_bg_ratio-fr_debt_gdp_s_fr_ratio=0",
            "bg_debt_gdp_s_bg_ratio-fn_debt_gdp_s_fn_ratio=0", "bg_debt_gdp_s_bg_ratio-gr_debt_gdp_s_gr_ratio=0",
            "bg_debt_gdp_s_bg_ratio-ir_debt_gdp_s_ir_ratio=0", "bg_debt_gdp_s_bg_ratio-it_debt_gdp_s_it_ratio=0",
            "bg_debt_gdp_s_bg_ratio-nl_debt_gdp_s_nl_ratio=0", "bg_debt_gdp_s_bg_ratio-oe_debt_gdp_s_oe_ratio=0",
            "bg_debt_gdp_s_bg_ratio-pt_debt_gdp_s_pt_ratio=0","bg_deficit_gdp_s_bg_ratio-es_deficit_gdp_s_es_ratio=0", "bg_deficit_gdp_s_bg_ratio-fr_deficit_gdp_s_fr_ratio=0",
            "bg_deficit_gdp_s_bg_ratio-fn_deficit_gdp_s_fn_ratio=0", "bg_deficit_gdp_s_bg_ratio-gr_deficit_gdp_s_gr_ratio=0",
            "bg_deficit_gdp_s_bg_ratio-ir_deficit_gdp_s_ir_ratio=0", "bg_deficit_gdp_s_bg_ratio-it_deficit_gdp_s_it_ratio=0",
            "bg_deficit_gdp_s_bg_ratio-nl_deficit_gdp_s_nl_ratio=0", "bg_deficit_gdp_s_bg_ratio-oe_deficit_gdp_s_oe_ratio=0",
            "bg_deficit_gdp_s_bg_ratio-pt_deficit_gdp_s_pt_ratio=0","bg_us_corp_spread_diff-es_us_corp_spread_diff=0","bg_us_corp_spread_diff-fr_us_corp_spread_diff=0",
            "bg_us_corp_spread_diff-ir_us_corp_spread_diff=0", "bg_us_corp_spread_diff-nl_us_corp_spread_diff=0",
            "bg_us_corp_spread_diff-pt_us_corp_spread_diff=0","bg_us_corp_spread_diff-fn_us_corp_spread_diff=0",
            "bg_us_corp_spread_diff-gr_us_corp_spread_diff=0","bg_us_corp_spread_diff-it_us_corp_spread_diff=0",
            "bg_us_corp_spread_diff-oe_us_corp_spread_diff=0","bg_us_corp_spread_lag-es_us_corp_spread_lag=0","bg_us_corp_spread_lag-fr_us_corp_spread_lag=0",
            "bg_us_corp_spread_lag-ir_us_corp_spread_lag=0", "bg_us_corp_spread_lag-nl_us_corp_spread_lag=0",
            "bg_us_corp_spread_lag-pt_us_corp_spread_lag=0","bg_us_corp_spread_lag-fn_us_corp_spread_lag=0",
            "bg_us_corp_spread_lag-gr_us_corp_spread_lag=0","bg_us_corp_spread_lag-it_us_corp_spread_lag=0",
            "bg_us_corp_spread_lag-oe_us_corp_spread_lag=0")

for (country in countries) {
  debt_ratio_var <- paste0("debt_gdp_s_", country, "_ratio")
  deficit_ratio_var <- paste0("deficit_gdp_s_", country, "_ratio")
  
  filteredDataset[[debt_ratio_var]] <- filteredDataset[[paste0("debt_gdp_s_", country)]] / 60
  filteredDataset[[deficit_ratio_var]] <- filteredDataset[[paste0("deficit_gdp_s_", country)]] / 3
}

equations <- list()

for (country in countries) {
  y1_var <- paste0("yield_s_", country, "_diff")
  x1_var <- paste0("yield_s_", country, "_lag")
  x2_var <- "us_corp_spread_lag"
  x3_var <- paste0("debt_gdp_s_", country, "_ratio")
  x4_var <- paste0("deficit_gdp_s_", country, "_ratio") 
  x5_var <- "us_corp_spread_diff"
  
  formula_str <- sprintf("%s ~ %s + %s + %s + %s + %s",
                         y1_var, x1_var, x2_var, x3_var, x4_var, x5_var)
  
  equations[[country]] <- as.formula(formula_str)
}

results <- systemfit(equations, data = filteredDataset, method = "SUR",restrict.matrix = restrict)

summary(results)

## -----------------------------------------------------------------------------
######################### PRINT THE RESULTS IN LATEX ###########################
summ <- summary(results)
coef_names <- rownames(summ$coefficients)
results_df <- data.frame(
  Country = rep(NA, length(coef_names)),
  Term = rep(NA, length(coef_names)),
  Estimate = rep(NA, length(coef_names)),
  Std.Error = rep(NA, length(coef_names)),
  t.value = rep(NA, length(coef_names)),
  Pr = rep(NA, length(coef_names)),
  stringsAsFactors = FALSE
)

for (i in 1:length(coef_names)) {
  split_name <- unlist(strsplit(coef_names[i], "_", fixed = TRUE))
  country <- split_name[1]
  term <- paste(split_name[-1], collapse = "_")
  
  results_df[i, "Country"] <- country
  results_df[i, "Term"] <- term
  results_df[i, "Estimate"] <- summ$coefficients[i, "Estimate"]
  results_df[i, "Std.Error"] <- summ$coefficients[i, "Std. Error"]
  results_df[i, "t.value"] <- summ$coefficients[i, "t value"]
  results_df[i, "Pr"] <- summ$coefficients[i, "Pr(>|t|)"]
}

adjusted_r_squared <- numeric(length(countries))

for (i in 1:length(countries)) {
  country <- countries[i]
  # Check if the current country is "gr"
  if (country == "gr") {
    # If yes, use R-squared value
    adjusted_r_squared[i] <- summ[["eq"]][[i]][["r.squared"]]
  } else {
    # If not, use adjusted R-squared value
    adjusted_r_squared[i] <- summ[["eq"]][[i]][["adj.r.squared"]]
  }
}

results_df$Adjusted.R.Squared <- rep(NA, nrow(results_df))

for (i in 1:length(countries)) {
  country <- countries[i]
  results_df$Adjusted.R.Squared[results_df$Country == country] <- adjusted_r_squared[i]
}

## -----------------------------------------------------------------------------
results_df$Estimate <- round(results_df$Estimate, 3)
results_df$Std.Error <- round(results_df$Std.Error, 3)
results_df$Adjusted.R.Squared <- round(results_df$Adjusted.R.Squared,2)
results_df$Pr <- as.numeric(as.character(results_df$Pr))
results_df$Pr <- round(results_df$Pr, digits = 2)

countries <- sort(c("bg", "es", "fn", "fr", "gr", "ir", "it", "nl", "oe", "pt"))

col_names <- c(paste0("beta_", 0:5), "adj.R^2")

printable_results <- data.frame(matrix(NA, nrow = 30, ncol = 7, dimnames = list(NULL, col_names)))

row_names <- vector("character", length = 30)

for (i in 1:length(countries)) {
  country <- countries[i]
  row_names[(i-1)*3+1] <- paste0("coefficients_", country)
  row_names[(i-1)*3+2] <- paste0("s.e._", country)
  row_names[(i-1)*3+3] <- paste0("p_value_", country)
}

rownames(printable_results) <- row_names

for (i in 1:length(countries)) {
  country <- countries[i]
  start_index <- (i - 1) * 6 + 1 
  coeffs_row_index <- which(rownames(printable_results) == paste0("coefficients_", country))
  printable_results[coeffs_row_index, 1:6] <- results_df$Estimate[start_index:(start_index + 5)]
}

for (i in 1:length(countries)) {
  country <- countries[i]
  start_index <- (i - 1) * 6 + 1 
  se_row_index <- which(rownames(printable_results) == paste0("s.e._", country))
  printable_results[se_row_index, 1:6] <- results_df$Std.Error[start_index:(start_index + 5)]
}

for (i in 1:length(countries)) {
  country <- countries[i]
  start_index <- (i - 1) * 6 + 1 
  pvalue_row_index <- which(rownames(printable_results) == paste0("p_value_", country))
  printable_results[pvalue_row_index, 1:6] <- results_df$Pr[start_index:(start_index + 5)]
}

for (i in 1:length(countries)) {
  country <- countries[i]
  country_index_in_results <- which(results_df$Country == country)[1]
  adjusted_r_squared_value <- results_df$Adjusted.R.Squared[country_index_in_results]
  se_row_index <- which(rownames(printable_results) == paste0("s.e._", country))
  printable_results[se_row_index, 7] <- adjusted_r_squared_value
}
## -----------------------------------------------------------------------------
######################### PRINT THE RESULTS IN LATEX ###########################

colnames(printable_results) <- c("$\\beta_0$", "$\\beta_1$", "$\\beta_2$","$\\beta_3$","$\\beta_4$","$\\beta_5$","$R^2$")

sanitize_for_latex <- function(str) {
  str <- gsub("%", "\\\\%", str, fixed = TRUE)  
  str <- gsub("#", "\\\\#", str, fixed = TRUE)  
  str <- gsub("&", "\\\\&", str, fixed = TRUE)  
  return(str)
}
unique_rownames <- unlist(lapply(countries, function(country) {
  paste("$",country, c("_1", "_2", "_3"),"$", sep = "")
}))

rownames(printable_results) <- unique_rownames

add_parentheses <- function(df) {
  df_with_parentheses <- df
  for (i in seq_len(nrow(df))) {
    if (i %% 3 == 2) {
      df_with_parentheses[i, -ncol(df)] <- paste0("(", df[i, -ncol(df)], ")")
    }
  }
  return(df_with_parentheses)
}

add_brackets <- function(df) {
  df_with_brackets <- df
  for (i in seq_len(nrow(df))) {
    if (i %% 3 == 0) { 
      df_with_brackets[i, c(-ncol(df))] <- sprintf("\\ensuremath{[%.2f]}", df[i, c(-ncol(df))])
    }
  }
  return(df_with_brackets)
}

printable_results <- add_parentheses(printable_results)
printable_results <- add_brackets(printable_results)

latexTable <- xtable(printable_results, caption="Detailed Regression Results", label="tab:detailed_regression_results")

latexCode <- print(latexTable, include.rownames=TRUE, floating=FALSE,
                   hline.after=NULL, booktabs=TRUE, print.results=FALSE,
                   sanitize.text.function=sanitize_for_latex)

latexHeader <- "
\\documentclass{article}
\\usepackage[utf8]{inputenc}
\\usepackage{booktabs}
\\usepackage{amsmath}
\\title{Regression Results}
\\author{}
\\begin{document}
\\maketitle
"

latexFooter <- "\\end{document}"

# fullLatexDoc <- paste0(latexHeader, latexCode, latexFooter)
# 
# writeLines(fullLatexDoc, "Table1panelrestriction.tex")
# 
# texFileName <- "Table1panelrestriction.tex"
# tools::texi2pdf(texFileName, clean = TRUE)


baseFolder <- "Tables"
subFolder1 <- "Table1"
subFolder2 <- "Panel restrictions"
nestedFolderPath <- file.path(baseFolder, subFolder1, subFolder2)

texFileName <- "Table1panelrestriction.tex"
fullFilePath <- file.path(nestedFolderPath, texFileName)

if (!dir.exists(nestedFolderPath)) {
  dir.create(nestedFolderPath, recursive = TRUE)
}

fullLatexDoc <- paste0(latexHeader, latexCode, latexFooter)

writeLines(fullLatexDoc, fullFilePath)

cmd <- "pdflatex"
args <- c("-output-directory", shQuote(nestedFolderPath), shQuote(fullFilePath))

system2(cmd, args)










