library(zoo)

 convert_quarter_to_date <- function(x) {
  year <- floor(x / 10)    # Extract the year by removing the last digit
  quarter <- x %% 10       # Extract the quarter as the last digit
  as.yearqtr(paste(year, quarter), format = "%Y %q")
}