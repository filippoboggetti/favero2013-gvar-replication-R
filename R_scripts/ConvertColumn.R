clean_data_frame <- function(df) {
  for(col_name in names(df)) {
    # Temporarily convert column to character for inspection
    temp_col <- as.character(df[[col_name]])
    
    # Check if the column contains any "NA" strings and if it might be numeric
    if("NA" %in% temp_col) {
      # Prepare to suppress warnings for the entire block of potentially warning-generating operations
      suppressWarnings({
        # Try converting "NA" strings to NA values and the rest of the column to numeric
        converted_col <- as.numeric(replace(temp_col, temp_col == "NA", NA))
        
        # Check if conversion introduced NAs as intended without converting everything to NA
        if(any(!is.na(converted_col))) {
          df[[col_name]] <- converted_col
        } else {
          # If conversion was not successful, revert to original character data
          df[[col_name]] <- temp_col
        }
      })
    }
  }
  
  return(df)
}