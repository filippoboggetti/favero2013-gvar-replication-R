safe_merge_by_date <- function(main_df, other_df) {
  if ("Date" %in% names(other_df)) {
    other_df$Date <- as.Date(other_df$Date)
    main_df <- merge(main_df, other_df, by = "Date", all.x = TRUE)
  } else {
    warning("Date column not found in the dataframe to merge.")
  }
  return(main_df)
}