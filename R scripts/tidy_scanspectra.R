tidy_scanspectra <- function(df, type) {
  # basic input validation is extremely useful
  stopifnot(is.data.frame(df))
  stopifnot(is.character(type))

  colnames(df) <- df[1,]                            
  df <- df %>% 
  slice(-1) %>%                                                               
  rename(Date.Time = "Date/Time") %>% 
  mutate(timestamp = parse_date_time(Date.Time, "%Y.%m.%d %H:%M:%S")) %>%     
  select(timestamp, Date.Time, everything()) %>% select(-Date.Time) %>%       
  select(-c(starts_with("Status"):"217.5", "735":"750"))
  # pass the character type
  colnames(df)[-1] <- paste(type, colnames(df)[-1], sep = "_")
  return(df)
}