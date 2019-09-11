processData <- function( df, be_flag){
  
  # aggregate it to have entries by TOD, DOW and Date
  if (be_flag = "begin"){
    
  }else{
    
  }
  x <- df %>% 
    select (beg_year, beg_month, beg_day, beg_dow, beg_tod) %>%
    rename(year=beg_year, month=beg_month, day=beg_day, DOW=beg_dow, TOD=beg_tod) 
  
  
  x <- df %>% group_by(beg_year, beg_month, beg_day, beg_dow, beg_tod) %>%
    summarize( entries =n_distinct(encntr_id))
  df2.entries <- df2.entries %>% ungroup()
  df2.entries$entryDate <- toDate(df2.entries$beg_year, df2.entries$beg_month, df2.entries$beg_day)
  
  #shorten df Name
  x <- df2.entries  
  u_dates <- data.frame( entryDate = seq(min(x$entryDate), max(x$entryDate),  by = "1 day"), foo=1) #unique dates
  u_hours <- data.frame( hours = seq(0, 23, 1), foo=1)
  placeholder <- u_dates %>% left_join(u_hours, by="foo") %>% select(-foo)
  placeholder$DOW <- wday(placeholder$entryDate)
  placeholder$key <- apply( placeholder[, c("entryDate","hours")] , 1 , paste , collapse = "ZZ" ) #key 
  x$key<- apply( x[, c("entryDate","beg_tod")] , 1 , paste , collapse = "ZZ" ) #key for joining
  
  #consolidate into final data set
  placeholder <- placeholder %>%  
    left_join( x, by="key", suffix=c("",".y")) %>% 
    select(names(placeholder),"entries", -"key")
  placeholder$entries[is.na(placeholder$entries)] <- 0  #set 0s
  entries_final <- placeholder
}

