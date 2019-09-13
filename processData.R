toDate <- function(year, month, day) {
  ISOdate(year, month, day)
}


#Compute events by day, tod, and note DOW
processData <- function( df, be_flag) {
  
  #troubleshooting
  # df <- df1.activity
  # be_flag <- "end"
  
  # determine which fields to use
  if (be_flag == "begin"){
    x <- df %>% 
      select (beg_year, beg_month, beg_day, beg_dow, beg_tod, encntr_id) %>%
      rename(year=beg_year, month=beg_month, day=beg_day, dow=beg_dow, tod=beg_tod) 
  }else if (be_flag == "end"){
    x <- df %>% 
      filter(!is.na(end_effective_dt_tm)) %>%
      select (end_year, end_month, end_day, end_dow, end_tod, encntr_id) %>%
      rename(year=end_year, month=end_month, day=end_day, dow=end_dow, tod=end_tod) 
  }else{
    
    warning("Be_flag can have values 'begin' or 'end', please respecify and retry.")
    return(-1)
  }
 
  y <- x %>% group_by(year, month, day, dow, tod) %>% summarize( events =n_distinct(encntr_id) )
  y <- y %>% ungroup()
  y$eventDate <- toDate(y$year, y$month, y$day)
  
  #shorten df Name
  u_dates <- data.frame( eventDate = seq(min(y$eventDate), max(y$eventDate),  by = "1 day"), foo=1) #unique dates
  u_tod <- data.frame( tod = factor(seq(0, 23, 1)), foo=1)
  placeholder <- u_dates %>% left_join(u_tod, by="foo") %>% select(-foo)
  placeholder$dow <- factor(wday(placeholder$eventDate))
  placeholder$month <- factor(month(placeholder$eventDate))
  placeholder$year <- factor(year(placeholder$eventDate))
  
  placeholder$key <- apply( placeholder[, c("eventDate","tod")] , 1 , paste , collapse = "ZZ" ) #key 
  y$key<- apply( y[, c("eventDate","tod")] , 1 , paste , collapse = "ZZ" ) #key for joining
  
  #consolidate into final data set
  placeholder <- placeholder %>%  
    left_join( y, by="key", suffix=c("",".y")) %>% 
    select(names(placeholder),"events", -"key")
  placeholder$events[is.na(placeholder$events)] <- 0  #set 0s
  
  if (be_flag == "begin"){
    placeholder <- placeholder %>% rename(entries = events)
  }else{
    placeholder <- placeholder %>% rename(departures = events)
  }
  
  return(placeholder) #return result
}

