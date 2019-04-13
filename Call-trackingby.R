calltrackby <- function(token, start, end){
  library('httr')
  library('dplyr')
  library('data.table')
  get_answer <- GET(url = "https://call-tracking.by/crm_api/v1/stats/raw_calls.json",
                    query = list(user_token=token,
                                 start_date=start,
                                 end_date=end))#get запрос
  a <- content(get_answer, "parsed", "application/json")#перевод в JSON
  
  df_calls <- data.frame('Date' = as.Date(rep("2019-01-01", length(a))))#DATAFRAME
  
  for (i in 1:length(a)){
    df_calls$Date[i] <- as.Date(substr(a[[i]]$datetime, start = 1, stop = 10))
    df_calls$timeDay[i] <- substr(a[[i]]$datetime, start = 12, stop = 13)
    ifelse( is.null(a[[i]]$caller_number), df_calls$number[i] <- "NA", df_calls$number[i] <- a[[i]]$caller_number)
    ifelse( is.null(a[[i]]$duration), df_calls$time[i] <- "NA", df_calls$time[i] <- a[[i]]$duration)
    ifelse( is.null(a[[i]]$utm_campaign),df_calls$campaign[i] <-  "NA", df_calls$campaign[i] <- a[[i]]$utm_campaign)
    ifelse( is.null(a[[i]]$utm_keyword),df_calls$key[i] <-  "NA", df_calls$key[i] <- a[[i]]$utm_keyword)
    ifelse( is.null(a[[i]]$utm_source), df_calls$source[i] <- "NA", df_calls$source[i] <- a[[i]]$utm_source)
    ifelse( is.null(a[[i]]$utm_medium), df_calls$medium[i] <- "NA", df_calls$medium[i] <- a[[i]]$utm_medium)
    ifelse( is.null(a[[i]]$google_analytics_id), df_calls$clientId[i] <- "NA", df_calls$clientId[i] <- a[[i]]$google_analytics_id)
  }
  
  for (i in 1:nrow(df_calls)){
    df_calls$number[i] <- as.numeric(df_calls$number[i])
  }
return(df_calls)}