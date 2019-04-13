calltrackby <- function(token, start, end){
  library('httr')
  library(stringr)
  a <- content((GET(url = "https://call-tracking.by/crm_api/v1/stats/raw_calls.json",
                    query = list(user_token=token,
                                 start_date=start,
                                 end_date=end))),
                    "parsed", "application/json")
  
  df_calls <- data.frame('Date' = as.Date(rep("2019-01-01", length(a))))
  
  for (i in 1:length(a)){
    df_calls$Date[i] <- as.Date(substr(a[[i]]$datetime, start = 1, stop = 10))
    df_calls$timeDay[i] <- substr(a[[i]]$datetime, start = 12, stop = 13)
    ifelse( is.null(a[[i]]$ads_number), df_calls$ads_number <- 'NA', df_calls$ads_number <- substr(a[[i]]$ads_number, 2, str_length(a[[i]]$ads_number)))
    ifelse( is.null(a[[i]]$caller_number), df_calls$number[i] <- "NA", df_calls$number[i] <- substr(a[[1]]$caller_number, 2, str_length(a[[1]]$caller_number)))
    ifelse( is.null(a[[i]]$duration), df_calls$time[i] <- "NA", df_calls$time[i] <- a[[i]]$duration)
    ifelse( is.null(a[[i]]$campaign_id), df_calls$campaign_id <- 'NA', df_calls$campaign_id <- a[[i]]$campaign_id)
    ifelse( is.null(a[[i]]$utm_source), df_calls$source[i] <- "NA", df_calls$source[i] <- a[[i]]$utm_source)
    ifelse( is.null(a[[i]]$utm_medium), df_calls$medium[i] <- "NA", df_calls$medium[i] <- a[[i]]$utm_medium)
    ifelse( is.null(a[[i]]$utm_campaign),df_calls$campaign[i] <-  "NA", df_calls$campaign[i] <- a[[i]]$utm_campaign)
    ifelse( is.null(a[[i]]$utm_keyword),df_calls$key[i] <-  "NA", df_calls$key[i] <- a[[i]]$utm_keyword)
    ifelse( is.null(a[[i]]$utm_content), df_calls$utm_content <- 'NA', df_calls$utm_content <- a[[i]]$utm_content)
    ifelse( is.null(a[[i]]$google_analytics_id), df_calls$clientId[i] <- "NA", df_calls$clientId[i] <- a[[i]]$google_analytics_id)
    ifelse( is.null(a[[i]]$yandex_metrica_id), df_calls$yandex_metrica_id <- "NA", df_calls$yandex_metrica_id <- a[[i]]$yandex_metrica_id)
  }
return(df_calls)}
