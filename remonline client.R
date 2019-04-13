remonline <- function(x){
library('httr')
library('anytime')
#post запросы
post_answer <- POST(url = "https://api.remonline.ru/token/new", 
                      body = list(api_key = x))
token <- content(post_answer, "parsed", "application/json")$token
#Получаем данные первые данные remonline
get_answer <- GET(url = "https://api.remonline.ru/order/",
                  query = list(
                  token=token,
                  page = 1))

df_crm <- data.frame(date_crm = as.Date(rep("2019-01-01", content(get_answer)$count)))
k <- 0
for(i in 1:round(content(get_answer)$count/50)+1){
  get_answer <- GET(url = "https://api.remonline.ru/order/",
                    query = list(token=token,
                    page = i))
  for (j in 1:length(content(get_answer)$data)){
    k <- k + 1
    df_crm$date_crm[k] <- as.Date(as.POSIXct((content(get_answer)$data[[j]]$created_at/1000), origin="1970-01-01"))
    ifelse( length(content(get_answer)$data[[j]]$client$phone) == 0, df_crm$number[k] <- "Na", df_crm$number[k] <- content(get_answer)$data[[j]]$client$phone)
    ifelse( is.null(content(get_answer)$data[[j]]$kindof_good), df_crm$type[k] <- "Na", df_crm$type[k] <- content(get_answer)$data[[j]]$kindof_good)
    ifelse( is.null(content(get_answer)$data[[j]]$payed), df_crm$money[k] <- "Na",df_crm$money[k] <-  content(get_answer)$data[[j]]$payed)
  }
}


df_crm$number <- paste0(substr(df_crm$number, start = 2, stop = 4), substr(df_crm$number, start = 7, stop = 8),
                              substr(df_crm$number, start = 11, stop =13), substr(df_crm$number, start = 15, stop = 16),
                              substr(df_crm$number, start = 18, stop = 19))
return(df_crm)}