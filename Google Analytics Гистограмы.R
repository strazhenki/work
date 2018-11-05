packages <- c("googleAnalyticsR", "tidyverse", "devtools", "googleVis")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

install.packages("devtools")
install.packages("googleVis")
install.packages("shiny")
install.packages("ggplot")
install.packages("sunbustR")
install.packages("googleAnalyticsR")
"tidyverse", "devtools", "googleVis", "sunburstR", "shiny", "ggplot")

library(googleAnalyticsR)
library(shiny)
library(sunburstR)
        
        library(googleAnalyticsR)
        my_client_id <- "470464748006-nknspmmtb127phlvd5fuddkstlmvnekc.apps.googleusercontent.com"
        my_client_secret <- "0ykcKErCRz_8irRsGqhzXAAs"
        options(googleAuthR.client_id = my_client_id) 
        options(googleAuthR.client_secret = my_client_secret)
        
        
        
        ga_data4 <-google_analytics(viewId = 166777564,
                                    date_range  = c(start.date = "2018-01-01", end.date = "2018-04-10"),
                                    metrics = c("users","goalcompletionsAll"),
                                    dimensions = c("userType","ga:deviceCategory","ga:sourceMedium"),
                                    filters = c("ga:medium=~cpc|organic;ga:source=~yandex|google|vk_target;ga:deviceCategory==desktop"),
                                    anti_sample = TRUE)
        
        ggplot(ga_data4, mapping = aes(x = userType, y = users, fill = userType))+
          geom_bar(stat = "identity")+
          facet_wrap(~sourceMedium)
        
        ggplot(ga_data4, mapping = aes(x = userType, y = goalcompletionsAll, fill = userType))+
          geom_bar(stat = "identity")+
          facet_wrap(~sourceMedium)
        
        
        
ga_data2 <-google_analytics(viewId = 166777564,
                                    date_range  = c(start.date = "2018-02-01", end.date = "2018-03-01"),
                                    metrics = c("sessions","goalcompletionsAll"),
                                    dimensions = c("ga:userType", "ga:deviceCategory"),
                                    anti_sample = TRUE)
        
        ggplot(ga_data2, mapping = aes(x = userType, y = goalcompletionsAll, fill = userType))+
          geom_bar(stat = "identity")+
          facet_wrap(~deviceCategory)        
        