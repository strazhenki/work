install.packages("googleAuthR")
install.packages("googleAnalyticsR")
service_token <- gar_auth_service("C:\Users\straz\Downloads")

library(googleAuthR) 
library(googleAnalyticsR)
service_token <- gar_auth_service("C:\Users\straz\Downloads")
ga_id <- "166777564"
path <- "C:\Users\straz\Downloads"
start_date <- "2018-02-07"
end_date <- "2018-02-21"

news <- ()

df1 <- dim_filter("medium","EXACT","organic", caseSensitive = FALSE)
df2 <- dim_filter("landingPagePath","REGEXP","(skype|online|/pt/$|/$|/pl/$|/es/$|/ru/$|/ua/$|/fr/$|/ko/$|/ja/$|/ar/$|/sp/$|/zh/$|/tr/$|/it/$|/id/$|/de/$)")
fc <- filter_clause_ga4(list(df1, df2), operator = "AND")

ga_data <- google_analytics_4(ga_id, 
                              date_range = c(start_date,end_date),
                              dimensions=c('landingPagePath','date','country'), 
                              metrics = c('newusers'), 
                              dim_filters = fc,
                              anti_sample = TRUE)

if(is.data.frame(ga_data) && nrow(ga_data)==0 ) {
  message("File hasn't update","\r",appendLF=FALSE)
} else {
  write.csv(ga_data, file = path, row.names=FALSE)
}

install.packages("sunburstR")
install.packages("shiny")
install.packages("googleAnalyticsR")
library(googleAnalyticsR)
library(shiny)
library(sunburstR)
# Ограницение максимальной длины луча в источниках ----
pathLimit <- 10
ga_auth()
my_accounts <- ga_account_list()
ga_view_id<- 166777564 # Номер представления ----

mcf_gadata <- google_analytics(id = ga_view_id, 
                               start="2018-01-01", end="2018-02-25", 
                               metrics = c("totalConversions", "totalConversionValue"), 
                               dimensions = c("sourceMediumPath"), 
                               # Если нужно отфильтровать по определенной цели, то указываем её номер ----
                               # filters = c("mcf:conversionGoalNumber==001"), ----
                               type="mcf")  


server <- function(input,output,session){
  output$sunburst <- renderSunburst({
    sequences <- mcf_gadata[,c(1,2)]
    
    
    sequences$sourceMediumPath <- gsub("CLICK", "", sequences$sourceMediumPath, fixed=TRUE)
    sequences$sourceMediumPath <- gsub("NA", "", sequences$sourceMediumPath, fixed=TRUE)
    sequences$sourceMediumPath <- gsub(":", "", sequences$sourceMediumPath, fixed=TRUE)
    
    sequences$sourceMediumPath <- lapply(strsplit(as.character(sequences$sourceMediumPath),">"), trimws)
    sequences$sourceMediumPath <- lapply(sequences$sourceMediumPath, head, pathLimit )
    sequences$sourceMediumPath <- lapply(sequences$sourceMediumPath, paste ,sep='',collapse ='-' )
    add_shiny(sunburst(sequences , count = T, legend=list(w=120)   ))
  })
  
  selection <- reactive({
    input$sunburst_mouseover
  })
  output$selection <- renderText(selection())
}

ui<-fluidPage(
  fluidRow(
    column(12,
           # plot sunburst ----
           
           sunburstOutput("sunburst", width = "100%", height = "600px")
    )
  )
)
shinyApp(ui = ui, server = server)
