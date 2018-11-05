library("googleAnalyticsR")
library("tidyverse")
library("devtools")
library("googleVis")
library("shiny")
library("RGA")
library("sunbustR")
ga_auth()
my_accounts <- ga_account_list()

ga_view_id<-166777564

mcf_gadata <- get_mcf(profileId  = "ga:166777564",
                      start.date    = "2018-06-01",
                      end.date      = "2018-10-01",
                      dimensions    = "mcf:sourceMediumPath",
                      metrics       = c("mcf:totalConversions"),
                      fetch.by      = "day",
                      samplingLevel =  "HIGHER_PRECISION",
                      max.results   = 10000)



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
           sunburstOutput("sunburst", width = "100%", height = "1000px")
    )
  )
)

shinyApp(ui = ui, server = server)

