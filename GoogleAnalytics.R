install.packages("RGoogleAnalytics")

key <- "8542871497-6jcd8csd4ee6j6dgs7f1m67q5mt7lkqh.apps.googleusercontent.com"
secret <- "l-bxpRb7yfMHWt5oaD-Fs6HL"
token <- Auth(key,secret)
ga_auth()

save(token,file="./token_file2")

query.list1 <-  Init(start.date = "2018-08-01",
                     end.date = "2018-08-22",
                     dimensions = "ga:UserType",
                     metrics = "ga:users",
                     max.results = 10000,
                     table.id = "ga:166777564")
query.list2 <- Init(start.date = "2018-08-01",
                    end.date = "2018-08-22",
                    dimensions = "ga:UserType,ga:deviceCategory",
                    metrics = "ga:users",
                    segment = "gaid::tKq8YR_lTEOgUot6SO9meA",
                    filter = "ga:devicecategory=~desktop|mobile",
                    max.results = 10000,
                    table.id = "ga:166777564")
query.list3 <- Init(start.date = "2018-08-01", 
                    end.date = "2018-08-22",
                    dimensions = "ga:date",
                    metrics = "ga:users,ga:goalCompletionsAll",
                    filter = "ga:devicecategory=~desktip|mobile",
                    max.results = 10000,
                    table.id = "ga:166777564")

ga.query1 <- QueryBuilder(query.list1)
ga.query2 <- QueryBuilder(query.list2)
ga.query3 <- QueryBuilder(query.list3) 
ga.data1 <- GetReportData(ga.query1, token)
ga.data2 <- GetReportData(ga.query2, token)
ga.data3 <- GetReportData(ga.query3, token)

cbind(ga.data1,ga.data2,ga.data3)


a <- c(1,2,3,4,5,6,7,8,6,7,8,6,5,6,3,2,5,6,1,2,3,6,5,7)
b <- c(2,4,5,6,1,2,5,6,7,7,8,9,1,2,4,7,8,1,2,5,7,5,4,3)

shapiro.test(a)
qqnorm(a)

shapiro.test(b)
qqnorm(b)


new <- boxplot(a)

x <- rnorm(c(11,23,24,5,6,17,124,1243,1234,100))
y <- rnorm(c(11,23,24,5,6,17,124,1243,1234,100))
plot(x, y)

?z
?binom.test
?rnorm
