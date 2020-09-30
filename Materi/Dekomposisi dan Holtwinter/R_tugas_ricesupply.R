library(RMySQL)
con = dbConnect(MySQL(), user = 'root', password = '', dbname ='db_da', host = 'localhost')
myQuery <- "select * from ricesupply;"
ricesupply_stev <- dbGetQuery(con, myQuery)
View(ricesupply_stev)

supply_tugas <- ts(ricesupply_stev$Jateng, start = c(2011, 1), frequency = 12)
plot(supply_tugas)
library(forecast)
fit <- HoltWinters(supply_tugas)
f1 <- forecast(fit,h=12)
accuracy (f1)
print(f1)
forecast(fit,12)

ricets_stev <- ts(ricesupply_stev$Jateng, frequency=12, start=c(2011,1))
ricets_stev
plot.ts(ricets_stev)
fit <- stl(ricets_stev,s.window="period")
plot(fit)
fit <- forecast(ricets_stev)
accuracy(fit)
ricedec <- decompose(ricets_stev)
ricedec$seasonal

plot(ricedec)
View(ricets_stev)
install.packages("xlsx")
library(xlsx)
write.xlsx(ricets_stev, "D:/semester 6/Data Analitik/Praktikum/prak9/Jateng.xlsx") 

