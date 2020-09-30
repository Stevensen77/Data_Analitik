library(RMySQL)
con = dbConnect(MySQL(), user = 'root', password = '', dbname ='db_da', host = 'localhost')
myQuery <- "select * from hargaberas;"
hargaberas_stev <- dbGetQuery(con, myQuery)
View(hargaberas_stev)

hargaberas_tugas <- ts(hargaberas_stev$Saigon, start = c(2011, 1), frequency = 12)
plot(hargaberas_tugas)
library(forecast)
fit <- HoltWinters(hargaberas_tugas)
f1 <- forecast(fit,h=12)
accuracy (f1)
print(f1)
forecast(fit,12)

berasts_stev <- ts(hargaberas_stev$Saigon, frequency=12, start=c(2011,1))
berasts_stev
plot.ts(berasts_stev)
fit <- stl(berasts_stev,s.window="period")
plot(fit)
fit <- forecast(berasts_stev)
accuracy(fit)
ricedec <- decompose(berasts_stev)
ricedec$seasonal

plot(ricedec)
View(berasts_stev)
install.packages("xlsx")
library(xlsx)
write.xlsx(ricets_stev, "D:/semester 6/Data Analitik/Praktikum/prak9/Saigon.xlsx") 

