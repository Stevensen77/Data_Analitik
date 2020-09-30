library(olsrr)
library(car)
library(lmtest)
library(ggpubr)


library(RMySQL)
con = dbConnect(MySQL(), user = 'root', password = '', dbname ='advertising', host = 'localhost')
myQuery <- "select * from advertising_table;"
advertising <- dbGetQuery(con, myQuery)
View(advertising)
data("advertising")
model_advertising=lm(Sales ~ TV + Radio + Newspaper, data = advertising)
summary(model_advertising)

hist(advertising$TV)
plot(Sales~TV,data=advertising)

dataku=advertising[,c(1,4,6,9)]
cor(dataku,method = "pearson")

ols_correlations(model_advertising)

ols_plot_resid_qq(model_advertising)
ols_test_normality(model_advertising)
lmtest::bptest(model_advertising)

dwtest(model_advertising)
ols_plot_resid_hist(model_advertising)
ols_vif_tol(model_advertising)
