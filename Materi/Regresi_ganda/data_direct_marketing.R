library(RMySQL)
con = dbConnect(MySQL(), user = 'root', password = '', dbname = 
                  +                     'db_reg', host = 'localhost')
myQuery <- "select * from reg_direct_marketing;"
reg_direct_marketing <- dbGetQuery(con, myQuery)
View(reg_direct_marketing)

head(reg_direct_marketing)
model_dm=lm(Y ~ X1 + X2, data = reg_direct_marketing)
model_dm
summary(model_dm)



myQuery <- "select * from reg_direct_marketing2;"
reg_direct_marketing2 <- dbGetQuery(con, myQuery)
View(reg_direct_marketing2)

head(reg_direct_marketing2)
model_dm2=lm(Y ~ X1, data = reg_direct_marketing2)
model_dm2
summary(model_dm2)


library(olsrr)
library(car)
library(lmtest)
library(ggpubr)

ols_plot_resid_qq(model_dm)
ols_test_normality(model_dm)

lmtest::bptest(model_dm)
dwtest(model_dm)
ols_plot_resid_hist(model_dm)


ols_plot_resid_qq(model_dm2)
ols_test_normality(model_dm2)

lmtest::bptest(model_dm2)
dwtest(model_dm2)
ols_plot_resid_hist(model_dm2)
