library(olsrr)
library(car)
library(lmtest)
library(ggpubr)


data(mtcars)                              
View(mtcars)                              
plot(mpg ~ wt, data=mtcars)  

model <- lm(mpg ~ wt, data=mtcars) #membuat model regresi
abline(model) # membuat garis pada plot regresi

summary(model) #melihat model regresi

predict(model, newdata=data.frame(wt=6)) 

ggscatter(mtcars, x = "mpg", y = "wt", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")
library(ggpubr)
ols_plot_resid_qq(model)

ols_norm_test(model)

data("mtcars")
model=lm(mpg ~ am + wt + hp, data = mtcars)
summary(model)


hist(mtcars$am)
plot(mpg~am,data=mtcars)

dataku=mtcars[,c(1,4,6,9)]
cor(dataku,method = "pearson")


dwtest(model)

ols_plot_resid_hist(model)

ols_correlations(model)

ols_rsd_qqplot(model)
ols_norm_test(model)
lmtest::bptest(model)
dwtest(model)
ols_vif_tol(model)