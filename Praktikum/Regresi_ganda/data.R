library(RMySQL)
con = dbConnect(MySQL(), user = 'root', password = '', dbname ='db_reg', host = 'localhost')
myQuery <- "select * from reg;"
reg_stev <- dbGetQuery(con, myQuery)
View(reg_stev)
head(reg_stev)
model=lm(Y ~ X1 + X2, data = reg_stev)
model
summary(model)

