library(RMySQL)
con = dbConnect(MySQL(), user = 'root', password = '', dbname = 
                  +                     'db_reg', host = 'localhost')
myQuery <- "select * from reg3;"
reg3_stev <- dbGetQuery(con, myQuery)
View(reg3_stev)

head(reg3_stev)
model_reg3=lm(Y ~ X + X.2, data = reg3_stev)
model_reg3
summary(model_reg3)
