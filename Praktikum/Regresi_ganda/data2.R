library(RMySQL)
con = dbConnect(MySQL(), user = 'root', password = '', dbname = 
                    +                     'db_reg', host = 'localhost')
myQuery <- "select * from reg2;"
reg2_stev <- dbGetQuery(con, myQuery)
View(reg2_stev)

head(reg2_stev)
model_reg2=lm(Y ~ X1 + X2 + X3, data = reg2_stev)
model_reg2
summary(model_reg2)
