library(RMySQL)
con2 = dbConnect(MySQL(), user = 'root', password = '', dbname = 'direct_marketing', host = 'localhost')
myQuery_directmarketing <- "select * from direct_marketing;"
df_directmarketing <- dbGetQuery(con2, myQuery_directmarketing)

cross2 <- xtabs(AmountSpent ~ Age+OwnHome, data=df_directmarketing)
cross2
prop.table(cross2, 1)
chisq.test(cross2)