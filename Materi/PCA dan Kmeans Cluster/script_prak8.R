library(RMySQL)
con = dbConnect(MySQL(), user = 'root', password = '', dbname = 'db_da', host = 'localhost') 
myQuery <- "select * from komoditas;"
data_steven <- dbGetQuery(con, myQuery)
View(data_steven)
str(data_steven)
data_steven.pca=data_steven[,2:6]
komoditas_stev.pca <- prcomp(data_steven.pca, center = TRUE, scale. = TRUE) 
print(komoditas_stev.pca)
plot(komoditas_stev.pca, type = "l")
summary(komoditas_stev.pca)
predict(komoditas_stev.pca,  newdata=tail(data_steven))
hasil_stev=predict(komoditas_stev.pca, newdata=tail(data_steven))
View(hasil_stev)

komoditas_stev.kota <- data_steven[, 1]

library(devtools)
library(ggbiplot)
g_stev <- ggbiplot(komoditas_stev.pca,ellipse = TRUE, circle = TRUE)
g_stev <- g_stev + scale_color_discrete(name = '')
g_stev <- g_stev + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g_stev)


g_stev <- ggbiplot(komoditas_stev.pca, obs.scale = 1, var.scale = 1, groups= komoditas_stev.kota, circle = TRUE)
g_stev <- g_stev + scale_color_discrete(name = '')
g_stev <- g_stev + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g_stev)



#KMEANS
library(factoextra)
library(NbClust)
str(data_steven)

data_steven.pca=data_steven[,2:6]
View(data_steven.pca)

nb <- NbClust(data_steven.pca, distance = "euclidean", min.nc =2, max.nc = 10, method = "complete", index ="all")
km.res=kmeans(data_steven.pca,3,nstart = 25)
fviz_cluster(km.res, data = data_steven.pca, geom = "point",stand = FALSE, frame.type = "norm")
fviz_cluster(km.res, data = data_steven.pca)
