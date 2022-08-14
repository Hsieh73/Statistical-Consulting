data<-read.csv("insect.csv")
length(table(data$freq_ind)) #freq_ind共有12種
data.clust<-data[,-(1:4)] #移除不是用來記錄昆蟲數量的欄位

#使用原始資料進行分析
#階層式分群
E.dist <- dist(data.clust, method="euclidean") #歐式距離
M.dist <- dist(data.clust, method="manhattan") #曼哈頓距離
library(cluster)
m=c("single","complete","average", "ward")
E.ac<-function(x){agnes(E.dist, method = x)$ac} 
M.ac<-function(x){agnes(M.dist, method = x)$ac}
library(purrr)
map_dbl(m, E.ac)
map_dbl(m, M.ac) 

h.cluster <- hclust(E.dist, method="ward.D2")      #華德法

#分群視覺化
plot(h.cluster)
rect.hclust(h.cluster, k=2, border=c(2,4))

#階層的結構縮減，變成分成二群
cut.h.cluster <- cutree(h.cluster, k=2)  
cut.h.cluster

#最佳分群數(Average Silhouette method)
require(factoextra)
fviz_nbclust(data.clust, hcut, method = "silhouette",
             hc_method = "ward.D2")

#k-means
fviz_nbclust(data.clust, kmeans, method = "silhouette")
kmeans.fit <- kmeans(data.clust, centers = 2, nstart = 10)
# 分群視覺化
fviz_cluster(kmeans.fit, data = data.clust, geom = c("point","text"), # 點和標籤(point & label)
             frame.type = "norm")
require(useful)
plot(kmeans.fit,data=data.clust)

#------------------------------------------------------------------------------------------------------------
#資料進行標準化
means = apply(data.clust, 2, mean)
sds = apply(data.clust, 2, sd) 
data.clust = scale(data.clust, center=means, scale=sds) 


M.dist <- dist(data.clust, method="manhattan")      
h.cluster <- hclust(M.dist, method="ward.D2") 
#視覺化
plot(h.cluster)
rect.hclust(h.cluster, k=2, border=c(2,4))

#階層的結構縮減，變成分成二群
cut.h.cluster <- cutree(h.cluster, k=2)  
cut.h.cluster

#最佳分群數(Average Silhouette method)
fviz_nbclust(data.clust, hcut, method = "silhouette",
             hc_method = "ward.D2")

#k-means
fviz_nbclust(data.clust, kmeans, method = "silhouette")
kmeans.fit <- kmeans(data.clust, centers = 2)
# 分群視覺化
fviz_cluster(kmeans.fit, data = data.clust, geom = c("point","text"), # 點和標籤(point & label)
             frame.type = "norm")
require(useful)
plot(kmeans.fit,data=data.clust)




