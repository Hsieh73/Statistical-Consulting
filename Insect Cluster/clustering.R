data<-read.csv("insect.csv")
length(table(data$freq_ind)) #freq_ind共有12種
data.clust<-data[,-(1:4)] #移除不是用來記錄昆蟲數量的欄位
means = apply(data.clust, 2, mean)
sds = apply(data.clust, 2, sd) 
data.clust = scale(data.clust, center=means, scale=sds) 

#階層式分群(Hierarchical Clustering)
#判斷資料之間的遠與近
E.dist <- dist(data.clust, method="euclidean") #歐式距離
M.dist <- dist(data.clust, method="manhattan") #曼哈頓距離

par(mfrow=c(1,2)) #讓圖片以1x2的方式呈現

#使用歐式距離進行分群
h.E.cluster <- hclust(E.dist)
plot(h.E.cluster, xlab="歐式距離")

#使用曼哈頓距離進行分群
h.M.cluster <- hclust(M.dist) 
plot(h.M.cluster, xlab="曼哈頓距離")

#不同的階層式分群方法
hclust(E.dist, method="single")   #最近法
hclust(E.dist, method="complete") #最遠法
hclust(E.dist, method="average")  #平均法
hclust(E.dist, method="centroid") #中心法
hclust(E.dist, method="ward.D2")  #華德法

#使用歐式距離&華德法進行分群
E.dist <- dist(data.clust, method="euclidean") #計算距離矩陣      
h.cluster <- hclust(E.dist, method="ward.D2")  #集群分析


#視覺化
plot(h.cluster)
rect.hclust(h.cluster, k=4, border="red")

#決定最佳的分群數目elbow plot
require(factoextra)
fviz_nbclust(data.clust, 
             FUNcluster = hcut,  #hierarchical clustering
             method = "wss",     #total within sum of square
             k.max = 12          #max number of clusters to consider
) 

plot(h.cluster, labels=data$family_C)

#階層的結構縮減，變成分成三群
cut.h.cluster <- cutree(h.cluster, k=4)  #指定歸類為n群,分成三群
cut.h.cluster

data$family_C[which(cut.h.cluster==2)]
data$family_C[which(cut.h.cluster==3)]

table(cut.h.cluster, data$freq_ind)
table(cut.h.cluster, data$family_C)

kmeans.cluster <- kmeans(data.clust, centers=4)
require(factoextra)
fviz_cluster(kmeans.cluster,           # 分群結果
             data = data.clust,              # 資料
             geom = c("point","text"), # 點和標籤(point & label)
             frame.type = "norm") 

#使用K-Medoid
require(cluster)
kmedoid.cluster <- pam(data, k=3) 
table(kmedoid.cluster$clustering, data$freq_ind) 
require(factoextra)
fviz_cluster(kmedoid.cluster,       # 分群結果
             data = data.clust,           # 資料
             geom = c("point"),     # 點 (point)
             frame.type = "norm")   # 框架型態

#以pamk函數找出的最佳分群數
library(fpc)
data.result=pamk(data.clust)
data.result$nc
