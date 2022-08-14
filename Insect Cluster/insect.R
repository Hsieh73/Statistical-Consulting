data<-read.csv("insect.csv")
length(table(data$freq_ind)) #freq_ind�@��12��
data.clust<-data[,-(1:4)] #�������O�ΨӰO�����μƶq�����

#�ϥέ�l��ƶi����R
#���h�����s
E.dist <- dist(data.clust, method="euclidean") #�ڦ��Z��
M.dist <- dist(data.clust, method="manhattan") #�ҫ��y�Z��
library(cluster)
m=c("single","complete","average", "ward")
E.ac<-function(x){agnes(E.dist, method = x)$ac} 
M.ac<-function(x){agnes(M.dist, method = x)$ac}
library(purrr)
map_dbl(m, E.ac)
map_dbl(m, M.ac) 

h.cluster <- hclust(E.dist, method="ward.D2")      #�ؼw�k

#���s��ı��
plot(h.cluster)
rect.hclust(h.cluster, k=2, border=c(2,4))

#���h�����c�Y��A�ܦ������G�s
cut.h.cluster <- cutree(h.cluster, k=2)  
cut.h.cluster

#�̨Τ��s��(Average Silhouette method)
require(factoextra)
fviz_nbclust(data.clust, hcut, method = "silhouette",
             hc_method = "ward.D2")

#k-means
fviz_nbclust(data.clust, kmeans, method = "silhouette")
kmeans.fit <- kmeans(data.clust, centers = 2, nstart = 10)
# ���s��ı��
fviz_cluster(kmeans.fit, data = data.clust, geom = c("point","text"), # �I�M����(point & label)
             frame.type = "norm")
require(useful)
plot(kmeans.fit,data=data.clust)

#------------------------------------------------------------------------------------------------------------
#��ƶi��зǤ�
means = apply(data.clust, 2, mean)
sds = apply(data.clust, 2, sd) 
data.clust = scale(data.clust, center=means, scale=sds) 


M.dist <- dist(data.clust, method="manhattan")      
h.cluster <- hclust(M.dist, method="ward.D2") 
#��ı��
plot(h.cluster)
rect.hclust(h.cluster, k=2, border=c(2,4))

#���h�����c�Y��A�ܦ������G�s
cut.h.cluster <- cutree(h.cluster, k=2)  
cut.h.cluster

#�̨Τ��s��(Average Silhouette method)
fviz_nbclust(data.clust, hcut, method = "silhouette",
             hc_method = "ward.D2")

#k-means
fviz_nbclust(data.clust, kmeans, method = "silhouette")
kmeans.fit <- kmeans(data.clust, centers = 2)
# ���s��ı��
fviz_cluster(kmeans.fit, data = data.clust, geom = c("point","text"), # �I�M����(point & label)
             frame.type = "norm")
require(useful)
plot(kmeans.fit,data=data.clust)



