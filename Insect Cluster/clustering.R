data<-read.csv("insect.csv")
length(table(data$freq_ind)) #freq_ind�@��12��
data.clust<-data[,-(1:4)] #�������O�ΨӰO�����μƶq�����
means = apply(data.clust, 2, mean)
sds = apply(data.clust, 2, sd) 
data.clust = scale(data.clust, center=means, scale=sds) 

#���h�����s(Hierarchical Clustering)
#�P�_��Ƥ��������P��
E.dist <- dist(data.clust, method="euclidean") #�ڦ��Z��
M.dist <- dist(data.clust, method="manhattan") #�ҫ��y�Z��

par(mfrow=c(1,2)) #���Ϥ��H1x2���覡�e�{

#�ϥμڦ��Z���i����s
h.E.cluster <- hclust(E.dist)
plot(h.E.cluster, xlab="�ڦ��Z��")

#�ϥΰҫ��y�Z���i����s
h.M.cluster <- hclust(M.dist) 
plot(h.M.cluster, xlab="�ҫ��y�Z��")

#���P�����h�����s��k
hclust(E.dist, method="single")   #�̪�k
hclust(E.dist, method="complete") #�̻��k
hclust(E.dist, method="average")  #�����k
hclust(E.dist, method="centroid") #���ߪk
hclust(E.dist, method="ward.D2")  #�ؼw�k

#�ϥμڦ��Z��&�ؼw�k�i����s
E.dist <- dist(data.clust, method="euclidean") #�p��Z���x�}      
h.cluster <- hclust(E.dist, method="ward.D2")  #���s���R


#��ı��
plot(h.cluster)
rect.hclust(h.cluster, k=4, border="red")

#�M�w�̨Ϊ����s�ƥ�elbow plot
require(factoextra)
fviz_nbclust(data.clust, 
             FUNcluster = hcut,  #hierarchical clustering
             method = "wss",     #total within sum of square
             k.max = 12          #max number of clusters to consider
) 

plot(h.cluster, labels=data$family_C)

#���h�����c�Y��A�ܦ������T�s
cut.h.cluster <- cutree(h.cluster, k=4)  #���w�k����n�s,�����T�s
cut.h.cluster

data$family_C[which(cut.h.cluster==2)]
data$family_C[which(cut.h.cluster==3)]

table(cut.h.cluster, data$freq_ind)
table(cut.h.cluster, data$family_C)

kmeans.cluster <- kmeans(data.clust, centers=4)
require(factoextra)
fviz_cluster(kmeans.cluster,           # ���s���G
             data = data.clust,              # ���
             geom = c("point","text"), # �I�M����(point & label)
             frame.type = "norm") 

#�ϥ�K-Medoid
require(cluster)
kmedoid.cluster <- pam(data, k=3) 
table(kmedoid.cluster$clustering, data$freq_ind) 
require(factoextra)
fviz_cluster(kmedoid.cluster,       # ���s���G
             data = data.clust,           # ���
             geom = c("point"),     # �I (point)
             frame.type = "norm")   # �ج[���A

#�Hpamk��Ƨ�X���̨Τ��s��
library(fpc)
data.result=pamk(data.clust)
data.result$nc