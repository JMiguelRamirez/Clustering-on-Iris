#Loading libraries
library(FactoMineR)
library(ggplot2)
library(cluster)
library(e1071)
library(ClusterR)
library(mclust)
library(ggpubr)



addline_format <- function(x,...){
  gsub('\\s','\n',x)
}


data(iris)
set.seed(123)
rows <- sample(nrow(iris))
iris <- iris[rows,]
X <- iris[,1:4]


pc <- PCA(X)
nd <- 2
psi <- pc$ind$coord[,1:nd]


ga <- ggplot(as.data.frame(psi)) + geom_point(aes(psi[,1], psi[,2], col=iris[,5])) + theme_bw() + 
  scale_color_manual(values = c("black", "red", "green")) + xlab("PC 1") + ylab("PC 2") +
  ggtitle("Reality") + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))


#K-Means
set.seed(123)
kmeans <- kmeans(psi, 3)

gb <- ggplot(as.data.frame(psi)) + geom_point(aes(psi[,1], psi[,2], col=as.factor(kmeans$cluster))) + 
  theme_bw() + scale_color_manual(values = c("black", "red", "green"),
                                  breaks=c("1","2","3"),
                                  labels=c("setosa","versicolor","virginica")) + 
  xlab("PC 1") + ylab("PC 2") + ggtitle("K-Means") + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) 

tab <- table(kmeans$cluster, iris[,5])
tab <- tab[,c("setosa", "versicolor", "virginica")]
(kmeans_total <- round((sum(diag(tab))/150)*100,2)) #83.33 %

# library(fossil)
# rand.index(as.integer(iris[,5]), kmeans$cluster) #0.832

#K-Medoids
set.seed(123)
pam <- pam(psi, 3, metric = "euclidean")

gc <- ggplot(as.data.frame(psi)) + geom_point(aes(psi[,1], psi[,2], col=as.factor(pam$clustering))) + 
  theme_bw() + scale_color_manual(values = c("black", "green", "red"),
                                  breaks=c("1","3","2"),
                                  labels=c("setosa","versicolor","virginica")) + 
  xlab("PC 1") + ylab("PC 2") + ggtitle("K-Medoids") + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) 


tab <- table(pam$clustering, iris[,5])
tab <- tab[,c("setosa", "virginica", "versicolor")]
(kmedoids_total <- round((sum(diag(tab))/150)*100,2)) #86


#Fuzzy C-means

set.seed(123)
cm <- cmeans(psi, 3)
booleans <- apply(X = cm$membership<0.6, 1, all)

gd <- ggplot(as.data.frame(psi)) + geom_point(aes(psi[,1], psi[,2], col=as.factor(cm$cluster))) + 
  theme_bw()  + 
  geom_point(data=as.data.frame(psi[booleans,]), aes(psi[booleans,1], psi[booleans,2], 
                                                     col="blue"), shape=1, size=2) +
  scale_color_manual(values = c("black", "red", "green", "blue"),
                     breaks=c("1","2","3", "blue"),
                     labels=c("setosa","versicolor","virginica", "low certainty")) +
  
  xlab("PC 1") + ylab("PC 2") + ggtitle("Fuzzy \nC-Means") + 
  theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) +
  guides(colour = guide_legend(override.aes = list(shape = c(19,19,19,1))))
leg <- get_legend(gd)
gd <- gd + theme(legend.position = "none")

tab <- table(cm$cluster, iris[,5])
tab <- tab[,c("setosa","versicolor", "virginica")]
(fuzzy_total <- round((sum(diag(tab))/150)*100,2)) #83.33 %

#Hierarchical

clu.iris <- HCPC(pc, nb.clust=-1, consol=F, graph=F)
barplot(clu.iris$call$t$inert.gain[1:40],main="Inertia loss at each iteration")
abline(v=2.5, col="red")
clu.iris <- HCPC(pc, nb.clust=3, consol=F, graph=F) 

ge <- ggplot(clu.iris$call$X[,1:2]) + geom_point(aes(clu.iris$call$X[,1], clu.iris$call$X[,2], col=as.factor(clu.iris$call$X$clust))) + 
  theme_bw() + scale_color_manual(values = c("black", "red", "green"),
                                  breaks=c("1","2","3"),
                                  labels=c("setosa","versicolor","virginica")) + 
  xlab("PC 1") + ylab("PC 2") + ggtitle("Agglomerative Hierarchical \nClustering") + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) 


tab_hier <- table(clu.iris$data.clust$clust, iris[,5])
(hier_total <- round((sum(diag(tab_hier))/150)*100,2)) #82.67 %

clu.iris.con <- HCPC(pc, nb.clust=3, consol=T, graph=F)
gf <- ggplot(clu.iris.con$call$X[,1:2]) + geom_point(aes(clu.iris.con$call$X[,1], clu.iris.con$call$X[,2], col=as.factor(clu.iris.con$call$X$clust))) + 
  theme_bw() + scale_color_manual(values = c("black", "red", "green"),
                                  breaks=c("1","2","3"),
                                  labels=c("setosa","versicolor","virginica")) + 
  xlab("PC 1") + ylab("PC 2") + ggtitle("Agglomerative Hierarchical \n+ K-Means") + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) 


tab_hier <- table(clu.iris.con$data.clust$clust, iris[,5])
(hier_kmeans_total <- round((sum(diag(tab_hier))/150)*100,2)) #83.33333


clu.iris <- HCPC(pc, nb.clust=3, consol=F, graph=F, method="single")
tab_hier <- table(clu.iris$data.clust$clust, iris[,5])
(sum(diag(tab_hier))/150)*100 #66

clu.iris <- HCPC(pc, nb.clust=3, consol=F, graph=F, method="complete")
tab_hier <- table(clu.iris$data.clust$clust, iris[,5])
(sum(diag(tab_hier))/150)*100 #78.66667

clu.iris <- HCPC(pc, nb.clust=3, consol=F, graph=F, method="ward") #default
tab_hier <- table(clu.iris$data.clust$clust, iris[,5])
(sum(diag(tab_hier))/150)*100 #82.66667

clu.iris <- HCPC(pc, nb.clust=3, consol=F, graph=F, method="average")
tab_hier <- table(clu.iris$data.clust$clust, iris[,5])
(sum(diag(tab_hier))/150)*100 #68.67

clu.iris <- HCPC(pc, nb.clust=3, consol=T, graph=F, method="ward")
tab_hier <- table(clu.iris$data.clust$clust, iris[,5])
(sum(diag(tab_hier))/150)*100 #83.33333

clu.iris <- HCPC(pc, nb.clust=3, consol=T, graph=F, method="single")
tab_hier <- table(clu.iris$data.clust$clust, iris[,5])
(sum(diag(tab_hier))/150)*100 #83.33333 

clu.iris <- HCPC(pc, nb.clust=3, consol=T, graph=F, method="average")
tab_hier <- table(clu.iris$data.clust$clust, iris[,5])
(sum(diag(tab_hier))/150)*100 #83.33333

clu.iris <- HCPC(pc, nb.clust=3, consol=T, graph=F, method="complete")
tab_hier <- table(clu.iris$data.clust$clust, iris[,5])
(sum(diag(tab_hier))/150)*100 


#Mixture of Gaussians 
set.seed(123)
gmm = GMM(psi, 3,  dist_mode = "maha_dist", seed_mode = "random_subset", 
          km_iter = 0, em_iter=10, seed=1234)          
pr = predict_GMM(psi, gmm$centroids, gmm$covariance_matrices, gmm$weights) 
booleans_2 <- apply(X = pr$cluster_proba<0.7, 1, all)


gg <- ggplot(as.data.frame(psi)) + geom_point(aes(psi[,1], psi[,2], col=as.factor(pr$cluster_labels))) + 
  theme_bw()  + 
  geom_point(data=as.data.frame(psi[booleans_2,]), aes(psi[booleans_2,1], psi[booleans_2,2], 
                                                     col="blue"), shape=1, size=2) +
  scale_color_manual(values = c("red", "black", "green", "blue"),
                     breaks=c("1","0","2", "blue"),
                     labels=c("setosa","versicolor","virginica", "low certainty")) +
  
  xlab("PC 1") + ylab("PC 2") + ggtitle("Gaussian \nMixture") + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  guides(colour = guide_legend(override.aes = list(shape = c(19,19,19,1))))

tab <- table(pr$cluster_labels, iris[,5])
tab <- tab[,c("versicolor", "setosa", "virginica")]
(gaus_total <- round((sum(diag(tab))/150)*100,2)) #86.67 % 


#k-means + gaussian
set.seed(1234)
gmm_2 = GMM(psi, 3,  dist_mode = "maha_dist", seed_mode = "random_subset", km_iter = 10, em_iter=10, seed=1234)          
pr_2 = predict_GMM(psi, gmm_2$centroids, gmm_2$covariance_matrices, gmm_2$weights) 
booleans_3 <- apply(X = pr_2$cluster_proba<0.7, 1, all) #<0-7? o poner 0.6?

gh <- ggplot(as.data.frame(psi)) + geom_point(aes(psi[,1], psi[,2], col=as.factor(pr_2$cluster_labels))) + 
  theme_bw()  + 
  geom_point(data=as.data.frame(psi[booleans_3,]), aes(psi[booleans_3,1], psi[booleans_3,2], 
                                                       col="blue"), shape=1, size=2) +
  scale_color_manual(values = c("red", "black", "green", "blue"),
                     breaks=c("1","0","2", "blue"),
                     labels=c("setosa","versicolor","virginica", "low certainty")) +
  xlab("PC 1") + ylab("PC 2") + ggtitle("K-Means \n+ Gaussian Mixture") + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  guides(colour = guide_legend(override.aes = list(shape = c(19,19,19,1))))

tab_2 <- table(pr_2$cluster_labels, iris[,5])
tab_2 <- tab_2[,c("versicolor", "setosa", "virginica")]
(kmeans_gaus_total <- round((sum(diag(tab_2))/150)*100,2)) #85.33 %


#agglomerative + gaussian
clu.iris <- HCPC(pc, nb.clust=3, consol=F, graph=F)
set.seed(1234)
gaus <- Mclust(psi, 3, initialization = clu.iris, modelNames = "EVV") #VVV default
#EVV = ellipsoidal, equal volume 90 %
#we do not want equal shape, but we do want equal volume
booleans_4 <- apply(X = gaus$z<0.7, 1, all)

gi <- ggplot(as.data.frame(psi)) + geom_point(aes(psi[,1], psi[,2], col=as.factor(gaus$classification))) + 
  theme_bw()  + 
  geom_point(data=as.data.frame(psi[booleans_4,]), aes(psi[booleans_4,1], psi[booleans_4,2], 
                                                       col="blue"), shape=1, size=2) +
  scale_color_manual(values = c("black", "red", "green", "blue"),
                     breaks=c("1","3","2", "blue"),
                     labels=c("setosa","versicolor","virginica", "low certainty")) +
  xlab("PC 1") + ylab("PC 2") + ggtitle("Agglomerative Hierarchical \n+ Gaussian Mixture") + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  guides(colour = guide_legend(override.aes = list(shape = c(19,19,19,1))))


tab <- table(gaus$classification, iris[,5])
tab <- tab[,c("setosa","versicolor", "virginica")]
(hier_gaus_total <- round((sum(diag(tab))/150)*100,2)) #85.33 %



g <- ggarrange(ga, gb, gc, gd, ge, gf, gg, gh, gi, ncol=3, nrow=3,
          labels = c("A", "B","C","D", "E", "F", "G", "H", "I"))

legend <- as_ggplot(leg)

#accuracy plot


bars <- t(data.frame(kmeans_total, kmedoids_total, fuzzy_total, hier_total, hier_kmeans_total, 
                     gaus_total, kmeans_gaus_total, hier_gaus_total))
rownames(bars) <- c("K-Means", "K-Medoids", "Fuzzy C-Means", "Agglomerative Hierarchical", "Hierarchical + K-Means",
                    "Gaussian Mixture", "K-Means + Gaussian Mixture", "Hierarchical + Gaussian Mixture")
names <- row.names(bars)

bars <- cbind(names, bars)
rownames(bars) <- NULL
colnames(bars) <- c("names", "values")
bars <- as.data.frame(bars)

bars$names <- factor(bars$names, levels = c(c("K-Means", "K-Medoids", "Fuzzy C-Means", "Agglomerative Hierarchical", "Hierarchical + K-Means",
                                       "Gaussian Mixture", "K-Means + Gaussian Mixture", "Hierarchical + Gaussian Mixture")
))




bar <- ggplot(bars, aes(bars$names, bars$values, fill=bars$names)) + geom_col() + 
  xlab("") + ylab("Accuracy (%)") + theme_bw() + theme(legend.position = "none") +
  scale_x_discrete(labels = addline_format(c("K-Means", "K-Medoids", "Fuzzy C-Means", "Agglomerative Hierarchical Clustering", 
                                             "Hierarchical + K-Means",
                                             "Gaussian Mixture", "K-Means + Gaussian Mixture", 
                                             "Hierarchical + Gaussian Mixture"))) 
