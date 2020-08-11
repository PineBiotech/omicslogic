library (ggplot2)
library(cluster)
library(ggfortify)

#Read and prepare data table
data <- read.table ('LIHC_BRCA_data1_marked_no0.txt', header = TRUE, row.names=1, stringsAsFactors=FALSE, check.names=FALSE)
Origin_data <- t(data)
Group = t (data [1,])
cnames =colnames (data)
data <- read.table ('LIHC_BRCA_data1_marked_no0.txt', header = FALSE, row.names=1, skip=2, stringsAsFactors=FALSE, check.names=FALSE)
colnames (data) = cnames        

data = data.frame (Group, t (data))
data = data [, -1]
data = transform (data, as.numeric)

#Main computation - perform PCA
pca <- prcomp(data, scale. = TRUE, center = TRUE)

#Plot PCA
#autoplot(pca, data = Origin_data, colour = 'class')

ggplot(pca_res, aes(x=PC1, y=PC2, colour = Group)) + geom_point() + stat_ellipse(level = 0.6) + theme(legend.position='bottom')

#autoplot "pam" option - partitioning around medoids (PAM) and 
#identifying X number of clusters (user pre-selects desired number as second parameter to pam()). 
#autoplot() then performs PCA on the dataset and shades the points based on the PAM cluster
#assignments

#autoplot(pam(data, 2), frame = TRUE, frame.type = 'norm')

#perform k-means
#kmeans <- kmeans(data, 4)

#make a k-means plot
#autoplot(kmeans, data = data, label = TRUE, label.size = 3, frame = TRUE, frame.type='t')

#perform hierarchical clustering
#cl <- hclust (dist (data, 'euclidian'), method = 'ward.D2')

#make a hierarchical clustering plot
#par(mar=c(7, 5, 5, 2), family='Avenir Next', cex = 0.6 )
#plot(cl, xlab="euclidian", sub="ward.D2")

#interactive cluster identification
#identify(cl, FUN = NULL, N = 20, MAXCLUSTER = 20, DEV.FUN = NULL)

#rect.hclust(cl, k = 4, border=rainbow(4))
#x <- rect.hclust(cl, h = 50, which = c(2,7), border = 3:4)
#x
