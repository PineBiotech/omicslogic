#Read and prepare data table
data <- read.table ('LIHC_BRCA_data1_marked_no0.txt', header = TRUE, row.names=1, stringsAsFactors=FALSE, check.names=FALSE)
rnames <- colnames (data)

Group <- t (data [1,])
cnames <- colnames (data)
data <- read.table ('LIHC_BRCA_data1_marked_no0.txt', header = FALSE, row.names=1, skip=2, stringsAsFactors=FALSE, check.names=FALSE)
colnames (data) = cnames        

data = data.frame (Group, t (data))

Group = data [, 1]
data = data [, -1]
data = transform (data, as.numeric)
Group = as.factor (Group)

cent = TRUE
sc = TRUE

#Main computation - perform PCA
pca <- prcomp (data, scale. = sc, center=cent)
pca_res = data.frame (pca$x, Group)
PCA_table<-as.matrix(pca$x)

#plot PCA
par(mar=c(5, 5, 5, 5), family='Avenir Next')
plot(PCA_table[,1:2], col='black', bg=Group, pch=21, lwd = 0.9, cex = 1, main="PCA plot for Cell Line Gene Expression Data", font.main=3, cex.main=1.5)

#add legend and text
legend("bottomleft", legend=unique(Group), cex=1, fill=unique(Group))
text(x=pca_res$PC1, y=pca_res$PC2, labels = Group, pos = 1, cex=1, font = 1)

