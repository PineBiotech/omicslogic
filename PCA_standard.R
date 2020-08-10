#Read and prepare data table
data <- read.table ('CellLines_52samples_ExprData_Group.txt', header = TRUE, row.names=1, stringsAsFactors=FALSE, check.names=FALSE)
rnames <- colnames (data)

Group <- t (data [1,])
cnames <- colnames (data)
data <- read.table ('CellLines_52samples_ExprData_Group.txt', header = FALSE, row.names=1, skip=2, stringsAsFactors=FALSE, check.names=FALSE)
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
plot(PCA_table[,1:2], col='black', bg=Group, pch=21, main="PCA plot for Cell Line Gene Expression Data", font.main=2, cex.main=1.5)
legend("bottomleft", legend=unique(Group), cex=0.8, fill=unique(Group))
text(x=pca_res$PC1, y=pca_res$PC2, labels = Group, pos = 3, cex=0.5)

