library(openxlsx)

#load data and prepare a gene matrix
GeneExpression <- read.table('CellLines_15Genes.txt', header = TRUE)
GeneExpression1 = GeneExpression[,-1]
GeneExpression1 = as.matrix(GeneExpression1)

#visualize gene expression using bar plot
par(mar=c(10,4,2,2))

barplot(GeneExpression1[10,], col = "blue", main = "Gene 10 Expression", font.axis=1, cex.axis=1, las=2)
barplot(sort(GeneExpression1[2,]), col = "blue", main = "Gene 2 Expression", font.axis=1, cex.axis=1, las=2)

#compare visually two gene expression profiles
plot(GeneExpression1[,1], GeneExpression1[,11], xlab = names(GeneExpression[2]), ylab = names(GeneExpression[12]))

#perform multiple comparisons between genes in a data set
library(GGally)

GeneExpression1 <- data.frame(GeneExpression)

ggpairs(GeneExpression1, columns = 2:5)