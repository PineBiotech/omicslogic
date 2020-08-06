#data frame (df)
SomeGenes = read.table('CellLines_15Genes.txt', header = TRUE)
AllGenes = read.table('CellLines_expression_FPKM.txt', header = TRUE)

ColNames1 = AllGenes$id

#Set the data portion (everything except the labels)
AllGenes = AllGenes[,-1]
AllGenes = as.matrix(AllGenes)

df <- AllGenes
logdata = log(df)

#Basic settings:
par(mar=c(14,4,2,2))
colors = c(rep('red',6),rep('blue',3),rep('green',2), rep('gray',2))

#Descriptive statistics
#summary(df)
#hist(df, col='blue')
#mean(df)

#Visual Summary: Boxplot and Heatmap packages:
hist(df, col="gray", main = 'Raw Data Histogram')
boxplot(df, col='blue', main = 'Raw Data Boxplots (FPKM)', las = 2, cex.axis=0.8)
boxplot(logdata, col='red', main = 'Log Transformed Data', las = 2, cex.axis=0.8)
hist(logdata, main = 'Log Transformed Histogram')
#heatmap(df)

#CompareGene Expression
#view(ColNames1)
barplot(df[2,], col = "blue", main = ColNames1[2], font.axis=1, cex.axis=1, las=2)
barplot(logdata[2,], col = "blue", main = ColNames1[2], font.axis=1, cex.axis=1, las=2)


