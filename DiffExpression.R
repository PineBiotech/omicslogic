#Load packages
library(DESeq2)
library(EnhancedVolcano)

#install.packages("dplyr")

PDXdataset = read.table('PDX_HTSeq_gene_expression.txt', header = TRUE, row.names = 1, sep='\t')

#Define data and remove id column
data <- (PDXdataset)

colnames(PDXdataset) -> names
meta_classes<- c('ER','ER','ER','ER', 'ER', 'ER', 'ER', 'TN','TN','TN','TN','TN', 'TN', 'TN')
meta_data <- data.frame(names, meta_classes)

#Perform first step of DESeq (Define DESeq dataset)
ds <- DESeqDataSetFromMatrix(countData=data, colData=meta_data, design=~meta_classes)

# Perform pre-filtering of the data
ds <- ds[rowSums(counts(ds)) > 2, ]

#Perform DEseq and create results object
deseq2Data <- DESeq(ds)
deseq2Results <- results(deseq2Data)
print(deseq2Results)

#Output results in.txt format
write.table(deseq2Results,'DESeq2_PDX_R.txt', row.names=TRUE,col.names=NA, sep='\t', quote=TRUE, append=TRUE)

#Create MA plot
plotMA(deseq2Results)

#Create enhance volcano plot with identified differentially expressed genes
EnhancedVolcano(res, x = 'log2FoldChange',
                y = 'pvalue', lab = rownames(res)
                , xlim = c(-5, 8))
   

# install.packages "ggplot2", "scales", "viridis"
library(ggplot2)
library(scales) # needed for oob parameter
library(viridis)

# Coerce to a data frame
deseq2ResDF <- as.data.frame(deseq2Results)

# Examine this data frame
head(deseq2ResDF)

# Set a boolean column for significance
deseq2ResDF$significant <- ifelse(deseq2ResDF$padj < .1, "Significant", NA)

# Convert the DESeq transformed object to a data frame
deseq2VST <- assay(deseq2Data)
deseq2VST <- as.data.frame(deseq2VST)
deseq2VST$Gene <- rownames(deseq2VST)
head(deseq2VST)

# Keep only the significantly differentiated genes where the fold-change was at least 3
sigGenes <- rownames(deseq2ResDF[deseq2ResDF$padj <= .05 & abs(deseq2ResDF$log2FoldChange) > 3,])
deseq2VST <- deseq2VST[deseq2VST$Gene %in% sigGenes,]

# Convert the VST counts to long format for ggplot2
library(reshape2)

# Now overwrite our original data frame with the long format
deseq2VST <- melt(deseq2VST, id.vars=c("Gene"))

# Make a heatmap
heatmap <- ggplot(deseq2VST, aes(x=variable, y=Gene, fill=value)) + geom_raster() + scale_fill_viridis(trans="sqrt") + theme(axis.text.x=element_text(angle=65, hjust=1), axis.text.y=element_blank(), axis.ticks.y=element_blank())
heatmap


        
        