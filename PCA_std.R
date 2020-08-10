library (ggplot2)
library (ggpubr)

options(echo=FALSE)
args <- commandArgs(trailingOnly = TRUE)

#args [1] - input file
#args [2] - transpose (YES/NO)
#args [3] - center (YES/NO)
#args [4] - scale (YES/NO)
#args [5] - number of axis to draw

MaxGroupsForLegend <- 50

#Read and prepare data table
data <- read.table (args [1], header = TRUE, row.names=1, stringsAsFactors=FALSE, check.names=FALSE)
if (args [2] == "YES") {
    if ((rownames (data)[1] ==  "group") || (rownames (data)[1] ==  "Group") || (rownames (data)[1] ==  "GROUP")) {
        Group = t (data [1,])
        cnames =colnames (data)
        data <- read.table (args[1], header = FALSE, row.names=1, skip=2, stringsAsFactors=FALSE, check.names=FALSE)
        colnames (data) = cnames        

        data = data.frame (Group, t (data))
    } else {
        data = t (data)
    }
}
if ((colnames (data) [1] == "group") || (colnames (data) [1] == "Group") || (colnames (data) [1] == "GROUP")) {
   Group = data [, 1]
   data = data [, -1]
   data = transform (data, as.numeric)
   LegendTitle="Group"
} else {
   Group = rownames (data)
   LegendTitle="Sample"
}
Group = as.factor (Group)

   
#Parse centering and scaling arguments
if(args [3] == "YES") {
    cent = TRUE
} else {
    cent = FALSE
}
if(args [4] == "YES") {
    sc = TRUE
} else {
   sc = FALSE
}


#Main computation - perform PCA
pca <- prcomp (data, scale. = sc, center=cent)
pca_res = data.frame (pca$x, Group)


#Draw PCA plots
todraw = as.numeric (args[5])
if (todraw < 2) {
    todraw = 2
}
if (todraw > length (colnames (pca$x))) {
    todraw = length (colnames (pca$x))
}

pdf ("PCA_plots.pdf")

# Add variation percent to PC names
ei = pca$sdev^2
vp = ei / sum(ei) * 100.0
prevnames <- colnames (pca$x)
newnames <- paste (colnames (pca$x), "(", round (vp, digits=2), "%)", sep="")
newnames_prt <- paste (colnames (pca$x), " (", round (vp, digits=2), "%)", sep="")


if (length (unique (Group)) <= MaxGroupsForLegend) {
    legend_pos <- "bottom"
} else {
    legend_pos <- "none"
}

for (i in 1:(todraw-1)) {
    for (j in (i+1):todraw) {
        plot = ggplot (data.frame (pca$x), aes_string(x=colnames (pca$x) [i], y=colnames(pca$x)[j], colour = "Group")) + geom_point() + labs(x=newnames_prt[i], y=newnames_prt[j], color=LegendTitle) + theme(legend.position=legend_pos)
        print (plot)
    }
}

tmp <- dev.off ()

#Save PCA table
colnames (pca$x) <- newnames
cat ("Obj\t", file="PCA_table.txt", append=FALSE)
write.table(pca$x, file="PCA_table.txt", row.names=TRUE, col.names=TRUE, sep="\t", quote=FALSE, append=TRUE)
colnames (pca$x) <- prevnames

#Save some additional information
cat ("Component\t", file="PCA_additional.txt", append=FALSE)
cat (colnames(pca$x), file="PCA_additional.txt", append=TRUE, sep="\t")
cat ("\n", file="PCA_additional.txt", append=TRUE)

cat ("SDev\t", file="PCA_additional.txt", append=TRUE)
cat (pca$sdev, file="PCA_additional.txt", append=TRUE, sep="\t")
cat ("\n", file="PCA_additional.txt", append=TRUE)

cat ("VariancePerccent\t", file="PCA_additional.txt", append=TRUE)
cat (vp, file="PCA_additional.txt", append=TRUE, sep="\t")
cat ("\n", file="PCA_additional.txt", append=TRUE)

cat ("CumulVariancePerccent\t", file="PCA_additional.txt", append=TRUE)
cat (cumsum(vp), file="PCA_additional.txt", append=TRUE, sep="\t")
cat ("\n\n", file="PCA_additional.txt", append=TRUE)


cnames <- row.names(pca$rotation)
cent_d <- row.names(pca$rotation)
shift_d <- row.names(pca$rotation)
if (cent == FALSE) {
    cent_d [] <- "NA(Centering=FALSE)"
} else {
    cent_d <- pca$center
}
if (sc == FALSE) {
    shift_d <- "NA(Scaling=FALSE)"
} else {
    shift_d <- pca$scale
}

out_t <- cbind (cnames, cent_d, shift_d)
colnames (out_t) <- c ("Feature", "CenteringShift", "ScalingCoefficient")

write.table (out_t, file="PCA_additional.txt", col.names=TRUE, row.names=FALSE, append=TRUE, sep="\t")


