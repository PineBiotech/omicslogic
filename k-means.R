library(cluster)

#Read and prepare data table
data <- read.table ('CellLines_52samples_ExprData_Group.txt', header = TRUE, row.names=1, stringsAsFactors=FALSE, check.names=FALSE)
Origin_data <- t(data)
Group = t (data [1,])
cnames =colnames (data)
data <- read.table ('CellLines_52samples_ExprData_Group.txt', header = FALSE, row.names=1, skip=2, stringsAsFactors=FALSE, check.names=FALSE)
colnames (data) = cnames        

data = data.frame (Group, t (data))
data = data [, -1]
data = transform (data, as.numeric)

#make a k-means plot
autoplot(kmeans(data, 4), data = data, label = TRUE, label.size = 3, frame = TRUE, frame.type='t')

#make a k-means plot
autoplot(kmeans(data, 4), data = data, label = TRUE, label.size = 3, frame = TRUE, frame.type='t')

#make a k-means plot
autoplot(kmeans(data, 4), data = data, label = TRUE, label.size = 3, frame = TRUE, frame.type='t')

#make a k-means plot
autoplot(kmeans(data, 4), data = data, label = TRUE, label.size = 3, frame = TRUE, frame.type='t')

