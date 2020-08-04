#Test your R code here

library(openxlsx)

#load data
ReasonJoin <- read.xlsx('Answers to questions1.xlsx', sheet = 6)
Answers <- read.xlsx('Answers to questions1.xlsx', sheet = 7)

table1 <- table(ReasonJoin$Score1)

sum(table1)
prop.table(table1)

#visualize
par(mar=c(10,4,2,2))

barplot(table1, col=rainbow(length(Answers$Answer)), main="Summary Bar Plot", names=Answers$Answer, font.axis=1, cex.axis=1, las=2)

par(mar=c(2,2,2,2))
pie(table1, col=rainbow(length(Answers$Answer)), main="Summary Pie Chart", c(Answers$Answer))

library(tidyverse)

All4 <- ReasonJoin[ReasonJoin$Score1 == "6",]
View(All4)

CleanList <- All4$Name %>% str_replace_all("From", "")
View(CleanList)