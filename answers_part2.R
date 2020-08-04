#Test your R code here

library(openxlsx)

#load data
ReasonJoin <- read.xlsx("Answers to questions1.xlsx", sheet = 6)
Answers1 <- read.xlsx("Answers to questions1.xlsx", sheet = 7)

#write my first function
ReasonJoin <- data.frame(lapply(ReasonJoin, function(x) {
  gsub("From ", "", x)
}))

#create a table of results
table1 <- table(ReasonJoin$Score1)

sum(table1)
prop.table(table1)

#visualize
par(mar=c(10,4,2,2))

barplot(table1, col=rainbow(length(Answers$Answer)), main="Summary Bar Plot", names=Answers$Answer, font.axis=1, cex.axis=1, las=2)

par(mar=c(2,2,2,2))
pie(table1, col=rainbow(length(Answers$Answer)), main="Summary Pie Chart", c(Answers$Answer))

#View Group of students by Type
library(sjPlot)     #to make nice tables: https://strengejacke.github.io/sjPlot/articles/table_css.html

tab_df(All4)

All4 <- ReasonJoin[ReasonJoin$Score1 == "6",]

tab_df(ReasonJoin, title = "Program Interest", sort.column = 4, alternate.rows = TRUE, CSS = list(css.centeralign = 'text-align: left;', css.firsttablerow = 'font-weight: bold;'))
