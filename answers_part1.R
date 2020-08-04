#First, we need to load data, since we exported our data to Excel, we will use a library of functions that deals with excel files and sheets

library(openxlsx)       # for reading in excel data

# then, we have to create a summary about students' answers

EducationLevel <- read.xlsx("Answers to questions.xlsx", sheet = 5)
Answers1 <- read.xlsx("Answers to questions.xlsx", sheet = 4)

table1 <- table(EducationLevel$Score)

#View the table "table1" 
table1

#Summarize categorical data and calculate percentages
sum(table1)
prop.table(table1)

#Visualize a bar plot
par(mar=c(11,4,4,4))

barplot(table1, col=rainbow(length(Answers1$Answer)), main = "Summary of Answers", names = Answers1$Answer, font.axis=1, cex.axis=1, las=2)

#Visualize a pie chart
pie(table1, col= rainbow(length(Answers1$Answer)), main = "Summary of Answers", c(Answers1$Answer), font.axis=1, cex.axis=1, las=2)

#summarize data
summary(table1)
HighSchoolers <- EducationLevel[EducationLevel$Score == "4",]

#clean up

library(tidyverse)         #For cleaning table data

CleanList <- HighSchoolers$Name %>% str_replace_all("From", "")
View(CleanList)



