# MICB 301 Data Science
# - Final Project -
# Name: Tracy Wang
# Student Number: 37637162

#LOADING PACKAGES (assuming package is already installed)
#Loading vegan & ggplot2 package 
library(vegan)
library(ggplot2)

#Reading and Importing 'Saanich_all2.txt' table into R, saving table as R object under 'shannon.table'
shannon.table <- read.table(file="Saanich_all2.txt", header=TRUE, row.names=1, sep="\t")
#Check imported table
shannon.table

#Reformat OTU data & performing linear regression bet. OTU0001 and Methane
#Reformat OTU data by making otu.data from shannon.table's column Otu0001
otu.data = as.data.frame(shannon.table$Otu0001)
#Rename column to OTU0001
colnames(otu.data) = "OTU0001"
#Check numbers to add to CH4 variable:
shannon.table$CH4_nM
#Add CH4 variable to otu.data table
otu.data$CH4_group = c("1030.478","3.231","3.463","4.815", "8.323","23.831","774.034")
#Changing CH4_groups strings to numeric values
otu.data$CH4_group = as.numeric(otu.data$CH4_group)
#confirm final table
otu.data
#Run linear regression bet. OTU0001 and CH4 & making points blue shape 17 & labeling axes
ggplot(data=otu.data, aes(x=CH4_group, y=OTU0001)) +
  geom_point(color="blue", shape=17) +
  geom_smooth(method='lm', color="black", formula = y ~ x) +
  labs(x="Methane (nM)", y="OTU0001") +
  theme_classic()
#Fitting the linear model & estimating p-value
summary(lm(OTU0001 ~ CH4_group, data=otu.data))


#Reformat diversity data & performing linear regression bet. Diversity and Methane
#Reformat shannon data by using shannon.table's column shannon
shannon.data = as.data.frame(shannon.table$shannon)
#Rename column to shannon
colnames(shannon.data) = "shannon"
#Check numbers to add to CH4 variable:
shannon.table$CH4_nM
#Add CH4 variable to shannon.data table
shannon.data$CH4_group = c("1030.478","3.231","3.463","4.815", "8.323","23.831","774.034")
#Changing CH4_groups strings to numeric values
shannon.data$CH4_group = as.numeric(shannon.data$CH4_group)
#confirm final table
shannon.data
#Running linear regression bet. shannon diveristy and CH4 & estimating p-value, making points red shape 17 & labeling axes
ggplot(data=shannon.data, aes(x=shannon, y=CH4_group)) +
  geom_point(color="red", shape=17) +
  geom_smooth(method='lm', color="black", formula = y ~ x) +
  labs(x="Shannon's diversity", y="Methane (nM)") +
  theme_classic()
#Fitting the linear model & estimating p-value
summary(lm(CH4_group ~ shannon, data=shannon.data))
