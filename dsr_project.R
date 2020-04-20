# Import libraries
#for data wrangling
library(tidyverse) 
library(forcats)
library(stringr)
library(caTools)
#for data visualisation
library(DT)
library(data.table)
library(pander)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(corrplot)
library(VIM) 
library(knitr)
library(vcd)
library(caret)

#Machine learning
library(xgboost)
library(MLmetrics)
library('randomForest') 
library('rpart')
library('rpart.plot')
library('car')
library('e1071')
library(vcd)
library(ROCR)
library(pROC)
library(VIM)
library(glmnet)

#Set directory of dataset
setwd("C:/Users/Abhilash/Documents/DSR_project")

#Import the dataset
train <- read.csv("train.csv",na.strings = c(""))
test <- read.csv("test.csv",na.strings = c(""))
head(train)
head(test)

#Creating a new dataset by combining test and train
train$set <- "train"
test$set  <- "test"
test$Survived <- NA #Adding Survived column in test
full <- rbind(train, test)
head(full)

#EXPLORATORY DATA ANALYSIS

#Check how data is scattered
str(full)
#Check the dimensions of the dataset
dim(full)
#How many unique values are there in dataset
lapply(full,function(x)length(unique(x)))

#Check for missing values
missing_values <- full %>% summarise_all(funs(sum(is.na(.))/n()))
missing_values <- gather(missing_values,key="feature",value="missing_pct")
missing_values

#Plot the values
missing_values %>%
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() + theme_bw()

#Data quality function for missing values
checkColumn = function(df,colname){
  testData = df[[colname]]
  numMissing = max(sum(is.na(testData)|is.nan(testData)|testData==''),0)
  if (class(testData) == 'numeric' | class(testData) == 'Date' | class(testData) == 'difftime' | class(testData) == 'integer'){
    list('col' = colname,'class' = class(testData), 'num' = length(testData) - numMissing, 'numMissing' = numMissing, 'numInfinite' = sum(is.infinite(testData)), 'avgVal' = mean(testData,na.rm=TRUE), 'minVal' = round(min(testData,na.rm = TRUE)), 'maxVal' = round(max(testData,na.rm = TRUE)))
  } else{
    list('col' = colname,'class' = class(testData), 'num' = length(testData) - numMissing, 'numMissing' = numMissing, 'numInfinite' = NA,  'avgVal' = NA, 'minVal' = NA, 'maxVal' = NA)
  }
}

checkAllCols = function(df){
  resDF = data.frame()
  for (colName in names(df)){
    resDF = rbind(resDF,as.data.frame(checkColumn(df=df,colname=colName)))
  }
  resDF
}

datatable(checkAllCols(full), style="bootstrap", class="table-condensed", 
          options = list(dom = 'tp',scrollX = TRUE))

miss_pct <- map_dbl(full, function(x) { round((sum(is.na(x)) / length(x)) * 100, 1) })

miss_pct <- miss_pct[miss_pct > 0]

data.frame(miss=miss_pct, var=names(miss_pct), row.names=NULL) %>%
  ggplot(aes(x=reorder(var, -miss), y=miss)) + 
  geom_bar(stat='identity', fill='red') +
  labs(x='', y='% missing', title='Percent missing data by feature') +
  theme(axis.text.x=element_text(angle=90, hjust=1))

#Replace missing age with mean age of all passengers
full <- full %>%
  mutate(
    Age = ifelse(is.na(Age), mean(full$Age, na.rm = TRUE), Age),
    `Age Group` = case_when(Age<13 ~ "Age.0012",
                            Age>=13 & Age<18 ~ "Age.1317",
                            Age>=18 & Age<60 ~ "Age.1859",
                            Age>=60 ~ "Age.600v"))

#Replacing NA with most common code in Embarked
full$Embarked <- replace(full$Embarked, which(is.na(full$Embarked)),'$')

#Extract the titles of the passengers
names <- full$Name
title <- gsub("^.*, (.*?)\\..*$", "\\1",names)

full$title <- title

table(title)

#Grouping the titles together
full$title[full$title == 'Mlle'] <- 'Miss' 
full$title[full$title == 'Ms'] <- 'Miss'
full$title[full$title == 'Mme'] <- 'Mrs' 
full$title[full$title == 'Lady'] <- 'Miss'
full$title[full$title == 'Dona'] <- 'Miss'

full$title[full$title == 'Capt'] <- 'Officer' 
full$title[full$title == 'Col'] <- 'Officer' 
full$title[full$title == 'Major'] <- 'Officer'
full$title[full$title == 'Dr'] <- 'Officer'
full$title[full$title == 'Rev'] <- 'Officer'
full$title[full$title == 'Don'] <- 'Officer'
full$title[full$title == 'Sir'] <- 'Officer'
full$title[full$title == 'the Countess'] <- 'Officer'
full$title[full$title == 'Jonkheer'] <- 'Officer'

#Grouping Family into different bins
full$FamilySize <-full$SibSp + full$Parch + 1
full$FamilySized[full$FamilySize == 1] <- 'Single' 
full$FamilySized[full$FamilySize < 5 & full$FamilySize >= 2] <- 'Small' 
full$FamilySized[full$FamilySize >= 5] <- 'Big'
full$FamilySized=as.factor(full$FamilySized)


#Grouping tickets together
ticket.unique <- rep(0, nrow(full))
tickets <- unique(full$Ticket)

for (i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(full$Ticket == current.ticket)
  
  for (k in 1:length(party.indexes)) {
    ticket.unique[party.indexes[k]] <- length(party.indexes)
  }
}

full$ticket.unique <- ticket.unique
full$ticket.size[full$ticket.unique == 1]   <- 'Single'
full$ticket.size[full$ticket.unique < 5 & full$ticket.unique>= 2]   <- 'Small'
full$ticket.size[full$ticket.unique >= 5]   <- 'Big'

#Frequency of Yes and No in Survived
full <- full %>%
  mutate(Survived = case_when(Survived==1 ~ "Yes", 
                              Survived==0 ~ "No"))

crude_summary <- full %>%
  filter(set=="train") %>%
  select(PassengerId, Survived) %>%
  group_by(Survived) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

crude_survrate <- crude_summary$freq[crude_summary$Survived=="Yes"]
kable(crude_summary, caption="2x2 Contingency Table on Survival.", format="markdown")

##EXPLORATORY DATA ANALYSIS

#Relationship to SURVIVAL RATE
#PClass
ggplot(full %>% filter(set=="train"), aes(Pclass, fill=Survived)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  ylab("Survival Rate") +
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  ggtitle("Survival Rate by Class") + 
  theme_minimal()

#Sex
ggplot(full %>% filter(set=="train"), aes(Sex, fill=Survived)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  ylab("Survival Rate") +
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  ggtitle("Survival Rate by Sex") + 
  theme_minimal()

#Age
tbl_age <- full %>%
  filter(set=="train") %>%
  select(Age, Survived) %>%
  group_by(Survived) %>%
  summarise(mean.age = mean(Age, na.rm=TRUE))

ggplot(full %>% filter(set=="train"), aes(Age, fill=Survived)) +
  geom_histogram(aes(y=..density..), alpha=0.5) +
  geom_density(alpha=.2, aes(colour=Survived)) +
  geom_vline(data=tbl_age, aes(xintercept=mean.age, colour=Survived), lty=2, size=1) +
  scale_fill_brewer(palette="Set1") +
  scale_colour_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  ylab("Density") +
  ggtitle("Survival Rate by Age") + 
  theme_minimal()

#AgeGroups
ggplot(full %>% filter(set=="train" & !is.na(Age)), aes(`Age Group`, fill=Survived)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  ylab("Survival Rate") +
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  ggtitle("Survival Rate by Age Group") + 
  theme_minimal()

#SibSp
ggplot(full %>% filter(set=="train"), aes(SibSp, fill=Survived)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  ylab("Survival Rate") +
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  ggtitle("Survival Rate by SibSp") + 
  theme_minimal()

#Parch
ggplot(full %>% filter(set=="train"), aes(Parch, fill=Survived)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  ylab("Survival Rate") +
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  ggtitle("Survival Rate by Parch") + 
  theme_minimal()

#Embarked
ggplot(full %>% filter(set=="train"), aes(Embarked, fill=Survived)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  ylab("Survival Rate") +
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  ggtitle("Survival Rate by Embarked") + 
  theme_minimal()

#Title
ggplot(full %>% filter(set=="train") %>% na.omit, aes(title, fill=Survived)) +
  geom_bar(position="fill") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  ylab("Survival Rate") +
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  ggtitle("Survival Rate by Title") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Family
ggplot(full %>% filter(set=="train") %>% na.omit, aes(`FamilySize`, fill=Survived)) +
  geom_bar(position="fill") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  ylab("Survival Rate") +
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  ggtitle("Survival Rate by Family Group") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##RELATIONSHIP BETWEEN VARIABLES

tbl_corr <- full %>%
  filter(set=="train") %>%
  select(-PassengerId, -SibSp, -Parch) %>%
  select_if(is.numeric) %>%
  cor(use="complete.obs") %>%
  corrplot.mixed(tl.cex=0.85)

##APPLYING MACHINE LEARNING TO PREDICT WHO SURVIVES

#Dataset preocessing
feauter1<-full[1:891, c("Pclass", "title","Sex","Embarked","FamilySized","ticket.size")]
response <- as.factor(train$Survived)
feauter1$Survived=as.factor(train$Survived)

#Taking 20% of data for cross validation
set.seed(500)
ind=createDataPartition(feauter1$Survived,times=1,p=0.8,list=FALSE)
train_val=feauter1[ind,]
test_val=feauter1[-ind,]

#Proportion of survival rate in training and testing data
round(prop.table(table(train$Survived)*100),digits = 1)
round(prop.table(table(train_val$Survived)*100),digits = 1)
round(prop.table(table(test_val$Survived)*100),digits = 1)

#USING RANDOM FOREST CLASSIFIER
set.seed(1234)

Model_DT=rpart(Survived~.,data=train_val,method="class")

rpart.plot(Model_DT,extra =  3,fallen.leaves = T)

PRE_TDT=predict(Model_DT,data=train_val,type="class")

confusionMatrix(PRE_TDT,train_val$Survived)

#head(full)