
#project-prediction Students percentage based on study hours


#packges needed for project
library(ggplot2)

library(tidyr)


library(dplyr)
library(readr)



library(e1071)
library(tidyverse) # metapackage with lots of helpful functions
list.files(path = "../input")

library(gridExtra)
library(RColorBrewer)
library(scales)
options(warn=-1)
library(rpart)
library(rpart.plot)
library(party)

library(caret)

#  read csv of student dataset  
df2=read.csv("C:/Users/thora/Downloads/archive/student-mat.csv")

#size of dataset
head(df2)
nrow(df2)
ncol(df2)
dim(df2)

#Attributes present in dataset
a=attributes(df2)
a

#Type of Attributes
str(df2)

#summery
summary(df2)

#new dataset for calculating average of marks
dm<-data.frame(df2$G1,df2$G2,df2$G3)
#score is a average of marks  
scores=data.frame(rowMeans(dm))
# we will combine scores with out dataset
df2=cbind.data.frame(df2,scores)

#After combining structure of dataset
str(df2)



#some Graphs for visualization
#1 student age vs count of student
table(df2$age)
var(df2$age)
age= ggplot(aes(x=age), data=df2)+
  geom_histogram(binwidth = 0.50, fill='darkred', color='black')+
  ggtitle("Age of students")
age

#2 count of student and study time
table(df2$studytime)
studytime= ggplot(aes(x=studytime), data=df2)+geom_histogram(binwidth = 0.50, fill='darkred', color='white')+
  ggtitle("study time of students")
studytime


#3 gender vs  count 
table(df2$Gender)
Gen= ggplot(data=df2,aes(x=Gender,fill=Gender))+geom_bar()
Gen

#4 health of Students
table(df2$health)
health= ggplot(data=df2,aes(x=health,fill=gender))+geom_histogram(binwidth=0.5, fill='red')
health

#5 area of living
area= ggplot(df2, aes(x=address)) +
  geom_bar(fill='orchid')
area
hist(df2$age)


#6 gender vise count of age
var(df2$G1)
ages= ggplot(data=df2,aes(x=age, fill=Gender))+geom_histogram(binwidth=0.50)
ages



#7 student with their gender and average marks  vs age 

rowMeans.dm.=ggplot(data=df2,aes(x=age, y=rowMeans.dm., col=Gender, shape=Gender))+
  geom_point()+geom_smooth(method="lm",se=F)+facet_grid(~Gender)
rowMeans.dm.

#8 student with their gender and absense vs average marks
absences= ggplot(data=df2,aes(x=absences, y=rowMeans.dm., col=Gender))+
  geom_point()+geom_smooth(method="lm",se=F)+facet_grid(~Gender)
absences

#9 student with their gender and traveltime and average marks
table(student$traveltime)
travel=ggplot(data=df2,aes(x=traveltime, y=rowMeans.dm., col=Gender))+
  geom_point()+geom_smooth(method="lm",se=F)+facet_grid(~Gender)
travel


#10 mothers job and count
Motherjob= ggplot(df2, aes(x=Mjob)) +
  geom_bar(fill='brown')
Motherjob

#11 fathers job and count
Fotherjob= ggplot(df2, aes(x=Fjob)) +
  geom_bar(fill='blue')
Fotherjob

#12 average marks and job of father and mother
ggplot(data=df2,aes(x=Fjob, y=rowMeans.dm.))+geom_point()+geom_smooth(method="lm",se=F)
ggplot(data=df2,aes(x=Mjob, y=rowMeans.dm.))+geom_point()+geom_smooth(method="lm",se=F)

#13 fathers educaion and their count 
Fedu= ggplot(df2, aes(x=Fedu)) +
  geom_bar(fill='black', color="red")
Fedu

#14 mothers education and their count
Medu= ggplot(df2, aes(x=Medu)) +
  geom_bar(fill='black', color='black')
Medu


#15 edu vs score
my_graph <- ggplot(df2, aes(x = Fedu, y = Medu)) +
  geom_point(aes(color = G1)) +
  stat_smooth(method = "lm",
              col = "red",
              se = FALSE,
              size = 1)
my_graph


#16 family size
table(df2$famsize)
ggplot(df2, aes(x=famsize))+
  geom_bar(fill='salmon',color='black')

#17 family size and grade
ggplot(data=df2,aes(x=famsize, y=rowMeans.dm., col=Gender))+geom_point()+
  geom_smooth(method="lm",se=F)+facet_grid(~Gender)
ggplot(data=df2,aes(x=famsize, y=rowMeans.dm., col=Gender))+geom_point()+
  geom_smooth(method="lm",se=F)+facet_grid(~Gender) -> rowMeans.dm.

#18 family rel and grade
ggplot(data=df2,aes(x=famrel, y=G1, col=Gender))+
  geom_point()+geom_smooth(method="lm",se=F)+facet_grid(~Gender)
ggplot(data=df2,aes(x=famrel, y=G1, col=df2))+geom_point()+
  geom_smooth(method="lm",se=F)+facet_grid(~Gender) -> g1

#19 heatmap
numeric_features <- Filter(is.numeric, df2)


#library for ploting heatmap
library(pheatmap)
pheatmap(cor(numeric_features))

# studytime vs grade
plot(df2$studytime,df2$G3)

#boxplots
boxplot(df2$age)
boxplot(df2$studytime)
boxplot(df2$rowMeans.dm.)


#line graphs
plot(df2$Mjob)

#convert all value into numeric type
df2$age = as.numeric(df2$age)
df2$Fedu = as.numeric(df2$Fedu)
df2$Medu = as.numeric(df2$Medu)
df2$Fjob = as.numeric(df2$Fjob)
df2$Mjob = as.numeric(df2$Mjob)
df2$traveltime = as.numeric(df2$traveltime)
df2$Gender = as.numeric(df2$Gender)
df2$absences = as.numeric(df2$absences)
df2$famsize = as.numeric(df2$famsize)
df2$famrel = as.numeric(df2$famrel)



# prediction of Grade 
set.seed(1234)
partition = createDataPartition(df2[,'rowMeans.dm.'], times=1, p=0.70, list=FALSE)
training=df2[partition,]
dim(training)
test=df2[-partition,]
dim(test)

lin_mod=lm(rowMeans.dm.~ G1+G2+age+Fedu+Medu+absences
          +famrel+traveltime, data=training)

pred_test <- predict(lin_mod, test)
cat("Test MAE:", round(mean(abs(pred_test-test$rowMeans.dm.)),6))
pred_train <-predict(lin_mod, training)
cat("Train MAE:", round(mean(abs(pred_train-training$rowMeans.dm.)),6))
dt_prediction = (data.frame((pred_test), (test$rowMeans.dm.)))
str(dt_prediction)
colnames(dt_prediction) <- c("Predicted rowMeans.dm.","RealrowMeans.dm.")
head(dt_prediction,10)


#Thank you!!! :)

