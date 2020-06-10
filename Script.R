##Read Data

library(ISLR)
head(College)
df<-data.frame(College)

#Scatterplot of Grad.Rate versus Room.Board, colored by the Private column
library(ggplot2)
pl<-ggplot(df,aes(Grad.Rate,Room.Board))+geom_point(aes(color=Private ))
print(pl)

##histogram of full time undergrad students, color by Private
pl2<-ggplot(df,aes(F.Undergrad))+geom_histogram(aes(fill=Private),bins = 50,color='black')
print(pl2)

## histogram of Grad.Rate colored by Private
pl3<-ggplot(df,aes(Grad.Rate))+geom_histogram(aes(fill=Private),bins = 50,color='black')
print(pl3)

a<-subset(df,Grad.Rate>100)
print(a)

df['Cazenovia College','Grad.Rate']<-100

##Train Test Split
library(caTools)
set.seed(101)

sample = sample.split(df$Private, SplitRatio = .70)
train = subset(df, sample == TRUE)
test = subset(df, sample == FALSE)

#Model
library(rpart)
tree<-rpart(Private~.,method = 'class',data = train)
tree.preds <- predict(tree,test)

head(tree.preds)

tree.preds<-as.data.frame(tree.preds)

joiner<-function(x)
{
  if (x>=0.5)
  {
    return('YES')
  }
  else
  {
    return('NO')
  }
}

tree.preds$Private<-sapply(tree.preds$Yes,joiner)
head(tree.preds)

table(tree.preds$Private,test$Private)

install.packages('rpart.plot')
library(rpart.plot)
prp(tree)

##Random Forest Model
install.packages('randomForest')
library(randomForest)

rf.model<-randomForest(Private~.,data = train,importance=T)

rf.model$confusion
rf.model$importance

rf.preds<-predict(rf.model,test)
table(rf.preds,test$Private)
