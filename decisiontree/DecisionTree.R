# Decision tree and Random forest
#install.packages('rpart')
#install.packages('rpart.plot')
#install.packages('randomForest')
#install.packages('caTools')
library(rpart)
library(rpart.plot)
library(randomForest)
library(caTools)
library(dplyr)

# using kyphosis dataset
#print(head(kyphosis))

# building decision tree model
tree <- rpart(Kyphosis ~ ., method = 'class', data = kyphosis)

#printcp(tree)
#plotcp(tree)
#print(tree)
plot(tree, uniform = T, main = 'Kyphosis Tree')
text(tree, use.n = T, all = T)

# plot decision tree by rpart.plot library
prp(tree)


### building random forest model 
rf.model <- randomForest(Kyphosis ~ ., data = kyphosis)

print(rf.model)


####### TREE METHODS PROJECT
## using College dataset in ISLR package
df <- College

# scatterplot of Grad.Rate versus Room.Board, colored by the Private column.
#pl <- ggplot(df, aes(Grad.Rate, Room.Board))+geom_point(aes(color = Private))+theme_bw()
#print(pl)

# histogram of full time undergrad students, color by Private.
#pl2 <- ggplot(df,aes(F.Undergrad)) + geom_histogram(aes(fill=Private),color='black',bins=50)
#print(pl2)

#Create a histogram of Grad.Rate colored by Private. You should see something odd here.
#pl3 <- ggplot(df, aes(Grad.Rate)) + geom_histogram(aes(fill=Private))
#print(pl3)

#sample <- subset(df, Grad.Rate > 100)
#print(sample)
# correct the anomaly 
df['Cazenovia College','Grad.Rate'] <- 100

# Train/Test split
set.seed(101)
split <- sample.split(df,SplitRatio = 0.7)
df.train <- subset(df , split == TRUE)
df.test <- subset(df, split == FALSE)

# Decision tree model
tree <- rpart(Private ~ ., method = 'class', data = df.train)
prp(tree)

# Predict 
predicted.schools <- predict(tree,df.test)
pred <- as.data.frame(predicted.schools)
pred$Private <- pred[,1]

for (i in 1:259){
  if(pred$Private [i] > 0.5){
    pred$Private [i] <- c('NO')
  } else {
    pred$Private [i] <- c('YES')
  }
}

# confusion matrix
confmat <- table(df.test$Private,pred$Private)
print(confmat)

# random forest model




