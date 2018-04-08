# KNN project
install.packages('ISLR')
library(ISLR)  # contains Caravan dataset
library(ggplot2)
library(class)   # has KNN model


#str(Caravan)

# check for missing data
any(is.na(Caravan))

# put purchase into different dataframe
purchase <- Caravan$Purchase

# check the variance of different columns
print(var(Caravan[,1]))   # 165.03
print(var(Caravan[,2]))   # 0.1647  
# because of large gap between variance of different column,
#it's better to standardize it first
# standardize different columns as they have different scale
standardized.Caravan <- scale(Caravan[,-86])   # scale all except purchase column [86]
print(var(standardized.Caravan[,1]))   # 1
print(var(standardized.Caravan[,2]))   # 1 

# Train/Test split
# test data
test.index <- 1:1000
test.data <- standardized.Caravan[test.index,]
test.purchase <- purchase[test.index]
# train data
train.data <- standardized.Caravan[-test.index,]
train.purchase <- purchase[-test.index]

### KNN model

set.seed(101)
predicted.purchase <- knn(train.data,test.data,train.purchase,k=1)
misclass.error <- mean(test.purchase != predicted.purchase)

# print misclassification error
print(misclass.error)

#### choosing a K-value
predicted.purchase <- NULL
error.rate <- NULL

for (i in 1:20) {
  predicted.purchase <- knn(train.data,test.data,train.purchase, k=i)
  error.rate[i] <- mean(test.purchase != predicted.purchase)
}

k.values <- 1:20
error.df <- data.frame(k.values, error.rate)

### visualize Elbow criterion plot 
pl <- ggplot(error.df,aes(k.values,error.rate))+geom_point() + geom_line(lty='dotted', color = 'red') + theme_bw()
print(pl)



