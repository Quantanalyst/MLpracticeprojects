## KNN project with IRIS dataset
#install.packages('ISLR')
library(ISLR)
library(class)
library(caTools)
library(ggplot2)

# check missing data 
any(is.na(iris))

#standardize the data
standardized.iris <- scale(iris[,-5])  # remove the species column

# add species to a new column 
species <- iris[,5] 

## join species and scaled data
standardized.iris <- cbind(standardized.iris,iris[5])


# split train/test sample 
set.seed(101)
split <- sample.split(standardized.iris, SplitRatio = 0.7)
iris.train <- subset(standardized.iris, split == TRUE)
iris.test <- subset(standardized.iris, split == FALSE)
iris.train <- iris.train[,-5]
iris.test <- iris.test [,-5]
species.train <- subset(species, split == TRUE)
species.test <- subset(species, split == FALSE)

#predicted.species <- knn(iris.train,iris.test,species.train, k=1)
#misclass.error <- mean(species.test != predicted.species)

### Elbow criterion 
#### choosing a K-value
predicted.species <- NULL
error.rate <- NULL

for (i in 1:10) {
  predicted.species <- knn(iris.train,iris.test,species.train, k=i)
  error.rate[i] <- mean(species.test != predicted.species)
}

k.values <- 1:10
error.df <- data.frame(k.values, error.rate)

### visualize Elbow criterion plot 
pl <- ggplot(error.df,aes(k.values,error.rate))+geom_point() + geom_line(lty='dotted', color = 'red') + theme_bw()
print(pl)








