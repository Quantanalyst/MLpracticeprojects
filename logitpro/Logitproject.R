# Logit project
library(ggplot2)
library(dplyr)
library(Amelia)
library(caTools)

## read the data 
df <- read.csv('adult_sal.csv')
df <- select(df, -X)  # droping the first column

# reduce the factors to the necessary categories
df$type_employer <- as.character(df$type_employer)
cfactor1 <- function(x){
  if(x == 'Never-worked' | x == 'Without-pay' ){
    return(c('Unemployed'))
  } else if (x == 'Local-gov' | x == 'State-gov'){
    return(c('SL-gov'))
  } else if (x == 'Self-emp-inc' | x == 'Self-emp-not-inc'){
    return('self-emp') 
  } else {
    return(x)
  }
}
df$type_employer <- sapply(df$type_employer, cfactor1)
df$type_employer <- as.factor(df$type_employer)

########
df$marital <- as.character(df$marital)
cfactor2 <- function(x){
  if(x == 'Married-AF-spouse' | x == 'Married-civ-spouse' | x == 'Married-spouse-absent'){
    return(c('Married'))
  } else if (x == 'Divorced' | x == 'Separated' | x =='Widowed'){
    return(c('Not-Married'))
  } else if (x == 'Self-emp-inc' | x == 'Self-emp-not-inc'){
    return('Never-Married') 
  } else {
    return(x)
  }
}
df$marital <- sapply(df$marital, cfactor2)
df$marital <- as.factor(df$marital)

########
df$country <- as.character(df$country)
cfactor3 <- function(x){
  if(x == 'Cambodia' | x == 'China' | x == 'India' | x == 'Iran' | x == 'Japan' | x == 'Laos' | x == 'Hong' | x == 'Philippines' | x == 'Taiwan' | x == 'Thailand' | x == 'Vietnam'){
    return(c('Asia'))
  } else if (x == 'Columbia' | x == 'Peru' | x =='Mexico' | x =='Ecuador' | x =='El-Salvador' | x =='Guatemala' | x =='Honduras' | x =='Nicaragua'){
    return(c('Latin.and.South.America'))
  } else if (x == 'England' | x == 'France' | x =='Germany' | x == 'Greece' | x == 'Holand-Netherlands' | x == 'Hungary' | x == 'Poland' | x == 'Portugal' | x == 'Ireland' | x == 'Italy' | x == 'Scotland' | x == 'Yugoslavia'  ){
    return('Europe') 
  } else if (x == 'Canada' | x == 'Cuba' | x =='Dominican-Republic' | x == 'United-States'){
    return('North.America ')
  } else {
    return('Other')
  }
}
df$country <- sapply(df$country, cfactor3)
df$country <- as.factor(df$country)

#####

df[df == '?'] <- NA

## droping the missing values
df <- na.omit(df)

pl <- ggplot(df,aes(age)) + geom_histogram(aes(fill = income),color = 'black', binwidth = 1) + theme_bw()
print(pl)

pl2 <- ggplot(df,aes(hr_per_week)) + geom_histogram(color = 'black', binwidth = 1) + theme_bw()
print(pl2)

# change country column to region column
colnames(df)[colnames(df) == 'country'] <- 'region'

pl3 <- ggplot(df,aes(region))+ geom_bar(aes(fill=income))
pl3 <- pl3 + theme_bw()
print(pl3)

### creating train/test sample by caTools

set.seed(101)
split <- sample.split(df,SplitRatio = 0.7)
data.train <- subset(df, split == TRUE)
data.test <- subset(df, split == FALSE)

### logistic regression model 
model <- glm(income ~ ., family = binomial('logit'), data = data.train)

# use step() for model selection
# step() uses AIC

new.model <- step(model)

# predict of the test data
fitted.prob <- predict(new.model,data.test, type = 'response')

# confusion matrix
confmat <- table(data.test$income,fitted.prob>0.5)

# accuracy of the model
acc <- (confmat[1,1]+confmat[2,2])/sum(confmat)

#print the accuracy
print(acc)