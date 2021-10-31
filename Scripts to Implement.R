rm(list=ls());
data <- read.csv("heart_failure_clinical_records_dataset.csv")
summary(data)
nrow(data)
ncol(data)
colnames(data)
sapply(data, class)

#since a couple of variables are stored in terms of 0 and 1, they are incorrectly interpreted as numeric/integer instead of factor values
#therefore, we can transform them into more adequate types using "transform"

data <- transform(
  data,
  age=as.integer(age),
  sex=as.factor(sex),
  anaemia=as.factor(anaemia),
  diabetes=as.factor(diabetes),
  high_blood_pressure=as.factor(high_blood_pressure),
  smoking=as.factor(smoking),
  DEATH_EVENT=as.factor(DEATH_EVENT)
)
sapply(data, class)
summary(data$DEATH_EVENT)
##########################################################
#
# Write a function to split a dataset
#
##########################################################

# split the data in two sets of p*N size and (1-p)*N size
# groups is the label of the groups to classify
# variables is the table of variables (must have the same N rows as the length of groups)
# p is the fraction of one of the 
# returns a list with two data.frames, one for training and another for replica
split.data <- function(groups, variables, p)
{
  #YOUR CODE HERE
  dt = sort(sample(nrow(data), nrow(data)*.7)) #p being 0.7
  train<-data[dt,] #70% of data used for training
  test<-data[-dt,] #30% of data used for testing
}
summary(test)
nrow(train)
nrow(test)

##########################################################
#
# Gain index
#
##########################################################
colSums(is.na(data)) #no missing values in the data
summary(data)

entropy <- function(category)
{
  category <- as.factor(category);
  count <- table(category)/length(category);
  count[count==0] <- 1; # why in the name of God/Darwin/Lao Tse/(choose your favorite one) is this one????
  return(-sum(count*log(count)));
}

# There is an error in this function. FIND ITTTTTTT!!!!!!!!
# to fix the error, we had to replace "category" by "features"
gain.index <- function(response, features)
{
  features <- as.factor(features);
  response <- as.factor(response);
  total.entropy.response <- entropy(response);
  gain <- total.entropy.response;
  labs <- levels(features);
  for(l in 1:length(labs))
  {
    gain <- gain + entropy(response[features==labs[l]]);
  }
  return(gain);
}

# Pipeline to run decision tree with rpart

library(rpart) #load the library

#apply function

mytree <- rpart(
  DEATH_EVENT ~ ., 
  data = train, 
  method = "class"
)

install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(mytree)
plot(mytree)

# Test the new model on new and unseen Data for reproducibility
pred = predict(mytree, newdata=test)
#draw_confusion_matrix(cm_under)
test_roc <- function(model, data) {
  roc(data$DEATH_EVENT, predict(model, data, type = "prob")[, "1"])
}
mytree %>% test_roc(data = test) %>% auc() ##AUC=0.81
###########################################################
#
# Random Forest
#
###########################################################

my.variable.bootstrap <- function(input.matrix, m)
{
  return(input.matrix[,sample(ncol(input.matrix),m)]);
}

my.random.forest <- function(group, input.matrix, B, m)
{
  l <- vector("list",B);
  for(i in 1:B)
  {
    samp <- sample(nrow(input.matrix),nrow(input.matrix),replace=T);
    new.input.matrix <- input.matrix[samp,];
    new.group <- group[samp];
    samp.var <- sample(ncol(input.matrix),m);
    new.input.matrix <- new.input.matrix[, samp.var];
    my.frame <- data.frame(new.group, new.input.matrix);
    formula <- as.formula(paste(names(my.frame)[1], paste(names(my.frame)[-1],collapse="+"),sep="~"));
    fit <- rpart(formula, data = my.frame, method = "class");
    l[[i]] <- fit;
  }
  return(l);
}

#Pipeline to run a random forest with randomForest
library(randomForest) #load library
#apply function
#set Death_EVENT as the predictor
# this is because I want to predict the chance of having a death event
#use the rest of the data to predict
rf <- randomForest(
  DEATH_EVENT ~ .,
  data=train
)
varImp(rf) #variable contribution
varImpPlot(rf)
plot(rf)

results <- data.frame(Sensitivity = rep(NA,10),Specificity = rep(NA,10),Score = rep(NA, 10),AUC = rep(NA, 10))
# Test the new model on new and unseen Data for reproducibility
pred = predict(rf, newdata=test)
cm_under<-confusionMatrix(pred,test$DEATH_EVENT,positive="1")
#draw_confusion_matrix(cm_under)
test_roc <- function(model, data) {
  roc(data$DEATH_EVENT, predict(model, data, type = "prob")[, "1"])
}
rf %>% test_roc(data = test) %>% auc()

results$Sensitivity<-cm_under$byClass["Sensitivity"]
results$Specificity<-cm_under$byClass["Specificity"]
results$Score<-(cm_under$byClass["Sensitivity"]+cm_under$byClass["Specificity"])/2
results$AUC<-rf %>% test_roc(data = test) %>% auc()


mean(results$AUC)
mean(results$Sensitivity)
mean(results$Specificity)
mean(results$Score)

sd(results$AUC)
sd(results$Sensitivity)
sd(results$Specificity)
sd(results$Score)
