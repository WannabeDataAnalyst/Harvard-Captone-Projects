# read train labels for customers who defaulted and did not default
tl <- ("C:/Users/YOGA12/Desktop/R/rstudio/projects/Harvard Capstone Project/train_labels.csv")

# reading 5 000 lines from train_data to use as train data
train <- read.csv("train_data.csv", nrows = 5000)

# save train data
write.csv(train,"C:/Users/YOGA12/Desktop/R/rstudio/projects/Harvard Capstone Project/CYN_train_data.csv", row.names = FALSE)

# Add column names
colnames(train) <- c("customer_ID","S_2", "P_2", "D_39", "B_1", "B_2", "R_1", "S_3", "D_41", "B_3")

#delete unused columns to predict on the first 10 predictors
train <- subset(train, select = c("customer_ID","S_2", "P_2", "D_39", "B_1","B_2", "R_1", "S_3", "D_41", "B_3"))

# join target data to train
train <- train %>%
  left_join(tl, by = "customer_ID")

# Used this file for the RMD file instead of join above 
write.csv(train,"C:/Users/YOGA12/Desktop/R/rstudio/projects/Harvard Capstone Project/train1.csv", row.names = FALSE)

#transform to date
train[,2] <-as.Date(train[,2])


#transform to factor and numeric
train[,3] <- as.numeric(train[,3])
train[,4] <- as.numeric(train[,4])
train[,5] <- as.numeric(train[,5])
train[,6] <- as.numeric(train[,6])
train[,7] <- as.numeric(train[,7])
train[,8] <- as.numeric(train[,8])
train[,9] <- as.numeric(train[,9])
train[,10] <- as.numeric(train[,10])
train[,11] <- as.factor(train[,11])


#Check that "target" is a factor
class(train$target)


#Check for NAs
no_nas <- ifelse(is.na(train), 0, train) 
sum(is.na(train))

# remove rows with NA's to only predict on clients when there all predictors are available
train <- train[complete.cases(train),]

# number of unique customers
unique_customers <- train%>%
  group_by(customer_ID)%>%
  summarize(n=n())%>%
  arrange(desc(n))%>%
  count()
unique_customers


# number of statement dates per client
train%>%
  group_by(customer_ID)%>%
  summarize(n=n())%>%
  ggplot(aes(n))+
  geom_histogram(bins = 30, color = "black")


# reading 5000 lines from train_data to use as test data
test <- read.csv("train_data.csv", nrows = 5000, skip = 20000)

#save train data
write.csv(test,"C:/Users/YOGA12/Desktop/R/rstudio/projects/Harvard Capstone Project/test_data.csv", row.names = FALSE)

# Add column names
colnames(test) <- c("customer_ID","S_2", "P_2", "D_39", "B_1", "B_2", "R_1", "S_3", "D_41", "B_3")


#delete unused columns
test <- subset(test, select = c("customer_ID","S_2", "P_2", "D_39", "B_1","B_2", "R_1", "S_3", "D_41", "B_3"))


# join target data to test data
test <- test %>%
  left_join(tl, by = "customer_ID")


# Used this file for the RMD file instead of join above 
write.csv(test,"C:/Users/YOGA12/Desktop/R/rstudio/projects/Harvard Capstone Project/test1.csv", row.names = FALSE)


#transform to date
test[,2] <-as.Date(test[,2])


#transform to factor and numeric
test[,3] <- as.numeric(test[,3])
test[,4] <- as.numeric(test[,4])
test[,5] <- as.numeric(test[,5])
test[,6] <- as.numeric(test[,6])
test[,7] <- as.numeric(test[,7])
test[,8] <- as.numeric(test[,8])
test[,9] <- as.numeric(test[,9])
test[,10] <- as.numeric(test[,10])
test[,11] <- as.factor(test[,11])


#Check that "target" is a factor
class(test$target)


#Check for NAs
no_nas <- ifelse(is.na(test), 0, test) 
sum(is.na(test))

# remove rows with NA's to only predict on clients when there all predictors are available
test <- test[complete.cases(test),]

# the dimensions of the traina an test sets
dim(train)
dim(test)

# Create an ensemble

set.seed(1, sample.kind = "Rounding") 

models <- c("glm", "lda", "nb", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf")

fit_models <- lapply(models, function(model){ 
  print(model)
  train(target ~ P_2+D_39+B_1+B_2+R_1+S_3+D_41+B_3, method = model, data = train)
}) 

names(fit_models) <- models

# Use ensemble to predict
predictions <- sapply(fit_models, function(x) 
  predict(x, newdata = test))
dim(predictions)

# Check accuracy of predicitions for each model and the mean accuracy across all models
accuracy <- colMeans(predictions == test$target)
accuracy
mean(accuracy)

# Check the accuracy of the ensemble
Model_Answers <- rowMeans(predictions == "1")
y_hat <- ifelse(Model_Answers > 0.5, "1", "0")
mean(y_hat == test$target)


