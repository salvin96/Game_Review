install.packages(c("rpart", "rpart.plot", "caret", "e1071", "randomForest"))
library("rpart")
library("e1071")
library("dplyr")
library("caret")
library("rpart.plot")
library("randomForest")
data <- read.csv("video_game_reviews.csv")

# Check the structure of the dataset
str(data)

data$Popularity <- ifelse(data$User.Rating >= 35.1, "Popular", "Unpopular")

# Convert relevant columns to factors
data$Popularity <- as.factor(data$Popularity)
#data$Genre <- as.factor(data$Genre)
#data$Platform <- as.factor(data$Platform)
#data$Story.Quality <- factor(data$Story.Quality,
#              levels = c("Poor", "Average", "Good", "Excellent"),
#              ordered = TRUE)
#data$Soundtrack.Quality<- factor(data$Soundtrack.Quality,
#                             levels = c("Poor", "Average", "Good", "Excellent"),
#                             ordered = TRUE)


data$User.Rating<-as.numeric(data$User.Rating)
#data$Story.Quality <- as.numeric(data$Story.Quality)
#data$Soundtrack.Quality <- as.numeric(data$Soundtrack.Quality)
#data$Genre <-as.numeric(data$Genre)
#data$Platform <- as.numeric(data$Platform)
#data$Story.Quality <- as.numeric(data$Story.Quality)

View(data)
#spliting the data into training 70% and testing 30%
library(caret)
set.seed(123)
train_index <- createDataPartition(data$Popularity, p=0.7, list=FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

message(
  "Initial: ", nrow(data), " rows.\n",
  "Train: ", nrow(train_data), " rows  \n",
  "Test: ", nrow(test_data), " rows ."
)




# Build the Decision Tree model
dt_model <- rpart(Popularity ~ Price + Game.Length..Hours.,
                  data = train_data,
                  method = "class")

# Summary of the model
summary(dt_model)

# Predict on the test data
dt_predictions <- predict(dt_model, newdata = test_data, type = "class")

# Visualize the Decision Tree
rpart.plot(dt_model)


# Confusion matrix to evaluate the model
confusionMatrix(dt_predictions, test_data$Popularity)








# Train the SVM model
svm_model <- svm(Popularity ~ Price + Game.Length..Hours.,
                 data = train_data,
                 type = "C-classification",
                 kernel = "linear")

# Summary of the model
summary(svm_model)

# Predict on the test data
svm_predictions <- predict(svm_model, newdata = test_data)

# View the predictions
print(predictions)

# Confusion matrix to evaluate the SVM model
confusionMatrix(svm_predictions, test_data$Popularity)





#regression
set.seed(123)
train_index <- createDataPartition(data$User.Rating, p=0.7, list=FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

message(
  "Initial: ", nrow(data), " rows.\n",
  "Train: ", nrow(train_data), " rows  \n",
  "Test: ", nrow(test_data), " rows ."
)

train_data$User.Rating <- as.numeric(train_data$User.Rating)
test_data$User.Rating <- as.numeric(test_data$User.Rating)


# Train the Random Forest model
rf_model <- randomForest(User.Rating ~ Price + Game.Length..Hours.,
                         data = train_data,
                         importance = TRUE,
                         ntree = 50)

# Print the model summary
print(rf_model)

# Make predictions on the test data
rf_predictions <- predict(rf_model, newdata = test_data)

# Calculate RMSE and R-Square
rmse <- sqrt(mean((rf_predictions - test_data$User.Rating)^2))
mae <- mean(abs(rf_predictions - test_data$User.Rating))
rsq <- cor(rf_predictions, test_data$User.Rating)^2
print(paste("R-squared:", rsq))
print(paste("Root Mean Squared Error (RMSE):", rmse))
print(paste("Root Mean Squared Error (RMSE):", mae))


# Show first 35 actual and predicted data
rf <- data.frame(y_test = test_data$User.Rating, y_pred = round(rf_predictions,1))
subset_rf<- rf[1:35, ]
print(subset_rf)

# Visualize prediction using plot
par(mar = c(5, 4, 4, 2))
plot(subset_rf$y_test, type = "l", col = "black", lwd = 2, xlab = "Index", ylab = "Value")
lines(subset_rf$y_pred, col = "blue", lwd = 2)
legend("topright", legend = c("Actual", "Predicted"), col = c("black", "blue"), lwd = 2)
title(main="Actual vs Predicted for Random Forest Regressor Model")






#Linear Regression
# Fit the linear regression model
lr_model <- lm(User.Rating ~ Price + Game.Length..Hours.,
            data = train_data)

# Summarize the model
summary(lr_model)

# Make predictions on the test set
lr_predictions <- predict(lr_model, newdata = test_data)

# Calculate RMSE and R-Square
rmse_lr <- sqrt(mean((lr_predictions - test_data$User.Rating)^2))
rsq_lr <- cor(lr_predictions, test_data$User.Rating)^2
mae_lr <- mean(abs(lr_predictions - test_data$User.Rating))
print(paste("Mean Absolute Error (MAE):", mae_lr))
print(paste("R-squared:", rsq_lr))
print(paste("Root Mean Squared Error (RMSE):", rmse_lr))

# Show first 35 actual and predicted data
lr <- data.frame(y_test = test_data$User.Rating, y_pred = round(lr_predictions,1))
subset_lr<- lr[1:35, ]
print(subset_lr)

# Visualize prediction using plot
par(mar = c(5, 4, 4, 2))
plot(subset_lr$y_test, type = "l", col = "black", lwd = 2, xlab = "Index", ylab = "Value")
lines(subset_lr$y_pred, col = "blue", lwd = 2)
legend("topright", legend = c("Actual", "Predicted"), col = c("black", "blue"), lwd = 2)
title(main="Actual vs Predicted for Linear Regression Model")



#compare model performance
evalution_table<-data.frame(
  Model = c('Random Forest', 'Linear Regression'),
  R_squared = c(rsq,rsq_lr),
  RMSE = c(rmse,rmse_lr),
  MAE = c(mae,mae_lr)
)
print(evalution_table)








