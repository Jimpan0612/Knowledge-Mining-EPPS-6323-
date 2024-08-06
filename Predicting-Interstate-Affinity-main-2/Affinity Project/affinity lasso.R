library(caret)
library(glmnet)

# Load the df15 dataset
df15
df15$affinity <- as.numeric(df15$affinity)
df16$affinity <- as.numeric(df16$affinity)
df17$affinity <- as.numeric(df17$affinity)
df18$affinity <- as.numeric(df18$affinity)
df19$affinity <- as.numeric(df19$affinity)

# Split the data into training and testing sets (70% train, 30% test)
set.seed(123)
train_index <- sample(1:nrow(df15), 0.7 * nrow(df15))
train_data <- df15[train_index, ]
test_data <- df15[-train_index, ]

# Set up the training control
train_control <- trainControl(method = "cv", number = 10)

# Train a LASSO regression model using caret
set.seed(123)
model15 <- train(affinity ~ ., data = train_data,
               method = "glmnet",
               trControl = train_control,
               tuneLength = 5,
               preProcess = c("center", "scale"))

print(model15)

# Make predictions on the test data
predictions <- predict(model15, test_data)

# Calculate the RMSE (Root Mean Squared Error) of the model
RMSE <- sqrt(mean((predictions - test_data$affinity)^2))
cat("RMSE2015:", RMSE)
#RMSE2015: 0.9386933


#2016
set.seed(123)
train_index <- sample(1:nrow(df16), 0.7 * nrow(df16))
train_data <- df16[train_index, ]
test_data <- df16[-train_index, ]

# Set up the training control
train_control <- trainControl(method = "cv", number = 10)

# Train a LASSO regression model using caret
set.seed(123)
model16 <- train(affinity ~ ., data = train_data,
               method = "glmnet",
               trControl = train_control,
               tuneLength = 5,
               preProcess = c("center", "scale"))

print(model16)

# Make predictions on the test data
predictions <- predict(model16, test_data)

# Calculate the RMSE (Root Mean Squared Error) of the model
RMSE <- sqrt(mean((predictions - test_data$affinity)^2))
cat("RMSE2016:", RMSE)
#RMSE2016: 0.8524418

#2017
set.seed(123)
train_index <- sample(1:nrow(df17), 0.7 * nrow(df17))
train_data <- df17[train_index, ]
test_data <- df17[-train_index, ]

# Set up the training control
train_control <- trainControl(method = "cv", number = 10)

# Train a LASSO regression model using caret
set.seed(123)
model17 <- train(affinity ~ ., data = train_data,
               method = "glmnet",
               trControl = train_control,
               tuneLength = 5,
               preProcess = c("center", "scale"))

print(model17)

# Make predictions on the test data
predictions <- predict(model17, test_data)

# Calculate the RMSE (Root Mean Squared Error) of the model
RMSE <- sqrt(mean((predictions - test_data$affinity)^2))
cat("RMSE2017:", RMSE)
#RMSE2017: 1.090398

#2018
set.seed(123)
train_index <- sample(1:nrow(df18), 0.7 * nrow(df18))
train_data <- df18[train_index, ]
test_data <- df18[-train_index, ]

# Set up the training control
train_control <- trainControl(method = "cv", number = 10)

# Train a LASSO regression model using caret
set.seed(123)
model18 <- train(affinity ~ ., data = train_data,
               method = "glmnet",
               trControl = train_control,
               tuneLength = 5,
               preProcess = c("center", "scale"))

print(model18)

# Make predictions on the test data
predictions <- predict(model18, test_data)

# Calculate the RMSE (Root Mean Squared Error) of the model
RMSE <- sqrt(mean((predictions - test_data$affinity)^2))
cat("RMSE2018:", RMSE)
#RMSE2018: 0.8387639

#2019
set.seed(123)
train_index <- sample(1:nrow(df19), 0.7 * nrow(df19))
train_data <- df19[train_index, ]
test_data <- df19[-train_index, ]

# Set up the training control
train_control <- trainControl(method = "cv", number = 10)

# Train a LASSO regression model using caret
set.seed(123)
model19 <- train(affinity ~ ., data = train_data,
               method = "glmnet",
               trControl = train_control,
               tuneLength = 5,
               preProcess = c("center", "scale"))

print(model19)

# Make predictions on the test data
predictions <- predict(model19, test_data)

# Calculate the RMSE (Root Mean Squared Error) of the model
RMSE <- sqrt(mean((predictions - test_data$affinity)^2))
cat("RMSE2019:", RMSE)
#RMSE2019: 1.0335


library(ggplot2)


# Create a data frame with the RMSE for each year
rmse_data <- data.frame(year = c(2015, 2016, 2017, 2018, 2019),
                        rmse = c(0.9386933, 0.8524418, 1.090398, 0.8387639, 1.0335))

# Plot the RMSE for each year
ggplot(rmse_data, aes(x = year, y = rmse)) +
  geom_point(size = 2, color = "blue") +
  geom_errorbar(aes(ymin = rmse - sd(rmse), ymax = rmse + sd(rmse)), 
                width = 0.2, color = "blue") +
  geom_line(color = "orange") +
  scale_y_continuous(limits = c(0.6, 1.3)) +
  labs(title = "RMSE of LASSO Regression Models", 
       x = "Year", y = "RMSE") +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"), 
        panel.grid.major = element_line(color = "white"))
