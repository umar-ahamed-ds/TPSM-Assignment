install.packages("readxl")
library(readxl)

data <- read_excel(file.choose())


head(data)
str(data)
summary(data)
names(data)


colSums(is.na(data))



model1 <- lm(final_score ~ study_hours_per_day + attendance_percentage + 
               concept_understanding_score + study_consistency_index + 
               class_participation_score, data = data)

summary(model1)



model2 <- lm(final_score ~ study_hours_per_day + attendance_percentage + 
               concept_understanding_score + study_consistency_index + 
               class_participation_score + last_exam_score + improvement_rate,
             data = data)

summary(model2)

install.packages("caret")
library(caret)


set.seed(123)   # IMPORTANT (for reproducibility)

train_index <- createDataPartition(data$final_score, p = 0.8, list = FALSE)

train_data <- data[train_index, ]
test_data  <- data[-train_index, ]

dim(train_data)
dim(test_data)

model_train <- lm(final_score ~ study_hours_per_day + attendance_percentage + 
                    concept_understanding_score + study_consistency_index + 
                    class_participation_score,
                  data = train_data)

summary(model_train)



predictions <- predict(model_train, newdata = test_data)


head(data.frame(
  Actual = test_data$final_score,
  Predicted = predictions
))

RMSE <- sqrt(mean((test_data$final_score - predictions)^2))
RMSE


MAE <- mean(abs(test_data$final_score - predictions))
MAE



plot(test_data$final_score, predictions,
     xlab = "Actual Final Score",
     ylab = "Predicted Final Score",
     main = "Actual vs Predicted")

abline(0,1, col="red")


predictions <- predict(model_train, newdata = test_data)

RMSE <- sqrt(mean((test_data$final_score - predictions)^2))
MAE  <- mean(abs(test_data$final_score - predictions))

RMSE
MAE
