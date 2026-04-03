# Read the dataset
data <- read.csv("student_performance_dataset_final.csv")

# View first rows
head(data)

# View last rows
tail(data)

# Check dimensions
dim(data)

# Number of rows
nrow(data)

# Number of columns
ncol(data)

# Column names
colnames(data)

# Structure of dataset
str(data)

# Summary of dataset
summary(data)


# Select only relevant columns
selected_data <- data[, c(
  "study_hours_per_day",
  "attendance_percentage",
  "assignment_scores_avg",
  "concept_understanding_score",
  "study_consistency_index",
  "class_participation_score",
  "last_exam_score",
  "improvement_rate",
  "final_score",
  "performance_category"
)]

# View selected data
head(selected_data)

# Check structure
str(selected_data)

# Summary
summary(selected_data)


# Select only relevant columns
selected_data <- data[, c(
  "study_hours_per_day",
  "attendance_percentage",
  "assignment_scores_avg",
  "concept_understanding_score",
  "study_consistency_index",
  "class_participation_score",
  "last_exam_score",
  "improvement_rate",
  "final_score",
  "performance_category"
)]

# View selected data
head(selected_data)

# Check structure
str(selected_data)

# Summary
summary(selected_data)


# Check total missing values
table(is.na(selected_data))

# Count missing values column by column
colSums(is.na(selected_data))


# Check total missing values
table(is.na(selected_data))

# Count missing values column by column
colSums(is.na(selected_data))


# Remove rows with missing values
selected_data <- na.omit(selected_data)

# Check again
colSums(is.na(selected_data))


# Boxplots for numeric variables
boxplot(selected_data$study_hours_per_day, main = "Boxplot of Study Hours Per Day")
boxplot(selected_data$attendance_percentage, main = "Boxplot of Attendance Percentage")
boxplot(selected_data$class_participation_score, main = "Boxplot of Class Participation Score")
boxplot(selected_data$study_consistency_index, main = "Boxplot of Study Consistency Index")
boxplot(selected_data$final_score, main = "Boxplot of Final Score")


Q1 <- quantile(selected_data$final_score, 0.25)
Q3 <- quantile(selected_data$final_score, 0.75)
IQR_value <- Q3 - Q1

outliers_final_score <- selected_data$final_score < (Q1 - 1.5 * IQR_value) |
  selected_data$final_score > (Q3 + 1.5 * IQR_value)

selected_data[outliers_final_score, ]


# Summary statistics
summary(selected_data)

# Mean of numeric variables
sapply(selected_data[, c(
  "study_hours_per_day",
  "attendance_percentage",
  "assignment_scores_avg",
  "concept_understanding_score",
  "study_consistency_index",
  "class_participation_score",
  "last_exam_score",
  "improvement_rate",
  "final_score"
)], mean)

# Standard deviation
sapply(selected_data[, c(
  "study_hours_per_day",
  "attendance_percentage",
  "assignment_scores_avg",
  "concept_understanding_score",
  "study_consistency_index",
  "class_participation_score",
  "last_exam_score",
  "improvement_rate",
  "final_score"
)], sd)

# Range
sapply(selected_data[, c(
  "study_hours_per_day",
  "attendance_percentage",
  "assignment_scores_avg",
  "concept_understanding_score",
  "study_consistency_index",
  "class_participation_score",
  "last_exam_score",
  "improvement_rate",
  "final_score"
)], range)


# Gender distribution from original dataset
table(data$gender)

# Grade level distribution
table(data$grade_level)

# Passed / failed distribution
table(data$passed)

# Performance category distribution
table(data$performance_category)


prop.table(table(data$performance_category))



hist(selected_data$study_hours_per_day,
     main = "Histogram of Study Hours Per Day",
     xlab = "Study Hours Per Day",
     col = "lightblue")

hist(selected_data$attendance_percentage,
     main = "Histogram of Attendance Percentage",
     xlab = "Attendance Percentage",
     col = "lightgreen")

hist(selected_data$final_score,
     main = "Histogram of Final Score",
     xlab = "Final Score",
     col = "lightpink")

hist(selected_data$improvement_rate,
     main = "Histogram of Improvement Rate",
     xlab = "Improvement Rate",
     col = "lightgray")




boxplot(selected_data$study_hours_per_day,
        main = "Boxplot of Study Hours Per Day",
        col = "orange")

boxplot(selected_data$attendance_percentage,
        main = "Boxplot of Attendance Percentage",
        col = "yellow")

boxplot(selected_data$final_score,
        main = "Boxplot of Final Score",
        col = "cyan")

boxplot(selected_data$improvement_rate,
        main = "Boxplot of Improvement Rate",
        col = "violet")





plot(selected_data$study_hours_per_day, selected_data$final_score,
     main = "Study Hours vs Final Score",
     xlab = "Study Hours Per Day",
     ylab = "Final Score",
     pch = 19, col = "blue")

plot(selected_data$attendance_percentage, selected_data$final_score,
     main = "Attendance Percentage vs Final Score",
     xlab = "Attendance Percentage",
     ylab = "Final Score",
     pch = 19, col = "darkgreen")

plot(selected_data$class_participation_score, selected_data$final_score,
     main = "Class Participation vs Final Score",
     xlab = "Class Participation Score",
     ylab = "Final Score",
     pch = 19, col = "red")

plot(selected_data$study_consistency_index, selected_data$final_score,
     main = "Study Consistency Index vs Final Score",
     xlab = "Study Consistency Index",
     ylab = "Final Score",
     pch = 19, col = "purple")

plot(selected_data$concept_understanding_score, selected_data$final_score,
     main = "Concept Understanding vs Final Score",
     xlab = "Concept Understanding Score",
     ylab = "Final Score",
     pch = 19, col = "brown")




boxplot(study_hours_per_day ~ performance_category, data = selected_data,
        main = "Study Hours by Performance Category",
        xlab = "Performance Category",
        ylab = "Study Hours Per Day",
        col = c("red", "yellow", "green"))

boxplot(attendance_percentage ~ performance_category, data = selected_data,
        main = "Attendance Percentage by Performance Category",
        xlab = "Performance Category",
        ylab = "Attendance Percentage",
        col = c("red", "yellow", "green"))

boxplot(class_participation_score ~ performance_category, data = selected_data,
        main = "Class Participation by Performance Category",
        xlab = "Performance Category",
        ylab = "Class Participation Score",
        col = c("red", "yellow", "green"))

boxplot(final_score ~ performance_category, data = selected_data,
        main = "Final Score by Performance Category",
        xlab = "Performance Category",
        ylab = "Final Score",
        col = c("red", "yellow", "green"))



# Correlation between selected numeric variables
cor_data <- selected_data[, c(
  "study_hours_per_day",
  "attendance_percentage",
  "assignment_scores_avg",
  "concept_understanding_score",
  "study_consistency_index",
  "class_participation_score",
  "last_exam_score",
  "improvement_rate",
  "final_score"
)]

cor_matrix <- cor(cor_data)

print(cor_matrix)







