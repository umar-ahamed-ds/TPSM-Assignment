#libraries 

library(dplyr)
library(ggplot2)
library(car)
library(lmtest)

#Import dataset
df <- read.csv("ai_impact_student_performance_dataset.csv")

#checking if data set successfully imported

head(df)
str(df)
summary(df)
colnames(df)

#selecting variables needed for refression

reg_df <- df %>%
  select(
    final_score,
    last_exam_score,
    study_hours_per_day,
    attendance_percentage,
    study_consistency_index,
    class_participation_score,
    tutoring_hours,
    concept_understanding_score
  ) %>%
  na.omit()

# multiple linear regression model

model_regression <- lm(
  final_score ~ last_exam_score +
    study_hours_per_day +
    attendance_percentage +
    study_consistency_index +
    class_participation_score +
    tutoring_hours +
    concept_understanding_score,
  data = reg_df
)

summary(model_regression)

model_summary <- summary(model_regression)

#R-squared

model_summary$r.squared
model_summary$adj.r.squared

#coefficients table

model_summary$coefficients

#checking assumtion 

# 1. Linearity

plot(model_regression, which = 1)

library(ggplot2)

ggplot(reg_df, aes(x = last_exam_score, y = final_score)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE)

ggplot(reg_df, aes(x = concept_understanding_score, y = final_score)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE)

# 2. Multicollinearity

vif(model_regression)

# 3. Homoscedasticity

bptest(model_regression)

# 4. Independence

dwtest(model_regression)

# 5. Normality

plot(model_regression,which = 2)

hist(residuals(model_regression), main = "Histogram of residuals", xlab = "Residuals")

shapiro.test(residuals(model_regression))
