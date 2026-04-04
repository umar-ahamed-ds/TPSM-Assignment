# Topic: Learning engagement influences long-term knowledge retention

# Set working directory
setwd("C:/Users/UMAR/Desktop/Y3S1 DS/TPSM/Assigment/new data set")
getwd()

# Load dataset
S_Data <- read.csv("student_performance_dataset_finall.csv")

# Check data
head(S_Data)
str(S_Data)
summary(S_Data)

# Remove missing values
data2 <- na.omit(S_Data)

# =========================================================
# 1. CORRELATION ANALYSIS
# =========================================================

# Engagement variables
engagement_var <- data2[, c("study_hours_per_day",
                            "attendance_percentage",
                            "study_consistency_index",
                            "class_participation_score",
                            "concept_understanding_score")]

# Outcome variable
outcome_var <- data2[, c("final_score")]

# Spearman correlation matrix
cor_matrix <- cor(cbind(engagement_var, outcome_var),
                  method = "spearman")
cor_matrix

# Individual Spearman correlation tests
cor.test(data2$study_hours_per_day,
         data2$final_score,
         method = "spearman")

cor.test(data2$attendance_percentage,
         data2$final_score,
         method = "spearman")

cor.test(data2$study_consistency_index,
         data2$final_score,
         method = "spearman")

cor.test(data2$class_participation_score,
         data2$final_score,
         method = "spearman")

cor.test(data2$concept_understanding_score,
         data2$final_score,
         method = "spearman")

# Plot for the most important relationship
plot(data2$concept_understanding_score, data2$final_score,
     main = "Concept Understanding Score vs Final Score",
     xlab = "Concept Understanding Score",
     ylab = "Final Score",
     col = "blue",
     pch = 16)

abline(lm(final_score ~ concept_understanding_score, data = data2),
       col = "red", lwd = 2)

# =========================================================
# 2. CREATE ENGAGEMENT SCORE AND GROUP
# =========================================================

# Standardize variables because scales are different
data2$study_hours_z <- scale(data2$study_hours_per_day)
data2$attendance_z <- scale(data2$attendance_percentage)
data2$consistency_z <- scale(data2$study_consistency_index)
data2$participation_z <- scale(data2$class_participation_score)
data2$concept_z <- scale(data2$concept_understanding_score)

# Create combined engagement score
data2$engagement_score <- rowMeans(
  data2[, c("study_hours_z",
            "attendance_z",
            "consistency_z",
            "participation_z",
            "concept_z")]
)

# Create Engagement Group: Low / Medium / High
data2$Engagement_Group <- cut(
  data2$engagement_score,
  breaks = quantile(data2$engagement_score,
                    probs = c(0, 0.33, 0.66, 1)),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE
)

# Check number of students in each group
table(data2$Engagement_Group)

# =========================================================
# 3. ANOVA
# =========================================================

# Group summaries
aggregate(final_score ~ Engagement_Group, data = data2, mean)
aggregate(final_score ~ Engagement_Group, data = data2, sd)
table(data2$Engagement_Group)

# Boxplot
boxplot(final_score ~ Engagement_Group,
        data = data2,
        main = "Final Score by Engagement Group",
        xlab = "Engagement Group",
        ylab = "Final Score",
        col = c("red", "yellow", "green"))

# ANOVA test
anova_result <- aov(final_score ~ Engagement_Group, data = data2)
summary(anova_result)

# Post-hoc test only if ANOVA is significant
if (summary(anova_result)[[1]][["Pr(>F)"]][1] < 0.05) {
  TukeyHSD(anova_result)
}

# =========================================================
# 4. T-TEST
# =========================================================

# Keep only Low and High engagement groups
t_data <- subset(data2, Engagement_Group %in% c("Low", "High"))

# Group summaries
aggregate(final_score ~ Engagement_Group, data = t_data, mean)
aggregate(final_score ~ Engagement_Group, data = t_data, sd)
table(t_data$Engagement_Group)

# Boxplot
boxplot(final_score ~ Engagement_Group,
        data = t_data,
        main = "Final Score: Low vs High Engagement",
        xlab = "Engagement Group",
        ylab = "Final Score",
        col = c("red", "green"))

# Independent sample t-test
t_test_result <- t.test(final_score ~ Engagement_Group, data = t_data)
t_test_result

# =========================================================
# 5. CHI-SQUARE TEST
# =========================================================

# Contingency table
table_data <- table(data2$Engagement_Group, data2$passed)
table_data

# Chi-square test
chisq_result <- chisq.test(table_data)
chisq_result

# Expected counts
chisq_result$expected

# Stacked bar chart
library(ggplot2)

ggplot(data2, aes(x = Engagement_Group, fill = factor(passed))) +
  geom_bar(position = "fill") +
  labs(title = "Pass Status by Engagement Group",
       x = "Engagement Group",
       y = "Proportion",
       fill = "Passed (0 = No, 1 = Yes)") +
  theme_minimal()

