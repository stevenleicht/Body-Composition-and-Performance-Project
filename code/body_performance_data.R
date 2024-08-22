#Project: Looking at how total number of sit-ups completed compares to physical/physiological factors such as age and body composition
#After analyzing the original data, I created a predictive model to measure the differences between the predicted and the actual values to measure its statistical significance
#This data is provided by the "Body performance Data" data set by Kukuroo3 (2022). This data set was discovered on Kaggle

#Opening required libraries:
install.packages("caTools")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("rsample")
library(rsample)
library(dplyr)
library(corrplot)
library(ggplot2)
library(caTools)
library(rpart)
library(rpart.plot)
library(boot)

#Importing the data:
file_path = "/Users/stevenleicht/Downloads/body_performance_data.csv"
bp_data = read.csv(file_path)
head(bp_data)
summary(bp_data)
str(bp_data)

#Selecting the data:
selected_bp_data = bp_data %>%
  select(sit.ups.counts, age, height_cm, weight_kg, body.fat_.)
print(selected_bp_data)

#Scaling/converting 'height_cm' to inches for proper analysis purposes:
scaled_height_data = selected_bp_data %>%
  mutate(height_cm * 0.393701)
print(scaled_height_data)

#Running a correlation matrix to determine if there is a correlation for sit-ups with body composition:
cor_matrix = cor(selected_bp_data, use = "complete.obs")
print(cor_matrix)
#Comparing age to sit-ups, it seems that there is a strong negative relationship. Moreover, the older the individual, the fewer sit-ups they can perform (-0.54)
#Comparing height to sit-ups, it seems that there is a mild positive relationship. Moreover, the taller the individual, the more likely they are to perform more sit-ups (0.50)
#Comparing weight to sit-ups, it seems that there is a slight positive relationship. Moreover, the heavier the individual, the more likely they are to perform more sit-ups (0.29)
#Comparing body fat percentage to sit-ups, it seems that there is a strong negative relationship. Moreover, the higher the body fat percentage, the fewer sit-ups completed (-0.61)
#Conclusion: it is certain that the younger an individual is, along with the lower their body fat percentage, the greater amount of sit-ups they can perform. Also, the taller and heavier the individual, the greater amount of sit-ups they can perform, but this is not as statistically significant

#Seeing if the correlation matrix is statistically significant:
cor.mtest = function(mat, conf.level = 0.95) {
  mat = as.matrix(mat)
  n = ncol(mat)
  p.mat = matrix(NA, n, n)
  diag(p.mat) = 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp = cor.test(mat[, i], mat[, j], conf.level = conf.level)
      p.mat[i, j] = p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) = rownames(p.mat) <- colnames(mat)
  p.mat
}
p_values_matrix = cor.mtest(selected_bp_data)
print(p_values_matrix)
#The p-value is <0.001 when comparing all selected factors of body composition to sit-ups, showcasing that the correlation matrix is statistically significant

#Creating a multiple linear regression model to compare the predicted number of sit-ups vs. actual number of sit-ups based on age and body composition:
mlr_model = lm(sit.ups.counts ~ age + height_cm + weight_kg + body.fat_., data = selected_bp_data)
summary(mlr_model)
#The residual standard error is 9.181 sit-ups on 13,388 degrees of freedom
#The adjusted r-squared value is 0.5864. In this case, that means that the model is relatively similar and significant to the actual values
#The p-value is <0.001, meaning that the model is very statistically significant

#Calculating the numerical differences between the predicted and actual values:
pm_vs_ad = 91.686 - 0.439 * selected_bp_data$age - 0.206 * selected_bp_data$height_cm +
  0.354 * selected_bp_data$weight_kg - 1.073 * selected_bp_data$body.fat_.
print(head(pm_vs_ad))

#Comparing the predicted values to the actual values:
comparison_df = data.frame(
  Actual = selected_bp_data$sit.ups.counts,
  Predicted = pm_vs_ad
)

#Visualizing the differences between the predicted and actual values:
visual_plot_data = data.frame(
  Index = 1:nrow(selected_bp_data),
  Actual = selected_bp_data$sit.ups.counts,
  Predicted = pm_vs_ad
)
ggplot(visual_plot_data, aes(x = Index)) +
  geom_point(aes(y = Predicted, color = "Predicted"), size = 2) +
  geom_point(aes(y = Actual, color = "Actual"), size = 2, alpha = 0.5) +
  geom_smooth(aes(y = Predicted, color = "Predicted"), method = "lm", se = FALSE, linewidth = 1, linetype = "dashed") +
  geom_smooth(aes(y = Actual, color = "Actual"), method = "lm", se = FALSE, linewidth = 1, linetype = "dashed", alpha = 0.5) +
  labs(title = "Actual vs. Predicted Sit-Ups by Body Composition",
       x = "Individual",
       y = "Sit-Ups",
       color = "Legend") +
  xlim(c(14, 113)) +
  theme_minimal()

#Overall conclusion: there are definitely significant correlations between the number of sit-ups performed and body composition
#The predicted values are similar to the actual values. However, there are plenty of other factors that can relate to physical performance
#I'm interested in continuing this research and discovering a more profound version or model to calculate overall physical performance for multiple populations (athletes, general public, etc.)
