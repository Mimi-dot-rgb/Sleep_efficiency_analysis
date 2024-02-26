install.packages("tidyverse")
install.packages("lmtest")
install.packages("reshape2")
library(tidyverse)
library(dplyr)

view(Sleep_Efficiency)

### Data Cleaning ###
names(Sleep_Efficiency)
Sleep_Efficiency$Wakeup_Time <- Sleep_Efficiency$`Wakeup time`
Sleep_Efficiency$Sleep_Duration <- Sleep_Efficiency$`Sleep duration`
Sleep_Efficiency$Sleep_Efficiency <- Sleep_Efficiency$`Sleep efficiency`
Sleep_Efficiency$REM_Percentage <- Sleep_Efficiency$`REM sleep percentage`
Sleep_Efficiency$Deep_Sleep_Percentage <- Sleep_Efficiency$`Deep sleep percentage`
Sleep_Efficiency$Light_Sleep_Percentage <- Sleep_Efficiency$`Light sleep percentage`
Sleep_Efficiency$Caffeine_Consumption <- Sleep_Efficiency$`Caffeine consumption`
Sleep_Efficiency$Alcohol_Consumption <- Sleep_Efficiency$`Alcohol consumption`
Sleep_Efficiency$Smoking_Status <- Sleep_Efficiency$`Smoking status`
Sleep_Efficiency$Excercise <- Sleep_Efficiency$`Exercise frequency`

names(Sleep_Efficiency)

Sleep_Efficiency <- na.omit(Sleep_Efficiency)


# Smoking Status Model
str(Sleep_Efficiency)
Sleep_Efficiency$Sleep_Efficiency <- Sleep_Efficiency$Sleep.efficiency

model1 <- lm(Sleep_Efficiency ~ Smoking.status, data = Sleep_Efficiency)
summary(model1)
par(mfrow = c(2,2))
plot(model1, main = "Model 1: Sleep Efficiency ~ Smoking Status")


# Exercise Frequency
model2 <- lm(Sleep_Efficiency ~ Exercise.frequency, data = Sleep_Efficiency)
summary(model2)
par(mfrow = c(2,2))
plot(model2, main = "Model 2: Sleep Efficiency ~ Exercise Frequency")

# Alcohol Consumption
model3 <- lm(Sleep_Efficiency ~ Alcohol.consumption, data = Sleep_Efficiency)
summary(model3)
par(mfrow = c(2,2))
plot(model3, main = "Model 3: Sleep Efficiency ~ Alcohol Consumption")

# Caffeine Consumption
model4 <- lm(Sleep_Efficiency ~ Caffeine.consumption, data = Sleep_Efficiency)
summary(model4)
par(mfrow = c(2,2))
plot(model4, main = "Model 4: Sleep Efficiency ~ Caffeine Consumption")





#converted Bedtime and Wakeup.time into numeric variables representing hours
# For example, "22:00:00" becomes 22
Sleep_Efficiency$Bedtime_Hour <- as.numeric(substr(Sleep_Efficiency$Bedtime, 12, 13))
Sleep_Efficiency$Wakeup_Hour <- as.numeric(substr(Sleep_Efficiency$Wakeup.time, 12, 13))

model5 <- lm(Sleep_Efficiency ~ Bedtime_Hour + Wakeup_Hour, data = Sleep_Efficiency)
summary(model5)
par(mfrow = c(2,2))
plot(model5, main = "Model 5: Sleep Efficiency ~ Bedtime + Wakeup Time")

# Analyzing the effect of gender
Sleep_Efficiency$Gender <- as.factor(Sleep_Efficiency$Gender)
model6 <- lm(Sleep_Efficiency ~ Gender, data = Sleep_Efficiency)
summary(model6)
par(mfrow = c(2,2))
plot(model6, main = "Model 6: Sleep Efficiency ~ Gender")

model7 <- lm(Sleep_Efficiency ~ Smoking.status * Alcohol.consumption, data = Sleep_Efficiency)
summary(model7)
par(mfrow = c(2,2))
plot(model7, main = "Model 7: Sleep Efficiency ~ Smoking & Alcohol Interaction")

library(dplyr)


# Scatterplot matrix using base R
numeric_data <- Sleep_Efficiency %>% select_if(is.numeric)
pairs(numeric_data)

# Correlation matrix heatmap using ggplot2
library(reshape2) # for melt function
library(ggplot2)

# Calculate the correlation matrix
cor_mat <- cor(numeric_data)

# Melt the correlation matrix into long format
cor_long <- melt(cor_mat)

# Plot the heatmap
ggplot(cor_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title = element_blank())


library(car) # car package provides vif() function

mlr_model <- lm(Sleep_Efficiency ~ Smoking.status + Exercise.frequency + Alcohol.consumption + Caffeine.consumption, data = Sleep_Efficiency)
summary(mlr_model)

par(mfrow = c(2, 2))
plot(mlr_model , main )
mtext("Model 8: Diagnostic Plots for Multiple Linear Regression", side = 3, line = -2, outer = TRUE)

# Calculate VIF for each predictor in the model
vif(mlr_model)





library(tidyverse)

# Numerical Summary for Quantitative Variables
Sleep_Efficiency %>% 
  select_if(is.numeric) %>% 
  summary()

# Count Summary for Qualitative Variables
Sleep_Efficiency %>% 
  select_if(is.factor) %>% 
  summary()


# Graphical Summary for Quantitative Variables
quantitative_vars <- Sleep_Efficiency %>% select_if(is.numeric)
for (var in names(quantitative_vars)) {
  print(
    ggplot(Sleep_Efficiency, aes_string(x = var)) +
      geom_histogram(binwidth = 1) +
      labs(title = paste("Histogram of", var))
  )
}

# Graphical Summary for Qualitative Variables
qualitative_vars <- Sleep_Efficiency %>% select_if(is.factor)
for (var in names(qualitative_vars)) {
  print(
    ggplot(Sleep_Efficiency, aes_string(x = var)) +
      geom_bar() +
      labs(title = paste("Bar Plot of", var))
  )
}



df <- Sleep_Efficiency
is.data.frame(df)

# Convert Bedtime and Wakeup.time to hour of the day
Sleep_Efficiency$Bedtime <- as.numeric(format(as.POSIXct(Sleep_Efficiency$Bedtime), "%H")) + 
  as.numeric(format(as.POSIXct(Sleep_Efficiency$Bedtime), "%M")) / 60
Sleep_Efficiency$Wakeup.time <- as.numeric(format(as.POSIXct(Sleep_Efficiency$Wakeup.time), "%H")) + 
  as.numeric(format(as.POSIXct(Sleep_Efficiency$Wakeup.time), "%M")) / 60

# Fit the model again with the processed data
mlr_model_continuous <- lm(Sleep_Efficiency ~ Age + Awakenings + Sleep.duration + 
                             REM.sleep.percentage + Deep.sleep.percentage + 
                             Alcohol.consumption + Caffeine.consumption + Gender + 
                             Bedtime + Wakeup.time, 
                           data = Sleep_Efficiency)
summary(mlr_model_continuous)

par(mfrow = c(2, 2))
plot(mlr_model_continuous)
mtext("Model 8:Plots for Multiple Linear Regression", side = 3, line = -2, outer = TRUE)

#LINE conditions
library(car)

# Fit your linear regression model
model <- lm(Sleep_Efficiency ~ Age + Awakenings + Sleep.duration + 
              REM.sleep.percentage + Deep.sleep.percentage + 
              Alcohol.consumption + Caffeine.consumption + 
              Gender + Bedtime + Wakeup.time, data = Sleep_Efficiency)

# Set up the plotting area to have 3 rows and 2 columns
par(mfrow = c(3, 2))

# 1. Scatterplot of Observed vs Fitted Values
plot(model$fitted.values, model$model$Sleep_Efficiency,
     xlab = "Fitted Values", ylab = "Observed Sleep Efficiency",
     main = "Fig(a): Scatterplot of Y vs Fitted Y")

# 2. Tukey's Curve
ncvTest(model)

# 3. Histogram of Residuals
hist(residuals(model), breaks = "Sturges",
     main = "Fig(b): Histogram of Residuals", xlab = "Residuals")

# 4. Q-Q Plot of Residuals
qqnorm(residuals(model))
qqline(residuals(model), col = "red", main = "Fig(c): Normal Q-Q Plot")

# 5. Residual Plot Residuals vs Fitted
plot(model$fitted.values, residuals(model),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Fig(d): Residual Plot Residuals vs Fitted")

# 6. Residuals Index Plot
plot(residuals(model) ~ seq_along(residuals(model)),
     xlab = "Index", ylab = "Residuals",
     main = "Fig(e):Residuals Index Plot")

# Reset the plotting area
par(mfrow = c(1, 1))

# Tukey's test
library(car)
tukey_test <- ncvTest(model)
print(tukey_test)

# Breusch-Pagan test
library(lmtest)
bp_test <- bptest(model)
print(bp_test)

Sleep_Efficiency$Log_Sleep_Efficiency <- log(Sleep_Efficiency$Sleep_Efficiency)

# Fit the model using the transformed response variable
log_model <- lm(Log_Sleep_Efficiency ~ Age + Awakenings + Sleep.duration + 
                  REM.sleep.percentage + Deep.sleep.percentage + 
                  Alcohol.consumption + Caffeine.consumption + Gender + 
                  Bedtime + Wakeup.time, 
                data = Sleep_Efficiency)

summary(log_model)

par(mfrow = c(2, 3))

# 1. Scatterplot of Observed vs Fitted Values (on the log scale)
plot(log_model$fitted.values, log(Sleep_Efficiency$Sleep_Efficiency),
     xlab = "Fitted Values (Log scale)", ylab = "Observed Sleep Efficiency (Log scale)",
     main = "Fig(a): Scatterplot of Y vs Fitted Y")

# 2. Histogram of Residuals
hist(residuals(log_model), main = "Fig(b): Histogram of Residuals", xlab = "Residuals")

# 3. Normal Q-Q Plot
qqnorm(residuals(log_model))
qqline(residuals(log_model), col = "red", main = "Normal Q-Q Plot")

# 4. Residual Plot Residuals vs Fitted
plot(log_model$fitted.values, residuals(log_model),
     xlab = "Fitted Values (Log scale)", ylab = "Residuals",
     main = "Fig(c): Residual Plot Residuals vs Fitted")

# 5. Scale-Location Plot
plot(log_model$fitted.values, sqrt(abs(residuals(log_model))),
     xlab = "Fitted Values (Log scale)", ylab = "Sqrt(|Residuals|)",
     main = "Fig(d): Scale-Location Plot")

# 6. Residuals vs Leverage
plot(hatvalues(log_model), residuals(log_model),
     xlab = "Leverage", ylab = "Residuals",
     main = "Fig(e): Residuals vs Leverage")

# Reset the plotting area
par(mfrow = c(1, 1))

tukey_test <- ncvTest(log_model)
print(tukey_test)

# Breusch-Pagan test
bp_test <- bptest(log_model)
print(bp_test)
#transformation failed

#SQRT_ROOT transformations
# Apply square root transformation to the outcome variable
Sleep_Efficiency$Sleep_Efficiency_Sqrt <- sqrt(Sleep_Efficiency$Sleep_Efficiency)

# Fit the square root transformed model
sqrt_model <- lm(Sleep_Efficiency_Sqrt ~ Age + Awakenings + Sleep.duration + 
                   REM.sleep.percentage + Deep.sleep.percentage + 
                   Alcohol.consumption + Caffeine.consumption + Gender + 
                   Bedtime + Wakeup.time, 
                 data = Sleep_Efficiency)
summary(sqrt_model)
# Create diagnostic plots
par(mfrow = c(2, 3)) # Set up the plotting area to have 2 rows and 3 columns

# Scatterplot of Observed vs Fitted Values
plot(sqrt_model$fitted.values, Sleep_Efficiency$Sleep_Efficiency_Sqrt,
     xlab = "Fitted Values", ylab = "Observed Sleep Efficiency (Sqrt)",
     main = "Fig(a): Scatterplot of Y vs Fitted Y")

# Histogram of Residuals
hist(residuals(sqrt_model), 
     main = "Fig(b): Histogram of Residuals", xlab = "Residuals")

# Normal Q-Q Plot
qqnorm(residuals(sqrt_model))
qqline(residuals(sqrt_model), col = "red")
title("Fig(c): Normal Q-Q Plot")

# Residual Plot Residuals vs Fitted
plot(sqrt_model$fitted.values, residuals(sqrt_model),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Fig(d): Residual Plot Residuals vs Fitted")

# Scale-Location Plot (Spread vs Level)
plot(sqrt_model$fitted.values, sqrt(abs(residuals(sqrt_model))),
     xlab = "Fitted Values", ylab = "Sqrt(|Residuals|)",
     main = "Fig(e): Scale-Location Plot")

# Residuals vs Leverage
plot(hatvalues(sqrt_model), residuals(sqrt_model),
     xlab = "Leverage", ylab = "Residuals",
     main = "Fig(f): Residuals vs Leverage")

# Reset to default plotting area
par(mfrow = c(1, 1))
#Tukey Test
tukey_test <- ncvTest(sqrt_model)
print(tukey_test)

# Breusch-Pagan test
bp_test <- bptest(sqrt_model)
print(bp_test)




library(MASS) # for boxcox
library(car)  # for ncvTest and vif
library(lmtest) # for bptest
library(ggplot2) # for plotting

# Fit the original model
original_model <- lm(Sleep_Efficiency ~ Age + Awakenings + Sleep.duration + 
                       REM.sleep.percentage + Deep.sleep.percentage + 
                       Alcohol.consumption + Caffeine.consumption + 
                       Gender + Bedtime + Wakeup.time, data = Sleep_Efficiency)

# Box-Cox transformation
bc <- boxcox(original_model, lambda = seq(-0.5, 1.5, by=0.1))
best_lambda <- bc$x[which.max(bc$y)]

# Apply the Box-Cox transformation 
Sleep_Efficiency$Transformed_Sleep_Efficiency <- ifelse(best_lambda == 0, 
                                                        log(Sleep_Efficiency$Sleep_Efficiency), 
                                                        (Sleep_Efficiency$Sleep_Efficiency^best_lambda - 1) / best_lambda)

# Fit the new model using the transformed Sleep Efficiency
boxcox_model <- lm(Transformed_Sleep_Efficiency ~ Age + Awakenings + Sleep.duration + 
                     REM.sleep.percentage + Deep.sleep.percentage + 
                     Alcohol.consumption + Caffeine.consumption + 
                     Gender + Bedtime + Wakeup.time, 
                   data = Sleep_Efficiency)
summary(boxcox_model)
# Plot the diagnostic plots
par(mfrow = c(2, 3))

# Scatterplot of observed vs fitted values
plot(boxcox_model$fitted.values, 
     Sleep_Efficiency$Transformed_Sleep_Efficiency,
     xlab = "Fitted Values", ylab = "Transformed Observed Sleep Efficiency",
     main = "Scatterplot of Y vs Fitted")

# Histogram of residuals
hist(residuals(boxcox_model),
     main = "Histogram of Residuals", xlab = "Residuals")

# Q-Q plot of residuals
qqnorm(residuals(boxcox_model))
qqline(residuals(boxcox_model), col = "red")

# Residuals vs fitted values
plot(residuals(boxcox_model) ~ fitted(boxcox_model),
     main = "Residual Plot Residuals vs Fitted")

# Scale-Location plot
plot(sqrt(abs(residuals(boxcox_model))) ~ fitted(boxcox_model),
     main = "Scale-Location Plot")

# Residuals vs leverage
plot(hatvalues(boxcox_model), residuals(boxcox_model),
     main = "Residuals vs Leverage")

# Reset graphics
par(mfrow = c(1, 1))

# Output the summary of the new model
summary(boxcox_model)

# Perform the Breusch-Pagan test
bp_test <- bptest(boxcox_model)

# Perform Tukey's test
tukey_test <- ncvTest(boxcox_model)

# Print the test results
print(bp_test)
print(tukey_test)



#POLYNOMIAL TRANSFORMATION
# Load necessary packages
library(ggplot2)
library(car)
library(lmtest)

# Assuming your data is in a dataframe called Sleep_Efficiency
# and your dependent variable is Sleep_Efficiency

# Polynomial transformation of degree 2 for Sleep.duration
Sleep_Efficiency$Sleep.duration2 <- Sleep_Efficiency$Sleep.duration^2

# Fit a new model with the polynomial term
poly_model <- lm(Sleep_Efficiency ~ Age + Awakenings + Sleep.duration + I(Sleep.duration^2) + 
                   REM.sleep.percentage + Deep.sleep.percentage + Alcohol.consumption + 
                   Caffeine.consumption + Gender + Bedtime + Wakeup.time, 
                 data = Sleep_Efficiency)
summary(poly_model)
# Diagnostic plots
par(mfrow = c(3, 2))  # Set up the plotting area

# Plot 1: Scatterplot of Observed vs Fitted
plot(poly_model$fitted.values, Sleep_Efficiency$Sleep_Efficiency,
     xlab = "Fitted Values", ylab = "Observed Sleep Efficiency",
     main = "Scatterplot of Y vs Fitted")

# Plot 2: Histogram of Residuals
hist(residuals(poly_model), breaks = "Sturges",
     main = "Histogram of Residuals", xlab = "Residuals")

# Plot 3: Q-Q Plot
qqnorm(residuals(poly_model))
qqline(residuals(poly_model), col = "red", main = "Normal Q-Q Plot")

# Plot 4: Scale-Location (Spread vs Level)
plot(poly_model$fitted.values, sqrt(abs(residuals(poly_model))),
     xlab = "Fitted Values", ylab = "Sqrt(|Residuals|)",
     main = "Scale-Location Plot")

# Plot 5: Residuals vs Fitted Values
plot(poly_model$fitted.values, residuals(poly_model),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residual Plot")

# Plot 6: Residuals vs Leverage
plot(hatvalues(poly_model), residuals(poly_model),
     xlab = "Leverage", ylab = "Residuals",
     main = "Residuals vs Leverage")

# Reset the plotting area
par(mfrow = c(1, 1))

# Breusch-Pagan test for heteroscedasticity
bp_test <- bptest(poly_model)

# Tukey test for non-constant variance
tukey_test <- ncvTest(poly_model)

# Print the test results
print(bp_test)
print(tukey_test)

# Output the summary of the polynomial model
summary(poly_model)


# Log-transform the skewed predictors
Sleep_Efficiency$log_Age <- log(Sleep_Efficiency$Age + 1)  # Adding 1 to avoid log(0)
Sleep_Efficiency$log_Awakenings <- log(Sleep_Efficiency$Awakenings + 1)

# Now, use these transformed predictors in the model
log_transformed_model <- lm(Sleep_Efficiency ~ log_Age + log_Awakenings + Sleep.duration + 
                              REM.sleep.percentage + Deep.sleep.percentage + 
                              Alcohol.consumption + Caffeine.consumption + 
                              Gender + Bedtime + Wakeup.time, data = Sleep_Efficiency)
summary(log_transformed_model)

# Check the diagnostic plots for the new model
par(mfrow = c(2, 2))
plot(log_transformed_model)

bp_test <- bptest(log_transformed_model)

# Tukey test for non-constant variance
tukey_test <- ncvTest(log_transformed_model)

# Print the test results
print(bp_test)
print(tukey_test)


Sleep_Efficiency$log_Age <- log(Sleep_Efficiency$Age + 1)  # Adding 1 to avoid log(0)
Sleep_Efficiency$log_Awakenings <- log(Sleep_Efficiency$Awakenings + 1)

# Fit the model with log-transformed predictors
log_transformed_model <- lm(Sleep_Efficiency ~ log_Age + log_Awakenings + Sleep.duration +
                              REM.sleep.percentage + Deep.sleep.percentage +
                              Alcohol.consumption + Caffeine.consumption +
                              Gender + Bedtime + Wakeup.time, data = Sleep_Efficiency)

# Summary of the model
summary(log_transformed_model)

# Diagnostic plots
par(mfrow = c(2, 3)) 

# 1. Scatterplot of Observed vs Fitted Values
plot(log_transformed_model$fitted.values, Sleep_Efficiency$Sleep_Efficiency,
     xlab = "Fitted Values", ylab = "Observed Sleep Efficiency",
     main = "Fig(a): Scatterplot of Y vs Fitted Y")

# 2. Normal Q-Q Plot
qqnorm(resid(log_transformed_model))
qqline(resid(log_transformed_model), col = "red", lwd = 2)
title("Fig(b): Normal Q-Q Plot")

# 3. Scale-Location Plot
scale_location <- sqrt(abs(resid(log_transformed_model))) ~ fitted(log_transformed_model)
plot(scale_location, xlab = "Fitted Values", ylab = "Sqrt(|Residuals|)",
     main = "Fig(c): Scale-Location Plot")

# 4. Residuals vs Leverage Plot
plot(hatvalues(log_transformed_model), resid(log_transformed_model),
     xlab = "Leverage", ylab = "Residuals",
     main = "Fig(d): Residuals vs Leverage")

# 5. Residuals vs Fitted Values
residuals_fitted <- resid(log_transformed_model) ~ fitted(log_transformed_model)
plot(residuals_fitted, xlab = "Fitted Values", ylab = "Residuals",
     main = "Fig(e): Residual Plot")

# 6. Histogram of Residuals
hist(resid(log_transformed_model), breaks = "Sturges", main = "Fig(f): Histogram of Residuals")

# Reset to default plotting area
par(mfrow = c(1, 1))

# Breusch-Pagan test
library(lmtest)
bp_test <- bptest(log_transformed_model)

# Tukey test for non-constant variance
library(car)
tukey_test <- ncvTest(log_transformed_model)

# Print the test results
print(bp_test)
print(tukey_test)





# Calculate the hat values (leverage statistics)
hat_values <- hatvalues(log_transformed_model)

# Identify high leverage points
# Common threshold is 2 * (number of predictors + 1) / number of observations
n <- nrow(Sleep_Efficiency)
p <- length(coef(log_transformed_model))
threshold_leverage <- 2 * (p + 1) / n
high_leverage_points <- which(hat_values > threshold_leverage)


# Calculate standardized residuals
standardized_residuals <- rstandard(log_transformed_model)

# Outliers are typically more than 3 standard deviations from the mean
outliers <- which(abs(standardized_residuals) > 3)
# Calculate Cook's distance for the model
cooks_d <- cooks.distance(log_transformed_model)

# influential if Cook's distance is greater than 4/(n-k-1)
threshold_cooks <- 4 / (n - p - 1)
influential_points <- which(cooks_d > threshold_cooks)

# Plot the diagnostics
par(mfrow = c(2, 2))

# Plot hat values
plot(hat_values, main = "Hat Values (Leverage)", ylab = "Hat Value")
abline(h = threshold_leverage, col = "red", lwd = 2)

# Plot standardized residuals
plot(standardized_residuals, main = "Standardized Residuals", ylab = "Standardized Residual")
abline(h = c(-3, 3), col = "red", lwd = 2)

# Plot Cook's distance
plot(cooks_d, main = "Cook's Distance", ylab = "Cook's Distance")
abline(h = threshold_cooks, col = "red", lwd = 2)

# Reset to default plotting area
par(mfrow = c(1, 1))




# Fit the log-transformed linear model
log_transformed_model <- lm(Sleep_Efficiency ~ log_Age + log_Awakenings + Sleep.duration +
                              REM.sleep.percentage + Deep.sleep.percentage +
                              Alcohol.consumption + Caffeine.consumption +
                              Gender + Bedtime + Wakeup.time, 
                            data = Sleep_Efficiency)

# Diagnostic plots for the original model
par(mfrow = c(2, 3))
plot(log_transformed_model)

# Calculate Cook's distance for the original model
cooks_d <- cooks.distance(log_transformed_model)

# Define the threshold for influential points
threshold <- 4 / (nrow(Sleep_Efficiency) - length(coef(log_transformed_model)) - 2)

# Identify influential points
influential_points <- which(cooks_d > threshold)

# Remove influential points
Sleep_Efficiency_clean <- Sleep_Efficiency[-influential_points, ]

# Refit the model without influential points
refitted_log_transformed_model <- lm(Sleep_Efficiency ~ log_Age + log_Awakenings + Sleep.duration +
                                       REM.sleep.percentage + Deep.sleep.percentage +
                                       Alcohol.consumption + Caffeine.consumption +
                                       Gender + Bedtime + Wakeup.time, 
                                     data = Sleep_Efficiency_clean)

# Diagnostic plots for the refitted model
par(mfrow = c(2, 3))
plot(refitted_log_transformed_model)

bp_test <- bptest(refitted_log_transformed_model)

# Tukey test for non-constant variance
library(car)
tukey_test <- ncvTest(refitted_log_transformed_model)

# Print the test results
print(bp_test)
print(tukey_test)



# Fit the log-transformed linear model
log_transformed_model <- lm(Sleep_Efficiency ~ log_Age + log_Awakenings + Sleep.duration +
                              REM.sleep.percentage + Deep.sleep.percentage +
                              Alcohol.consumption + Caffeine.consumption +
                              Gender + Bedtime + Wakeup.time, 
                            data = Sleep_Efficiency)
summary(log_transformed_model)
# Diagnostic plots for the original model with labels
par(mfrow = c(2, 3))
plot(log_transformed_model, which = 1, main = "Residuals vs Fitted")
plot(log_transformed_model, which = 2, main = "Normal Q-Q")
plot(log_transformed_model, which = 3, main = "Scale-Location")
plot(log_transformed_model, which = 4, main = "Cook's Distance")
plot(log_transformed_model, which = 5, main = "Residuals vs Leverage")
plot(log_transformed_model, which = 6, main = "Residuals vs Leverage (with Cook's distances)")

# Calculate Cook's distance for the original model
cooks_d <- cooks.distance(log_transformed_model)
threshold <- 4 / (nrow(Sleep_Efficiency) - length(coef(log_transformed_model)) - 2)

influential_points <- which(cooks_d > threshold)

# Remove influential points
Sleep_Efficiency_clean <- Sleep_Efficiency[-influential_points, ]

# Refit the model without influential points
refitted_log_transformed_model <- lm(Sleep_Efficiency ~ log_Age + log_Awakenings + Sleep.duration +
                                       REM.sleep.percentage + Deep.sleep.percentage +
                                       Alcohol.consumption + Caffeine.consumption +
                                       Gender + Bedtime + Wakeup.time, 
                                     data = Sleep_Efficiency_clean)
summary(refitted_log_transformed_model)
# Diagnostic plots for the refitted model with labels
par(mfrow = c(2, 3))
plot(refitted_log_transformed_model, which = 1, main = "Residuals vs Fitted")
plot(refitted_log_transformed_model, which = 2, main = "Normal Q-Q")
plot(refitted_log_transformed_model, which = 3, main = "Scale-Location")
plot(refitted_log_transformed_model, which = 4, main = "Cook's Distance")
plot(refitted_log_transformed_model, which = 5, main = "Residuals vs Leverage")
plot(refitted_log_transformed_model, which = 6, main = "Residuals vs Leverage (with Cook's distances)")


# Fit your log-transformed linear model
log_transformed_model <- lm(Sleep_Efficiency ~ log_Age + log_Awakenings + Sleep.duration +
                              REM.sleep.percentage + Deep.sleep.percentage +
                              Alcohol.consumption + Caffeine.consumption +
                              Gender + Bedtime + Wakeup.time, 
                            data = Sleep_Efficiency)

# Perform ANOVA on the model
anova_results <- anova(log_transformed_model)

# View the ANOVA table
anova_results



# Assuming that 'log_transformed_model' is regression model
library(car) # The vif function is in the "car" package

# Calculate VIF
vif_values <- vif(log_transformed_model)

# Display VIF values
print(vif_values)

# You can also check for high VIF values
high_vif <- vif_values[vif_values > 5] # Using 5 as a threshold for high VIF
print(high_vif)



full_model <- lm(Sleep_Efficiency ~ log_Age + log_Awakenings + Sleep.duration + 
                   REM.sleep.percentage + Deep.sleep.percentage +
                   Alcohol.consumption + Caffeine.consumption +
                   Gender + Bedtime + Wakeup.time, 
                 data = Sleep_Efficiency)


# Assuming 'Alcohol.consumption', 'Caffeine.consumption', and 'Gender' were found to be insignificant
reduced_model <- lm(Sleep_Efficiency ~ log_Age + log_Awakenings + Sleep.duration + 
                      REM.sleep.percentage + Deep.sleep.percentage +
                      Bedtime + Wakeup.time, 
                    data = Sleep_Efficiency)

# Compare the two models using ANOVA
model_comparison <- anova(full_model, reduced_model)

# Print the comparison
print(model_comparison)



confint_reduced_model <- confint(reduced_model, level = 0.95)
print(confint_reduced_model)


Sleep_Efficiency$Sleep_Time_Category <- cut(Sleep_Efficiency$Sleep.duration,
                                            breaks = c(-Inf, 6, 8, Inf),
                                            labels = c("Short Sleep", "Moderate Sleep", "Long Sleep"))
table(Sleep_Efficiency$Sleep_Time_Category)


# Load the ggplot2 package
library(ggplot2)

# Create the categorical variable based on Sleep.duration
Sleep_Efficiency$Sleep_Time_Category <- cut(Sleep_Efficiency$Sleep.duration,
                                            breaks = c(-Inf, 6, 8, Inf),
                                            labels = c("Short Sleep", "Moderate Sleep", "Long Sleep"))

# Generate scatterplots
ggplot(Sleep_Efficiency, aes(x = Age, y = Sleep_Efficiency, color = Sleep_Time_Category)) +
  geom_point() +
  facet_wrap(~ Sleep_Time_Category) +
  labs(title = "Scatterplots of Sleep Efficiency vs Age for Different Sleep Time Categories",
       x = "Age",
       y = "Sleep Efficiency") 
  



# Ensure that Gender is a factor
Sleep_Efficiency$Gender <- as.factor(Sleep_Efficiency$Gender)

# Fit the MLR model with the categorical variable Gender
model <- lm(Sleep_Efficiency ~ Age + Awakenings + Sleep.duration + 
              REM.sleep.percentage + Deep.sleep.percentage + Alcohol.consumption + 
              Caffeine.consumption + Gender, data = Sleep_Efficiency)

# Get the summary of the model
summary(model)

# Ensure that the categorical variables are factors
Sleep_Efficiency$Gender <- as.factor(Sleep_Efficiency$Gender)
Sleep_Efficiency$Sleep_Time_Category <- as.factor(Sleep_Efficiency$Sleep_Time_Category)
# Fit the MLR model with categorical variables
model_with_categories <- lm(Sleep_Efficiency ~ Age + Awakenings + Sleep.duration +
                              REM.sleep.percentage + Deep.sleep.percentage +
                              Alcohol.consumption + Caffeine.consumption +
                              Gender + Sleep_Time_Category,
                            data = Sleep_Efficiency)
# Get the summary of the model
summary(model_with_categories)
# Plot the fitted models for each category
ggplot(Sleep_Efficiency, aes(x = Age, y = Sleep_Efficiency, color = Sleep_Time_Category)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ Sleep_Time_Category) +
  labs(title = "Sleep Efficiency vs Age for Different Sleep Time Categories",
       x = "Age", y = "Sleep Efficiency")

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Ensure that Sleep_Time_Category is a factor if it isn't already
Sleep_Efficiency$Sleep_Time_Category <- as.factor(Sleep_Efficiency$Sleep_Time_Category)

# Fit the MLR model with interaction terms between Age and Sleep Time Category
interaction_model <- lm(Sleep_Efficiency ~ Age * Sleep_Time_Category +
                          Awakenings + Sleep.duration +
                          REM.sleep.percentage + Deep.sleep.percentage +
                          Alcohol.consumption + Caffeine.consumption +
                          Gender + Bedtime + Wakeup.time, 
                        data = Sleep_Efficiency)

summary(interaction_model)

ggplot(Sleep_Efficiency, aes(x = Age, y = Sleep_Efficiency, color = Sleep_Time_Category)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = Sleep_Time_Category), formula = y ~ x) +
  facet_wrap(~ Sleep_Time_Category) +
  labs(title = "Sleep Efficiency vs Age for Different Sleep Time Categories",
       x = "Age",
       y = "Sleep Efficiency") +
  theme_minimal()




library(tidyverse)

Sleep_Efficiency$Gender <- as.factor(Sleep_Efficiency$Gender)

# Fit the MLR model with interaction terms
full_model <- lm(Sleep_Efficiency ~ Age * Sleep_Time_Category + Awakenings * Sleep_Time_Category +
                   Sleep.duration * Sleep_Time_Category + REM.sleep.percentage * Sleep_Time_Category +
                   Deep.sleep.percentage * Sleep_Time_Category + Alcohol.consumption * Sleep_Time_Category +
                   Caffeine.consumption * Sleep_Time_Category + Gender * Sleep_Time_Category + 
                   Bedtime * Sleep_Time_Category + Wakeup.time * Sleep_Time_Category, 
                 data = Sleep_Efficiency)
summary(full_model)

# Visualize the fitted models for each category
ggplot(Sleep_Efficiency, aes(x = Age, y = Sleep_Efficiency, color = Sleep_Time_Category)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = Sleep_Time_Category), se = FALSE) +
  facet_wrap(~ Sleep_Time_Category) +
  labs(title = "Fitted Models for Sleep Efficiency by Sleep Time Categories", x = "Age", y = "Sleep Efficiency")






