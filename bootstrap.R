# Load necessary libraries
library(MASS)
library(boot)

# Load dataset
data <- read.csv("./data/data_100529711.csv", header = TRUE)

# 1. linear regression model with the three covariates 
# Fit robust linear regression model
robust_model <- rlm(y ~ x1 + x2 + x3, data=data)
summary(robust_model)

# Bootstrap for confidence intervals
boot_func <- function(data, indices) {
  fit <- rlm(y ~ x1 + x2 + x3, data=data[indices, ])
  return(coef(fit))
}
boot_results <- boot(data, boot_func, R=1000)
boot_ci <- boot.ci(boot_results, type="bca")

# Extract bootstrap confidence intervals for coefficients
conf_intervals <- boot_ci$bca[1, c(3, 4)]

# Extract lower and upper bounds of confidence intervals
lower_bound <- conf_intervals[1]
upper_bound <- conf_intervals[2]

# 2. Backward elimination
# Identify non-significant coefficients
non_sig_vars <- which(lower_bound > 0 | upper_bound < 0)

# Identify significant variables
sig_vars <- paste("x", setdiff(1:3, non_sig_vars), sep = "")

# Construct new formula
new_formula <- as.formula(paste("y ~", paste(sig_vars, collapse = " + ")))

# Fit model with significant variables only
final_model <- rlm(new_formula, data = data)
summary(final_model)

# Step 3: Confidence intervals on the regression coefficients of final model
final_boot_func <- function(data, indices) {
  fit <- rlm(y ~ x1 + x2 + x3, data=data[indices, ])
  return(coef(fit))
}

final_boot_results <- boot(data, final_boot_func, R=1000)
final_boot_ci <- boot.ci(final_boot_results, type="bca")

# Step 4: Confidence interval on the mean response
# Define new data point
new_data <- data.frame(x1 = 14, x2 = 14, x3 = 14)
# Predict response for new data
response_pred <- predict(final_model, newdata = new_data, interval = "confidence", level = 0.95)
response_pred