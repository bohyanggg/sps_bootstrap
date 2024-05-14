# Load necessary libraries
library(MASS) # for rlm function
library(boot) # for boot function

# Step 1: Read the data file for a specific student
data <- read.csv("./data/data_100529711.csv", header = TRUE)

# Step 2: Build robust linear regression model
model <- rlm(y ~ x1 + x2 + x3, data = data)

# Convert categorical variables to factors
data$x1 <- as.factor(data$x1)
data$x2 <- as.factor(data$x2)
data$x3 <- as.factor(data$x3)

# Step 3: Bootstrap for confidence intervals
boot_func <- function(data, indices) {
  # Subset data based on indices
  boot_data <- data[indices, ]
  
  # Reorder rows to match original data
  boot_data <- boot_data[match(rownames(data), rownames(boot_data)), ]
  
  # Fit robust linear regression model
  fit <- rlm(y ~ x1 + x2 + x3, data = boot_data)
  
  # Return coefficients
  return(coef(fit))
  
  
  bootstrap_results <- boot(data = data, statistic = boot_func, R = 1000)
  
  boot_ci <- boot.ci(bootstrap_results, type = "perc", index = 1:4)
  
  # Print bootstrap confidence intervals
  print(boot_ci)
}
  # Step 3: Backward elimination
  while(TRUE) {
    # Get confidence intervals for coefficients
    boot_ci <- boot.ci(bootstrap_results, type = "perc", index = 1:4)
    
    # Extract confidence intervals from the 'percent' component
    lower_bounds <- boot_ci$percent[1, 2]
    upper_bounds <- boot_ci$percent[1, 3]
    
    # Find the widest confidence interval
    widest_ci_index <- which.max(upper_bounds - lower_bounds)
    
    # Check if the widest confidence interval includes zero
    if(lower_bounds[widest_ci_index] <= 0 && upper_bounds[widest_ci_index] >= 0) {
      # If widest confidence interval includes zero, break the loop
      break
    } else {
      # Otherwise, remove the corresponding covariate and rebuild the model
      removed_covariate <- names(data)[widest_ci_index] # No need to adjust index
      data <- data[, -which(names(data) == removed_covariate)]
      bootstrap_results <- boot(data = data, statistic = boot_func, R = 1000)
      
      print(paste("Remaining covariates:", paste(names(data), collapse = ", ")))
    }
  }