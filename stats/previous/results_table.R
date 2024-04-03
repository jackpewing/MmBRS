# Assuming MmGEE_ind is your fitted GEE model
summary_results <- summary(MmGEE_ind)

# Extracting the required components
# The names might vary based on the package, so adjust as necessary
estimates <- summary_results$coefficients[, "Estimate"]
std_err <- summary_results$coefficients[, "Std.err"]
wald_stat <- summary_results$coefficients[, "Wald"]
p_values <- summary_results$coefficients[, "Pr(>|W|)"]

# Creating a data frame for the table
results_df <- data.frame(Estimate = estimates, 
                         `Std. Err` = std_err, 
                         Wald = wald_stat, 
                         `Pr(>|W|)` = p_values)

# Optionally, round the values for a cleaner presentation
results_df <- round(results_df, 4)

