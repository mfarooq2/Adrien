# Data provided by you
income_levels <- c(0, 13, 36, 65, 91, 104)
joint_prob_men <- c(0.01, 0.05, 0.06, 0.16, 0.14, 0.15)
joint_prob_women <- c(0.00, 0.09, 0.11, 0.12, 0.07, 0.04)

# 1. Marginal distributions of gender and income, expected yearly income, and sqrt of variance
P_men <- sum(joint_prob_men)
P_women <- sum(joint_prob_women)

# Marginal distribution of income
marginal_prob_income <- P_men * joint_prob_men + P_women * joint_prob_women

# Expected yearly income
expected_income <- sum(income_levels * marginal_prob_income)

# Variance and Standard Deviation
variance_income <- sum((income_levels^2) * marginal_prob_income) - expected_income^2
std_income <- sqrt(variance_income)

# 2. Conditional distributions for men and women and CDF visualization
conditional_income_men <- joint_prob_men / P_men
conditional_income_women <- joint_prob_women / P_women

# CDFs
cdf_men <- cumsum(conditional_income_men)
cdf_women <- cumsum(conditional_income_women)

# Plotting CDF
plot(income_levels, cdf_men, type = "o", col = "blue", ylim = c(0, 1),
     xlab = "Income", ylab = "Cumulative Probability", main = "CDF of Income by Gender")
lines(income_levels, cdf_women, type = "o", col = "red")
legend("bottomright", legend = c("Men", "Women"), col = c("blue", "red"), lty = 1)

# 3. Conditional expectations and square roots of variances
expected_income_men <- sum(income_levels * conditional_income_men)
variance_income_men <- sum((income_levels^2) * conditional_income_men) - expected_income_men^2
std_income_men <- sqrt(variance_income_men)

expected_income_women <- sum(income_levels * conditional_income_women)
variance_income_women <- sum((income_levels^2) * conditional_income_women) - expected_income_women^2
std_income_women <- sqrt(variance_income_women)

# 4. Covariance and correlation between income and gender
# Treat male as 0 and female as 1 for gender dummy variable
gender_dummy <- c(rep(0, length(joint_prob_men)), rep(1, length(joint_prob_women)))
income_all <- rep(income_levels, 2) # Combine income levels for both men and women
joint_prob_all <- c(joint_prob_men, joint_prob_women) # Combined joint probabilities

# Covariance calculation
expected_income_total <- sum(income_all * joint_prob_all)
expected_gender <- sum(gender_dummy * joint_prob_all)
cov_income_gender <- sum(income_all * gender_dummy * joint_prob_all) - (expected_income_total * expected_gender)

# Variance of gender and income
var_gender <- sum((gender_dummy^2) * joint_prob_all) - expected_gender^2
var_income <- sum((income_all^2) * joint_prob_all) - expected_income_total^2

# Correlation
cor_income_gender <- cov_income_gender / (sqrt(var_gender) * sqrt(var_income))

# 5. Test for independence
# Two variables are independent if their joint distribution equals the product of their marginals
# Calculate the product of marginals
marginal_gender_men <- rep(P_men, length(income_levels))
marginal_gender_women <- rep(P_women, length(income_levels))

product_marginals_men <- marginal_gender_men * marginal_prob_income
product_marginals_women <- marginal_gender_women * marginal_prob_income

# Check independence by comparing joint probabilities with product of marginals
independence_test_men <- all(joint_prob_men == product_marginals_men)
independence_test_women <- all(joint_prob_women == product_marginals_women)

# Output results
cat("1. Marginal Probability for Men: ", P_men, "\n")
cat("   Marginal Probability for Women: ", P_women, "\n")
cat("   Marginal Distribution of Income: ", marginal_prob_income, "\n")
cat("   Expected Yearly Income: ", expected_income, "\n")
cat("   Variance of Income: ", variance_income, "\n")
cat("   Standard Deviation of Income: ", std_income, "\n\n")

cat("2. Conditional Distribution for Men:\n", conditional_income_men, "\n")
cat("   Conditional Distribution for Women:\n", conditional_income_women, "\n\n")

cat("3. Expected Yearly Income for Men: ", expected_income_men, "\n")
cat("   Variance of Income for Men: ", variance_income_men, "\n")
cat("   Standard Deviation of Income for Men: ", std_income_men, "\n")
cat("   Expected Yearly Income for Women: ", expected_income_women, "\n")
cat("   Variance of Income for Women: ", variance_income_women, "\n")
cat("   Standard Deviation of Income for Women: ", std_income_women, "\n\n")

cat("4. Covariance between Income and Gender: ", cov_income_gender, "\n")
cat("   Correlation between Income and Gender: ", cor_income_gender, "\n\n")

cat("5. Are income and gender independent for men? ", independence_test_men, "\n")
cat("   Are income and gender independent for women? ", independence_test_women, "\n")
