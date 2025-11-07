#!/usr/bin/env Rscript

# Generate random data
set.seed(42)  # For reproducibility
data <- rnorm(100, mean=50, sd=10)

cat("Sample size:", length(data), "\n")
cat("Mean:", mean(data), "\n")
cat("Median:", median(data), "\n")
cat("Standard deviation:", sd(data), "\n")
cat("Variance:", var(data), "\n")
cat("Min:", min(data), "\n")
cat("Max:", max(data), "\n")
cat("Range:", range(data), "\n")

# Quantiles
cat("\nQuantiles:\n")
print(quantile(data))

# Summary
cat("\nSummary statistics:\n")
print(summary(data))

# Correlation between two variables
x <- c(1, 2, 3, 4, 5)
y <- c(2, 4, 5, 4, 5)
cat("\nCorrelation:", cor(x, y), "\n")

# Linear regression
model <- lm(y ~ x)
cat("\nLinear regression:\n")
print(summary(model))

# T-test
group1 <- rnorm(50, mean=50, sd=10)
group2 <- rnorm(50, mean=55, sd=10)
t_test_result <- t.test(group1, group2)
cat("\nT-test p-value:", t_test_result$p.value, "\n")
