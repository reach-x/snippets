#!/usr/bin/env Rscript

# Create a data frame
df <- data.frame(
  name = c("Alice", "Bob", "Charlie"),
  age = c(30, 25, 35),
  city = c("New York", "Los Angeles", "Chicago"),
  stringsAsFactors = FALSE
)

cat("Data Frame:\n")
print(df)

cat("\nDimensions:", dim(df), "\n")
cat("Number of rows:", nrow(df), "\n")
cat("Number of columns:", ncol(df), "\n")

cat("\nColumn names:", colnames(df), "\n")
cat("\nStructure:\n")
str(df)

cat("\nAccess column:\n")
print(df$name)

cat("\nAccess row:\n")
print(df[1, ])

cat("\nFilter rows where age > 26:\n")
print(df[df$age > 26, ])

cat("\nSelect specific columns:\n")
print(df[, c("name", "age")])

# Add new column
df$country <- "USA"
cat("\nAfter adding country column:\n")
print(df)

# Summary statistics
cat("\nSummary:\n")
print(summary(df))
