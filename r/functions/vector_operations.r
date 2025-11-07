#!/usr/bin/env Rscript

# Vector operations
numbers <- c(1, 2, 3, 4, 5)

cat("Numbers:", numbers, "\n")
cat("Length:", length(numbers), "\n")
cat("Sum:", sum(numbers), "\n")
cat("Mean:", mean(numbers), "\n")
cat("Median:", median(numbers), "\n")
cat("Max:", max(numbers), "\n")
cat("Min:", min(numbers), "\n")

# Add element
numbers <- c(numbers, 6)
cat("After append:", numbers, "\n")

# Remove last element
numbers <- numbers[-length(numbers)]
cat("After remove:", numbers, "\n")

# Vectorized operations
squared <- numbers^2
cat("Squared:", squared, "\n")

# Filtering
evens <- numbers[numbers %% 2 == 0]
cat("Even numbers:", evens, "\n")

# Apply function
doubled <- sapply(numbers, function(x) x * 2)
cat("Doubled:", doubled, "\n")

# Sequences
seq1 <- 1:10
cat("Sequence 1:10:", seq1, "\n")

seq2 <- seq(0, 1, by=0.1)
cat("Sequence 0 to 1 by 0.1:", seq2, "\n")
