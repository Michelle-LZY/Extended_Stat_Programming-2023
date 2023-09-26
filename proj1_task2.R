# Convert the vector to lowercase
lowercase_vector <- tolower(a)
  
# Find unique words
unique_vector <- unique(lowercase_vector)
print(unique_vector) 

# Use match() to determine indices
indices <- match(lowercase_vector,unique_vector)

# Use tabulate to count occurrences of each unique word
counts <- tabulate(indices)
print(counts)

# Choose the 1000th frequency as the threshold after sorted the frequencies decreasingly
threshold <- sort(counts,decreasing = TRUE)[1000]

# Create a vector b of the m most commonly occurring words (m ≈ 1000)
b <- unique_vector[counts>=threshold]

