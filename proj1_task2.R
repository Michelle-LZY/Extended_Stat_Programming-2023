# This definition for a is just for testing.
# a<-c("tum","tee","tumpty","wibble","TUm","TeE","wobble","WoBBle")

# Convert the vector to lowercase
lowercase_vector <- tolower(a)
  
# Find unique words
b <- unique(lowercase_vector)
print(b) 
# (tum","tee","tumpty","wibble","wobble")

# Use match() to determine indices
indices <- match(lowercase_vector,b)

# Use tabulate to count occurrences of each unique word
counts <- tabulate(indices)
print(counts)
# (2,2,1,1,2)