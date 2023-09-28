# Task 6
# This definition for a is only for testing
a <- c("tum","COLD","tee","GREAT","HOT","tumpty","wibble","the","weather","is","hot","TUm","thE","puppy","looks","cute","TeE","wobble","WoBBle",'tHe',"apple","pie","is","great","the","weather","is","great","the","weather","is","cold","today")
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
threshold <- sort(counts,decreasing = TRUE)[2]
# Create a vector b of the m most commonly occurring words (m ≈ 1000)
b <- unique_vector[counts>=threshold]

# Task 7
# Create common words triplets matrix T
col_1 <- match(lowercase_vector,b)
col_2 <- match(lowercase_vector[2:length(lowercase_vector)],b)
col_3 <- match(lowercase_vector[3:length(lowercase_vector)],b)
t <- cbind(col_1,col_2,col_3)[-which(is.na(rowSums(cbind(col_1,col_2,col_3)))),]
# Use the same idea to create common words pairs matrix P
p <- cbind(col_1,col_2)[-which(is.na(rowSums(cbind(col_1,col_2)))),]

# Task 8















