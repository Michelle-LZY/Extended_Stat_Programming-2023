# Group members: Bo Gao(s2511232), Zheyue Lin(s2519324) and Xinran Zhu(s2508695)
# Bo Gao(s2511232) completed pre-processing of orginal text, designed 'split_punct()' and created vector a
# Zheyue Lin(s2519324) counted the frequencies of unique words and made matrix T and P
# Xinran Zhu(s2508695) and Bo Gao(s2511232) collaborated on building the model to generate 50-word section
# Bo Gao(s2511232) crafted the comparison section
# Zheyue Lin(s2519324) modified the vector b 
# Our tasks are nearly evenly distributed, we communicated well and we supported each other during this journey

#Task 1-3
# setwd("D:/Edinburgh University/ESP/Work/") ## comment out of submitted
a <- scan("4300-0.txt",what="character",skip=73,nlines=32858-73)
a <- gsub("_(","",a,fixed=TRUE) ## remove "_("



#Task 4
# The function of split_punct for separate punctuation marks
split_punct<-function(punctuation){
  
  # Get the location of punctuation in a
  location<-grep(punctuation,a,fixed = TRUE)
  # Create new vector to store separated words
  a_new<-rep("",length(a)+length(location))
  # Find the location for punctuation in a
  insert_location<-location+1:length(location)
  # Get vector a without punctuation
  a_nopunctuation<-gsub(punctuation,"",a,fixed = TRUE)
  # Insert punctuation in the returned vector
  a_new[insert_location]<-punctuation
  # Insert words in the returned vector
  a_new[-insert_location]<-a_nopunctuation
  
  return(a_new)
}



# Task 5
punctuation_list<-c(",",".",";","!",":","?")
for (i in punctuation_list){
  # Use split_punct in a
  a<-split_punct(i)
}



# Task 6
# Convert the vector to lowercase
lowercase_vector <- tolower(a)
# Find unique words
unique_vector <- unique(lowercase_vector)
# Use match() to determine indices
indices <- match(lowercase_vector,unique_vector)
# Use tabulate to count occurrences of each unique word
counts <- tabulate(indices)
# Sorted the frequencies decreasingly, then choose the 1000th frequency as the threshold
threshold <- sort(counts,decreasing = TRUE)[1000]
# Create a vector b of the m most commonly occurring words (m ≈ 1000)
b <- unique_vector[counts>=threshold]



# Task 7
# Create common words triplets matrix T
col_1 <- match(lowercase_vector,b)
col_2 <- match(lowercase_vector[2:length(lowercase_vector)],b)
col_3 <- match(lowercase_vector[3:length(lowercase_vector)],b)
# The number of rows are different for col_1, col_2 and col_3
# So we dropped the redundant rows
t <- cbind(col_1,col_2,col_3)[1:length(col_3),]
t <- t[-which(is.na(rowSums(t))),]
# Use the same idea to create common words pairs matrix P
p <- cbind(col_1,col_2)[1:length(col_2),]
p <- p[-which(is.na(rowSums(p))),]



# Task 8
# Get the frequency of words in b
freq_b<-counts[counts>=threshold]

# Define the function to get values from p matrix
func_p<-function(p_1){
  # Get the submatrix of p according to the first column
  submatrix_p<-p[p[,1]==p_1,]
  # If submatrix of p is empty, sample according to the frequency of words in b
  # If it has only a single row, then return the value in the second column in that row
  if (length(submatrix_p)==0){
    return(sample(c(1:length(b)),size=1,replace = FALSE, prob = freq_b))
  }else if (length(submatrix_p)==2){ 
    return (as.numeric(submatrix_p[2]))
  }
  # Else, there are more than two rows in the submatrix, then we have to use 'sample()'
  # Get the words and their frequencies in the second column in the submatrix
  freq_p <- as.numeric(table(submatrix_p[,2]))
  name_p <- as.numeric(names(table(submatrix_p[,2])))
  # If it has a single row, return the last value
  if(length(name_p)==1) return (as.numeric(as.numeric(name_p[1])))
  # Use sample() to get a value
  return (sample(name_p, size=1, replace = FALSE, prob = freq_p))
}

# Choose the beginning word for 50-words section form b
ty1 <- sample(b,size=1,replace = FALSE, prob = freq_b)
# Choose the second word from p
ty2 <- func_p(ty1)
# Put them into output list
word <- c(ty1,b[ty2])

# Sample the remaining 48 words
for (n in 1:48){
  # Get the submatrix of t according to the first and the second columns
  submatrix <- t[t[,1] == ty1 & t[,2] ==  ty2,]
  # If the submatrix is empty, sample according to matrix p 
  if (length(submatrix)==0){
    ty3<-func_p(ty2)
  }
  # If it has a single row, return that value
  else if(length(submatrix)==3){
    ty3<-as.numeric(submatrix[3])
  }
  else{
    # Get the words and their frequencies in the third column
    freq <- as.numeric(table(submatrix[,3]))
    name <- as.numeric(names(table(submatrix[,3])))
    # If it has a single item, return that value
    if(length(name)==1) ty3 <- as.numeric(as.numeric(name[1]))
    # Use 'sample()' to get a value
    else ty3 <- sample(name,size=1,replace = FALSE, prob = freq)
    
  }
  #put the value in output list
  word <- append(word,b[ty3])
  # Update the words used to begin a new iterative
  ty1 <- ty2
  ty2 <- ty3
}
cat("Task 8 output:\n",word)



#Task 9
#use sample() to get 50 words according the frequency of words in b
word_task9 <- sample(b,size=50,replace = TRUE, prob = freq_b)
cat("Task 9 output:\n",word_task9)



# Task 10
# Find the words in the common words' list
# that usually begin with a capital letter in the original text 
# and then substitute them in the matrix T and P
frequencies <- table(a)
# Modify vector
B<-b
# Iterative all the words in b
for (i in 1:length(b)){
  # Make this word beggin with a capital letter, e.g. apple -> Apple
  upper_word<-paste(toupper(substring(b[i], 1, 1)), substring(b[i], 2, nchar(b[i])), sep = "")
  # Check whether this word appears more often with a capital letter or not
  if (upper_word %in% a){
    if(b[i] %in% a){
      if (frequencies[upper_word]>frequencies[b[i]]){
        # If this word appears with a capital letter more often, 
        # we update the new word in the modified vector B
        B[i]<-upper_word
      } 
      # If in the original text this word only appears beginning with a capital
      # letter, then immediately update the new word
    }else{B[i]<-upper_word}
  }
}








