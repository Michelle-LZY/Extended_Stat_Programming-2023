#Task 1-3
setwd("D:/Edinburgh University/ESP/Work/") ## comment out of submitted
a <- scan("p1.txt",what="character",skip=73,nlines=32858-73)
a <- gsub("_(","",a,fixed=TRUE) ## remove "_("


#Task 4
#the function of split_punct for separate punctuation marks
split_punct<-function(punctuation){
  
  #get the location of punctuation in a
  location<-grep(punctuation,a,fixed = TRUE)
  #create new vector to store separated words
  a_new<-rep("",length(a)+length(location))
  #find the location of a that punctuation is inserted
  insert_location<-location+1:length(location)
  #get the vector of a without punctuation
  a_nopunctuation<-gsub(punctuation,"",a,fixed = TRUE)
  # insert punctuation in the returned vector
  a_new[insert_location]<-punctuation
  # insert words in the returned vector
  a_new[-insert_location]<-a_nopunctuation
  
  return(a_new)
}


#Task 5
#create punctuation vector
punctuation_list<-c(",",".",";","!",":","?")
for (i in punctuation_list){
  # use split_punct in a
  a<-split_punct(i)
}


# Task 6
# This definition for a is only for testing
#a <- c("tum","COLD","tee","GREAT","HOT","tumpty","wibble","the","weather","is","hot","TUm","thE","puppy","looks","cute","TeE","wobble","WoBBle",'tHe',"apple","pie","is","great","the","weather","is","great","the","weather","is","cold","today")
# Convert the vector to lowercase
lowercase_vector <- tolower(a)
# Find unique words
unique_vector <- unique(lowercase_vector)
 
# Use match() to determine indices
indices <- match(lowercase_vector,unique_vector)
# Use tabulate to count occurrences of each unique word
counts <- tabulate(indices)

# Choose the 1000th frequency as the threshold after sorted the frequencies decreasingly
threshold <- sort(counts,decreasing = TRUE)[1000]
# Create a vector b of the m most commonly occurring words (m ≈ 1000)
b <- unique_vector[counts>=threshold]


# Task 7
# Create common words triplets matrix T
col_1 <- match(lowercase_vector,b)
col_2 <- match(lowercase_vector[2:length(lowercase_vector)],b)
col_3 <- match(lowercase_vector[3:length(lowercase_vector)],b)
t <- cbind(col_1,col_2,col_3)[1:length(col_3),]
t <- t[-which(is.na(rowSums(t))),]
# Use the same idea to create common words pairs matrix P
p <- cbind(col_1,col_2)[1:length(col_2),]
p <- p[-which(is.na(rowSums(p))),]


# Task 8
#get the frequency of items in b
freq_b<-counts[counts>=threshold]


#define the function about getting values from p matrix
func_p<-function(p_1){
  #get the submatrix of p according the first column
  submatrix_p<-p[p[,1]==p_1,]
  #if submatrix of p is empty, sample according to the frequency of items in b 
  if (length(submatrix_p)==0){
    return(sample(c(1:length(b)),size=1,replace = FALSE, prob = freq_b))
  }
  #if it has a single row, return the last value. Because it means the probablity of the value is 1.
  if (length(submatrix_p)==2){
    return (as.numeric(submatrix_p[2]))
  }
  #get frequency of items and unique values in second column in submatrix of p 
  freq_p <- as.numeric(table(submatrix_p[,2]))
  name_p <- as.numeric(names(table(submatrix_p[,2])))
  #if it has a single row, return the last value
  if(length(name_p)==1) return (as.numeric(as.numeric(name_p[1])))
  #use sample() to get a value
  return (sample(name_p,size=1,replace = FALSE, prob = freq_p))
}


#get the first value of 50-words section according to b and the second one according to p
ty1 <- sample(b,size=1,replace = FALSE, prob = freq_b)
ty2 <- func_p(ty1)
#put them in output list
word <- c(ty1,b[ty2])

#sample the remaining 48 words
for (n in 1:48){
  #get the submatrix of t according the first and second column
  submatrix <- t[t[,1] == ty1 & t[,2] ==  ty2,]
  #if submatrix of t is empty, sample according to the matrix of p 
  if (length(submatrix)==0){
    ty3<-func_p(ty2)
  }
  #if it has a single row, return the last value.
  else if(length(submatrix)==3){
    ty3<-as.numeric(submatrix[3])
  }
  else{
    #get frequency of items and unique values in third column in submatrix of t 
    freq <- as.numeric(table(submatrix[,3]))
    name <- as.numeric(names(table(submatrix[,3])))
    #if it has a single item, return the value
    if(length(name)==1) ty3 <- as.numeric(as.numeric(name[1]))
    #use sample() to get a value
    else ty3 <- sample(name,size=1,replace = FALSE, prob = freq)
    
  }
  #put the value in output list
  word <- append(word,b[ty3])
  #update the words used to match
  ty1 <- ty2
  ty2 <- ty3
}
print("task 8 output")
word


#Task 9
print("task 9 output")
#use sample() to get 50 words according the frequency of words in b
sample(b,size=50,replace = TRUE, prob = freq_b)


#Task 10









