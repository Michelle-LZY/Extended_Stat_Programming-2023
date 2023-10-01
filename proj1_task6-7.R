#s<-("You, may. copy; it! give: it? away, or. re-use; it! under: the? terms,
#of. the; Project!")
#a<-strsplit(s," ",)[[1]]

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
word <- c(b[t[1,1]],b[t[1,2]])


ty1 <- t[1,1]
ty2 <- t[1,2]

probability <- function(name,freq){
  pb <- c()
  for (i in 1:length(name)){
    pb <- append(pb,freq[i]/sum(freq))
  }
  return (pb)
}

for (n in 1:48){
  tun <- c()
  tun1<- c()
  for (i in 1:nrow(t)) {
    if (t[i,1] == ty1 && t[i,2] == ty2){
      tun <- rbind(tun,t[i,3])
      tun1<- rbind(tun1,t[i,])
    }
  }
  print(tun1)
  freq1 <- as.numeric(table(tun))
  name1 <- as.numeric(names(table(tun)))
  
  
  print(probability(name1,freq1))
  
  if(length(probability(name1,freq1))==1){
    ty3<-as.numeric(tun[1])
  }
  else{
    ty3 <- sample(as.numeric(names(table(tun))),size=1,replace = FALSE, prob = probability(name1,freq1))
  }
  
  print(ty3)
  print(b[ty3])
  
  word <- append(word,b[ty3])
  ty1 <- ty2
  ty2 <- ty3
}
word


#######
ty1 <- t[120,1]
ty2 <- t[120,2]
word <- c(b[ty1],b[ty2])

probability <- function(name,freq){
  pb <- c()
  for (i in 1:length(name)){
    pb <- append(pb,freq[i]/sum(freq))
    return(pb)
  }
}

for (n in 1:48){
  tun <- c()
  for (i in 1:nrow(t)) {
    if (t[i,1] == ty1 & t[i,2] == ty2){
      tun <- rbind(tun,t[i,3])
    }
  }
  tun

  freq <- as.numeric(table(tun))
  name <- as.numeric(names(table(tun)))
  
  if(length(probability(name,freq))==1){
    ty3<-as.numeric(tun[1])
  }
  else{
    ty3 <- sample(name,size=1,replace = FALSE, prob = probability(name,freq))
  }
  
  word <- append(word,b[ty3])
  ty1 <- ty2
  ty2 <- ty3
}
word










