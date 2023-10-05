# Stat Programming - Group Projects 1 - Due on 6th Oct
# Group members: Bo Gao, Zheyue Lin(s2519324) and Xinran Zhu(s2508695)
# Task 1: Create the group repo: Zheyue Lin
# Task 2-5: Bo Gao
- [x] Read txt file into R
- [x] Write a function 'split_punct', which takes a vector of words as input along with a punctuation mark
      Search for each word containing the punctuation mark  
      Remove it from the word  
      And add the mark as a new entry in the vector of words after the word it came from  
      This function will return the updated vector extract and then remove all the punctuations in the text and save the words vector as 'a'  
      **Note: Function 'grep,rep and gsub are the ones to use for this task. Some punctuation marks are special characters, which grep and gsub can interpret.'**
- [x] Use 'split_punct' function to seperate the punctuation marks
      

# Task 6-7: Zheyue Lin
- [x] Create a vector 'b' that hold the m most commonly occuring words. (m ≈ 1000)
- [x] Make the matrices of common word triplets(T) and pairs(P).

# Task 8: Xinran Zhu, Bo Gao
- [ ] Write code to simulate 50-word sections from your model.  
      Do this by using the model to simulate integers indexing words in vector 'b'.  
      Then print out the corresponding text with 'cat'.
      The 'sample' function should be used to select a word(index) with a given probability.

# Task 9: Bo Gao
- [ ] For comparison, simulate 50 word sections of text where the word probabilities are simply

# Task 10: Zheyue Lin
- [ ] Keep words having capital letter the most often in the original text.
**Note: Be careful not to mess up the frequencies**

# Some questions to ask:
1. About the working path
2. What about these special marks? 
    ![8216fae1ded322a2cf5138aa395b90f](https://github.com/Michelle-LZY/Proj1/assets/136700489/7837896c-3778-4043-8d0e-57a33e71647e)
    ![9086e44a00d91ab74d04a093a9a67cf](https://github.com/Michelle-LZY/Proj1/assets/136700489/c87fe0e8-3f28-4415-af41-3993c50d2170)
3. Where should we start the first two columns for genrate the sections 50-word sections?
4. To continuously generate the words in a way Where to break?
5. How to decide the first two words of task 9's section. How many sections we should generate.
6. How task 10 work!!! 
7. search 1000 words

