#Day 9 : Find the first number in a list which cannot be expressed as the sum of the previous x (25 or 5) numbers.
#Add a column with "Execution count" to track when a line has been executed twice.
library(tidyverse)
library(stringr)

input <- readLines("Day 9/input.txt")       #1000 strings in a vector
input <- readLines("Day 9/test input.txt")  #20 strings in a vector

input <- strtoi(input) #convert to integers

pa  <- 5        #pre-ample length


#define a function to check the given number in the given list - is the number the some of two different previous x values.
check_number <- function(list, pos, pa){
  for(i in seq(pos-pa, pos-1)){
    for(j in seq(pos-pa, pos-1)){
      if(list[i] + list[j] == list[pos] & !i==j){
        return(TRUE)
      }
    }
  }
  return(FALSE)  #no pair was found
} 

#call the function for all rows after pre-able
for(pos in seq(pa+1, length(input))){
  if(!check_number(input, pos, pa)) {
    print(pos)
    print(input[pos])
    break}
}

#Position 562 = 70639851 : Correct answer!

##############################################################
#Part 2 : find a contiguous set of at least two numbers in your list which sum to the invalid number from step 1.
#To find the encryption weakness, add together the smallest and largest number in this contiguous range; in this example, these are 15 and 47, producing 62.

#Strategy : Start at first position, accumulate sum until we get our number (correct answer) of the total > our number (increment startpos and try again).

invalidnumber <- 70639851   #127 for test input
startpos <- 0
acc <- 0


while(!acc == invalidnumber){
  acc <- 0
  startpos <- startpos +1
  for(pos in seq(startpos, length(input))){
    acc <- acc + input[pos]
    if(acc == invalidnumber){
      print(startpos)            #find the start position
      print(pos)                 #find the end position
      print(min(input[seq(startpos,pos)]) + max(input[seq(startpos,pos)]))   #print part 2 answer 
      break
    }
    if(acc > invalidnumber){break}  #try next startpos
  }
}

#TODO : find the min and max value in the given interval, find their sum = answer.
min(input[seq(startpos,pos)]) + max(input[seq(startpos,pos)])
