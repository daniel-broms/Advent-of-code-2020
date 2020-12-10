#Day 10. Find the differences in adapter chain. 
#What is the number of 1-jolt differences multiplied by the number of 3-jolt differences?
library(tidyverse)

#Strategy : Import the list. Add a "0" entry (the charging outlet). Sort the list. 
#Add a lagged column with diff from previous column. Calculate (nr of 1 diffs) * (nr of 3 diffs +1). 
#(the +1 represents the final diff to the device, which is not in the list.)

input <- readLines("Day 10/input.txt")                            #read 95 strings to a vector
#input <- readLines("Day 10/test input.txt")  
t <- tibble(jolt = sort(strtoi(c(input, '0'))))                   #Add a zero representing the outlet, convert to integers, sort, convert to tibble
t$diff <- t$jolt - lag(t$jolt, 1)                                 #Compute lagged diff, add as a new column "diff" 
(nrow(filter(t, diff == 1))) * (nrow(filter(t, diff == 3)) + 1)   #Compute (nr of 1 diffs) * (nr of 3 diffs + 1) = 1980 = correct answer


#########################################################################
#Part 2 : What is the total number of distinct ways you can arrange the adapters to connect the charging outlet to your device? 
#Start with the outlet (0) and end with the device (=max adapter jolt+3 = 156) which are not in the input.
#The next adapter in the sequence can take an input 1, 2, or 3 jolts lower than its rating

#Note : 
#We only have 1 or 3 diff adapters.
#All 3-diff-adapters must be used. The only flexibility is deciding which 1-diff adapters we can exclude.
#All diff 1 adapters before a 3-adapter cannot be excluded (or else the jolt diff becomes 4).
#Each 1-diff adapter that we can exclude doubles the number of combinations.
#We can exclude any adapter which does not cause the difference in the adapter chain to be > 3.
#it is not possible to remove more than 3 1-adapters in a row because then the jolt diff becomes 4 : we need to exclude these combinations!

#Strategy : Find all individual sequences of excludeable adapters. In each sequence, find how many combinations there are:
# 1 adapter  : 2 combinations (2^1)
# 2 adapters : 4 combinations (2^2)
# 3 adapters : 7 combinations (2^3 - 1), where -1 represents all three adapters excluded which is not allowed.
# 4 adapters : 12 combinations(2^4 - 4), where -4 represents the four combinations not allowed (3 * 3 in a row + 1 all four in a row)

#Find sequences of excludable adapters.
#Add column "excludeable" = if diff = 1 and next diff is not 3.
t$excludable <- (t$diff == 1 & !lead(t$diff,1) == 3)

#list each "excludeable" sequence and how many in a row there are. Record each sequence length in "g".
acc <-0
g <- vector()
for(i in 3:nrow(t)-1){
  
  if(t$excludable[i]){
    acc <- acc + 1
  } else {
    if(!acc == 0) {
      g <- c(g,acc)
      }
    acc <- 0
  }
}
g <- c(g,acc)     #Add the last sequence 

#define a function to map sequence length to nr of allowed combinations
combs <- function(l) {
  return(case_when(l ==1 ~ 2 , l == 2 ~ 4, l ==3 ~ 7, l == 4 ~ 12 ))
}

#list the product of allowed combinations
print(prod(combs(g)), digits = 16)
