#Day 10. Find the differences in adapter chain. 
#What is the number of 1-jolt differences multiplied by the number of 3-jolt differences?
library(tidyverse)

#Strategy : Import the list. Add a "0" entry (the charging outlet). Sort the list. 
#Add a lagged column with diff from previous column. Calculate (nr of 1 diffs) * (nr of 3 diffs +1). 
#(the +1 represents the final diff to the device, which is not in the list.)

input <- readLines("Day 10/input.txt")                            #read 95 strings to a vector
t <- tibble(jolt = sort(strtoi(c(input, '0'))))                   #Add a zero representing the outlet, convert to integers, sort, convert to tibble
t$diff <- t$jolt - lag(t$jolt, 1)                                 #Compute lagged diff, add as a new column "diff" 
(nrow(filter(t, diff == 1))) * (nrow(filter(t, diff == 3)) + 1)   #Compute (nr of 1 diffs) * (nr of 3 diffs + 1) = 1980 = correct answer


