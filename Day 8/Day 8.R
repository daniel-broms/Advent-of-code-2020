#Day 8 - run assembler-like code, stop when we encounter loop

#Strategy:
#Import thee data to tibble. Split by space to separate instruction from data. 
#Add a column with "Execution count" to track when a line has been executed twice.
library(tidyverse)
library(stringr)

input <- readLines("Day 8/input.txt")  #629 strings in a vector
#input <- readLines("Day 8/test part 2.txt") #test program where we know the solution

t <- tibble(input)                                 #convert to a tibble
t$operation <- str_sub(t$input, 1,3)               #split out operations
t$argument <- strtoi(str_sub(t$input, 5))          #split out argument, convert argument to integer
t$exec_count <- 0                                  #add a column to track execution count
t <- rowid_to_column(t, "row_num")                 #add row number as a column
t <- select (t, c("row_num", "operation", "argument", "exec_count"))  #remove unwanted columns.

#initialize instruction pointer and accumulator.
ptr <- 1
acc <- 0

#iteratively process instructions in the list. Keep track of instruction pointer (1) and accumulator (0).
#if nop : just increase the pointer +1, increase execution count.
#if acc : increease pointer +1, add argument to accumulator.
#if jmp : add argument to pointer. increase execution count.


while(1==1){
  t$exec_count[ptr] <-  t$exec_count[ptr] + 1
  
  if(t$exec_count[ptr] > 1) {break}
  
  if(t$operation[ptr] == 'nop'){
    ptr <- ptr + 1
  } else  if(t$operation[ptr] == 'acc'){
    acc <- acc + t$argument[ptr]
    ptr <- ptr + 1
  } else  if(t$operation[ptr] == 'jmp'){
    ptr <- ptr +t$argument[ptr]
  }
}

#acc is 2051 when the loop breaks = correct answer!


###############################################################

t[1,'operation']    #returns a 1x1 tibble
t[[1,'operation']]  #returns a scalar
t$operation[1]      #returns a scalar

t$exec_count[ptr]
t$operation[ptr]
t$argument[ptr]
