#Day 8 part 2
###################################################################################################################
#Part 2 : fix the program by changing one JMP to a NOP, or a NOP to a JMP. It should then terminate (i.e. not loop).
#What is the accumulator value  after the program has finished?

#Brute force approach : try changing, one at a time, ALL JMP to NOP, or NOP to JMP. 
#See which change results in program execution without ever executing the same instruction twice. 

# t %>% filter(operation %in% c('jmp','nop')) %>% nrow() #278 lines are jmp or nop.

input <- readLines("Day 8/test part 2.txt") #test program where we know the solution
input <- readLines("Day 8/input.txt")  #629 strings in a vector

t <- tibble(input)                                 #convert to a tibble
t$operation <- str_sub(t$input, 1,3)               #split out operations
t$argument <- strtoi(str_sub(t$input, 5))          #split out argument, convert argument to integer
t$exec_count <- 0                                  #add a column to track execution count
t <- rowid_to_column(t, "row_num")                 #add row number as a column
t <- select (t, c("row_num", "operation", "argument", "exec_count"))  #remove unwanted columns.

#declare a function to test a given program, return TRUE if it completes or FALSE if not
check_program <- function(x){
  lastline <- nrow(x)
  
  #re-initialize instruction pointer and accumulator
  ptr <- 1
  acc <- 0
  
  while(1==1){
    x$exec_count[ptr] <-  x$exec_count[ptr] + 1
    
    if(x$exec_count[ptr] > 1) {return(0)}        #We have been here before and have entered a loop ; abort!
    
    if(x$operation[ptr] == 'nop'){
      ptr <- ptr + 1
    } else  if(x$operation[ptr] == 'acc'){
      acc <- acc + x$argument[ptr]
      ptr <- ptr + 1
    } else  if(x$operation[ptr] == 'jmp'){
      ptr <- ptr + x$argument[ptr]
    }
    
    if(ptr == lastline + 1){return(acc)}                #We have reached the end of the program!
  }
}


#Call check_program with all version of t where one jmp has been substituted for a nop:
ptr_mod <- 1  #initialize pointer to where we last modified an instruction
result <- 0
while(1==1){
  
  #find the next jmp or nop row to try to change
  ptr_mod <- ptr_mod + 1
  while(t$operation[ptr_mod] == 'acc'){ptr_mod <- ptr_mod + 1 } #skip acc operations
  
  #make a copy of the original program (t), modify row ptr_mod in the copy
  p <- cbind(t)
  if(p$operation[ptr_mod] == "nop") {
    p$operation[ptr_mod] <- "jmp"
  } else {
    p$operation[ptr_mod] <- "nop"
  }
  
  #check if this copy of the program works - if so exit. If not, try again.
  result <- check_program(p)
  if(!result == 0) {break}
}

print(result)

#Result = 2304 when the above ends. Row 322 was modified (from jmp to nop) to make the program work.

rm(result)

#TODO : Change return value for check_program() : 0 could be the correct accumulator answer! We need a better signal that the check failed.