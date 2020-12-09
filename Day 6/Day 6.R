#For each group, count the number of questions to which anyone answered "yes". What is the sum of those counts?
#Re-phrased : find the number of unique characters in each group. Find the sum of this over all groups.

#Strategy : Import data line by line. Concatenate the data to a single string until we reach an empty line
#When we reach an empty line (or end of file):
#  split the string into a vector of individual characters using strsplit(x, split=''). 
#  Find the unique values with unique(). Find the length of this vector. 
#  Accumulate this length in a variable. The answer is the result of this variable after all lines have been imported.

input <- readLines("Day 6/input.txt")   #2172 lines are read to a vector
input <- readLines("Day 6/test.txt")    #Test input where we know the answer = 11.


tally <- function(input){
  input <- c(input, "")  #Add an empty line to the end
  s <- ''
  total <- 0  

  for(line in input){
    
    #if we have an empty line: find how many unique characters we had in the last group, accumulate the total. 
    if(line==''){
      sv <- unlist(strsplit(s, ''))
      sv <- unique(sv)
      total <- total + length(sv)  
      s <-''
    }
    else{
      s <- paste(str_trim(s),line, sep="") #accumulate the trimmed string in the group
    }
  }
  return(total)
}

tally(input)    #Total = 6590 : Correct answer!


###############################################################################################################
#Part 2 : modify the function to only count the answers which everyone in a group answered yes to.
#Instead of accumulating the string, for each new person only keep the characters which already exist in the list.


tally2 <- function(input){
  input <- c(input, "")  #Add an empty line to the end
  s <- NULL
  total <- 0  
  
  for(line in input){
    
    #if we have an empty line: find how many unique characters we had in the last group found for all persons. 
    if(line==''){
      total <- total + length(s)  
      s <-NULL
    }
    else{
      line <- unlist(strsplit(line, ''))
      if(is.null(s)){
        s <- line
      }
      else{
        s <- intersect(s, line)  #find the intersection between the characters we already have and the new characters.
      }
    }
  }
  return(total)
}

tally2(input)    #Total 3288

