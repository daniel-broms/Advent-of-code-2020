#D19 19 : Identify which messages conform to the specified rule 0.
library(stringr)
library(tidyverse)

#read test input:
input <- readLines("Day 19/test input.txt")
irules <- input[1:6]
irules <- str_replace_all(irules, "\"", "")
messages <- input[8:12]

#read real input
input <- readLines("Day 19/input.txt")
irules <- input[1:136]
irules <- str_replace_all(irules, "\"", "")
messages <- input[138:627]

#sort the rules by the index
t <- tibble(irules)
t$idx <- str_match(t$irules, "[:digit:]{1,3}:")
t$idx <- strtoi(str_sub(t$idx, 1, str_length(t$idx)-1))
t <- arrange(t, idx )
irules <- t$irules

#parse rules ineratively Replace each number with the rule it represents, within parens.
parserules <-function(pattern){
  #loop though the string, replace each number with the rule it represents.
  l <- unlist(str_split(pattern, " "))   #split the string to a vector with components. 
  l <- sapply(l, replaceidx)
  return(paste(l, sep="", collapse=""))
}

#return the specified rule
getrule <- function(x){
  rule <- irules[x+1]                                   #get rule x (remember R lists are 1-based!)
  rule <- str_replace(rule, "[:digit:]{1,3}:", "")      #strip the rule number part
  return(rule)
}

#if the input is a number : return the rule for that number surrounded by parens if it is another rule or wihtout parens if it is a or b.. Else, return the input.
replaceidx <- function(x){
  if(str_detect(x,"^[:digit:]{1,3}$")){
    r <- getrule(strtoi(x))
    if( str_trim(r) == "a" | str_trim(r) == "b") {
      return(r)
    }
    else {
      return(paste( " (", r, ") "))
    }
  }           
  else{ 
    return(x)
  }
}

#Prime the function with rule 0, iterative expand the pattern until it does not change.
pattern <- " 0 "
pp <- ""
while(!pattern == pp){
  pp <- pattern
  pattern <- parserules(pattern)
}
pattern <- paste('^', pattern, '$', sep="")      #add start and end anchors so that we match the whole string.
length(messages[str_detect(messages,pattern)] )  #find messages that match the pattern : 2 messages for test input, 233 for real : Correct!


