#Day 19 : Identify which messages conform to the specified rule 0. 
#Parse the rule structure to a single regexp pattern, then apply this pattern to the messages to see which messages match.
library(stringr)
library(tidyverse)

#read real input
input <- readLines("Day 19/input.txt")
irules <- input[1:136]
irules <- str_replace_all(irules, "\"", "")
messages <- input[138:627]

#read test input, part 2:
input <- readLines("Day 19/test input part 2.txt")
irules <- input[1:31]
irules <- str_replace_all(irules, "\"", "")
messages <- input[33:47]

#sort the rules by the index!!
t <- tibble(irules)
t$idx <- str_match(t$irules, "[:digit:]{1,3}:")
t$idx <- strtoi(str_sub(t$idx, 1, str_length(t$idx)-1))
t <- arrange(t, idx )
irules <- t$irules


#parse rules iteratively. Replace each number with the rule it represents, within parens.
parserules <-function(pattern){
  #loop though the string, replace each number with the rule it represents.
  l <- unlist(str_split(pattern, " "))   #split the string to a vector with components. 
  l <- sapply(l, replaceidx)
  return(paste(l, sep="", collapse=""))
}

#return the specified rule
getrule <- function(x){
  #rule <- irules[x+1]                                  #get rule x (remember R lists are 1-based!)
  rule <- t$irules[t$idx ==x]                           #get the rule from the tibble instead
  rule <- str_replace(rule, "[:digit:]{1,3}:", "")      #strip the rule number part
  return(rule)
}

#if the input is a number : return the rule for that number surrounded by parens if it is another rule or wihtout parens if it is a or b.. Else, return the input.
replaceidx <- function(x){
  if(str_detect(x,"^[:digit:]{1,3}$")){
    r <- getrule(strtoi(x))
    if( str_trim(r) == "a" | str_trim(r) == "b" | str_length(r) > 40 ){
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

#Part 2 changes : 
# replace rule 8 with 42 +:
t$irules[t$idx == 8] <- "8: 42 +"

#replace rule 11 with  42 31 +
t$irules[t$idx == 11] <- "11: 42 31 +"   #Works with test input but not with real input!
t$irules[t$idx == 11] <- "11: ( 42  31 | 42 ( 42  31 | 42 ( 42  31 | 42 ( 42  31 | 42 ( 42  31 | 42  (  42 31 | 42 31 )  31 ) 31 ) 31 ) 31 ) 31 )"  #Expand rule 11 5 times, then replace without the self-reference!


#Prime the function with rule 0, iteratively expand the pattern until it does not change.
pattern <- " 0 "
pp <- ""
while(!pattern == pp){
  pp <- pattern
  pattern <- parserules(pattern)
}
pattern <- paste('^', pattern, '$', sep="")      #add start and end anchors so that we match the whole string.
length(messages[str_detect(messages,pattern)] )  #find messages that match the pattern : 2 messages for test input, 413 for real : NOT CORRECT! too high. After expansion of rule 11 : 396 : Correct!

######################################## Expand rule 11 5 times and hope that is enough: ###################
#replace rule 11 with  the recursive rule
t$irules[t$idx == 11] <- "11: 42 31 | 42 11 31"

#temporary version which does not expand 42 or 31:
replaceidx <- function(x){
  if( str_trim(x) == "42" | str_trim(x) == "31" ){return(paste("", x, ""))}
  if(str_detect(x,"^[:digit:]{1,3}$")){
    r <- getrule(strtoi(x))
    if( str_trim(r) == "a" | str_trim(r) == "b" | str_length(r) > 40 ){
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

#Expand 11 5 times, remove the 11 : "( 42  31 | 42 ( 42  31 | 42 ( 42  31 | 42 ( 42  31 | 42 ( 42  31 | 42  (  42 31 | 42 31 )  31 ) 31 ) 31 ) 31 ) 31 )"
pattern <- " 11 "
pattern <- parserules(pattern)
pattern

