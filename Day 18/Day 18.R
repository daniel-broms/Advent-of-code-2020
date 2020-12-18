#Day 18:  V2 Evaluate the expression on each line of the homework; what is the sum of the resulting values?
library(stringr)
input <-readLines("Day 18/input.txt")


#evaluate the expression left to right, start at 0. 
#Get the next number (always single-digit.) If this digit is a parent : Find the corresponding closing parens and evaluate this expression first.
#then add or multiply the result of the first number/expr with the next number/expr. Advance the pointer to after the nect number/expr and continue.

#we can cal "eval" recursively to deal with the expressions in parens.
#we can keep state as a value (acc) whihc is the value we have parsed so far and the remainig expression.
#accumuate acc as we shorten the expession.


evale <- function(acc, e){
  pos <- 1                                        #start at the beginning of the expre
  
  #get next operator:
  op <- str_sub(e,pos,pos)
  pos <- pos + 2
  val <- 0
  
  #Get next term:
  l <- str_sub(e,pos,pos)                         #get the next letter in turn
  if(str_detect(l,'\\d')){                        #if the next letter is a digit:
    val  <-strtoi(l)                              #this is the next term.
    pos <- pos + 2                                #advance  position to the next operator
  }
  else if(l == '(') {
    #we have a parens expression.
    #extract the whole parens expression beginning at pos, remove the outer parens, call eval recursively on this. 
    inner_e <- getparens(str_sub(e, pos))
    val <- as.numeric(evale(0, paste("+",inner_e))[1])       #get the value if the inner term recursively.
    pos <- pos + str_length(inner_e) + 3          #advance  position to the next operator 
  }
  

  #calculate the result of operator applied to the term and the ACC input value.
  if(op == '+'){
    acc <- acc + val
  } else if (op == '*'){
    acc <- acc * val
  } else {
    print("ERROR")  #we should not get here
  }
  
  #If we have no string left we are done, otherwise call tihs recursively.
  if(pos >= str_length(e)){
    return(c(acc, ''))
  }
  else{
    return(evale(acc, str_sub(e,pos)))
  }
}


#Extract the inner expression beginning at pos 1 of the given parens.
getparens <- function(e){
  pos <- 1
  pcount <- 1
  while(pcount > 0){
    pos <- pos + 1
    l <- str_sub(e,pos,pos)
    if(l == ')'){pcount <- pcount - 1}
    if(l == '('){pcount <- pcount + 1}
  }
  return(str_sub(e,2,pos-1))
}

evalx <- function(e){
  as.numeric(evale(0, paste('+',e))[1])
}

#Run on all input
r <-sapply(input, evalx)

print(sum(r), digits=16)  #5374004645253 : Corrrect!
