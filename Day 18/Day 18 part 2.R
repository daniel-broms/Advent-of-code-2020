#Day 18:  V2 Evaluate the expression on each line of the homework; what is the sum of the resulting values?
library(stringr)
input <-readLines("Day 18/input.txt")


#evaluate the expression left to right, start at 0. 
#In part 2, + is evaluated before * 
#We need to check is the NEXT operator is a * If so, push the current value to a stack. Multiply all numbers in the stack when we are done with all +.
#instead of acc being a single number, acc is now a stack.


evale <- function(acc,  e){
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
    acc[1] <- acc[1] + val   #add this to the top of the stack
  } else if (op == '*'){
    #Add acc to the list of numbers we need to find the product of after the expression is done.
    acc <- c(val, acc)        #add val as a new number on  the top of the stack.
  } else {
    print("ERROR")  #we should not get here
  }
  
  #If we have no string left we are done, otherwise call this recursively.
  if(pos >= str_length(e)){
    result <-  prod(acc)    #return the product of 
    return(c(result, ''))
  }
  else{
    return(evale(acc,str_sub(e,pos)))
  }
}


#Extract the inner expression beginning at pos 1 of the given parenthesis.
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


# evale(0, '+ ((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2')    #test the function on one expression. 


evalx <- function(e){
  as.numeric(evale(0, paste('+',e))[1])
}

#Run on all input
r <-sapply(input, evalx)
print(sum(r), digits=16)  #88782789402798 : Corrrect!


