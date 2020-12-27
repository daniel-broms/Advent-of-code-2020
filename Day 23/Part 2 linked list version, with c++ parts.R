#Day 3 Part 2 - use a vector with pointers to the next cup. The vector index = cup label.
#Linked list version. The vector version was too slow for part 2, would have taken 80 hours to complete.

nrofcups <- 1000000     #1 million
iterations <- 10000000  #10 million

#Replace this function with wrapC written in c++      RESULT : the function took LONGER time since there are so many calls, and the overhead of a c++ call seems to be higher than an R function call.
# wrap<- function(x,nrofcups){
#   if(x>nrofcups){x<- x-nrofcups}
#   if(x<1){x<- x+nrofcups}
#   return(x)
# }

library(Rcpp)
sourceCpp("Day 23/wrapC.cpp")    #Note that this will also run the R chunks in file wrapC.cpp, producing "9" in this example!

#Initialize the vector and starting cup:
cups <- c(2,5,8,6,4,7,3,9,1)         #cups vector : The index represents the cup label, and the value is a pointer to the next cup value.

#part 2 sample data version with one million cups. The 7 now points to 10 instead of 3. positions 10and forward point to itself +1. Position 1000000 points to the first cup = 3.
cups <- c(2,5,8,6,4,7,10,9,1)        #sample input 389125467
cups <- c(cups, 11:nrofcups, 3)
ccup <- 3 #we being with cup 3.

#part 2 input:
cups <- c(9,10,4,6,3,2,5,7,8)        #input 198753462
cups <- c(cups, 11:nrofcups, 1)
ccup <- 1 #we being with cup 1.

MakeMoves <- function(iterations){
  for(move in 1:iterations){
  
    #Find the three cups after the current cup:
    n1 <- cups[ccup]    #the next cup is the one that the current cup points to
    n2 <- cups[n1]      #the next cup is the one that n1 points to 
    n3 <- cups[n2]      #the next cup is the one that n2 points to 
    
    #Find the destination cup as current cup -1.
    dest_cup <- wrapC(ccup - 1, nrofcups)
    
    #If this is one of the cups to move : subtract 1 until it is not.
    while(dest_cup == n1 | dest_cup == n2 | dest_cup == n3){
      dest_cup <- wrapC(dest_cup - 1, nrofcups)
    }
    
    #"Move" cups n1/n2/n3 by changing the destination cup to point to n1 and n3 to point to the cup after the destination cup.
    cups[ccup]     <- cups[n3]           #The current cup is now followed by the cup which was after n3.  
    cups[n3]       <- cups[dest_cup]
    cups[dest_cup] <- n1
    
    #Set the next current cup as the cup which the current cup points to  
    ccup <- cups[ccup]
  }
  
  #The answer is the labels of the two cups following 1 multiplied by each other : 693659135400
  return(cups[1] * cups[cups[1]])
}

MakeMoves(iterations)   #693659135400



