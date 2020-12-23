#Day 23 : move cups, find final order.
library(stringr)

wrap<- function(x){
  if(x>9){x<- x-9}
  if(x<1){x<- x+9}
  return(x)
}

#find the remaining cups after cup x, not including cups in next_cups_index
rc <- function(x, next_cups_index){
  remaining_cups <- vector(mode="integer", length=5)
  for(i in 1:5){
    x <- wrap(x+1)
    while(x %in% next_cups_index){x <- wrap(x + 1)}  #skip cups in next_cups_index
    remaining_cups[i] <- x
  }
  return(remaining_cups)
}

input <- "198753462" #real input
#input <- "389125467" #sample input : digits 1-9.

cups <- strtoi(unlist(str_split(input, '')))
ccup <- 1  #current cup

for(move in 1:100){
  ccup_label <- cups[ccup]
  
  #find the next three cup indexes
  next_cups_index <- c(ccup + 1, ccup + 2, ccup + 3)
  next_cups_index <-sapply(next_cups_index, wrap)
  
  #find the destination cup : cups[ccup]-1. 
  dest_cup_label <- wrap(cups[ccup]-1)
  dest_cup_index <- which(cups==dest_cup_label)
  
   #if the destination cup is one of the three next cups: subtract dest_cup_label until it finds a cup not amoung those three.
  while(dest_cup_index %in% next_cups_index){
    dest_cup_label <- wrap(dest_cup_label-1)
    dest_cup_index <- which(cups==dest_cup_label)
  }

  #move the three next cups immediately clockwise of the destination cup. The new  list is (destination cup), (next three cups), (remaining cups).
  remaining_cups_index <- rc(dest_cup_index, next_cups_index)
  #do not include the removed cups in the remaining cups!
  
  cups <- c(cups[dest_cup_index], cups[next_cups_index], cups[remaining_cups_index])
  
  #find the next  cup : the index of the previous current cup + 1.
  ccup <- wrap(which(cups == ccup_label) + 1)
}

cups  #62934785 : Correct!
   
