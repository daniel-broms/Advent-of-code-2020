# Day 22 : What is the winning player's score?
library(stringr)
library(tidyverse)

#read  input
#player 1card decks. Px[1] is the top card.
input <- readLines("Day 22/input.txt")
p1 <- input[2:26] %>% strtoi
p2 <- input[29:53] %>% strtoi

input <- readLines("Day 22/test input.txt")
p1 <- input[2:6] %>% strtoi
p2 <- input[9:13] %>% strtoi

#play until one player runs out of cards.
while(length(p1) > 0 & length(p2 > 0 )){
  
  #play a round
  if(p1[1] > p2[1]){
    p1 <- c(p1, p1[1], p2[1])
  } 
  else{
    p2 <- c(p2, p2[1], p1[1])
  }
  p1 <- p1[-1]
  p2 <- p2[-1]
}

#calculate score : 
score <- 0
for(i in 1:length(p1)){
  score <- score + (p1[i] * (length(p1) - i + 1))
  print(p1[i] * (length(p1) - i + 1))
}

score # answer! 34255


