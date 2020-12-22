# Day 22 : What is the winning player's score?
library(stringr)
library(tidyverse)

#####################################################################################
#Part 2: Recursive Combat
#End game if a card list is identical to any previous round card list. Player 1 wins.
#End game if a player has all cards.

#if any player has less cards in their deck (not counting p[1]!) than the value of the next card p[1] : The winner is player with the highest card.

#Otherwise (if both players have at least as many cards as the value of their P[1]) : play a (recursive) sub-game:
  #copy the next n cards in the deck (p[2]..p[5] if n = 4) where n = the value of p[1].
  #play a sub-game with the copies.

input <- readLines("Day 22/test input.txt")
p1 <- input[2:6] %>% strtoi
p2 <- input[9:13] %>% strtoi

input <- readLines("Day 22/input.txt")
p1 <- input[2:26] %>% strtoi
p2 <- input[29:53] %>% strtoi

calc_score <- function(p1,p2){
  if(length(p1) > length(p2)){p <- p1} else {p <- p2}
  score <- 0
  for(i in 1:length(p)){
    score <- score + (p[i] * (length(p) - i + 1))
  }
  return(score)
}

#return who won this round (1 or 2), including sub-games.
playround <- function(p1, p2){
  
  #if any of the players has less cards left than their p1 : the winner it the one with the highest card in p1.
  if(length(p1) -1 < p1[1] | length(p2) -1 < p2[1]){
    if(p1[1] > p2[1]){return(1)} else {return(2)}
  }
  
  #otherwise both players have more cards left then their p[1]. Play a recursive sub-game without the first card P[1] and with the number of cards equal to p[1]
  p1s <- p1[2:(1+p1[1])]
  p2s <- p2[2:(1+p2[1])]
  return(playgame(p1s, p2s))
}


#play a whole game until one payer has won. return which player has won (and the card decks to calculate scores?) 
playgame <- function(p1, p2){
  
  statelist <- list(-1) #keep track of previous rounds in this (sub-)game.
  
  #play until someone runs out of cards.
  while(length(p1) > 0 & length(p2 > 0 )){

    #if there was a previous round in this game that had exactly the same cards in the same order in the same players' decks, the game instantly ends in a win for player 1.
    #check if the cards vector has been seen before. (use 999 to separate p1 and p2.)
    p <- c(p1, 999,  p2)
    for(i in 1:length(statelist)){
      if(length(p) == length(statelist[[i]]) && all(p == statelist[[i]])) {
        print("breaking endless loop")
        return   #Player 1 wins when this happens.
      }
    }
    statelist <- c(statelist, list(p))

    winner <- playround(p1,p2)

    if(winner == 1){p1 <- c(p1, p1[1], p2[1])} else{p2 <- c(p2, p2[1], p1[1])}
    p1 <- p1[-1]
    p2 <- p2[-1]
    
    #end if a player has all cards : return the winner.
    if(length(p1)==0 | length(p2)==0){
      print(paste("score : ", calc_score(p1, p2)))
      return(winner)}
  }  
}

#play the whole game Works for sample input => score 291 for player 2! Real input=> 33369 - Correct! took forever...
playgame(p1,p2)





