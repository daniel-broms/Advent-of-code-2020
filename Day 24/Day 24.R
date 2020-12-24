#Day 24  : How may tiles are black after flippen x of them? Instructions include E, W, NE, NW, SE, SW. (Not N or S)
#Use a coord system to uniquely identify tiles : Nx, Ey.  S is negative N, W is negative E. NE is one a step east, one step north. E is two  steps east.
library(stringr)
library(tidyverse)

#Parse input:
input <- readLines("Day 24/test input.txt")  #20 lines in test. How many unique tiles?
input <- readLines("Day 24/input.txt")  #20 lines in test. How many unique tiles?
tiles <- vector()


for(i in 1:length(input)){
  l <- input[i]
  
  totnorth <-  0
  toteast <- 0
  pos <- 1
  while(pos <=  str_length(l)){
    if(str_sub(l,pos,pos) == 'e' | str_sub(l,pos,pos)== 'w'){
      ilen <- 1
    } else {
      ilen <- 2
    }
    
    instr <- str_sub(l, pos, pos + ilen-1)
    north <- switch(instr, 'e'=0, 'w'=0, 'ne'=1, 'nw'= 1, 'se'= -1, 'sw' = -1)
    east <- switch(instr, 'e'=2, 'w'=-2, 'ne'=1, 'nw'= -1, 'se'= 1, 'sw' = -1)
  
    totnorth <- totnorth + north
    toteast <- toteast + east
    pos <- pos + ilen
  }
  tiles[i] <- paste('n', totnorth, 'e',toteast, sep='' )
}

#list each tile and how many times it has been turned. Odd numbers are black tiles.
t <- as.tibble(tiles)
group_by(t, value ) %>% summarize(cnt=n()) %>% filter(cnt %% 2 == 1) %>% summarize(cnt=n())   #10 for sample input
