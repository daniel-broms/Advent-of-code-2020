#Day 24  : How may tiles are black after flippen x of them? Instructions include E, W, NE, NW, SE, SW. (Not N or S)
#Use a coord system to uniquely identify tiles : Nx, Ey.  S is negative N, W is negative E. NE is one a step east, one step north. E is two  steps east.
library(stringr)
library(tidyverse)

#Parse input:
input <- readLines("Day 24/test input.txt")  #20 lines in test. How many unique tles?
input <- readLines("Day 24/input.txt")  #20 lines in test. How many unique tiles?
tiles <- vector()
floor <- matrix(1, nrow=400, ncol=400)   #make a large floor matrix, center is 200, 200. 0=black, 1=white.

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
  
  #flip the tile in the floor matrix.
  if(floor[toteast+200, totnorth + 200] == 0 ){
    floor[toteast+200, totnorth + 200] <- 1
  }
  else
  {
    floor[toteast+200, totnorth + 200] <- 0
  }
}

#list each time and how many times it has been turned. Odd numbers are black tiles.
t <- as.tibble(tiles)
group_by(t, value ) %>% summarize(cnt=n()) %>% filter(cnt %% 2 == 1) %>% summarize(cnt=n())   #10 for sample input, 287 for real input
   
length(floor[floor == 0]) #also 287

###################################################
#Part 2 : Flip tiles in 100 iterations.

#flip 100 days
for(move in 1:100){
  newfloor <- floor #edit a copy of floor.
  for(x in 3:398){
    for(y in 3:398){
      wt <- floor[x-1,y+1] + floor[x-2,y] + floor[x-1,y-1] + floor[x+1,y+1] + floor[x+2,y] + floor[x+1,y-1] #count adjacent white tiles
      bt <- 6-wt
      if(floor[x,y] == 0 & (bt == 0 | bt > 2)){ newfloor[x,y] <- 1}                                         # Any black tile with zero or more than 2 black tiles immediately adjacent to it is flipped to white.
      if(floor[x,y] == 1 & bt == 2  )         { newfloor[x,y] <- 0}                                         # Any white tile with exactly 2 black tiles immediately adjacent to it is flipped to black.   
    }
  }
  floor <- newfloor
}

length(floor[floor == 0]) #answer : 3636


