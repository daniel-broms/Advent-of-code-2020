#Day 12 : Find final ship position after following input instructions.
library(tidyverse)
library(stringr)

input <- readLines("Day 12/input.txt")                                  #read 762 strings to a vector
#input <- readLines("Day 12/input test.txt")                             #read 5 strings to a vector
t<-tibble(instr = str_sub(input,1,1), val = strtoi(str_sub(input,2)) )         #Split to a tibble

#record state
spos <- c(0,0)         #ship position, start in origo
angle <- 90            #facing east

#Execute each instruction iteratively. North and east are positive, south and west are negative.
for(i in 1:nrow(t)){
  instr <- t$instr[i]
  val   <- t$val[i]
  
  if(instr == "R"){                                  #Rotate left or right
    angle <- norm_angle(angle + val)
  }else if(instr == "L"){
    angle <- norm_angle(angle - val)
  }else if(instr == "F"){                            #Move forward in the current angle
    if(angle == 0)  {spos[2] <- spos[2] + val}
    if(angle == 90) {spos[1] <- spos[1] + val}
    if(angle == 180){spos[2] <- spos[2] - val}
    if(angle == 270){spos[1] <- spos[1] - val}
  } else {                                           #Move in an absolute direction (N,S,E,W) :Add or subtract the value to x or y of ship position                      
    spos <- spos + switch(instr,"E"= c(1,0),"W"= c(-1,0),"N"= c(0,1),"S"= c(0,-1)) * val
  }
}

print(abs(spos[1])+abs(spos[2]))  #The manhattan distance = the answer; 25 for test data; 1007

#normalize the angle to 0-270
norm_angle <- function(angle){
  if(angle < 0){angle <- angle + 360}
  if(angle > 270){angle <- angle - 360}
  return(angle)
}

###############################################################
#Part 2 : New rules. Also use vectors for coordinates.

input <- readLines("Day 12/input.txt")                                   #read 762 strings to a vector
#input <- readLines("Day 12/input test.txt")                             #read 5 strings to a vector
t<-tibble(instr = str_sub(input,1,1), val = strtoi(str_sub(input,2)) )   #Split to a tibble

#Record state
spos <- c(0,0)         #Ship position. Start at origo.
wpos <- c(10,1)        #waypoint vector. Start a east 10, north 1.

#Execute each instruction iteratively
for(i in 1:nrow(t)){
  instr <- t$instr[i]
  val   <- t$val[i]

  if(instr == "R" | instr == "L"){  #Rotate using trig rotation of the angle converted to radians. Right rotation is a negative rotation.
    if(instr == "R"){val <- -val}
    a <- val * (pi/180)
    wx <- wpos[1] * cos(a) - wpos[2] * sin(a)
    wy <- wpos[1] * sin(a) + wpos[2] * cos(a)
    wpos <- c(wx,wy)
  } else if(instr == "F"){          #Move ship (spos)in waypoint direction (wpos) if F.
    spos <- spos + wpos * val
  } else{                           #Move waypoint if instruction is E,W,N,S.
    wpos <- wpos + switch(instr,"E"= c(1,0),"W"= c(-1,0),"N"= c(0,1),"S"= c(0,-1)) * val
  }
}

print(abs(spos[1])+abs(spos[2]))  #The manhattan distance = the answer; 286 for test data; 41212 for real data.


