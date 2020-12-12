#Day 12 : Find final ship position after following input instructions.
library(tidyverse)
library(stringr)

input <- readLines("Day 12/input.txt")                                  #read 762 strings to a vector
#input <- readLines("Day 12/input test.txt")                             #read 5 strings to a vector
t<-tibble(instr = str_sub(input,1,1), val = strtoi(str_sub(input,2)) )         #Split to a tibble

#record state
xpos <- ypos <- 0      #Start at origo
angle <- 90            #facing east

#Possible instruction: 
#Move in an absolute direction (N,S,E,W) :                              Add or subtract the value to xpos or ypos
#Turn left (L) or right(R) the given number of degrees (90, 180, 270) : Add (R) or subtract (L) from the angle, normalize to 0-270. (360 =>0)
#Forward(F) in the current angle (0, 90, 180, 270) :                    Add or subtract the value to xpos or ypos

#Execute each instruction iteratively. North and east are positive, south and west are negative.
for(i in 1:nrow(t)){
  instr <- t$instr[i]
  val   <- t$val[i]
  if(instr == "N") {ypos <- ypos + val}
  if(instr == "S" ){ypos <- ypos - val}
  if(instr == "E" ){xpos <- xpos + val}
  if(instr == "W" ){xpos <- xpos - val}
  
  if(instr == "R"){angle <- norm_angle(angle + val)}
  if(instr == "L"){angle <- norm_angle(angle - val)}
  
  if(instr == "F"){
    if(angle == 0)  {ypos <- ypos - val}
    if(angle == 90) {xpos <- xpos + val}
    if(angle == 180){ypos <- ypos + val}
    if(angle == 270){xpos <- xpos - val}
  }  
}

print(abs(xpos)+abs(ypos))  #The manhattan distance = the answer; 25 for test data; 1007


#normalize the angle to 0-270
norm_angle <- function(angle){
  if(angle < 0){angle <- angle + 360}
  if(angle > 270){angle <- angle - 360}
  return(angle)
}


###############################################################
#Part 2 : New rules

input <- readLines("Day 12/input.txt")                                  #read 762 strings to a vector
#input <- readLines("Day 12/input test.txt")                             #read 5 strings to a vector
t<-tibble(instr = str_sub(input,1,1), val = strtoi(str_sub(input,2)) )         #Split to a tibble

#record state
xpos <- ypos <- 0      #Ship position. Start at origo
wx <- 10               #waypoint x position (10 east of ship)
wy <- 1                #waypoint y position (1 north of ship)

#Possible instruction: 
#Move in an absolute direction (N,S,E,W) :                              Add or subtract the value to wx or wy
#Turn left (L) or right(R) the given number of degrees (90, 180, 270) : Add (R) or subtract (L) from the angle, normalize to 0-270. (360 =>0)
#Forward(F) in the current angle (0, 90, 180, 270) :                    Add or subtract the value to xpos or ypos

#Execute each instruction iteratively
for(i in 1:nrow(t)){
  instr <- t$instr[i]
  val   <- t$val[i]
  if(instr == "N") {wy <- wy + val}
  if(instr == "S" ){wy <- wy - val}
  if(instr == "E" ){wx <- wx + val}
  if(instr == "W" ){wx <- wx - val}
  
  #Rotate using trig rotation of the angle converted to radians. Right rotation is a negative rotation.
  if(instr == "R" | instr == "L"){
    if(instr == "R"){val <- -val}
    a <- val * (pi/180)
    wx_new <- wx * cos(a) - wy * sin(a)
    wy     <- wx * sin(a) + wy * cos(a)
    wx <- wx_new
  }

  #F : Move forward to waypoint x times.
  if(instr == "F"){
    xpos <- xpos + wx * val
    ypos <- ypos + wy * val
  }  
}

print(abs(xpos)+abs(ypos))  #The manhattan distance = the answer; 286 for test data; 41212

#################################################################################################

  