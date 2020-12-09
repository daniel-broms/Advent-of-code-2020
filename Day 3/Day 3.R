#Day 3 : Count the number of trees, traversing the matrix right 3, down one until we reach the bottom of the matrix.
#The matrix has 323 rows and 31 columns.
#We need 323*3 = 969 columns - or we just wrap around 31 (more elegant).
library(tidyverse)
library(stringr)
library(dplyr)

#Read the input to a data frame with one column.
input <- read.table(file="Day 3/input.txt", header=FALSE, colClasses = "character", comment.char="")


counttrees <- function(step){
  #define starting positions
  col <- 1
  trees <- 0
  
  #iterate the matrix one row at a time, jumping "step" right each row.
  for(row in input[, 1]){
    trees <- trees + (str_sub(row, col ,col) == "#")
    col <- col + step
    if(col>31){
      col <- col - 31
    }
  }
  return(trees)
}

#Run with step 3:
counttrees(3) # 211 = answer! 


##############################################################################################
# Part 2 : find the number of trees found with several slopes
# Right 1, down 1.   67
# Right 3, down 1. (This is the slope you already checked.) 211
# Right 5, down 1.  77
# Right 7, down 1.  89
# Right 1, down 2.  37

#rewrite the function to allow steps for rows also.
counttrees2 <- function(stepcol, steprow){
  #define starting positions
  col <- 1
  row <- 1
  trees <- 0
  
  #iterate the matrix steprow at a time, jumping stepcol right each row.
  while (row <= length(input[, 1])) {
    trees <- trees + (str_sub(input[row,1], col ,col) == "#")
    row <- row + steprow
    col <- col + stepcol
    if(col>31){
      col <- col - 31
    }
  }
  return(trees)
}


#Find the product of all slopes : 3584591857
counttrees2(1,1) * counttrees2(3,1) * counttrees2(5,1) * counttrees2(7,1) * counttrees2(1,2)

