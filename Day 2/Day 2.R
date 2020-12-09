# Day 2 : How many passwords are valid according to their policies?
library(tidyverse)
library(stringr)
library(dplyr)

#Import the input data to a data frame.
input <- read.table(file="Day 2/input.txt", header=FALSE, sep = ' ', col.name=c('limits', 'letter', 'password'),stringsAsFactors=FALSE)

#Split the first column to a min and a max value, convert to integers.
df <- as_tibble(input)
df <- cbind(input, str_split_fixed(input[,1], '-', 2), stringsAsFactors=FALSE)
df <- rename(df, min = 4)
df <- rename(df, max = 5)
df$min <- strtoi(df$min)
df$max <- strtoi(df$max)

#Remove the colon in column 'letter':
df$letter <- str_sub(df$letter, 1,1)


#Declare a function which determine if a password is valid, i,e it has the given letter between min and max times in the password.
checkpassword <- function(password, letter, min, max) {
  cnt <- str_count(password, letter)   #Count how many times 'letter' exists in 'password'
  return((cnt >= min) & (cnt <=max))
}

#add a column with password check result
df$pwdok <- checkpassword(df$password, df$letter, df$min, df$max)

#find nr of OK passwords (TRUE=1, FALSE=0)
sum(df$pwdok) #434 passwords are OK


###############################################################################################
# Part 2 : Exactly one of positions min and max must contain the given letter.

#Declare a function which determine if a password is valid
checkpassword2 <- function(password, letter, min, max) {
  cnt <- (str_sub(password,min,min) == letter) + (str_sub(password,max,max) == letter) #Each logical expression returns 1 if true
  return(cnt==1)
}

#add a column with password check result
df$pwdok2 <- checkpassword2(df$password, df$letter, df$min, df$max)

#find nr of OK passwords (TRUE=1, FALSE=0)
sum(df$pwdok2) #509 passwords are OK
