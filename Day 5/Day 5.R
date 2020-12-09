#Day 5. Convert seat identities in semi-binary format to seat id = row*8 + column.
# Use strtoi("000101", base = 2) to convert binary strings to decimal

library(tidyverse)
library(stringr)
library(dplyr)

#Import the data as a list of lists. Each inner list is one passport with one entry per field.
input <- readLines("Day 5/input.txt")          #This imports each line as a string.

#input <- c('BFFFBBFRRR', 'FFFBBBFRRR', 'BBFFBBFRLL')  #sample input with answers


#Split 7-character row identifier and 3-character column identifier to separate columns.
#Replace letters F=0, B=1 and R=0, L=1. Convert from binary to decimal, calculate seat_id, and find the highest seat_id.
df <- tibble(input)
df$row_binary <- str_sub(input, 1,7)
df$col_binary <- str_sub(input,8, 10)

df$row_binary  <- str_replace_all(df$row_binary, 'F', '0')
df$row_binary  <- str_replace_all(df$row_binary, 'B', '1')

df$col_binary  <- str_replace_all(df$col_binary, 'L', '0')
df$col_binary  <- str_replace_all(df$col_binary, 'R', '1')

df$row_decimal <- strtoi(df$row_binary, base = 2)
df$col_decimal <- strtoi(df$col_binary, base = 2)

df$seat_id <- df$row_decimal * 8 + df$col_decimal
max(df$seat_id)   #896  <= This is the answer to part 1.

##################################################################################################################
#Part 2 : Find my seat, the only missing seat identity in the list. 659!
df %>% arrange(seat_id)                                                       #See the list sorted by id
df <- mutate(df, seat_id_diff = seat_id-lag(seat_id, order_by=seat_id))       #Add a column with the difference between seat_id and lagged seat_id, sorted by seat_id
df %>% filter(!seat_id_diff==1)                                               #Show all lines where diff is not 1. => 660, which means that seat 659 is missing in the list!
