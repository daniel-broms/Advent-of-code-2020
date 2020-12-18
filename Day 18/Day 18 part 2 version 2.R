#create an own %plus% operator with higher precedence than * operator, then just evaluate the expressions.
library(stringr)

`%plus%` <- function(x,y) x+y 
print(sum(sapply(readLines("Day 18/input.txt") %>% str_replace_all('\\+', '%plus% '), function(x) eval(parse(text=x)))), digits = 16)
