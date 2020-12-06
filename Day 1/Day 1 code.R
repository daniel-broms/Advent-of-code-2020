# Day 1 : Find the pair of numbers whose total is 2000. Calculate the product of these two numbers.
# Advent of code - proof of github account ownership : 1201504-20201206-5572bc01
#Import the input data to a vector.
input <- scan(file="Day 1/input.txt")

#Declare a function to iterate over the list to find pairs whose sum is 2020. Return the product of these pairs.
FindPair <- function(input) {
  for(value1 in input) {
    for (value2 in input) {
      if (value1 + value2 == 2020) {
        return(value1*value2)
      }
    }
  }  
}

#Call the function to show the answer: 436404
print(FindPair(input))


###########################################################################################
#Part 2 : Find the three numbers in the list which sum up to 2020.


#Declare a function to iterate over the list to find triplets whose sum is 2020. Return this triplet as a vector.
FindTriplet <- function(input) {
  for(value1 in input) {
    for (value2 in input) {
      for (value3 in input) {
        if (value1 + value2 + value3 == 2020) {
          #print(c(value1, value2, value3))   #Show the triplet
          return( c(value1,  value2,  value3))
        }
      }
    }
  }  
}

#Call the function to show the answer: 274879808, the product of 721, 851, 448.
triplet <- FindTriplet(input)
print(triplet)
print(prod(triplet))


