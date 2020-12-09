#Day 4 : Find the numer of valid passports having all 7 required fields with cid as optional:
#   byr (Birth Year)
#   iyr (Issue Year)
#   eyr (Expiration Year)
#   hgt (Height)
#   hcl (Hair Color)
#   ecl (Eye Color)
#   pid (Passport ID)
#   cid (Country ID)

library(tidyverse)
library(stringr)
library(dplyr)

#Import the data as a list of lists. Each inner list is one passport with one entry per field.
input <- readLines("Day 4/input.txt")          #This imports each line as a string.

input2 <- paste(input, collapse=';')           #paste together as one long string, replacing newlines with semicolon.
input2 <- str_replace_all(input2, ";;", "ö")   #replace double semicolon with an ö
input2 <- str_replace_all(input2, ";", " ")    #replace remaining semicolons with a space so that all value pairs have space as separator
input2 <- unlist(str_split(input2, pattern="ö"))       #split by ö to create one string per passport


#Function to validate a passport string: check that it contains all 7 required entries.
Validate <-function(passport){
  
  passport <- unlist(str_split(passport, " ", ))   #Make one vector entry per attribute : split with space
  passport <- str_sub(passport,1,3)        #strip the data, only leave the attribute name
  
  #Check if the passport vector contains all required attributes
  return('byr' %in% passport & 'iyr' %in% passport & 'eyr' %in% passport & 'hgt' %in% passport &'hcl' %in% passport & 'ecl' %in% passport & 'pid' %in% passport )
  
}


#Check all passports : apply the validation function. I was not able to use Validate as a vectorized function - unclear why! 
result <- sapply(input2, Validate)
sum(result) #219 valid passports


#Testing
Validate('cid:242 iyr:2011 pid:953198122 eyr:2029 ecl:blu hcl:#888785')           #incomplete passport
Validate("eyr:2033 hgt:177cm pid:173cm ecl:utc byr:2029 hcl:#efcc98 iyr:2023")    #Complete passport


passport <- 'cid:242 iyr:2011 pid:953198122 eyr:2029 ecl:blu hcl:#888785'
test <- unlist(str_split(passport, " ", ))
test <- str_split(test, ":")

##############################################################################################################################
# Part 2 : Add validation of the data. 


#Function to validate a passport string: check that it contains all 7 required entries.
Validate2 <-function(passport){
  
  passport <- unlist(str_split(passport, " ", ))   #Make one vector entry per attribute : split with space
  passport <- str_split(passport, ":")              #Separate label from data. We now have a list for each passport with one entry (another list) per value. The inner list is one long an
  
  #Check if the passport vector contains all required attributes

}


passport <- 'cid:242 iyr:2011 pid:953198122 eyr:2029 ecl:blu hcl:#888785'
test <- unlist(str_split(passport, " ", ))
test <- str_split(test, ":")
test

#function to check  value. It must exist,and conform to rules.
test_val <- function(passport, val){
  passport <- unlist(str_split(passport, " ", ))    #Make one vector entry per attribute : split with space
  passport <- str_split(passport, ":")              #Separate label from data. We now have a list for each passport with one entry (another list) per value. The inner list is one long an
  labels <- unlist(lapply(passport,'[[',1))         #Get the labels in a separate vector.
   
  position <- match(val, labels)                    #Get the position of 'byr' in the list. Return FALSE if it does not exist.  
  if(is.na(position)){
    return(FALSE)
  }
    
  value <- passport[[position]][2]                   #extract the value
  if(val=='byr'){
    return(str_detect(value, '^[0-9]{4}$') & between(value, 1920, 2002))          #byr : the value must be four digits from 1920 to 2002
  }
  
  if (val=='iyr'){
    return(str_detect(value, '^[0-9]{4}$') & between(value, 2010, 2020))          #iyr (Issue Year) - four digits; at least 2010 and at most 2020
  }
  
  if (val=='eyr'){
    return(str_detect(value, '^[0-9]{4}$') & between(value, 2020, 2030))          # eyr (Expiration Year) - four digits; at least 2020 and at most 2030
  }
  
  if (val=='hgt'){
    height <- str_sub(value, 1, str_length(value)-2) #extract the height 
    return(  (str_detect(value, '^[0-9]{3}cm$') & between(height, 150, 193)) | (str_detect(value, '^[0-9]{2}in$') & between(height, 59, 76)) ) # hgt (Height) - a number followed by either cm or in:    If cm, the number must be at least 150 and at most 193.    If in, the number must be at least 59 and at most 76.
  }
  
  if (val=='hcl'){ 
    return(str_detect(value, '^#[0-9a-f]{6}$'))                                    # hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
  }
  
  if (val=='ecl'){
    return(value %in% c('amb', 'blu', 'brn', 'gry', 'grn','hzl', 'oth'))           # ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
  }
  
  if(val=='pid'){
    return(str_detect(value, '^[0-9]{9}$'))                                        # pid (Passport ID) - a nine-digit number, including leading zeroes.
  }
    
  return(FALSE)   #If no match above return false.
}


#Apply this function to all passports to find how many conform to all 7 rules.
result <- sapply(input2, test_val, 'byr')  + sapply(input2, test_val, 'iyr') + sapply(input2, test_val, 'eyr') + sapply(input2, test_val, 'hgt') + sapply(input2, test_val, 'hcl') + sapply(input2, test_val, 'ecl') + sapply(input2, test_val, 'pid')
sum(result==7)   #127 passports conform to all 7 rules.


#regexp notes:
#   ^     Start of string
#   $     End of string
#   {5}   Repeat exactly 5 times
#   [0-9] Matches any of chars 0-9

# https://regexr.com/ is a good site to write and test regexp on!


###############  Testing #######################
test_val('cid:242 iyr:2011 pid:953198122 eyr:2029 ecl:blu hcl:#888785 byr:2002', 'byr' )  #byr works!
test_val('cid:242 iyr:2009 pid:953198122 eyr:2029 ecl:blu hcl:#888785 byr:2002', 'iyr' )  #iyr works!
test_val('cid:242 iyr:2009 pid:953198122 eyr:2030 ecl:blu hcl:#888785 byr:2002', 'eyr' )  #eyr works!
test_val('cid:242 iyr:2009 pid:953198122 eyr:2031 ecl:blu hcl:#888785 byr:2002 hgt:58in', 'hgt' )  #hgt works!
test_val('cid:242 iyr:2009 pid:953198122 eyr:2031 ecl:blu hcl:#88878x byr:2002 hgt:58in', 'hcl' ) #hcl works!
test_val('cid:242 iyr:2009 pid:953198122 eyr:2031 ecl:blu hcl:#88878x byr:2002 hgt:58in', 'ecl' ) #ecl works!
test_val('cid:242 iyr:2009 pid:953198122 eyr:2031 ecl:blu hcl:#88878x byr:2002 hgt:58in', 'pid' ) #pid works!

test2 <- unlist(lapply(test,'[[',1)) #Get the first element in each list.

'eyr' %in% test2
match('eyr', test2)
match('eyr', test)

test[[4]][2]

str_detect('180cm', '^[0-9]{3}cm$')

str_length('180cm')
value <-'180cm'
hgt <- str_sub(value, 1, str_length(value)-2)
hgt

# hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
str_detect("#88878a", '^#[0-9a-f]{6}$')

# ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
'oth' %in% c('amb', 'blu', 'brn', 'gry', 'grn','hzl', 'oth')

# pid (Passport ID) - a nine-digit number, including leading zeroes.
str_detect('95319812', '^[0-9]{9}$')



     