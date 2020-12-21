#Day 21 : Identify ingred/allergen combinations. When done, how many ingred remain unpaired?
#determine which ingredients can't possibly contain any of the allergens in any food in your list. 

#In the example, none of the ingredients 
#   kfcds, nhms, sbzzf, or trh 
#can contain an allergen

library(stringr)
library(tidyverse)
options(dplyr.summarise.inform = FALSE)  # Suppress summarise info

#read  input
input <- readLines("Day 21/test input.txt")
input <- readLines("Day 21/input.txt")
input <- str_replace(input, '\\)', '')
input <- matrix(unlist(str_split(input, ' \\(contains ' )), nrow = length(input), byrow=T)
     
t <- as.tibble(input)
t <- tibble::rowid_to_column(t, "food")
t <- rename(t, ingr= V1, allergen = V2)

#split to two tibbles : one with food/ingr (tfi), one with food/allergen (tfa)
tfi <- tibble(food=t$food, ingr=t$ingr)
tfi <- separate_rows(tfi, ingr)

tfa <- tibble(food=t$food, allergen=t$allergen)
tfa <- separate_rows(tfa, allergen)

#for each allergen : find the distinct foods it appears in (ex. dairy =1,2)
#Find the ingredient which also appears in the same foods
aipairs <- list()

ta <- distinct(tfa, allergen)  #list of distinct allergens remaining
for(arow in 1:nrow(ta)){
  callergen <- unlist(ta[arow,1])

  #allergen food list
  afl <- tfa %>% filter(allergen == callergen) %>% distinct(food)   
  
  #find ingr which appear in all foods in afl
  il <- inner_join(tfi, afl, by="food") %>% group_by(ingr) %>% summarise(n = n()) %>% arrange(desc(n)) %>% filter(n == nrow(afl))  
  
  if(nrow(il) == 1) {
    #There is only one ingr is the same foods are this allergen :we have a pair! 
    #add this pair to our pair list:
    cingr <- unlist(il[1,1])
    aipairs <- c(aipairs, c(cingr, callergen))
    print(c(cingr, callergen))
    
    #remove these allergens and ingredients from our lists:
    tfa <- filter(tfa, !allergen == callergen)
    tfi <- filter(tfi, !ingr == cingr)
  }
}

#check is there are any allergens left. We might need to do multiple passes (5 passes needed with real data)
tfa  #should be empty when we are done.

nrow(tfi)   #5 ingr/food combinations left = sample input answer! With real input : 2170 = correct answer!

##########################################################################################################################
#Part 2 : 
#Arrange the ingredients alphabetically by their allergen and separate them by commas to produce your canonical dangerous ingredient list.
#(There should not be any spaces in your canonical dangerous ingredient list.) 
#In the above example, this would be mxmxvkd,sqjhc,fvjkl.

p <- as_tibble(matrix(aipairs, ncol=2, byrow=T))
p <- rename(p, ingr= V1, allergen = V2)
p <- arrange(p,allergen)

#Solution : nfnfk,nbgklf,clvr,fttbhdr,qjxxpr,hdsm,sjhds,xchzh
