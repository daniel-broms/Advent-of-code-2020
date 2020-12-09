#Day 7 : How many bag colors can eventually contain at least one shiny gold bag?
# This can be seen as a BOM "where-used" problem. We need to find all BOM entries, on any level, which contain a gold bag.

#Import the data as a BOM structure : parent color, child color, quantity.
#For each record where child color = "gold": traverse the BOM up to the top, record all bag colors that we encounter.

input <- readLines("Day 7/input.txt")   #594 rules are imported.
#input <- readLines("Day 7/test part 2.txt")    #Test input where we know the answer = 4 bags.

#Create a tibble with the parent in one column and the child list in another column.
input <- str_replace_all(input, 'bags', 'bag')    #we have mixed bag/bags, make all singular.
t <- tibble(parent=str_sub(input, 1, str_locate(input, " contain")[,1]-1) , children=str_sub(input, str_locate(input, " contain")[,1]+8) )

t$child <- str_split(t$children, ',')         #Split the children to a vector with each child as one element
t <- unnest(t, child)                         #unnest the child vector to create one row per child
t <- select(t, -children)                     #remove th eno longer needed children column

#Split out quantity column. Remove periods from child.
t$qty <- str_sub(t$child,2,2)
t$child <- str_sub(t$child,4)
t$child <- str_remove(t$child, fixed("."))

#fix the "qty" column in t : change "n" to 0, convert strings to int.
t$qty <- str_replace(t$qty, 'n', '0')
t$qty <- strtoi(t$qty)

#initialize our vector
bags <- c("shiny gold bag")

#recursive function to find parents
find_parents <- function(bags){
  parents <- unlist(filter(t, child %in% bags)[,1])  #Find all parents of the input bag list
  parents <- setdiff(parents, bags)                  #only include parents not already in the bag list
  if(length(parents)==0){
    return(unique(bags))
  }else{
    return(find_parents(unique(c(bags, parents))))   #here is the recursive magic
  }
}

#call the function. 178 bags in list, exclude "shiny gold bags" => 177 bags can be parents! WRONG!
find_parents(bags) %>% length()-1

#TODO : rephrase the above with pipes!

############################################################################################
#Part 2 : How many individual bags are required inside your single shiny gold bag?
# This is the opposite : recursively find all children and their quantity to the top-level shiny gold bag.
# Very similar to finding the total number of components in the BOM of an item.

#recursive function to find children. Keep track of quantities also! Maybe generation too while we are at it. Pass around a tibble instead of a vector.
find_children <- function(tbags){
  
  #Find child bags and their total quantities (=parent qty * child qty/parent)
  child_bags <- inner_join(x=tbags, y=t, by=c("bag" = "parent")) %>% 
    transmute(bag=child, qty=qty.y * qty.x, generation = generation + 1)

  #Stop if no new children found. Otherwise recursively find and add the children of the found children.
  if(nrow(child_bags) == 0){
    return(tbags)
  }else{
    return(bind_rows(tbags, find_children(child_bags)))   #here is the recursive magic. Only find new children 
  }
}


#Initialize a tibble with the shiny gold bag, recursively find children and ad to the tibble, then sum the total quantity of bags, minus 1 for the original bag.
tibble(bag=c("shiny gold bag"), qty=c(1), generation=c(1)) %>%
  find_children() %>%
  summarize(sum(qty))-1   #126 for the test data (correct!) , 34988 for the real data.

  

