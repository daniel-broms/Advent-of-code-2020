#Day 13 : Which bus can I take earliest, when? Multiply the bus ID by the number of minutes you'd need to wait

input <- readLines("Day 13/input.txt")                                   #read 2 strings to a vector

time <- strtoi(input[1])
bus <- unlist(str_split(input[2], ','))
bus <- sort(strtoi(bus[!bus=="x"]))

wait <- (floor(time/bus) + (!time %% bus == 0)) * bus - time    #List wait time per bus for a given time.
bus[which.min(wait)] * min(wait)                                #Multiply the bus ID (37) by th ewait time (9) => 333 


################################################################################################################
#Part 2 : Optimized version

#strategy : for each multiple of of the previous solution cycle, find the remainder of  (time + bus index) / (bus frequency). When all are 0 we have found our number.
#Find previous solution cycle length by finding the distance between two consecutive solutions.

input <- readLines("Day 13/input.txt")  
bus <- unlist(str_split(input[2], ','))                       
#bus <- unlist(str_split("7,13,x,x,59,x,31,19", ','))         #sample input. 
index <- which(!bus=="x")-1                                   #create zero-based list of bus indexes.
bus <- strtoi(bus[!bus=="x"])                                 #strip x:s and convert to integers             

time <- 0
inc  <- bus[1]                                                #Initial increment is the frequency of the first bus. 
i    <- 2                                                     #begin by finding two consecutive solutions for the first two buses. Find the difference between these and set the increment to this value.
s1   <- 0                                                     #keep track of the first solution for each i.

while(T){
  time <- time + inc                                          #increase time with current increment
  if(sum((time + index[1:i]) %% bus[1:i]) == 0) {             #check if we have a solution for the current number of buses (i)
    if(i == length(bus)) {break}                              #if we have a solution for all buses : we are done! finish here.
    else if(s1 == 0){                                         #if this is the first solution for bus i, 
      s1 <- time                                              #then store this first solution in s1
     } else {
      inc <- time-s1                                          #else if this is the second solution for bus i, set the increment to the difference between the consecutive solutions and start looking for a solution for the next bus.
      s1 <- 0
      i <- i + 1
     }
   }
}

print(time, digits = 16)   #1068781 for sample input. 690123192779524 for real input


