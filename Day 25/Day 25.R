#Day 25 : Find th encryption key.
#We know two public keys- one for the card and one for the door.
#We know the "initial subject" number to transform.
#We can figure out loop size for the card and door which produces the public keys. 

#Input : 
#door public key 11239946   # loop size 1665442
#card public key 10464955

#Initial subject = 7.
#Sample input: Door public key = 17807724  => loop size 11. 
#Card public key = 5764801 => loop size 8.

#Part 1 : find the loop sizes of the card/door. Then transform the public key of the card using the loop size of the door.

#Try loop sizes until we find one
sn <- 7                                      #Subject number
loopsize <- function(sn, pk){
  n <- 1                                     #Begin with 1.
  for(i in 1:1000000000){
    n <- n *sn
    n <- n %% 20201227
    if(n == pk) return(i)
  }
}

#calculate the encryption key: transform one pk with the others loop size.
encryption_key<- function(sn, loops){
  n <- 1
  for(i in 1:loops){
    n <- n *sn
    n <- n %% 20201227
  }
  return(n)
}

#use the given key pair to calculate the encryption key
encryption_key(10464955, loopsize(7,11239946))  #711945 : correct
encryption_key(11239946, loopsize(7,10464955))  #Also produces 711945
