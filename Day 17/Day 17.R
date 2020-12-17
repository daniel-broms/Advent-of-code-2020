#Day 17 : Find cube state.

input <- "#....#.#
..##.##.
#..#..#.
.#..#..#
.#..#...
##.#####
#..#..#.
##.##..#
"

#Test input
input <- ".#.
..#
###
"

size <- 8 #size of the input : 3 for test, 8 for real.


#six cycles : track state in 3 dimensions. Use a 3-dim array? Need to grow it by one in each direction fr each cycle!
input <- unlist(str_split(input, "\n"))
input <- unlist(str_split(input, ''))

a <- matrix(input, nrow=size, byrow=T)

#large 3D array : add 8 to each index so that there is room to grow. Add a "z" dimension. Copy the initial 2D matrix into this 3D array.
b <- array('.', dim=c(24,24, 24))
for(i in 1:size){
  for(j in 1:size){
    b[i+8, j+8, 8] <- a[i,j]
    }
}



#process the array in 3D: for each cell, count nr of neighbors.
#If a cube is active and exactly 2 or 3 of its neighbors are also active, the cube remains active. Otherwise, the cube becomes inactive.
#If a cube is inactive but exactly 3 of its neighbors are active, the cube becomes active. Otherwise, the cube remains inactive.

#count active neighbors of 3d coordinate co:
active_neig <- function(x){
  tmp <- b[(x[1]-1) : (x[1]+1), (x[2]-1) : (x[2]+1), (x[3]-1) : (x[3]+1)]
  tmp[2, 2, 2] <- '.'  #no not count the current center point!
  sum(tmp == "#")
}

c <- b
#do 6 iterations
for(i in 1:6){
  c <- b #write to a copy of b, read from b
  for(x in 2:23){
    for(y in 2:23){
      for(z in 2:23){
        an <- active_neig(c(x,y,z))
        if(b[x,y,z] == "#"){
          if(an %in% c(2,3)){c[x,y,z] <- "#"} else {c[x,y,z] <- "."}
        }
        else
        {
          if(an == 3)       {c[x,y,z] <- "#"} else {c[x,y,z] <- "."}
        }
      }
    }
  }
  b <- c  #write back c to b
}


sum(b == "#")  #Count active cells.  322 :Correct!

#the original 2D input.
b[9:11, 9:11, 11]

#check top left pos 9,9,8:
b[9,9,10]                #.
active_neig(c(9,9,10))   #1


