# Day 20 : rotate,  flip and rearrange tiles so that all borders between tiles line up.
library(stringr)
library(tidyverse)
library(stringi)

#each tile can have 8 states : 4 orientations (rotation angles), each flipped or not.
# ? can we rely on each "edge match" to be unique? =>are all edges unique? If so once we have found an edge match we can keep it and eliminate othe rmatches for this edge.
# ? Do we need to check that other, adjacent edges match also? or can we assume that if one edge matches the other edges will also match?

#strategy 2 : build a puzzle : select one piece from the pool of unmatched tiles.. Loop though all tiles in the pool to find an edge match. Only rotate/flip the pool piece - keep the already found pieces where they are.
#Once a match is found, remove the matching piece from the pool and determine its orientation and xy position relative its matching tile.
#again, loop though the pool to find tiles that match any of the edges of the already fitted pieces Continue until no pieces are left in the pool.

#for found tiles : record which edge is already matched and to which other tile edge. Do not try to fit pieces to already matched edges.
#Also record which of the 8 states each matched tile has : rotation (0-3) and flip (0 or 1).

################################ helper functions  ##################################
#rotate a matrix 90 deg
rotate <- function(x) t(apply(x, 2, rev))

#flip a matrix horizontally (switch top/bottom)
flip <- function(x) apply(x, 2, rev)

#Flip a tile top to bottom: flip the matrix, recalculate edges.
fliptile <- function(t){
  #Flip the matrix
  m <- tiles[[t]][[2]] 
  m <- flip (m)
  tiles[[t]][[2]] <<- m      ##NOTE that we need to use <<- to modify the global environment from inside a function!!!
  
  #recalculate the edges
  tiles[[t]][[3]] <<- gettileedges(t)
  
  #mark this tile as flipped
  tiles[[t]][[4]] <<- 2
}

#Rotate tile t i times clockwise
rotate_tile <- function(t, i){
  m <- tiles[[t]][[2]] 
  for(j in 1:i){m <- rotate(m)}
  tiles[[t]][[2]] <<- m
  
  #recalculate the edges
  tiles[[t]][[3]] <<- gettileedges(t)
  
  #rotate the edge match references also : 1->4, 2->1, 3->2, 4->3
  for(j in 1:i){
    em <- tiles[[t]][[6]]
    emr <- list(em[[2]], em[[3]], em[[4]], em[[1]])
    tiles[[t]][[6]] <<- emr
  }
  
  #record rotations of this tile, might be good to have.
  tiles[[t]][[8]] <<- i   
  
}

#return a list of the tile edges.
gettileedges <- function(t){
  m <- tiles[[t]][[2]] 
  edges <- list()
  for(i in 1:4){
    edge <- paste(m[1,], collapse="")
    edges[[i]] <- c(edge,stri_reverse(edge) )
    m <- rotate(m)                                     #turn m 90 deg clockwise and repeat
  }
  return(edges)
}

gettileedges2 <- function(t){    #only return the 4 edges.
  m <- tiles[[t]][[2]] 
  edges <- vector()
  for(i in 1:4){
    edge <- paste(m[1,], collapse="")
    edges[i] <-edge
    m <- rotate(m)                                     #turn m 90 deg clockwise and repeat
  }
  return(edges)
}

gettileedges2_rev <- function(t){    #only return the 4 reversed edges.
  m <- tiles[[t]][[2]] 
  edges <- vector()
  for(i in 1:4){
    edge <- paste(m[1,], collapse="")
    edges[i] <-edge
    m <- rotate(m)                                     #turn m 90 deg clockwise and repeat
  }
  return(edges)
}

################################### Process input ##################################################
#begin by recording each tile and its 4 edges signatures/inverse signatures in a list. 
input <- readLines("Day 20/test input.txt")  #9 tiles, each 10*10. Real input = 1727 rows =144 tiles  = 12*12 matrix of tiles.
tilesnr <- 9 

input <- readLines("Day 20/input.txt")  #9 tiles, each 10*10. Real input = 1727 rows =144 tiles  = 12*12 matrix of tiles.
tilesnr <- 144 


#register each tile in a tile list : 1=id, 2=original matrix, 3= list of edges and reverse edges 4=status flipped (T) or not(F)
tiles <- vector("list", tilesnr) 
for(t in 1:tilesnr){
  tile <- list()
  row <- (t-1) * 12 + 1
  tile_id <- str_sub(input[row],6,9) 
  m <- matrix(unlist(str_split(input[(row + 1 ): (row + 11)], "")), ncol = 10, byrow=T)
  tile[[1]] <- tile_id 
  tile[[2]] <- m 
  tile[[4]] <- 1                     #Record if we have flipped this or not. 1= original, 2= flipped.
  tile[[5]] <- TRUE                  #Pool status. All tiles are in the unmatched pool when we begin.
  tile[[6]] <- vector("list", 4)     #matched edges : list of (tile id, edge id) of matched edges. Unmatched edges contain NULL.
  tile[[7]] <- vector()              #position relative other tiles.
  tile[[8]] <- 0                     #rotation.
  tiles[[t]] <- tile                 #Add this tile to our list of tiles
  tiles[[t]][[3]] <- gettileedges(t) #calculate edge lists to use when matching edges.  
}

#take the first tile, put it in fitted pool.
#for each unmatched edge in the the fitted pool tile edges (4 in the first turn:)
  #search all tiles in the pool to find an edge with matches the current edge or its reverse.
  #if a reverse match is found, we can match the edges - record the match in both tile edges : matching tile id and  edge id. 
  #if a non-reverse match is found, we can match the edges after flipping the pooled tile.  If so register this tile as "flipped = true".
  #continue until no tiles are left in the pool.
  #then we need to investigate the shape of what we have built to find the corners!

#Prime the puzzle by setting the first tile, unflipped, on our table.
tiles[[1]][[5]] <- FALSE      #set Pooled to false
tiles[[1]][[7]] <- c(0,0)     #this tile position is origo.

x<- tilesnr

while(x>0){
    
  #loop though all tiles on the table, match the tile against all tiles in the pool. Continue until all tiles have been matched.
  for(ct in 1:tilesnr){
    if(tiles[[ct]][[5]] == FALSE) {                         #only match tiles on the table as ct (current tile)
      for(te in 1:4){                                       #for each tile edge of the current tile ct
        if(is.null(tiles[[ct]][[6]][[te]])){                #only process unmatched edges THIS SEEMS TO NOT WORK AS DESIGNED - ALL EDGES LOOK MATCHED!!
          for(tp in 1:tilesnr){
            if(tiles[[tp]][[5]] == TRUE){                   #only process unmatched tiles
              for(tpe in 1 : 4){                            #check all four edges of this unmatched tile
                if(tiles[[ct]][[3]][[te]][1] == tiles[[tp]][[3]][[tpe]][2]){
                  
                  #we have an edge match!
                  print(c("match", ct, te, tp, tpe))
                  
                  #check that this position is not occupied!
                  collision <- FALSE
                  newpos <- tiles[[ct]][[7]] + switch(te ,c(0,1), c(-1,0), c(0,-1), c(1,0)) 
                  for(i in 1:tilesnr){
                    if(!is.na(tiles[[i]][[7]][1]) && tiles[[i]][[7]][1]  == newpos[1] & tiles[[i]][[7]][2]  == newpos[2]){
                      print("position collision at")
                      collision <- TRUE
                    }
                  }
                  
                  if(!collision){
                    tiles[[tp]][[5]]  <- FALSE                #this tile is no longer pooled
                    tiles[[tp]][[6]][[tpe]] <- c(ct, te)      #record match on pooled tile edge
                    tiles[[ct]][[6]][[te]]  <- c(tp, tpe)     #record match on the current tile also. TODO : this is incorrect if we rotate tp!
                    tiles[[tp]][[7]] <- tiles[[ct]][[7]] + switch(te ,c(0,1), c(-1,0), c(0,-1), c(1,0))              #calculate this tiles position based on which the edge was matched
                    
                    #rotate the found tile so that the matching edge is opposite its mate
                    rotation <- switch(paste(te, tpe, sep=''), "11"=2, "22"= 2, "33"=2, "44"=2, "13"=0, "31"= 0, "24"=0, "42"=0, "12"=3, "23"=3, "34"=3, "41"=3, "14"=1, "43"=1, "32"=1, "21"=1 )
                    if(!rotation == 0) {rotate_tile(tp, rotation)}
                  }
                  break
                 }
               }
            }
          }
        }
      }
    }
  }
  #flip all pooled tiles, then match again!
  for(i in 1:tilesnr){
    if(tiles[[i]][[5]] == TRUE) {fliptile(i)}
  }
  
  #how many unmatched tiles are there? 
  x <- 0
  for(i in 1:tilesnr){
    if(tiles[[i]][[5]] == T) {x <- x + 1}
  }
}

#check which tiles are in which positions. Find the corner tiles. : 1709, 2023, 3457, 1571!
for(i in 1:tilesnr){print(c(tiles[[i]][[1]], tiles[[i]][[7]]))}

###############################################################################################
# part 2 : Assemble the image as one big matrix with tile borders removed.
# then search for sea monsters !

#Our matrix goes from -9, -9 to 2,2 (since we picked a random tile as origo).
#find each tile in order, remove its borders to make an 8*8 matrix, assemble to one large matrix.

#find tiles from -9, -9 to 2,2. Work column by column, top-down and right-left.
xpos <- 1
ypos <- 1
img <- matrix("", nrow=12*8, ncol = 12*8)
for(x in -9:2){
  for(y in 2:-9){
    for(i in 1:tilesnr){
      
      #find the tile at the current x,y position
      if(tiles[[i]][[7]][1] == x & tiles[[i]][[7]][2] == y) {
        t <- i
        break
      }
    }
      
    #Copy the inner 8*8 elements to our complete image
    m <- tiles[[i]][[2]]
    
    for(j in 2:9){
      for(k in 2:9){
        img[  ((ypos - 1) * 8)  + j - 1 , ((xpos-1) * 8) + k - 1] <- m[j,k]
      }
    }

    #increase x and y 
    ypos <- ypos + 1
    if(ypos == 13){
      ypos <- 1
      xpos <- xpos + 1
    }
  }
}


#Our image is now in matrix img. Next : find sea monsters of size 3*20. Convert this string to a matrix:
s <-       "                  # "
s <- c(s, "#    ##    ##    ###")
s <- c(s, " #  #  #  #  #  #   ")

s <- (paste(s, collapse=""))
s <- unlist(str_split(s, ""))
s <- matrix(s, ncol=20, byrow=T)
s

#compare this 3*20 matrix with all 3*20 matrixes inside img. 
#When we find a match : remove the matching "#" in img (replace them with "O")
#if we find no monsters : rotate img three times, then flip the image, then rotate again up to three time, each time scanning for monsters.
#When we have found 2 monsters : count the number of remaining #.

#return the number of matching # when scanning position x,y of img.
scan <- function(x,y){
  cnt <- 0
  for(i in 1:20){
    for(j in 1:3){
      if(s[j,i] == img[y + j-1, x + i-1]){cnt <- cnt + 1}
    }
  }
  return(cnt)
}

#When we have found a moster at x,y : replace the monsters # with O.
replace_monster <- function(x,y){
  for(i in 1:20){
    for(j in 1:3){
      if(s[j,i] == img[y + j-1, x + i-1]){img[y + j-1, x + i-1] <<- "O"}
    }
  } 
}

img_backup <- img #save the original image so that we can see if rotations/flips have worked as designed.

#Scan the whole image.
for(x in 1:76){
  for(y in 1:93){
    cnt <- scan(x,y)
    if(cnt == 15 ) {
      print(c(x,y))
      replace_monster(x,y)}
  }
}

#try rotating 3 times.
img <- rotate(img)

#then flip and rotate three more times.
img <- flip(img)

#count the number of remaining #: 2489 Correct!
length(img[img=="#"])


#monster positions:
#[1]  5 85 15
#[1]  6 44 15
#[1] 12  4 15
#[1] 13 24 15
#[1] 13 79 15
#[1] 18 63 15
#[1] 20 92 15
#[1] 22 36 15
#[1] 23 18 15
#[1] 29 44 15
#[1] 34 30 15
#[1] 37 80 15
#[1] 52 39 15
#[1] 52 68 15
#[1] 68 74 15
#[1] 69 25 15
#[1] 71 79 15
#[1] 72 14 15


