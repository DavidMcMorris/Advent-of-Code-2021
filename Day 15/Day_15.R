# Day 15

Puzzle <- 1 #Either 1 or 2

input1 <- read.table("Day_15_Input.txt",colClasses= 'character')
input1 <- matrix(nrow=nrow(input1),as.numeric(unlist(strsplit(as.matrix(input1),split=""))),byrow=T)

# Set Up Full Matrix For Puzzle 2
if(Puzzle == 2){
  input2 <- input1 + 1
  input2[input2==10]<-1
  input3 <- input2 + 1
  input3[input3 == 10] <- 1
  input4 <- input3 + 1
  input4[input4 == 10] <- 1
  input5 <- input4 + 1
  input5[input5 == 10] <- 1
  FullColumn <- rbind(input1,input2,input3,input4,input5)
  FullInput <- FullColumn
  for(i in 1:4){
    FullColumn <- FullColumn + 1
    FullColumn[FullColumn == 10] <- 1
    FullInput <- cbind(FullInput,FullColumn)
  }
}

if(Puzzle == 1){
  input <- input1
}
else {input <- FullInput}

# Setup for Dijkstra's Algorithm
dims <- dim(input)
costs <- matrix(nrow=dims[1],ncol=dims[2],Inf)
costs[1] <- 0
unvisited <- matrix(nrow=dims[1],ncol=dims[2],1)
start <- 1
end <- prod(dims)

# Dijkstra's Algorithm
current <- 1
while(current != end){
  current <- which(costs == min(costs[which(unvisited==1)]) & unvisited==1)[1]
  currentAI <- arrayInd(current,dims)
  adjacent_inds <- rbind(currentAI + c(0,1), currentAI + c(1,0), currentAI - c(0,1), currentAI - c(1,0))
  adjacent_inds[which(adjacent_inds == (max(dims) + 1))] <- 0
  adjacent_inds <- adjacent_inds[which(rowSums(sign(adjacent_inds))==2),]
  connected_verts <- (adjacent_inds[,2]-1)*(dims[2]) + adjacent_inds[,1]
  for(i in 1:length(connected_verts)){
    j <- connected_verts[i]
    costs[j] <- min(costs[j],costs[current] + input[j])
  }
  unvisited[current] <- 0
}
