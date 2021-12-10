# Day 9

input <- read.table("Day_9_Input.txt",colClasses= 'character')
input <- matrix(nrow=nrow(input),as.numeric(unlist(strsplit(as.matrix(input),split=""))),byrow=T)
dims <- dim(input)

# Part 1

# Find where elevation increases in each direction
DU <- rbind(rep(1,dims[2]),(diff(input) < 0) * 1)
UD <- rbind((diff(input) > 0) * 1,rep(1,dims[2]))
LR <- cbind(t((diff(t(input)) > 0)) * 1,rep(1,dims[1]))
RL <- cbind(rep(1,dims[1]),t((diff(t(input)) < 0)) * 1)

# Find where elevation increases in every direction simultaneously and compute total_risk
total_risk <- sum(LR*RL*UD*DU*(input+1))
print(total_risk)


# Part 2

# Ignore peaks
indices <- which(input < 9)
sink_counter <- matrix(0,nrow=dims[1]*dims[2],ncol=1)

# Trace a starting point to a low point
low_point_finder <- function(coord, input, dims){
  searching <- 1
  current <- arrayInd(coord,dims)
  while(searching == 1){
    previous <- current
    adjacent_inds <- rbind(current, current + c(0,1), current + c(1,0), current - c(0,1), current - c(1,0))
    adjacent_inds[which(adjacent_inds == (dims[1] + 1) | adjacent_inds == (dims[2] + 1))] <- 0
    adjacent_inds <- adjacent_inds[which(rowSums(sign(adjacent_inds))==2),]
    current <- adjacent_inds[arrayInd(which(input[adjacent_inds] == min(input[adjacent_inds]))[1],dim(adjacent_inds))[1],]
    if(setequal(current,previous) == TRUE){
      searching <- 0
      index <- dims[1]*(current[2]-1) + current[1]
      return(index)
    }
  }
}

# Loop over each possible starting point and trace to low point
for(i in 1:length(indices)){
  low_point <- low_point_finder(indices[i], input, dims)
  sink_counter[low_point,1] <- sink_counter[low_point,1] + 1
}

# Compute/print final answer
print(prod(rev(sink_counter[order(sink_counter),])[1:3]))
