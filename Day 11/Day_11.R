# Day 11

# Initialize
input <- read.table("Day_11_Input.txt",colClasses= 'character')
input <- matrix(nrow=nrow(input),as.numeric(unlist(strsplit(as.matrix(input),split=""))),byrow=T)
dims <- dim(input)
ind_mat <- matrix(1:prod(dims),nrow = dims[1])

# Set up adjacency matrix
adj_mat <- matrix(0,nrow=prod(dims),ncol=prod(dims))

for(i in 1:prod(dims)){
  current <- arrayInd(i,dims)
  adjacent_inds <- rbind(current + c(0,1), current + c(1,0), current - c(0,1), current - c(1,0), current + c(1,1), current + c(1,-1), current + c(-1,1), current + c(-1,-1))
  adjacent_inds[which(adjacent_inds == (max(dims) + 1))] <- 0
  adjacent_inds <- adjacent_inds[which(rowSums(sign(adjacent_inds))==2),]
  adjacent_inds <- (adjacent_inds[,2]-1)*(dims[2]) + adjacent_inds[,1]
  adj_mat[i,ind_mat[adjacent_inds]]<-1
}

# Compute next matrix
flash <- function(mat, adj_mat){
  new_mat <- mat + 1
  if(sum(new_mat==10)>0){
    i <- 1
    inds <- which(new_mat == 10)
    while(i < (length(inds)+1)){
      new_mat[which(adj_mat[inds[i],]==1)] <- new_mat[which(adj_mat[inds[i],]==1)] + 1
      i <- i + 1
      inds <- c(inds, setdiff(which(new_mat == 10),inds))
    }
  }
  new_mat[which(new_mat >9)] <- 0
  return(new_mat)
}

P1_count <- 0
current_mat <- input
for(i in 1:100){
  next_mat <-flash(current_mat,adj_mat)
  P1_count <- P1_count + sum(next_mat==0)
  current_mat<-next_mat
}

P2_count <- 0
allflash <- 0
total <- prod(dims)
current_mat <- input
while(allflash == 0){
  next_mat <-flash(current_mat,adj_mat)
  P2_count <- P2_count + 1
  flashing <- sum(next_mat == 0)
  if(flashing == total){allflash = 1}
  current_mat <- next_mat
}

print(c(P1_count,P2_count))
