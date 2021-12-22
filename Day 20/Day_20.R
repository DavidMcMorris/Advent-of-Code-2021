# Day 20

algo <- read.table("Day_20_Input.txt",colClasses='character',comment.char = ",",nrow=1)
algo <- strsplit(algo$V1,split="")[[1]]

im <- read.table("Day_20_Input.txt",colClasses = 'character', comment.char = ",", skip = 1)
im <- matrix(nrow = nrow(im),do.call('rbind', strsplit(as.character(im$V1),'',fixed=TRUE)))

padder_odd <- function(mat, char){
  rows <- nrow(mat)
  mat <- cbind(matrix(ncol=2,nrow=rows,char), mat, matrix(ncol=2,nrow=rows,char))
  cols <- ncol(mat)
  mat <- rbind(matrix(ncol=cols, nrow=2,char),mat,matrix(ncol=cols, nrow=2,char))
  return(mat)
}

padder_even <- function(mat, char){
  rows <- nrow(mat)
  mat <- cbind(matrix(ncol=3,nrow=rows,char), mat, matrix(ncol=3,nrow=rows,char))
  cols <- ncol(mat)
  mat <- rbind(matrix(ncol=cols, nrow=3,char),mat,matrix(ncol=cols, nrow=3,char))
  return(mat)
}

pixel_odd <- function(i, mat, algo){
  current <- arrayInd(i,dim(mat))
  box_inds <- rbind(current + c(-1,1), current + c(0,1), current + c(1,1), current + c(-1,0), current, current + c(1,0), current + c(-1,-1), current + c(0,-1), current + c(1,-1))
  box_inds[which(box_inds == (max(dim(mat)) + 1))] <- 0
  box_inds <- box_inds[which(rowSums(sign(box_inds))==2),]
  submat <- mat[min(box_inds[,1]):max(box_inds[,1]),min(box_inds[,2]):max(box_inds[,2])]
  submatvec <- as.vector(t(submat))
  submatvec[which(submatvec == ".")] <- 0
  submatvec[which(submatvec == "#")] <- 1
  submatvec <- paste0(as.numeric(submatvec),collapse = "")
  value <- strtoi(submatvec,base=2)
  pix <- algo[value+1]
  return(pix)
}

pixel_even <- function(i, mat, algo){
  current <- arrayInd(i,dim(mat))
  box_inds <- rbind(current + c(-1,1), current + c(0,1), current + c(1,1), current + c(-1,0), current, current + c(1,0), current + c(-1,-1), current + c(0,-1), current + c(1,-1))
  box_inds[which(box_inds == (max(dim(mat)) + 1))] <- 0
  box_inds <- box_inds[which(rowSums(sign(box_inds))==2),]
  submat <- mat[min(box_inds[,1]):max(box_inds[,1]),min(box_inds[,2]):max(box_inds[,2])]
  if(length(submat) != 9){
    pix <- algo[10]
  }
  else {
    submatvec <- as.vector(t(submat))
    submatvec[which(submatvec == ".")] <- 0
    submatvec[which(submatvec == "#")] <- 1
    submatvec <- paste0(as.numeric(submatvec),collapse = "")
    value <- strtoi(submatvec,base=2)
    pix <- algo[value+1]
  }
  return(pix)
}

# Set n = 2 for part 1, n = 50 for part 2
n <- 0
while(n < 50){
  padded_mat <- padder_odd(im, ".")
  new_mat <- matrix(nrow=nrow(padded_mat),ncol=ncol(padded_mat))

  for(i in 1:length(padded_mat)){
    new_mat[i] <- pixel_odd(i, padded_mat, algo)
  }
  n <- n+1
  if(n == 50){break}

  new_padded_mat <- padder_even(new_mat,"#")
  new_new_mat <- matrix(nrow=nrow(new_padded_mat),ncol=ncol(new_padded_mat))

  for(i in 1:length(new_padded_mat)){
    new_new_mat[i] <- pixel_even(i, new_padded_mat, algo)
  }

  im <- new_new_mat
  n <- n+1
}

print(sum(new_new_mat == "#"))
