# Day 13

input <- read.table("Day_13_Input.txt",colClasses= 'character')
split_ind <- which(input == "fold")[1]
coords <- input[1:(split_ind-1),]
coords <- strsplit(coords, split=",")
coords <- matrix(as.numeric(unlist(coords)), nrow=(split_ind - 1), byrow = T)
folds <- input[split_ind:dim(input)[1],]
folds <- folds[which(folds != "fold" & folds != "along")]

for(i in 1:length(folds)){
  aor <- strsplit(folds[i],split="=")[[1]]
  num <- as.numeric(aor[2])
  var <- aor[1]
  if(var == "x"){
    dists <- abs(coords[,1] - num)
    inds <- which(coords[,1] < num)
    coords[inds,1] <- coords[inds,1] + 2*dists[inds]
    coords[,1] <- coords[,1] - (num + 1)
  }
  else {
    dists <- abs(coords[,2] - num)
    inds <- which(coords[,2] > num)
    coords[inds,2] <- coords[inds,2] - 2*dists[inds]
  }
}

plot(-coords)
