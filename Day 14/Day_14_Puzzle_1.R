# Day 14 Part 1
# This is how I solved part 1, it requires far too much memory for part 2

poly_temp <- read.table("Day_14_Input.txt",colClasses = 'character', nrows=1)
rules <- read.table("Day_14_Input.txt", colClasses = 'character', skip = 1)[,c(1,3)]


updater <- function(string){
  x<-strsplit(as.character(string),split="")[[1]]
  pairs <- NULL
  for(i in 1:(length(x)-1)){
    pairs <- c(pairs,paste0(x[i],x[i+1]))
  }

  new<-NULL
  for(i in 1:length(pairs)){
    a<-strsplit(pairs[i],split="")[[1]]
    new<-c(new,paste0(a[1],rules[which(rules[,1] %in% pairs[i]),2],a[2]))
  }

  new_split <- strsplit(paste0(new,collapse=""),split="")[[1]]

  new_string <- NULL
  for(i in 1:length(new_split)){
    if(i%%3 != 0){
      new_string <- c(new_string,new_split[i])
    }
    if(i==length(new_split)){
      new_string <- c(new_string,new_split[i])
    }
  }

  new_string <- paste0(new_string,collapse = "")
  return(new_string)
}

string <- poly_temp
for(i in 1:10){
  string <- updater(string)
}
counts <-table(strsplit(string,split="")[[1]])
answer <- diff(range(counts))

print(counts)
