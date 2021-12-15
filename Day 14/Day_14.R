# Day 14
# This solves either part 1 or part 2

# Either 10 or 40 for part 1 or part 2, respectively
n <- 40

poly_temp <- read.table("Day_14_Input.txt",colClasses = 'character', nrows=1)
rules <- read.table("Day_14_Input.txt", colClasses = 'character', skip = 1)[,c(1,3)]
colnames(rules)<-c("pair","ins")
rules$New1 <- ""
rules$New2 <- ""
rules$Split1 <- ""
rules$Split2<- ""

for(i in 1:nrow(rules)){
  split <- strsplit(rules$pair[i],"")[[1]]
  rules$Split1[i] <- split[1]
  rules$Split2[i] <- split[2]
  rules$New1[i] <- paste0(split[1],rules$ins[i],collapse="")
  rules$New2[i] <- paste0(rules$ins[i],split[2],collapse="")
}
rules<-rules[order(rules$pair),]

initial_split <- strsplit(as.character(poly_temp),split="")[[1]]

initial_pairs <- NULL
for(i in 1:(length(initial_split)-1)){
  initial_pairs <- c(initial_pairs, paste0(initial_split[i],initial_split[i+1]))
}

initial_freq <- as.data.frame(table(initial_pairs))

counts <- data.frame("pair" = rules$pair,"previous" = 0)
counts <- counts[order(counts$pair),]
counts$previous[which(counts$pair %in% initial_freq[,1])] <- initial_freq[,2]


for(i in 1:n){
  counts$current <- 0
  non_zero_inds <- which(counts$previous != 0)
  for(j in 1:length(non_zero_inds)){
    first <- rules$New1[non_zero_inds[j]]
    second <- rules$New2[non_zero_inds[j]]
    counts$current[which(first==rules$pair)] <- counts$current[which(first==rules$pair)] + counts$previous[non_zero_inds[j]]
    counts$current[which(second==rules$pair)] <- counts$current[which(second==rules$pair)] + counts$previous[non_zero_inds[j]]
  }
  counts$previous <- counts$current
}

letts <- unique(unlist(strsplit(rules$pair,"")))
final_counts <- rep(0,length(letts))
for(i in 1:length(letts)){
  num <- sum(counts$previous[which(rules$Split1==letts[i])], counts$previous[which(rules$Split2==letts[i])])
  if(letts[i] == initial_split[i] | letts[i] == initial_split[length(initial_split)]){
    final_counts[i] <- (num + 1)/2
  }
  else {
    final_counts[i] <- num/2
  }
}

print(ceiling(diff(range(final_counts))),digits=16)
