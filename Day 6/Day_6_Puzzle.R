#Day 5, Puzzle

library(expm)

input <- scan("Day_6_input.txt",sep=",")

x0<-matrix(ncol=1,nrow=9)

for(i in 1:9){
  x0[i,] <- sum(input==i)
}

A <- matrix(nrow=9,ncol=9,0)

for(i in 1:8){
  A[i,i+1]<-1
}
A[7,1] <- 1
A[9,1] <- 1

A80 <- A %^% 79
x80 <- A80 %*% x0

A256 <- A %^% 255
x256 <- A256 %*% x0

print(sum(x80))
print(sum(x256),digits=12)
