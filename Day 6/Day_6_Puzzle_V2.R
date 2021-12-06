#Day 6 Puzzle Version 2

input <- scan("Day_6_input.txt",sep=",")

x0<-rep(0,9)

for(i in 1:9){
  x0[i] <- sum(input==(i-1))
}

for(i in 1:256){
  x0 <- x0[c(2,3,4,5,6,7,8,9,1)]
  x0[7] <- x0[7] + x0[9]
}

print(sum(x0),digits=12)
