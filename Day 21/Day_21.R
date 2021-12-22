# Day 21 Par 1

P1 <- 6
P2 <- 9

P1Score <- 0
P2Score <- 0

rolls <- rowSums(matrix(ncol=3,1:99,byrow=T))
rolls <- c(rolls,rowSums(matrix(ncol=3,c(100,1:98),byrow=T)))
rolls <- c(rolls,rowSums(matrix(ncol=3,c(99,100,1:97,98,99,100),byrow=T)))

n <- 0
num_rolls <- 0
while(P2Score < 1000){
  n <- n + 1
  if(n == (length(rolls)+1)){n <- 1}
  num_rolls <- num_rolls + 3
  P1 <- P1 + rolls[n]
  P1 <- as.numeric(rev(strsplit(as.character(P1),split="")[[1]])[1])
  if(P1 == 0){P1 <- 10}
  P1Score <- P1Score + P1

  if(P1Score >= 1000){break}
  n <- n+1
  if(n == (length(rolls)+1)){n <- 1}
  num_rolls <- num_rolls + 3
  P2 <- P2 + rolls[n]
  P2 <- as.numeric(rev(strsplit(as.character(P2),split="")[[1]])[1])
  if(P2 == 0){P2 <- 10}
  P2Score <- P2Score + P2
}

final_answer <- num_rolls*min(P1Score,P2Score)
print(final_answer)
writeClipboard(as.character(final_answer))
