# Day 16 Puzzle 1

input <- read.table("Day_16_Input.txt",colClasses = 'character')
input <- unlist(strsplit(as.character(input),split=""))


hex2bin <- function(x){
  convert <- data.frame("hex" = c(0:9,LETTERS[1:6]), "dec" = c('0000','0001','0010','0011','0100','0101','0110','0111','1000','1001','1010','1011','1100','1101','1110','1111'),stringsAsFactors = F)
  x <- convert$dec[which(convert$hex == x)]
  return(x)
}

input <- sapply(input, hex2bin)
input <-as.vector(unlist(strsplit(input,split="")))


literal_jump <- function(i,input){
  prefix <- input[i]
  i <- i+5
  while(prefix == 1){
    prefix <- input[i]
    i<-i+5
  }
  return(i)
}

packet_scan <- function(input, VerSum = 0, NumPackets = 1, LengthPackets = length(input),b=0){
  valueNP <- 0
  valueLP <- 0
  i<-1
  while(valueNP < NumPackets || valueLP < LengthPackets && length(unique(input[i:length(input)])) != 1){
    if(b == 1){return(c(VerSum,valueLP,valueNP,i,b))}
    VerSum <- VerSum + strtoi(paste0(input[i:(i+2)],collapse=''),2)
    i<-i+3
    ID <- strtoi(paste0(input[i:(i+2)],collapse=''),2)
    i<-i+3
    if(ID == 4){
      i <- literal_jump(i,input)
      valueLP <- i - valueLP
      valueNP <- valueNP + 1
      if(i < length(input) && length(unique(input[i:length(input)])) != 1){
        NumPackets <- max(1,NumPackets - valueNP)
        LengthPackets <- min(length(input), LengthPackets - valueLP)
        output <- packet_scan(input[i:length(input)], VerSum=VerSum, NumPackets, LengthPackets,b)
        VerSum <- output[1]
        valueLP <- output[2]
        valueNP <- output[3]
        i <- output[4]
        b <- output[5]
      }
      else {
        b<-1
        return(c(VerSum,valueLP,valueNP,i,b))
        print(VerSum)
      }
    }
    else {
      type <- input[i]
      i<-i+1
      if(type == as.character(0)){
        LengthPackets = strtoi(paste0(input[i:(i+14)],collapse=''),2)
        i<-i+15
        output <- packet_scan(input[i:length(input)], VerSum=VerSum, 1, LengthPackets,b)
        VerSum <- output[1]
        valueLP <- output[2]
        valueNP <- output[3]
        i <- output[4]
        b <- output[5]
      }
      if(type == as.character(1)){
        NumPackets = strtoi(paste0(input[i:(i+10)],collapse=''),2)
        i <- i+11
        output <- packet_scan(input[i:length(input)], VerSum=VerSum, NumPackets, length(input[i:length(input)]),b)
        VerSum <- output[1]
        valueLP <- output[2]
        valueNP <- output[3]
        i <- output[4]
        b <- output[5]
        }
      }
    }
  return(c(VerSum,valueLP,valueNP,i,b))
}

a <- packet_scan(input)
print(a[1])
