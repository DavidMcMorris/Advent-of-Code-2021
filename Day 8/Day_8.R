#Day 8

input <- read.table("Day_8_input.txt",sep=" ")

# Puzzle 1
outputs <- input[paste("V",12:15,sep="")]
outputs[] <- lapply(outputs, as.character)
output_lengths <- outputs
output_lengths[] <- lapply(outputs,nchar)
number <- sum(output_lengths == 2 | output_lengths == 3 | output_lengths == 4 | output_lengths == 7 )
print(number)

# Puzzle 2
digits <- input[paste("V",1:10,sep="")]
digits[] <- lapply(digits, as.character)

# Determine which wire goes where for a set of digits
rewire <- function(x){
  wires <- NULL
  one <- strsplit(x[which(nchar(x) == 2)], split = "")[[1]]
  seven <- strsplit(x[which(nchar(x) == 3)], split = "")[[1]]
  four <- strsplit(x[which(nchar(x) == 4)], split = "")[[1]]
  length_fives <- x[which(nchar(x) == 5)]
  length_fives <- strsplit(length_fives, split = "")
  length_fives <- t(matrix(unlist(length_fives),nrow=5))
  length_fives_common <- intersect(length_fives[1,],intersect(length_fives[2,],length_fives[3,]))
  wires[6] <- intersect(setdiff(four,one),setdiff(letters[1:7],length_fives_common))
  wires[1] <- setdiff(seven,one)
  wires[7] <- intersect(length_fives_common,four)
  wires[4] <- setdiff(length_fives_common,wires)
  five <- length_fives[which(apply(length_fives,2,is.element,setdiff(setdiff(four,one),length_fives_common)),arr.ind=TRUE)[1],]
  wires[3] <- intersect(five,one)
  wires[2] <- setdiff(one,five)
  wires[5] <- setdiff(letters[1:7],wires)
  return(wires)
}

# Interpret a combination of segments as a number
interpreter <- function(x){
  seven_digit <- data.frame(arrangement = c("1111110","0110000","1101101","1111001","0110011","1011011","1011111","1110000","1111111","1111011"), number = 0:9, stringsAsFactors = FALSE)
  num <- seven_digit$number[which(seven_digit$arrangement == x)]
  return(num)
}

# Decode the four-digit output
four_digit_decoder <- function(digits, output){
  number <- NULL
  wiring <- rewire(digits)
  for(i in 1:4){
    number[i] <- interpreter(paste(as.numeric(wiring %in% strsplit(output[i],split="")[[1]]),collapse=""))
  }
  number <- as.numeric(paste(number, collapse = ""))
  return(number)
}

# Find all the four-digit outputs and add them
result <- 0
for(i in 1:nrow(outputs)){
  result <- result + four_digit_decoder(as.character(digits[i,]),as.character(outputs[i,]))
}

print(result)
