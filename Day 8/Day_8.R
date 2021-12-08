#Day 8

input <- read.table("Day_8_input.txt",sep=" ")

# Puzzle 1
outputs <- input[paste("V",12:15,sep="")]
outputs[] <- lapply(outputs, as.character)
output_lengths <- outputs
output_lengths[] <- lapply(outputs,nchar)
number <- sum(output_lengths == 2 | output_lengths == 3 | output_lengths == 4 | output_lengths == 7 )
print(number)
