#Advent of Code Day 1, Puzzle 1

data <- read.csv("Puzzle_1_Data.csv", header = FALSE)
counter <- 0

for(i in 2:nrow(data)){
	if(data[i,1] > data[i-1,1]){
		counter <- counter + 1
	}
}

print(counter)