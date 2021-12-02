#Advent of Code Day 1, Puzzle 2

data <- read.csv("Puzzle_1_Data.csv", header = FALSE)
counter <- 0

current_window <- sum(data[1:3,1])
next_window <- 0

for(i in 2:(nrow(data)-2)){
	next_window <- sum(data[i:(i+2),1])
	if(next_window > current_window){
		counter <- counter + 1
	}	
	current_window <- next_window
}

print(counter)
