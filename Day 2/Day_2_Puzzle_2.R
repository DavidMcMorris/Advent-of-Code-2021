#Day 2, Puzzle 2

input <- read.table("Day_2_input.txt")
colnames(input) <- c("direction","value")

aim <- 0
depth <- 0

for(i in 1:nrow(input)){
	if(input$direction[i] == "forward"){
		depth <- depth + aim*input$value[i]}
	else if(input$direction[i] == "down"){
		aim <- aim + input$value[i]}
	else {aim <- aim - input$value[i]}
}

height <- sum(input$value[input$direction == "forward"])
position <- c(height, depth)
product <- prod(position)

print(product)