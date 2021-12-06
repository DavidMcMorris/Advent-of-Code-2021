#Day 5, Puzzle 1


input <- read.table("Day_5_input.txt", colClasses="character")
# input <- read.table("test_input.txt", colClasses="character")

input <- data.frame(input$V1,input$V3)
colnames(input) <- c("start","end")

start_mat <- matrix(nrow = nrow(input),do.call('rbind', strsplit(as.character(input$start),split=',')))
end_mat <- matrix(nrow = nrow(input),do.call('rbind', strsplit(as.character(input$end),split=',')))

start_mat <- matrix(as.numeric(start_mat),nrow=nrow(input))
end_mat <- matrix(as.numeric(end_mat),nrow=nrow(input))

h_inds <- which(start_mat[,1] == end_mat[,1])
v_inds <- which(start_mat[,2] == end_mat[,2])

h_lines <- cbind(start_mat[v_inds,],end_mat[v_inds,])
v_lines <- cbind(start_mat[h_inds,],end_mat[h_inds,])

h_coords <- NULL
for(i in 1:nrow(h_lines)){
	h_coords <- rbind(h_coords,cbind(h_lines[i,1]:h_lines[i,3],rep(h_lines[i,2],abs(h_lines[i,3] - h_lines[i,1])+1)))
}

v_coords <- NULL
for(i in 1:nrow(v_lines)){
	v_coords <- rbind(v_coords,cbind(rep(v_lines[i,1],abs(v_lines[i,4] - v_lines[i,2])+1),v_lines[i,2]:v_lines[i,4]))
}

coords <- rbind(h_coords,v_coords)
strings <- paste(coords[,1],coords[,2])

number <- sum(table(strings)==max(table(strings)))

print(number)
