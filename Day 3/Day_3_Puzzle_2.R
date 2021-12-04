#Day 3, Puzzle 2


input <- read.table("Day_3_input.txt",colClasses="character")

char_mat <- matrix(nrow = nrow(input),do.call('rbind', strsplit(as.character(input$V1),'',fixed=TRUE)))

num_mat <- matrix(as.numeric(char_mat),nrow=nrow(input))

nrows <- nrow(num_mat)

col_sums <- as.numeric(colSums(num_mat))

current_mat <- num_mat

for(i in 1:ncol(num_mat)){
	if(!is.vector(current_mat)){
		nrows <- nrow(current_mat)
		if(sum(current_mat[,i]) < nrows/2){current_mat <- current_mat[current_mat[,i]==0,]}
		else {current_mat <- current_mat[current_mat[,i]==1,]}
	}
}

O2_gen <- current_mat

current_mat <- num_mat

for(i in 1:ncol(num_mat)){
	if(!is.vector(current_mat)){
		nrows <- nrow(current_mat)
		if(sum(current_mat[,i]) < nrows/2){current_mat <- current_mat[current_mat[,i]==1,]}
		else {current_mat <- current_mat[current_mat[,i]==0,]}
	}
}

CO2_scrub <- current_mat

O2_gen_int <- strtoi(paste(O2_gen,collapse=""),base = 2)
CO2_scrub_int <- strtoi(paste(CO2_scrub,collapse=""),base = 2)

life_support_rating <- O2_gen_int * CO2_scrub_int

print(life_support_rating)
