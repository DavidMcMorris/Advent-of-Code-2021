input<-read.table("Day_12_Input.txt",sep='-',stringsAsFactors=TRUE)
lvls <- factor(unique(c(input$V1,input$V2)))

caps <- lapply(input,toupper)
inds <- which(caps==input)
start <- which(input=="start")
end <- which(input=="end")

input[] <- lapply(input,factor,levels=lvls)
input<-data.frame(lapply(input,as.numeric))
adj_list<- list()
for(i in 1:max(input)){
	adj_list[[i]]<-c(input$V2[which(input$V1 == i)],input$V1[which(input$V2==i)])
}
inds <- unique(as.matrix(input)[inds])
caves <- rep(0,max(input))
caves[inds]<-1

start <- unique(as.matrix(input)[start])
end <- unique(as.matrix(input)[end])


paths <- 0

path_finder <- function(current,adj,paths,caves){
	if(current == end | length(adj[[current]]) == 0){
		if(current == end){
			paths <- paths + 1
		}
		return(paths)
	}
	if(caves[current] == 0){
		for(i in 1:length(caves)){
			adj[[i]] <- setdiff(adj[[i]],current)
		}
	}

	for(i in 1:length(adj[[current]])){
		paths <- path_finder(adj[[current]][i],adj,paths,caves)
	}
	return(paths)
}

number <- path_finder(start,adj_list,paths,caves)
print(number)