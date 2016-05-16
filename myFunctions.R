add2 <- function(x, y) {
  x + y
}

above10 <- function (x) {
  use <- x >10
  x[use] 
}

columnmean <- function (y, removeNA = TRUE) {
  
  nc <- ncol(y)
  
  means <- numeric(nc)
  
  for (i in 1:nc) {
     means[i] <- mean(y [, i], na.rm = removeNA)
  }
  means
}



pollutantmean <- function (directory, pollutant, id=1:332) {

	#setwd(directory)

	all_file_list <- list.files(path = file.path(".", directory))

	selected_file_list <- all_file_list[id]

	#print(selected_file_list)

	for (file in selected_file_list){
       
 		# if the merged dataset doesn't exist, create it
		if (!exists("dataset")){
    			dataset <- read.table(file.path(".", directory, file), header=TRUE, sep=",")
		} else {
 
    			temp_dataset <-read.table(file.path(".", directory, file), header=TRUE, sep=",")
    			dataset<-rbind(dataset, temp_dataset)
	    		rm(temp_dataset)
  		}
 	}
	#debug <- paste("# of row:", nrow(dataset))
	#print(debug)
	colmean <- mean(dataset[, eval(pollutant)], na.rm = TRUE)

	print(round(colmean, 3))
}


complete <- function (directory, id = 1:332) {

	all_file_list <- list.files(path = file.path(".", directory))

	selected_file_list <- all_file_list[id]

	#print(selected_file_list)

	for (file in selected_file_list){
       
		
 		# if the merged dataset doesn't exist, create it
		if (!exists("dataset")){
  			dataset <- read.table(file.path(".", directory, file), header=TRUE, sep=",")

		} else {
 
    			temp_dataset <-read.table(file.path(".", directory, file), header=TRUE, sep=",")
    			dataset<-rbind(dataset, temp_dataset)
	    		rm(temp_dataset)
  		}
 	}
	
	vdataset <- na.omit(dataset)
	
	finaldataset = data.frame(table(vdataset$ID))
	
	#print(finaldataset)

	finaldataset


}



corr <- function (directory, threshold = 0) {

	all_file_list <- list.files(path = file.path(".", directory), pattern="*.csv", full.names=TRUE)

	selected_file_list <- all_file_list

	print(selected_file_list)
	

	datalist <- lapply(selected_file_list, read.table, header=TRUE, sep=",")
	
	datalistsize <- length(datalist)
	vResult = c()	
	
	n <- 1

	for (i in 1:datalistsize)
	{
		vdataset <- datalist [[i]]
		fdataset <- na.omit(vdataset)
		#print(fdataset)
		
		if (nrow(fdataset) > threshold) {
			
			vResult[n] <- cor(fdataset$sulfate, fdataset$nitrate)
			
			n <- n + 1

			print(paste("vdataset: ", nrow(vdataset), " fdataset", nrow(fdataset)))
		}
	}

	#vResult <- na.omit(vResult)
	vResult

	

}






