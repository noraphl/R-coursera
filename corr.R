corr <- function(directory, threshold = 0){
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files

	## 'threshold' is a numeric vector of length 1 indicating the 
	## numer of complete observations requiered to compute the 
	## correlation between nitrate and sulfate; the default is 0

	## Return a numeric vector of correlations


	curr_dir <- getwd()	## Saves the working directory for restablishing it later
	setwd(directory)	## Sets the working directory to 'directory'
	
	## Create a list with all CSV files contained in 'directory'
	all_files <- list.files(pattern = "*.CSV", ignore.case = TRUE)	
	
	## Create a list with the contents of each file as a data frame
	sel_files <- lapply(all_files, read.csv)	
	
	setwd(curr_dir)		## Return to the original working directory

	nobs <- vector(length = length(sel_files))	## Initialize a vector of the same length
	
	## Loop through the list of data frames to obtain the number of complete observations in each one
	for (i in 1:length(sel_files)){
		nobs[i] <- sum(complete.cases(sel_files[[i]]))
	}

	comp_files <- sel_files[nobs > threshold]

	corr <- vector('numeric')

	if (length(comp_files) == 0){
		corr
	}
	else {
		for (i in 1:length(comp_files)){
			corr[i] <- as.numeric(cor(comp_files[[i]]$nitrate, comp_files[[i]]$sulfate, use = "complete.obs"))
		}
	}
	corr

}