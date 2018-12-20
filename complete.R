complete <- function(directory, id = 1:332){
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files

	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used

	## Return a dataframe of the form:
	## id nobs
	## 1  117
	## 2  1041
	## ...
	## where 'id' is the monitor ID number and 'nobs' is the
	## number of complete cases

	
	curr_dir <- getwd()	## Saves the working directory for restablishing it later
	setwd(directory)	## Sets the working directory to 'directory'
	
	## Create a list with all CSV files contained in 'directory'
	all_files <- list.files(pattern = "*.CSV", ignore.case = TRUE)	
	
	## Create a list with the contents of the selected files indicated in 'id'
	## each element of the list is a data frame
	sel_files <- lapply(all_files[id], read.csv)	
	
	setwd(curr_dir)		## Return to the original working directory

	nobs <- vector(length = length(id))	## Initialize a vector of the same length as 'id'
	
	## Loop through the list of data frames to obtain the number of complete observations
	for (i in 1:length(id)){
		nobs[i] <- sum(complete.cases(sel_files[[i]]))
	}

	data.frame(id,nobs)

}