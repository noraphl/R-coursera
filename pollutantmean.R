pollutantmean <- function(directory, pollutant, id = 1:332){
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files

	## 'pollutant' is a character vector of length 1 indicating
	## the name of the pollutant for which the mean will be 
	## calculated; could be "nitrate" or "sulfate"

	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used

	## Return the mean of the pollutant across all monitors list
	## in the 'id' vector (ignoring NA values)

	
	curr_dir <- getwd()	## Saves the working directory for restablishing it later
	setwd(directory)	## Sets the working directory to 'directory'
	
	## Create a list with all CSV files contained in 'directory'
	all_files <- list.files(pattern = "*.CSV", ignore.case = TRUE)	
	
	## Create a list with the contents of the selected files indicated in 'id',
	## each element of the list is a data frame
	sel_files <- lapply(all_files[id], read.csv)	
	
	setwd(curr_dir)		## Return to the original working directory

	## Merge all data frames in 'sel_files' into a single data frame
	df <- do.call("rbind", sel_files)	

	## Calculate the mean of the selected 'pollutant' ignoring missing values
	mean(df[[pollutant]][!is.na(df[[pollutant]])])

}