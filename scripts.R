#Compares the means of columns within in "range" of two given datasets. A positive value means dset2>dset1
#range should be a vector of at least two numbers (e.g. 2:7 or c(1,4,5,6,7,99))
#Dsets must have columns line up (e.g. they both contain final grade data on column x) to give a meaningful answer
#values are named after dset1 columns
testChange <- function(dset1,dset2,range){
	diffMean = colMeans(dset2[,range]) - colMeans(dset1[,range])
	return (diffMean)
}