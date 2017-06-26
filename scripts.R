#runs the wilcox test against two datasets for columns in "range"
#range should be a vector of at least two numbers (e.g. 2:7 or c(1,4,5,6,7,99))
#Dsets must have columns line up (e.g. they both contain final grade data on column x) to give a meaningful answer
#values are named after dset1 columns
#returns a named matrix with w and p values
testChange <- function(dset1,dset2,range){
	temp = matrix (nrow = 2, ncol = length(range))
	i = 1
	dimnames(temp) = list(c("W", "p"), colnames(dset1[range]))
	for(col in range){
		output = wilcox.test(dset1[,col], dset2[,col])
		temp[1,i] = output$statistic
		temp[2,i] = output$p.value
		i= 1 + i
	}
	return (temp)
}
