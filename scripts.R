#runs the wilcox test against two datasets for columns in "range"
#range should be a vector of at least two numbers (e.g. 2:7 or c(1,4,5,6,7,99))
#Dsets must have columns line up (e.g. they both contain final grade data on column x) to give a meaningful answer
#values are named after dset1 columns
#returns a named matrix with w and p values, as well as "improvement"
#"Improvement" is positive if dset2 mean > dset1, zero if the same, negative otherwise
testChangeV <- function(dset1,dset2,range){
	temp = matrix (nrow = 3, ncol = length(range))
	i = 1
	dimnames(temp) = list(c("W", "p","improvement"), colnames(dset1[range]))
	for(col in range){
		output = wilcox.test(dset1[,col], dset2[,col])
		temp[1,i] = output$statistic
		temp[2,i] = output$p.value
		temp[3,i] = .05<output$p.value
		if(mean(dset1[,col])>mean(dset2[,col])){
        	temp[3,i] = -1;
        }
        else
        {
        	if(mean(dset1[,col])<mean(dset2[,col])){
        		temp[3,i] = 1;
        	}
        	else
        	{
        		temp[3,i] = 0;
        	}
        }
		i= 1 + i
	}
	return (temp)
}
#works the same as above but range is now a ?x2 matrix. 
#The first column contains the column numbers for dset1, the second for dset2
#This is so one can have columns that don't line up between datasets
testChangeM <- function(dset1,dset2,range){
    temp = matrix (nrow = 3, ncol = length(range[,1]))
    dimnames(temp) = list(c("W", "p","improvement"), colnames(dset1[range[,1]]))
    for(i in 1:length(range[,1])){
        output = wilcox.test(dset1[,range[i,1]], dset2[,range[i,2]])
        temp[1,i] = output$statistic
        temp[2,i] = output$p.value
        if(mean(dset1[,range[i,1]])>mean(dset2[,range[i,2]])){
        	temp[3,i] = -1;
        }
        else
        {
        	if(mean(dset1[,range[i,1]])<mean(dset2[,range[i,2]])){
        		temp[3,i] = 1;
        	}
        	else
        	{
        		temp[3,i] = 0;
        	}
        }
    }
    return (temp)
}
