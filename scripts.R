#runs the wilcox test against two datasets for columns in "range"
#range should be a vector of at least two numbers (e.g. 2:7 or c(1,4,5,6,7,99))
#Dsets must have columns line up (e.g. they both contain final grade data on column x) to give a meaningful answer
#values are named after matx1 columns
#returns a named matrix with w and p values, as well as "improvement"
#"Improvement" is positive if matx2 mean > matx1, zero if the same, negative otherwise
testChangeV <- function(matx1,matx2,range){
    result = matrix (nrow = 3, ncol = length(range))
    i = 1
    dimnames(result) = list(c("W", "p","improvement"), colnames(matx1[,range]))
    for(col in range){
        output = wilcox.test(matx1[,col], matx2[,col])
        result[1,i] = output$statistic
        result[2,i] = output$p.value
        result[3,i] = .05<output$p.value
        if(mean(matx1[,col])>mean(matx2[,col])){
            result[3,i] = -1;
        }
        else
        {
            if(mean(matx1[,col])<mean(matx2[,col])){
                result[3,i] = 1;
            }
            else
            {
                result[3,i] = 0;
            }
        }
        i= 1 + i
    }
    return (result)
}


#works the same as above but range is now a ?x2 matrix. 
#The first column contains the column numbers for matx1, the second for matx2
#This is so one can have columns that don't line up between datasets
#Dsets should be numeric matrices
testChangeM <- function(matx1,matx2,range){
	#the output matrix
    result = matrix (nrow = 3, ncol = length(range[,1]))
    dimnames(result) = list(c("W", "p","improvement"), colnames(matx1[range[,1]]))
    #Runs the Wilcox test for each pair of data
    for(i in 1:length(range[,1])){
        output = wilcox.test(matx1[,range[i,1]], matx2[,range[i,2]])
        result[1,i] = output$statistic
        result[2,i] = output$p.value
        #Says whether tests improved or not
        if(mean(matx1[,range[i,1]])>mean(matx2[,range[i,2]])){
        	result[3,i] = -1;
        }else{
        	if(mean(matx1[,range[i,1]])<mean(matx2[,range[i,2]])){
        		result[3,i] = 1;
        	}else{
        		result[3,i] = 0;
    }}}
    return (result)
}

#For a paired t-test, if your dataset has both the before and after values, put it in as both matx1 and matx2. pair = true
#range should be a matrix with indexes in column one corresponding to index in column two (e.g. range[1,1] is pre-Q1 and range [1,2] is post-Q1)

massTTestM <- function(matx1,matx2,range, pair = FALSE){
	#Create output matrix
    result = matrix (nrow = 3, ncol = length(range[,1]))
    dimnames(result) = list(c("t", "p","df"), colnames(matx1[range[,1]]))
    #extract relavant data from test for each index
    for(i in 1:length(range[,1])){
        output = t.test(matx1[,range[i,1]], matx2[,range[i,2]], paired = pair)
        result[1,i] = output$statistic
        result[2,i] = output$p.value
        result[3,i] = output$parameter
    }
    return (result)
}
#Outputs matrix of tallied responses(including NA)
tallyResponse <- function(matrix,range,numBins, label){
	#Output matrix
    output = matrix(ncol=numBins+1)
    colnames(output) = label[1:ncol(output)]
    for(i in range){
    	#separates data into numBins different factors
        temp = cut(matrix[,i],0:numBins)
        #handles whether N/A got counted or not
        if(length(summary(temp)) < 6){
        	output = rbind(output, c(summary(temp),0))
        }else{
        		output = rbind(output, summary(temp))
        }
        
    }
    output = output[-1,]
    rownames(output) = colnames(matrix[,range])
    return(output)
}


#Does the above in terms of percentages (100% = 1). Doesn't count N/A responses in total
percentTally <- function(matrix,range,numBins,label){
    output = matrix(ncol=numBins)
    colnames(output) = label[1:ncol(output)]
    for(i in range){
        temp = summary(cut(matrix[,i],0:numBins))
        if(length(temp) < 6){
        	temp=c(temp,0)
        }
        temptot = temp/(sum(temp) - temp[length(temp)])
        #Removes NAs
        temptot = temptot[-(length(temptot))]
        output = rbind(output, temptot)
    }
    output = output[-1,]
    rownames(output) = colnames(matrix[,range])
    return(output)	
}

#convert a column to a factor
factor = cut((matrix[,column"#"]),number of bins, labels = vector of labels)
#Name "NA" factor (not sure if this is necessary for most things but just in case)
`levels<-`(addNA(factor), c(levels(factor), "No Response"))

#Use fisher's test measuring the difference between soft drops in 15 and in 16

#Compare final grades by demographics

#Poster: Report findings (what was significant, what were we measuring (dfw) what groups showed what tendencies)

#ToDone: Now that I've gotten a list of indexes for each demographic, find a way to run the wilcox test on just those indices. (f15gradesM[asian15,] gives the right input data)

#Runs Chi-Square test on 2 matricies, in columns specified in range (vector). outputs X-squared, P value and degrees of freedom.
compareSurvey <- function(matx1,matx2,range){
    output = matrix(ncol = 3, nrow = length(range))
    colnames(output) = c("X-square", "p","df")
    for(i in 1:length(range)){
        temp = rbind(matx1[range[i],],matx2[range[i],])
        result = chisq.test(temp)
        temp2 = c(result$statistic,result$p.value,result$parameter)
        output[i,1] = result$statistic
        output[i,2] =result$p.value
        output[i,3] =result$parameter
    }
    rownames(output) = rownames(matx1[range,])
    return(output)
}
 #How to select the numbers NOT in a vector
 #temp = c(1,2,3,5,9)
 #temp = c(1:20,temp)
 #temp = Filter(function(elem) length(which(temp == elem)) <= x,temp) This returns all values that are there x times or less
 #this would return 4,6,7,8,10-20
 invertVector <- function(x,min,max) {
 	x = c(min:max,x)
 	x = Filter(function(elem) length(which(x == elem)) <= 1,x)
 	return(x)
 }
