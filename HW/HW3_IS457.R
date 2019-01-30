# Do not remove any of the comments. These are marked by #

# HW 3 - Due Monday, Oct 1, 2018 in moodle and hardcopy in class

# (1). Please upload R code and report to Moodle 
#      with filename: HW3_IS457_YourClassID.
# (2). Turn in hard copy of your report in class
#      without your name but only your class ID

## Important: Make sure there is no identifying information on your printout, including name, username etc. 
## Only include your class ID on there.

### ClassID:127

# In this assignment you will practice "apply" family function
# You will also work with linear regression in R

# Part 1. Start with "apply" function (9pts)

# (1) Create a new matrix by the following codes, briefly but completely explain what each line is doing. (3pts)
set.seed(457)
one_num <- sample(1:9,25,replace=TRUE)
one_matrix <- matrix(one_num,ncol=5)

### Your answer here

# Use the "apply" function on one_matrix to answer questions(2)(3)(4).

# (2) Calculate the mean of each row. (1pt)
#     Calculate the sum of each column. (1pt)

### Your code here
rowmeans <- apply(one_matrix,1,mean)
print(rowmeans)
colmeans <- apply(one_matrix,2,mean)
print(colmeans)

# (3) Find the difference between the biggest and the smallest number for each row. (2pts)

### Your code here
difference<- function(x)
{
  difference = max(x)-min(x)
}
diff<- apply(one_matrix,1,difference)
print(diff)

# (4) Calculate the sum of all numbers smaller than 5 for each column. (2pts)

### Your code here

subsumcal<-function(x)
{
  if (x<=5)
    x=x
  else
    x=0
}

one_matrix1 = matrix(0,5,5) #tansform the matrix 
for(i in 1:5){
  for(j in 1:5)
    one_matrix1[i,j] = subsumcal(one_matrix[i,j])
}

print(one_matrix1) 
subsum<- apply(one_matrix1,2,sum)
print(subsum)

# Part 2. Get familiar with "sapply" and "lapply" functions (6pts)

# (5) Let's play with the "iris" dataset. Here's the command to load it:
data(iris)

#     Tell me the data types of each column(variable) by using "sapply" function (1pt)

### Your code here
sapply(iris,class)

# (6) Do question(5) again and get the same result (check the data type), but use "lapply" function. (1pt)
#     Based on the output format, briefly explain the difference between "sapply" and "lapply" functions. (2pts)

### Your code/answer here
lapply(iris,class)

# (7) Now create a new list from one_matrix by using the following codes.
list_1 <- list(a=one_matrix[,1],b=c(one_matrix[,2],one_matrix[,3]))

#     Take natural log of this list using the following code:
#log(list_1)

#     Please briefly explain why the code above doesn't work. (1pt)

#     Now write code that takes the natural log of list_1 (using the apply family of functions). (1pt)

### Your code/answer here
log_list<-list(loga = log(list_1[[1]]),logb =log(list_1[[2]]))
print(log_list)

# Part 3. Try "tapply" function and its equivalents. (12pts)

# we will use the data set "mtcars"; familiarize yourself with it first. Here is the code to load the data:
data(mtcars)

# (8) Subset 4 columns "mpg", "hp", "wt" and "am" to a new data frame, named df_car. (1pt)
#     In df_car, convert "am" to a factor variable with two levels: "automatic" and "manual" (2pts)
#     (Hint: read help documentation of mtcars)

### Your code here
df_car<-data.frame(mpg=mtcars$mpg,hp=mtcars$hp,wt=mtcars$wt,am =mtcars$am)
df_car$am = ifelse(df_car$am==0,"automatic","manual")
print(df_car)


# (9) Use "tapply" function on df_car to find the mean of mpg by different am levels. (2pts)

### Your code here
mean_mp<-tapply(df_car$mpg,df_car$am=="manual",mean)
names(mean_mp)<-c("automatic","manual")
print(mean_mp)

# (10) Look up the function "by".
#      Obtain the mean of mpg, hp and wt, by different am levels, using only one function call. (2pts)

### Your code here
by(df_car[,1:3],df_car[,4],colMeans)
# (11) Do question(10) again by using the "aggregate" function. You may need to look this up as well. (2pts)

### Your code here
aggregate(df_car[,1:3], list(df_car[,4]),mean)
# (13) Same as question(10), but this time use the combination of "apply" and "tapply" functions to get the same results. (3pts)

### Your code here
sort<-function(x){
  tapply(x,df_car$am=="manual",mean)
}
result<-apply(df_car[,1:3], 2, sort)
rownames(result)<-c("automatic","manual")

# Part 4. More functions in "apply" family (4pts)

# (14) "mapply" function is a multivariate version of "sapply".
#      Create list_2 by following code:
list_2 <- list(a=one_matrix[,2],b=c(one_matrix[,3],one_matrix[,4]))
#      Add up corresponding elements in list_1 and list_2, then take natural log of it. (2pts)

### Your code here
list3<-c(a=list(rep(0,5)),b = list(rep(0,10)))
add<-function(x){
  list3[[x]] <- list_1[[x]]+list_2[[x]]
}
result14<-mapply(add,1:length(list_2))
mapply(log,result14)

# (15) "rapply" function is used to apply a function to all elements of a list recursively.
#      Create list_3 by following code:
list_3 <- list(aa=one_matrix[,1],b=c("this","is","character"))
#      Calculate the natural log of all integer in list_3. (2pts)
#      Do not remove characters in the list or subsetting.

### Your code here
logint<-function(x){
  if (class(x) == 'integer')
    y = log(x)
  else
    y =x
}
rapply(list_3,logint)

# Part 5. Linear regression (9pts)

# (16) Look back in the "iris" dataset.
data(iris)
#      Fit a simple linear regression model using lm() to predict Petal.Length from Petal.Width. (2pts)

#      How do you interpret the result of regression? Hint: interpret the two coefficients from the output of lm(). (2pts)

### Your code/answer here
data(iris)
lm(Petal.Width~Petal.Length, data = iris)

# (17) Create a scatterplot with x-axis of Petal.Width and y-axis of Petal.Length. (2pt)

#      Add the linear regression line you found above to the scatterplot. (1pt) 

#      Provide an interpretation for your plot. (2pts)

### Your code/answer here
plot(x=iris$Petal.Width,y=iris$Petal.Length,xlab = "Petal.Width",ylab="Petal.Length")
abline(lm(Petal.Length~Petal.Width, data = iris),col = "red")
