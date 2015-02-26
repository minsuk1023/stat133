# HW 4
# Writing functions
# Due Thursday February 26th by midnight 
# This .r file should contain your code


#### Function #1
# Implement the function "listLengths". 

# Input variable:
# <data.list>: a list whose elements are vectors of varying length

# Output variable:
# <element.lengths>: a numeric vector whose entries are the lengths of each
#   element of <data.list>

listLengths <- function(data.list) {
  listlength=c()
  for (i in 1:length(data.list)){
    listlength[i]=length(data.list[[i]])
  }
  return (listlength)
}

list=list(c(1,2,3,4),c(1,2,3,4,5),c(1,2,3,4,5,6))
listLengths(list)

#### Function 2
#### Implement the function "powers"

# Input variable :
# <x> : a numeric vector of length n
# <k> : an integer

# Output variable
# <x.powers> : A matrix of size [n x k] where the first column is x, the second column x^2, the third column x^4, etc.
#              the column names should be : "x", "x^2", "x^3" etc.

powers <- function(x, k){ 
  original_x=x
  for (i in 2:k) {
    x=c(x,original_x^i)
  }
  a=matrix(x,nrow=length(original_x),ncol=k)
  return (a)
}


#### Function #3
#### Implement the function "recipeConversion"

# Input variable:
# <recipe> : A data frame with three columns named "amount", "unit" and "ingredient"

# Output variable:
# <recipe.metric> : A data frame with three columns where cups have been converted to ml and ounces to grams.
#                   the number in "amount" should be updated, and the entry in "unit" changed
#                   both ml and gr should be rounded to the nearest multiple of 5,
#                   e.g. a row that read : [2 cups flour] should now be [475 ml flour]
#                   Note, if the "unit" is neither "cup"/"cups" nor "oz" the row should not be changed

# The conversion constants are: 
# 1 cup = 236.6 ml and 1 oz = 28.3 gr
# Please use these exact numbers, do not add decimal places.

# "unit" can take any of a number of values but you need to find the rows where
# "unit" is : "cup", "cups" or "oz"

# If the column names of the input data frame are not "amount", "unit" and "ingredient" the
# function should stop and print out an error message

# Put your code here
recipeConversion <- function(recipe){
  recipe=data.frame(recipe,stringsAsFactors=F)
  if (any(names(recipe)!=c("amount","unit","ingredient"))) {
    warning("Wrong type data frame")
    break
  }
  n=nrow(recipe)
  for (i in 1:n){
    if (any(recipe[i,2]=="cup"|recipe[i,2]=="cups")){
      recipe[i,1]=recipe[i,1]*236.6
      recipe[i,2]="ml"
    }
    else if (recipe[i,2]=="oz"){
      recipe[i,1]=recipe[i,1]* 28.3
      recipe[i,2]="gr"
    }
  }
  return (recipe)
}

df=data.frame(amount=c(1,2,3),unit=c("cup","cups","oz"),ingredient=c("a","b","c"))
df=data.frame(amount=c(1,2,3),unit=c("cup","cups","oz"),ingredient=c("a","b","c"),stringsAsFactors=F)

#### Function #4a
# Implement the function "bootstrapVarEst"

# Input variable:
# <x> : data vector

# Output variable:
# <boot.sigma.est> : Bootstrap estimate for the variance of the sample mean (see lecture notes)

bootstrapVarEst <- function(x, B){
  mu=c()
  for (i in 1:B){
    mu[i]=mean(sample(x,size=length(x),replace=TRUE))
  }
  return (var(mu))
}

#### Function #4b
#### Implement the function "jackknifeVarEst"

# Input variable:
# <x> : data vector

# Output variable:
# <jack.sigma.est> : Jackknife estimate for the variance of the sample mean (see lecture notes)

jackknifeVarEst <- function(x){
  mu=c()
  for (i in 1:length(x)){
    mu[i]=mean(x[-i])
  }
  return (var(mu))
}
x=c(1,2,3,4)

#### Function #4c
#### Implement the function "samplingVarEst"

# Input variables:
# <x> : data vector
# <type> : string that takes the values "bootstrap" or "jackknife", the default should be bootstrap.

# Output variable:
# <sampling.sigma.est> : The bootstrap estimate if type="bootstrap" and the jackknife estimate if type="jackknife"

# Note: this function calls the previous two functions.

samplingVarEst <- function(x,type="bootstrap"){
  if (type=="bootstrap"){
    return (bootstrapVarEst(x,5000))
  }
  else if (type=="jackknife"){
    return (jackknifeVarEst(x))
  }
}


