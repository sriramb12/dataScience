## ------------------------------------------------------------------------
# getwd()

## ------------------------------------------------------------------------
# setwd()

## ------------------------------------------------------------------------
mtcars = read.csv(file = "mtcars.csv",header=TRUE,sep=",")

class(mtcars)

## ------------------------------------------------------------------------
head(mtcars)

## ------------------------------------------------------------------------
tail(mtcars)

## ------------------------------------------------------------------------
colnames(mtcars)

## ------------------------------------------------------------------------
rownames(mtcars)

## ------------------------------------------------------------------------
rownames(mtcars) = mtcars$X
rownames(mtcars)
mtcars$X=NULL
mtcars

## ------------------------------------------------------------------------
colnames(mtcars)[6] = "weight"

## ------------------------------------------------------------------------
# Data type of the Column checked by is.* will return in a logical vector of length 1
is.numeric(mtcars$Cars)
is.character(mtcars$hp)

## ------------------------------------------------------------------------
str(mtcars)

## ------------------------------------------------------------------------

mtcars$cyl = as.factor(mtcars$cyl)
# converting am column to factor 
mtcars$am = as.factor(mtcars$am)


## ------------------------------------------------------------------------
mtcars[,'carb']


## ------------------------------------------------------------------------
mtcars[,11]


## ------------------------------------------------------------------------
mtcars[,c('mpg','carb')]

## ------------------------------------------------------------------------

mtcars[,c(1,11)]
class(mtcars[,c(1,11)])


## ------------------------------------------------------------------------
rownames(mtcars)
mtcars['Duster 360',]
class(mtcars['Duster 360',])

## ------------------------------------------------------------------------
mtcars[7,]
class(mtcars[7,])

## ------------------------------------------------------------------------
mtcars[c(6,7),]
class(mtcars[c(6,7),])

## ------------------------------------------------------------------------
#Subsetting data
s1 <-subset(mtcars,select=c(cyl,mpg))
s1

## ------------------------------------------------------------------------
mtcars[c('Duster 360','Valiant'),c('mpg','hp')]
class(mtcars[c('Duster 360','Valiant'),c('mpg','hp')])

## ------------------------------------------------------------------------
mtcars[c(6,7),c(1,2)]
class(mtcars[c(6,7),c(1,2)])

## ------------------------------------------------------------------------
colsum=c()
for (i in colnames(mtcars))
{
  sum = 0
  for (j in mtcars[,i])
  {
    if(is.na(j)==TRUE)
    {
      sum = sum+1
    }else
    {
      sum = sum
    }
  }
  colsum[i] = sum
}

colsum

## ------------------------------------------------------------------------
colSums(is.na(mtcars))

## ------------------------------------------------------------------------
sum(is.na(mtcars))

## ------------------------------------------------------------------------
library(DMwR)
mtcars_imputed  = centralImputation(mtcars)
colSums(is.na(mtcars_imputed))

## ------------------------------------------------------------------------
mtcars_imputed$pwr <-mtcars_imputed$hp/mtcars_imputed$weight 
head(mtcars_imputed)

## ------------------------------------------------------------------------
mtcars_imputed $hp
# After looking the numbers we can get the idea of how to bin them 
bins = cut(mtcars_imputed $hp,breaks = seq(0,500,20),include.lowest = T,right = T)
bins
# Count the number of rows that comes under a particular bin
table(bins)


## ------------------------------------------------------------------------
library(infotheo)
(nbins = floor(sqrt(NROW(mtcars_imputed $hp))))
discretize(mtcars_imputed $hp,disc = "equalwidth",nbins)

## ------------------------------------------------------------------------
discretize(mtcars_imputed $hp,disc = "equalfreq",nbins)

## ------------------------------------------------------------------------
str(mtcars_imputed)

## ------------------------------------------------------------------------
library(dummies)
dummy(mtcars_imputed$vs)

## ------------------------------------------------------------------------
library(vegan)
mtcars_imputed_standardized <- decostand(x =mtcars_imputed[,c('mpg','disp','drat','weight','qsec','gear','carb')],method ="range",MARGIN = 2)
str(mtcars_imputed_standardized)
range(mtcars_imputed_standardized$mpg)

## ------------------------------------------------------------------------
pow <- function(x, y = 3) #Default Values for y Arguments
  {
  result <- x^y
  print(paste(x, "raised to the power", y, "is", result))
}
pow(3)

## ------------------------------------------------------------------------
pow(3) # takes the default y value

pow(8, 2) # we can pass the arguments , r considers them in the same order and passes them in the place of prameters

pow(x = 8, y = 2) # Named Arguments 

pow(y = 2, x = 8)

## ------------------------------------------------------------------------
check = function(x) {
  if (x > 0) {
    result <- "Positive"
  }
  else if (x < 0) {
    result <- "Negative"
  }
  else {
    result <- "Zero"
  }
  return(result)
}

check(1)

## ------------------------------------------------------------------------
# If there are no explicit returns from a function, the value of the last evaluated expression is returned automatically in R.

check <- function(x) {
  if (x > 0) {
    result <- "Positive"
  }
  else if (x < 0) {
    result <- "Negative"
  }
  else {
    result <- "Zero"
  }
  result
}

check(-1)


## ------------------------------------------------------------------------
multiReturn = function() {
  myList <- list(1, 20, c("a","b"))
  return(myList) 
}

multiReturn()

## ------------------------------------------------------------------------
lapply(mtcars['mpg'],mean)
class(lapply(mtcars['mpg'],mean))

## ------------------------------------------------------------------------
sapply(mtcars['mpg'],max)
class(sapply(mtcars['mpg'],max))

## ------------------------------------------------------------------------
str(mtcars)

## ------------------------------------------------------------------------
tapply(mtcars$weight, mtcars$cyl, mean)

## ------------------------------------------------------------------------
tapply(mtcars$hp,mtcars$cyl,max)

## ------------------------------------------------------------------------
colnames(mtcars_imputed)
colnames(mtcars_imputed[,-c(1)])
mtcars_imputed[,-c(1)]
apply(mtcars_imputed[,-c(1)],1,max)

## ------------------------------------------------------------------------
apply(mtcars_imputed[,-c(1)],2,max)
apply(mtcars_imputed[,-c(1)],2,max)
class(apply(mtcars_imputed[,-c(1)],2,max))

## ------------------------------------------------------------------------
xlcust_bank_details = read.table("Customer_Bank Details_MV.csv",sep = ",",header = T)
cust_demographics = read.table("Customer_Demographics_MV_DOB.csv",sep = ",",header = T)

## ------------------------------------------------------------------------
head(cust_bank_details)

## ------------------------------------------------------------------------
colnames(cust_demographics)[1] = "ID"
head(cust_demographics)

## ------------------------------------------------------------------------
library(data.table)
merged_df = merge(cust_bank_details,cust_demographics,by.x="ID",by.y = "ID")
head(merged_df)

