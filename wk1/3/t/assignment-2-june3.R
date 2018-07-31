#Load the Input DATA
cust_bank_details = read.table("data/Customer_Bank Details_MV.csv",sep = ",",header = T, na.strings=c('NA','?'))
cust_demographics = read.table("data/Customer_Demographics_MV_DOB.csv",sep = ",",header = T, na.strings=c('NA','?'))


print("1. Print the column names.")
colnames(cust_bank_details)
colnames(cust_demographics)
str(head(cust_demographics))
print("2. Find the number of columns.")
print("a.Number of columns in Customer_Bank Details table:") 
ncol(cust_bank_details)
print("b.Number of columns in Customer_Demographics Details table:")
ncol(cust_demographics)

print("3. Find the number of rows.")
print("a.Number of rows in Customer_Bank Details table")
print(nrow(cust_bank_details))

print("b.Number of rows in Customer_Demographics Details table:")
nrow(cust_demographics)

print("4. Look at the data types.")
print("5. Convert the data types.")
#dummy(cust_bank_details$Mortgage)
# converting am column to factor 

print("1. Check for the NA values in the overall dataset")
print("2. Check the number of NA values in each column.")
print("a. using for loop and")
colsum=c()
for (i in colnames(cust_bank_details))
{
  sum = 0
  for (j in cust_bank_details[,i])
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
print("b. colSums() functions.")
colSums(is.na(cust_bank_details))

sum(colsum)


print("3. Impute the NA values if any.")
library(DMwR)
custBankClean  = centralImputation(cust_bank_details)

cust_bank_details$mg = as.factor(cust_bank_details$Mortgage)
head( cust_bank_details)

colSums(is.na(custBankClean))
custDemoClean  = centralImputation(cust_demographics)


colSums(is.na(custDemoClean))

print("4. Create a new column based on the following conditions")
print("    a. If Experience <= 5 =Fresher If Experience > 5 = Experienced")
custDemoClean$Exp <- ifelse(custDemoClean$Experience>5, 'Experienced', 'Fresher')
head(custDemoClean)

#Create factors which represent human readable version of Education column`
edLevel<-1:5
names(edLevel) <- c('Uneducated', 'Matriculate', 'Bachelors', 'Masters', 'Doctorate')

print(names(edLevel[4]))

xlateEd <- function(x){
 return(names(edLevel[x]))
}   

custDemoClean$elevel <- sapply(custDemoClean$Education, xlateEd)
head(custDemoClean)

exit
print("5. Find the number of rows that fall under each of the levels in Education column.")
print("6. Find the mean of each numeric column using apply functions.")

fun <- function(x){
  if(is.numeric(x)){mean(x)}
  else 
   print("na")
}   

#Use colwise from plyr package
library(plyr)
print( colwise(fun)(custDemoClean))

print("#7. Standardize the Income column.")
require(vegan)
library(vegan)

# "id","dob","Exp","income","ZIP","fmly","ed" 
# need to find a way to apply decostand on specific (numeric) columns
stdIncome <- decostand(x =custDemoClean$Income,method ="range",MARGIN = 2)

custDemoCleanStd<- cbind(custDemoClean, "stdincome"= stdIncome)
colnames(custDemoCleanStd)


print("#8. Using for loop , create a new column which has Y if Mortgage is more than 0, else add it to the merged data frame.")
j<-1
for(i in head(custBankClean$Mortgage))
{
 j <- j+1
 if(i == 0){
    custBankClean$mortgageYN <- 'N'
 }
 else {
    custBankClean$mortgageYN <- 'Y'
 }
}
print(head(custBankClean))
print("#9. Dummify the Education column and add it to the merged data frame, remove the Education column.")
library(dummies)
dummy(custDemoClean$Education)
custDemoClean$Education <- NULL
print("#10. Find the average mortgage based on the education level of the employee.")
