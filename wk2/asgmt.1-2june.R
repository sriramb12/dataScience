---
title: "Lab 1"
output:
  html_document:
    toc: yes
    toc_float: yes
  pdf_document:
    toc: yes
---

# Assignment-1
## Multiply 13 with 9 and then subtract 10 and add 35?
m<-13*9-10+35
# Divide this result by 42 
print(' What will be the final output?')
m <- m/42
print(m)
print('-------------------------')
#' 
print('Assignment 2')

# Assign a value to the variables my_apples and my_oranges
my_apples <- 32
my_oranges <- 10
my_apples + my_oranges
# Add these two variables together
# Create a variable my_fruit to store the sum of my_apples and my_oranges

my_fruit <- my_apples + my_oranges
my_fruit
print('-------------------------')
print('Assignment 3')
print(" Create a vector that contains numbers between the interval of 1 to 2500, with an increment of 6.")
v1to2500 <- seq(from = 1, to = 2500, by=6)
v1to2500
print(" Subset the first 50 elements of that vector")
first50<-v1to2500[1:50]
first50
print('-------------------------')

print('Assignment 4')
# Try creating following vectors -

# For poker_vector:
# On Monday you won $140
# Tuesday you lost $50
# Wednesday you won $20
# Thursday you lost $120
# Friday you won $240
poker_vector<-c(140, -50, 20,-120,240)
# For roulette_vector:
# On Monday you lost $24
# Tuesday you lost $50
# Wednesday you won $100
# Thursday you lost $350
# Friday you won $10
roulette_vector<-c(-24, -50, 100,-350, 10)

# Poker winnings from Monday to Friday
poker_vector <- c(140, -50, 20, -120, 240)
print( sum(poker_vector))
# Roulette winnings from Monday to Friday
print( sum(roulette_vector))

# Find the poker and roulette winnings from Monday to Friday

print( sum(roulette_vector) + sum(poker_vector))

print('-------------------------')

print('Assignment 5')

print(" Create a vector Player with two elements. Assign column names as Name and Profession. Print the result.")

cnames<-c('Name', 'Profession')
Player <-rbind(cnames, c('Ram', 'shyam'))
print( Player)
print('-------------------------')

#' 
print('Assignment 6')

# Create a matrix with 4 rows and 3 columns having Months of the year e.g. "Jan", "Feb", etc. as values

monthMatrix = matrix(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"), nrow=3, byrow=TRUE)
print(monthMatrix)
# Create a matrix by combining following vectors using rbind and cbind -
vector_sub <- c(1, 2, 3, 4)
vector_sub2 <- c(8, 16, 24, 32)
print(rbind(vector_sub, vector_sub2))
print(cbind(vector_sub, vector_sub2))

print('-------------------------')

print("Question 1: Create a vector with elements being 98, 82, 102, 99, 100")
av <-c(98, 82, 102, 99, 100)
print("#   # a) Extract the first element")
av[1]

print("#   # b) Extract the first and the fifth element from the vector.")
print(av[c(1,5)])
#'" 
print("Question 2: Create a vector containing a sequence of numbers in the interval of 40 to 1000, separated by 10. Find the length of the vector.")
v40to1k<-seq(40 ,1000, 10)
print(length(v40to1k))
#' 
print("Question 3: Create a vector containing a sequence of numbers in the interval of 1 to 2500, separated by 6 and subset the first 50 elements of the vector")
v1to2500<-seq(1 ,2500, 6)
print(v1to2500[1:50])
#' 
print("Question 4: Create a vector that has elements: 3, 4, 5, 7 and another vector with elements : 6, 9, 12, 15, 18, 21 ; Use the + and - operator between the two vectors and report your observation.")
print("Answer:")
v1 <- c(3,4,5,7)
v2 <- c(6, 9, 12, 15, 18, 21)
v1+v2
v1-v2
print("Observation: the operations + and - do not make sense on 2 vectors of different sizes")
#' 

print("Question 5: Multiply c(34, 43, 22, 43) and c(13, 17)")
v1<-c(34, 43, 22, 43) 
v2<-c(13, 17)
(v1*v2)
#' 
print('Question 6: Create two vectors of sequence 1 to 10 and 11 to 20; column bind these vectors; Now create a matrix containing data from 21 to 40 and column bind that matrix to the resultant matrix of the first operation.')
v1<-1:10
v2<-11:20
v3<-cbind(v1,v2)
m1<-matrix(21:40, nrow=10)
(cbind(v3, m1))
print('-------------------------')
print('Question 7: ')
my_list <- list("first_element" = F,
                "second_element" = matrix(data = 1:6, nrow = 2, ncol = 3),
                "third_element" = 20:200)
     
#print (my_list)
j<-1
for (item in my_list)
{
 if (is.logical(item))
  print(my_list[j])
 j<-j+1
}
print('-------------------------')

print("Question 8: Create a data frame with two columns using the data.frame() function. First column should contain characters, second column should contain numeric objects. The columns should be assigned valid names.")
df <- data.frame(ch = c('a','b','c', 'd'), num = 1:4)
print (df)
print('-------------------------')
print('Question 9: Create a data frame with one column as a squence of numbers between 1 to 50, the other being from 51 to 100; After creating the data frame change the column names to column1 and column2')
numDf <- data.frame(c1 = 1:50, c2 = 51:100)
print("Before changing column names")
print(head(numDf))
print("After changing column names")
colnames(numDf) <- c("column1", "column2")
print(head(numDf))
print('')
print("===End of Assignment-1====")
