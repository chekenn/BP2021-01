###Q1. Create and print test score variables###
#Five students took the test. Create a variable that contains students' test scores and print them out. Make the variable name considering its meaning.
#80 60 70 50 90
score <- c(80, 60, 70, 50, 90)
score


###Q2. Finding the overall mean###
#Use the variables created in the previous question to find the overall average score for these students.
mean(score)


###Q3. Create and print the overall mean variable###
#Create and print a new variable that contains the overall average score. Just apply the code you used to solve the previous problem.
mean_variable <- c(score, mean(score))
#mean_score <- mean(score)

###Q4. Create a data frame###
#Combine data.frame() and c() to make the contents of the table below into a data frame and print it out.
#product    price sales_rate
#apple      1800  24
#strawberry 1500  38
#watermelon 3000  13
product <- c("apple","strawberry", "watermelon")
price <- c(1800, 1500,3000)
sales_rate <- c(24, 38, 13)
df <- data.frame(product, price, sales_rate)
df


###Q5. Finding the average###
#Use the data frame you created earlier to find the average of the price of the fruit and the average of the sales volume.
mean(sales$price)
mean(sales_rate)


###Q6. Import csv file###
#Insert the csv_task1.csv file into the working directory (project folder) and load it as a data frame
df_csv_task <- read.csv("csv_task1.csv")
df_csv_task


###Q7. Checking the variable type###
#Check the variable type of the drv variable (the driving method of the car) in the mpg data of ggplot2.
library(ggplot2)
class(mpg$drv)


###Q8. Converting variable type###
#Convert the drv variable to factor type and check the type again.
new_drv <- as.factor(mpg$drv)
new_drv
class(new_drv)


###Q9. Checking the categories###
#Check out what categories the drv variable consists of.
levels(new_drv) #범주확인가능

