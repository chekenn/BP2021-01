#변수 하나에 값 하나 할당
a <- 1
a

b <- 2
c <- 3.5
b
c

a+b
a+b+c
4 / b

#c로 숫자 여러개 묶어서 변수 만들기
var1 <- c(1,2,5,7,8)
var1

#연속된 숫자로 변수 만들기
var2 <- c(1:5) #시작숫자 콜론(:) 마지막숫자
var2

#seq(시작숫자, 끝나는숫자)
var3 <- seq(1,5)
var3

#by로 옵션지정
var4 <- seq(1,10, by=2)
var4

var5 <- seq(1,10,by=3)
var5

#var 연산
var1
var1+2
var1+var2

#문자를 넣을 때는 따옴표
str1 <- "a"
str1
str2 <- "text"
str2
str3 <- "Hello world!"
str3

str4 <- c("a","b")
str4

str5 <- c("Hello!", "world", "is", "nice")
str5

str1 + 2 #error

x <- c(1,2,3)
x

#함수
mean(x)
min(x)
x_mean <- mean(x)
x_mean

#paste
str5
paste(str5, collapse=",")#collapse는 구분자 옵션
paste(str5, collapse =" ")

str5_paste <- paste(str5, collapse=" ")
str5_paste
