#ggplot 설치 및 로드 
install.packages("ggplot2")
library(ggplot2)

x <- c("a","a","b","c")
x

#qplot
qplot(x)

qplot(data = mpg, x = hwy)
qplot(data = mpg, x = cty)

#x축, y축 지정
qplot(data = mpg, x = drv, y = hwy, geom = "line") #geom = "line" 
qplot(data = mpg, x = drv, y = hwy, geom = "boxplot") #geom = "boxplot"
qplot(data = mpg, x = drv, y = hwy, geom = "boxplot", colour=drv) #drv별로 다른 색깔

?qplot # ? 설명

#dataframe 생성
english <- c(90, 80, 60, 70)
math <- c(50, 60, 100, 20)

df_midterm <- data.frame(english, math)
df_midterm

class <- c(1,1,2,2)
df_midterm <- data.frame(english, math, class)
df_midterm

#mean 구하기 
mean(df_midterm$english)
mean(df_midterm$math)

df_midterm2 <- data.frame(english = c(90, 80, 60, 70),
                          math = c(50, 60, 100, 20),
                          class = c(1,1,2,2))

df_midterm2

#working directory 가져오기
getwd()

#readxl
install.packages("readxl")
library(readxl)

df_exam <-read_excel("excel_exam.xlsx")
df_exam

df_exam <-read_excel("./data/excel_exam.xlsx") #data라는 폴더 안에 있을 때 
df_exam

df_exam <- read_excel("C:/Users/shong/Desktop/BP2021-01/excel_exam.xlsx") #다른 디렉토리에 있을 때 
df_exam

mean(df_exam$english)
mean(df_exam$math)

df_exam_novar <- read_excel("excel_exam_novar.xlsx")
df_exam_novar

df_exam_novar <- read_excel("excel_exam_novar.xlsx", col_names = F) #col_names = F
df_exam_novar

df_exam_sheet <- read_excel("excel_exam.xlsx", sheet=2) #2번째 sheet 가져오기
df_exam_sheet

#csv파일 가져오기
df_csv_exam <- read.csv("csv_exam.csv")
df_csv_exam

#csv파일로 저장
df_midterm2
write.csv(df_midterm2, file = "df_midterm.csv") #file=파일명

#변수 타입
var1 <- c(1,2,3,1,2)
var2 <- factor(c(1,2,3,1,2)) #범주
var1
var2

var1+2
var2+2 #error

class(var1) #numeric
class(var2) #factor

levels(var1) #레벨이 없으면 NULL
levels(var2)

var3 <- c("a", "b", "b", "c")
var4 <- factor(c("a", "b", "b", "c"))

var3
var4

class(var3)
class(var4)

mean(var1)
mean(var2)

#변수 타입 변경
var2 <- as.numeric(var2)
var2
mean(var2)

a <- 1
a

#matrix
x2 <- matrix(c(1:12, ncol = 2))
x2

#array
x3 <- array(1:20, dim=c(2,5,2)) #행, 열, 차원
x3
class(x3)

#list
x4 <- list(f1=a,
           f2=df_midterm, 
           f3=x2,
           f4=x3)

x4
class(x4)

x <- boxplot(mpg$cty)
x

x$stats[,1]
x$stats[,1][3]
