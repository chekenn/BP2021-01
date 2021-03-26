library(readxl)
exam <- read.csv("csv_exam.csv")

#가져온 데이터의 앞부분 살펴보기
head(exam)
head(exam, 10)

#가져온 데이터의 뒷부분 살펴보기
tail(exam)
tail(exam, 9)

#뷰어 창에서 데이터 확인
View(exam) #V는 대문자

#차원
dim(exam)

#데이터의 속성 출력
str(exam)

#요약 통계량 출력
summary(exam)

#ggplot2 패키지의 mpg 데이터를 dataframe 형태로 가져오기 
mpg <- as.data.frame(ggplot2::mpg)

head(mpg)
tail(mpg)

View(mpg)
dim(mpg)

str(mpg)
summary(mpg)

#mpg 데이터가 궁금하다면
?mpg

#dataframe 생성
df_raw <- data.frame(var1 = c(1,2,1),
                     var2 = c(2,3,2))

df_raw

#dplyr 설치
install.packages("dplyr")
library(dplyr)

#dataframe 복사
df_new <- df_raw

df_new <- rename(df_new, v2 = var2)

df_new <- rename(df_new, v1 = var1, v2 = var2)

#파생변수 생성
df <- data.frame(var1 = c(4,3,8),
                 var2 = c(2,6,11))

df$var_sum <- df$var1 + df$var2
df

df$var_mean <- (df$var1 + df$var2)/2
df

mpg$total <- (mpg$cty +mpg$hwy) / 2
head(mpg)

mean(mpg$total)

#히스토그램
hist(mpg$total)

#조건문
library(ggplot2)
mpg$total_test <- ifelse(mpg$total >= 20, "pass", "fail")
head(mpg, 20)

table(mpg$total_test)
qplot(mpg$total_test)

#중첩 조건문
mpg$total_grade <- ifelse(mpg$total >= 30, "A", 
                          ifelse(mpg$total>=20, "B", "C"))

head(mpg, 20)

table(mpg$total_grade)
qplot(mpg$total_grade)
