#test1, test2 df 생성
test1 <- data.frame(id = c(1,2,3,4,5), 
                    midterm = c(60, 80, 70, 90, 85))
test2 <- data.frame(id = c(1,2,3,4,5),
                    final = c(70, 83, 65, 95, 80))
test1
test2

#dplyr로 join
library(dplyr)

#id 기준으로 left_join
total <- left_join(test1 , test2, by = "id")
total

total2 <- left_join(test2 , test1, by = "id")
total2

#name df 생성
name <- data.frame(class = c(1,2,3,4,5), teacher = c("kim","lee", "park","choi","jung"))
name

#csv_exam.csv 불러오기
library(readxl)
exam <- read.csv("csv_exam.csv")
exam

exam_new <- left_join(exam, name, by="class")
exam_new

exam_new <- left_join(name, exam, by="class")
exam_new

#left_join과 right_join의 차이점?
#순서가 바뀌는 것

#group_a, group_b df 생성
group_a <- data.frame(id = c(1,2,3,4,5),
                      test = c(60, 80, 70, 90, 85))

group_b <- data.frame(id = c(6,7,8,9,10),
                      test = c(70, 83, 65, 95, 80))
group_a
group_b

#row로 결합하기(세로로)
group_all <- bind_rows(group_a, group_b)
group_all

#결측치가 있는 df 생성
df <- data.frame(sex = c("M","F",NA,"M","F"),
                 score = c(5,4,3,4,NA))
df

#결측치 확인
is.na(df)
table(is.na(df))

table(is.na(df$sex))
table(is.na(df$score))

mean(df$score)
sum(df$score)

df %>%
  filter(is.na(score))

#score 값 중 결측값 제외(!)
df_nomiss <- df %>%
  filter(!is.na(score))
mean(df_nomiss$score)
sum(df_nomiss$score)

#score 값과 sex 값 중 결측값 제외(!)
df_nomiss2 <- df %>%
  filter(!is.na(score) & !is.na(sex))
df_nomiss2

#omit으로 결측값 한 번에 지우기
df_nomiss3 <- na.omit(df)
df_nomiss3

#na.rm=T (Remove)
mean(df$score, na.rm=T)
sum(df$score, na.rm=T)

