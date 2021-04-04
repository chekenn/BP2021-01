#test1, test2 df 생성
test1 <- data.frame(id = c(1,2,3,4), 
                    midterm = c(60, 80, 70, 90))
test2 <- data.frame(id = c(3,4,5, 6, 7),
                    final = c(70, 83, 65, 95, 80))
test1
test2

#inner_join
library(dplyr)
inner_join(test1, test2, by = "id")

#full_join
full_join(test1, test2, by = "id")

#right_join
right_join(test1, test2, by = "id")

#left_join
left_join(test1, test2, by = "id")
left_join(test2, test1, by = "id")

#df생성
df <- data.frame(sex = c("M", "F", NA, "M", "F"),
                 score = c(5,4,3,4,NA))
df

mean(df$score)
mean(df$score, na.rm = T)

#3,8,15행 math값을 NA로
exam <-read.csv("csv_exam.csv")
exam[c(3,8,15), "math"] <- NA
exam

#na.rm =T
exam %>%
  summarise(mean_math = mean(math))
exam %>%
  summarise(mean_math = mean(math, na.rm = T))

exam %>%
  summarise(mean_math = mean(math, na.rm = T),
            sum_math = sum(math, na.rm = T),
            median_math = median(math, na.rm = T))

#결측치 대치
##평균값
exam$math <- ifelse(is.na(exam$math), mean(exam$math, na.rm=T), exam$math)

mean(exam$math)
exam[3, "math"] <- NA
exam[8, "english"] <- NA

#outlier 
outlier <- data.frame(sex = c(1,2,1,3,2,1),
                      score = c(5,4,3,4,2,6))
outlier

table(outlier$sex)
table(outlier$score)

#outlier 값 지정 & ifelse문
outlier$sex <- ifelse(outlier$sex ==3 , NA, outlier$sex)

outlier$score <- ifelse(outlier$score >5, NA, outlier$score)

outlier$sex <- ifelse(outlier$sex != 1 & outlier$sex != 2, NA, outlier$sex)
outlier

#결측값이 아니라면 조건을 filter로 지정하고 summarise
outlier %>%
  filter(!is.na(sex) & !is.na(score))
  group_by(sex) %>%
  summarise(mean_score = mean(Score))

#mpg 데이터를 df로
library(ggplot2)
mpg <- as.data.frame(ggplot2::mpg)

#boxplot
hwy_boxplot <- boxplot(mpg$hwy)
hwy_boxplot

#$stats
boxplot(mpg$hwy)$stats

mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy >37, NA, mpg$hwy)
table(is.na(mpg$hwy))

mpg %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy, na.rm = T))

?read.csv
library(realxl)
?read_excel

#산점도 그리기
library(ggplot2)
ggplot(data=mpg, aes(x=displ, y=hwy))
ggplot(data=mpg, aes(x=displ, y=hwy)) +geom_point()
ggplot(data=mpg, aes(x=displ, y=hwy)) +geom_point() +xlim(3,6) #warning message
ggplot(data=mpg, aes(x=displ, y=hwy)) +geom_point() +xlim(3,6) +ylim(10,30)

#mpg
mpg <- as.data.frame(ggplot2::mpg)
df_mpg <- mpg %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy))
df_mpg

#막대그래프 그리기
ggplot(data=df_mpg)
ggplot(data=df_mpg, aes(x=drv, y=mean_hwy)) + geom_col()
#정렬이 필요하면
ggplot(data=df_mpg, aes(x=reorder(drv, -mean_hwy), y=mean_hwy)) + geom_col() #-는 내림차순
ggplot(data=df_mpg, aes(x=reorder(drv, mean_hwy), y=mean_hwy)) + geom_col() #디폴트는 오름차순

#qplot보다 ggplot이 설정값의 종류가 多
qplot(mpg$hwy)
ggplot(data=mpg, aes(x=hwy)) + geom_bar()

 



