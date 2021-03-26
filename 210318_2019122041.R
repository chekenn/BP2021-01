library(readxl)
exam <- read.csv("csv_exam.csv")

#apply로 파생변수 생성
exam$mean_score <- apply(exam[,3:5],1,mean)
exam

#첫번째 column 뽑아내기
exam[1]
#첫번째 row, column 뽑아내기
exam[1,1]
#3-5열 뽑아내기
exam[,3:5]

mean(exam$math, exam$english) #error
apply(exam[,3:5],1,mean) #1은 행, 2는 열
apply(exam[,3:5],2,mean)
mean(exam$math + exam$english)/2 #error

#filter
library(dplyr)

exam %>%
  filter(class == 1)

exam %>%
  filter(class != 3)

exam %>%
  filter(class == 1 & math >= 50)

exam %>%
  filter(class == 3 & math >= 80)

exam %>%
  filter(math >= 90 | english >= 90)

exam %>%
  filter((class == 1 | class == 3) & math >= 50)

exam %>%
  filter((class == 1 | class == 3) | class == 5)

exam %>%
  filter(class %in% c(1,3,5)) #%in% 매칭 확인

class1 <- exam %>% filter(class==1)
class1 <- exam %>% filter(class==3)
class1
class2

mean(class1$math)
mean(class2$math)

#연산
1+1
1-1
1*1
1/1
2^2
2**2
3^2
3**2
7%/%3 #나눗셈의 몫
7%%3 #나눗셈의 나머지
11/7
11%/%7
11%%7

#select
exam %>%
  select(math)

exam %>%
  select(class, math, english)

exam %>%
  select(-math)

exam %>%
  select(-math, -english)

exam %>%
  filter(class == 1 ) %>%
  select(english)

exam %>%
  select(id, math) %>%
  head(10)

exam %>%
  select(id, math) %>%
  filter(id <=5 | id >= 15)

#arrange 정렬
exam %>%
  arrange(math) #오름차순이 디폴트

exam %>%
  arrange(desc(math)) #desc 내림차순

exam %>%
  arrange(class, math)

#mutate
exam %>%
  mutate(total = math + english + science) #total 이라는 새로운 변수 생성

exam %>%
  mutate(total = math + english + science,
         mean = (math + english +science)/3) %>%
  head

exam %>%
  mutate(test=ifelse(science >= 60, "pass","fail")) %>%
  head

exam %>%
  mutate(total = math + english + sciece) %>%
  arrange(total)

#summarise
exam %>%
  summarise(mean_math = mean(math))
mean(exam$math)

exam %>%
  group_by(class) %>%
  summarise(mean_math = mean(math))

mpg <- ggplot2::mpg
mpg2 <- as.data.frame(ggplot2::mpg)
class(mpg)
class(mpg2)
head(mpg)
head(mpg2)

exam %>%
  group_by(class) %>%
  summarise(mean_math = mean(math),
            sum_math = sum(math),
            median_math = median(math), 
            n = n())

#평균 mean()
#표준편차 sd()
#합계 sum()
#중앙값 median()
#최소값 min()
#최대값 max()
#빈도 n()

mpg <- as.data.frame(ggplot2::mpg)

mpg %>%
  group_by(manufacturer, drv) %>%
  summarise(mean_hwy = mean(hwy)) %>%
  head(10)

#Quiz>회사별로 "suv"자동차의 통합 연비(도시연비, 고속도로연비의 평균) 평균을 구해서 통합 연비의 평균 기준으로 내림차순으로 정렬하고, 상위 5위까지 출력하기

?mpg
str(mpg)

mpg %>%
  group_by(manufacturer) %>%
  filter(class == "suv") %>%
  mutate(total = (cty+hwy)/2) %>%
  summarise(mean_total = mean(total)) %>%
  arrange(desc(mean_total)) %>%
  head(5)




