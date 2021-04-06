#reshape 패키지 설치
!install.packages("reshape")
library(reshape)
library(ggplot2)
library(dplyr)

#corona 데이터 불러오기
corona <- read.csv("corona_rok.csv")
head(corona)

corona$dates <- as.Date(corona$dates, "%m/%d/%y") #날짜형 변수 format 지정
head(corona)
str(corona)

corona$day <- format(corona$dates, format = "%a") #%a로 날짜 가져오기
?format
head(corona)

class(corona$day)

#factor로 만들어주기
corona$day <- factor(corona$day, levels = c("일","월","화","수","목","금", "토"))

class(corona$day)
levels(corona$day)

corona$weekend <- ifelse(corona$day %in% c("일", "토"), "weekend", "weekdays")
head(corona)

m_corona <- corona %>%
  melt(id.vars = c("dates", "day", "weekend")) %>%
  arrange(dates)

head(m_corona)






