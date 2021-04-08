#reshape 패키지 설치
#install.packages("reshape")
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

ggplot(m_corona)+
  geom_point(aes(x=dates, y = value, col = variable)) +
  geom_smooth(aes(x=dates, y=value, col = variable)) +
  labs(col="Types") +
  xlab("Dates") + ylab("Counts")+
  theme_bw() + #배경 테마 바꾸기 : theme_**()
  theme(text= element_text(size = 15, face = "bold"), legend.position = c(0.2, 0.80))

#확진/회복/사망 level 지정 -> 막대그래프 그리기
m_corona %>%
  mutate(variable = factor(variable, levels=c("confirmed","recovered", "deaths"))) %>%
  group_by(weekend, variable) %>%
  summarise(Count = sum(value)) %>%
  mutate(Perc = Count /sum(Count)) %>%
  
ggplot() + 
  geom_bar(aes(x = weekend, y = Perc * 100,
               fill = variable), stat = 'identity') + 
  #stat='identity' : y축의 높이를 데이터의 값으로 하는 bar그래프의 형태로 지정
  geom_text(aes(x = weekend, y = Perc * 100,
                fill = variable, label = paste0(round(Perc * 100,2),"%")), #round 
                position = position_stack(0.5)) +
  scale_y_continuous(breaks=seq(0,100, by=10)) #간격10 + 
  labs(fill = "Type") + ylab("Percent (%)") +
  theme_bw() +
  theme(text = element_text(size = 15, face = "bold"))

#반복문
Increase = c()
head(corona)
Increase[1] = 0
Increase
nrow(corona)

for (i in 2:nrow(corona)){
  Increase[i] = corona$confirmed[i] - corona$confirmed[i-1]
}

names(Increase) = 1:length(Increase)
Increase

class(names(Increase)) #index값은 character

Increase2 = Increase[Increase > 0]
Increase2

ggplot(NULL) + 
  geom_bar(aes(x = 1:length(Increase), y=Increase, fill=Increase), stat = "identity") + 
  geom_text(aes(x = as.numeric(names(Increase2)), y=Increase2 + 30, label=Increase2),
  stat = "identity", size = 3)+scale_fill_gradientn(colours = c("tan3" , "royalblue")) + 
  xlab("Days after") + ylab("Increase of Confirmed") + 
  theme_bw() + theme(text = element_text(size = 15, face = "bold"))
