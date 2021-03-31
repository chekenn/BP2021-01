library(ggplot2)
?economics

#시계열 그래프
ggplot(data=economics, aes(x = date, y = unemploy)) + geom_line()

#boxplot 그래프
ggplot(data=mpg, aes(x=drv, y=hwy)) + geom_boxplot()

boxplot(mpg$hwy)

#패키지 설치
install.packages("mapproj")
install.packages("ggiraphExtra")
library(mapproj)
library(ggiraphExtra)

#USArrests 데이터 살펴보기
str(USArrests)
head(USArrests)

#ggChoropleth(단계구분도) 그래프
library(tibble)
crime <- rownames_to_column(USArrests, var = "state")
head(crime)
?rownames_to_column

crime$state <- tolower(crime$state)
head(crime)

install.packages("maps")
library(maps)
library(ggplot2)
states_map <- map_data("state")
head(states_map)
str(states_map)

ggChoropleth(data=crime, aes(fill=Murder, map_id=state), map = states_map)