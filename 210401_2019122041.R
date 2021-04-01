library(ggiraphExtra)

#USArrests 데이터 살펴보기
str(USArrests)
head(USArrests)

#ggChoropleth(단계구분도) 그래프
library(tibble)

crime <- rownames_to_column(USArrests, var = "state")
head(crime)

crime$state <- tolower(crime$state)
head(crime)

#install.packages("maps")
library(maps)
library(ggplot2)
states_map <- map_data("state")
head(states_map)
str(states_map)

#interactive = T
ggChoropleth(data=crime, 
             aes(fill=Murder, 
                 map_id=state), 
             map = states_map)

ggChoropleth(data=crime, 
             aes(fill=Murder, 
                 map_id=state), 
             map = states_map,
             interactive = T)

#plotly 설치
install.packages("plotly")
library(plotly)
library(ggplot2)

p <- ggplot(data=mpg, aes(x=displ, y=hwy, col=drv)) + geom_point()
ggplotly(p)

#position="dodge"
pp <- ggplot(data=diamonds, aes(x=cut, fill=clarity)) + geom_bar(position="dodge")
ggplotly(pp)

#스택형
ppp <- ggplot(data=diamonds, aes(x=cut, fill=clarity)) + geom_bar()

#패키지 dygraphs 설치
install.packages("dygraphs")
library(dygraphs)

#economics 데이터 가져오기
economics <- ggplot2 :: economics
head(economics)

#시계열 그래프
library(xts)
eco <- xts(economics$unemploy, order.by=economics$date)
dygraph(eco)

library(zoo)
mpg <- as.data.frame(ggplot2::mpg)

library(dplyr)

mpg_diff <- mpg %>%
  select(class, cty) %>%
  filter(class %in% c("compact", "suv"))
head(mpg_diff)
table(mpg_diff$class)

#t.test
t.test(data=mpg_diff, cty ~ class, var.equal = T) #p-value(유의확률)

mpg_diff2 <- mpg %>%
  select(fl, cty) %>%
  filter(fl %in% c("r", "p"))
table(mpg_diff2$fl)

t.test(data = mpg_diff2, cty ~ fl, var.equal=T)

#상관분석
economics <- as.data.frame(ggplot2::economics)
cor.test(economics$unemploy, economics$pce)

head(mtcars)

car_cor <- cor(mtcars)
round(car_cor, 2)

#상관분석 시각화를 위한 corrplot 패키지 설치
install.packages("corrplot")
library(corrplot)

corrplot(car_cor)
corrplot(car_cor, method="number")

col <- colorRampPalette(c("#BB4444", "#EE9988","#FFFFFF", "#77AADD", "#4477AA"))

corrplot(car_cor,
         method="color",
         col = col(200),
         type = "lower", 
         order="hclust",
# "FPC": First Principle Component
# "hclust" : hierarchical clustering
# "AOE" : Angular Order of Eigenvectors
         addCoef.col = "black", #상관계수 색깔
         tl.col="black", #변수명 색깔
         tl.srt = 45, #각도
         diag =F)

