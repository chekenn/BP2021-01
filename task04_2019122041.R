###Q1. mpg데이터의 cty(도시 연비)와 hwy(고속도로 연비) 간에 어떤 관계가 있는지 알아보려고 합니다. x축은 cty, y축은 hwy로 된 산점도를 만들어 보세요.###
###Q1. We want to find out what is the relationship between cty (city fuel efficiency) and hwy (highway fuel efficiency) in mpg data. Make a scatterplot with cty for the x-axis and hwy for the y-axis.###
library(ggplot2)
mpg <- as.data.frame(ggplot2::mpg)
ggplot(data=mpg, aes(x=cty, y=hwy)) +geom_point() 

###Q2. midwest데이터를 이용해 전체 인구와 아시아인 인구 간에 어떤 관계가 있는지 알아보려고 합니다. x축은 poptotal(전체 인구), y축은 popasian(아시아인 인구)으로 된 산점도를 만들어 보세요. 전체 인구는 50만명 이하, 아시아인 인구는 1만명 이하인 지역만 산점도에 표시되게 설정하세요.###
###Q2. Using midwest data, we want to find out what the relationship is between the total population and the Asian population. Create a scatterplot with poptotal on the x-axis and popasian on the y-axis. Set the scatterplot to display only regions with a total population of 500,000 or less and Asian populations of 10,000 or less.###
?midwest
library(ggplot2)
library(dplyr)

midwest <- as.data.frame(ggplot2::midwest)

df_midwest <- midwest %>%
  filter((poptotal<=500000) & (popasian<=10000))
df_midwest

ggplot(data=df_midwest, aes(x=poptotal, y=popasian)) +geom_point() 

ggplot(data=midwest, aes(x=poptotal, y=popasian)) +   geom_point() +
  xlim(0,500000) +
  ylim(0,10000)

  
###Q3. mpg 데이터에서 어떤 회사에서 생산한 "suv" 차종의 도시 연비가 높은지 알아보려고 합니다. "suv"차종을 대상으로 평균 cty(도시 연비)가 가장 높은 회사 다섯 곳을 막대 그래프로 표현해 보세요. 막대는 연비가 높은 순으로 정렬하세요.###
###Q3. In mpg data, we want to find out which company produced "suv" cars have high city fuel economy. Express the five companies with the highest average cty (city fuel efficiency) for the "suv" car model as a bar graph. Sort the bars in order of highest fuel efficiency.###
library(dplyr)
df <- mpg %>% 
  filter(class == "suv") %>% 
  group_by(manufacturer) %>% 
  summarise(mean.cty = mean(cty)) %>% 
  arrange(desc(mean.cty)) %>% 
  head(5)
df

ggplot(df, aes(x = reorder(manufacturer,-mean.cty), y = mean.cty)) + geom_col()

###Q4. mpg 데이터에서 자동차 중에서 어떤 class(자동차 종류)가 가장 많은지 알아보려고 합니다. 자동차 종류별 빈도를 표현한 막대 그래프를 만들어 보세요.###
###Q4. We want to find out which class (car type) has the most among cars in mpg data. Create a bar graph that expresses the frequency of each car type.###
ggplot(mpg, aes(x = class)) + geom_bar()

###Q5. economics 데이터에서 psavert(개인 저축률)가 시간에 따라 어떻게 변해 왔는지 알아보려고 합니다. 시간에 따른 개인 저축률의 변화를 나타낸 시계열 그래프를 만들어 보세요.###
###Q5. In economics data, we want to see how psavert (personal savings rate) has changed over time. Create a time series graph showing the change in personal savings rate over time.###
?economics
economics <- as.data.frame(ggplot2 :: economics)
ggplot(data=economics, aes(x = date, y = psavert)) + geom_line()

###Q6. mpg 데이터에서 class(자동차 종류)가 "compact", "subcompact", "suv"인 자동차의 cty(도시 연비)가 어떻게 다른지 비교해보려고 합니다. 세 차종의 cty를 나타낸 상자 그림을 만들어 보세요.###
###Q6. In mpg data, We want to compare how the cty (city fuel efficiency) of cars with class (car type) of "compact", "subcompact", and "suv" is different. Make a box plot showing the cty of the three models. ###
df_mpg <- mpg %>% 
  filter(class == "compact" | class == "subcompact" | class == "suv")
  
df_mpg
ggplot(data=df_mpg, aes(x=class, y=cty)) + geom_boxplot()

###Q7. mpg 데이터에서 drv(구동 방식) 간에 cty(도시 연비)가 통계적으로 유의한 차이가 있는지 t-test를 각각 해보세요.###
###Q7. In mpg data, try each t-test to see if there is a statistically significant difference in cty (city fuel efficiency) between drv (drive method). ###
table(mpg$drv)

mpg_diff1 <- mpg %>%
  select(drv, cty) %>%
  filter(drv %in% c("f","4"))
t.test(data=mpg_diff1, cty ~ drv, var.equal = T)

mpg_diff2 <- mpg %>%
  select(drv, cty) %>%
  filter(drv %in% c("f","r"))
t.test(data=mpg_diff2, cty ~ drv, var.equal = T)

mpg_diff3 <- mpg %>%
  select(drv, cty) %>%
  filter(drv %in% c("4","r"))
t.test(data=mpg_diff3, cty ~ drv, var.equal = T)

###결론 class "4"와 "r"은 통계적으로 유의미하지 않다. 


###Q8. 아래 데이터에서 모든 변수의 상관 관계를 나타낸 상관 행렬을 만들어보고, 히트맵으로 표현해보세요. 소수점 둘째 자리까지만 표시되도록 하세요.###
###Q8. Create a correlation matrix that shows the correlation of all variables from the data below, and express it as a heat map. Make sure to display only 2 decimal places. ###
mydata = read.csv("https://wiki.q-researchsoftware.com/images/b/b9/Ownership.csv", header = TRUE, fileEncoding="latin1")
head(mydata)
mydata_cor <- cor(mydata)
round(mydata_cor, 2)

library(corrplot)
corrplot(mydata_cor)
corrplot(mydata_cor, method="number")

col <- colorRampPalette(c("#BB4444", "#EE9988","#FFFFFF", "#77AADD", "#4477AA"))

corrplot(mydata_cor,
         method="color",
         col = col(200),
         type = "lower", 
         order="hclust",
         addCoef.col = "black",
         tl.col="black",
         tl.srt = 45,
         diag =F)

###Q9. 다양한 파라미터를 지정해서(수업시간 외에 알게된 파라미터도 가능) 히트맵으로 다시 시각화 해보세요.###
###Q9. Specify various parameters (parameters learned outside of class are possible) and re-visualize them as a heat map. ###
col <- colorRampPalette(c("#BB4444", "#EE9988","#FFFFFF", "#77AADD", "#4477AA"))

corrplot(mydata_cor,
         method="color",
         col = col(200),
         type = "lower", 
         order="AOE",
         addCoef.col = "gray",
         tl.col="black",
         tl.srt = 90,
         diag =T)
