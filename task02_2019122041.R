###Q1. Load midwest data from the ggplot2 package in the form of a data frame. Then use the functions you learned to characterize the data to figure it out.###
###문제1. ggplot2 패키지의 midwest 데이터를 데이터 프레임 형태로 불러오세요. 그 다음 데이터의 특징을 배운 함수들을 활용해서 파악하세요.###
library(ggplot2)
mw <- as.data.frame(ggplot2::midwest)

head(mw)
tail(mw)

View(mw)
dim(mw)

str(mw)
summary(mw)

###Q2. Change the poptotal variable to total and the popasian variable to asian.###
###문제2. poptotal 변수를 total로, popasian 변수를 asian으로 수정하세요.###
library(dplyr)
mw <- rename(mw,total = poptotal, asian = popasian)
head(mw)

###Q3. Using the total variable and the asian variable, create a derived variable asian_total, which is the'Percentage of Asian Population to Total Population'. Create a histogram to see how the cities are distributed.###
###문제3. total 변수, asian 변수를 이용해서 '전체 인구 대비 아시아 인구 백분율' 파생변수 asian_total을 만드세요. 히스토그램을 만들어 도시들이 어떻게 분포하는지 살펴보세요.###
mw$asian_total <- (mw$asian / mw$total)*100
head(mw)
hist(mw$asian_total)

###Q4. Create a derived variable that takes the average of the overall Asian population percentage, and gives "large" if it exceeds the average, and "small" otherwise.###
###문제4. 아시아 인구 백분율 전체 평균을 구하고, 평균을 초과하면 "large", 그 외에는 "small"을 부여하는 파생변수를 만들어 보세요.###
mean(mw$asian_total)
mw$asian_total_test <- ifelse(mw$asian_total > mean(mw$asian_total), "large", "small")

###Q5. Create a frequency table and a frequency bar graph of how many regions are in "large" and "small".###
###문제5. "large"와 "small"에 해당하는 지역이 얼마나 되는지 빈도표와 빈도 막대 그래프를 만들어 보세요.###
table(mw$asian_total_test)
qplot(mw$asian_total_test)

###Q6. Find variables representing the adult and total population, and add the variable'Percentage of Minor(Underage) Population to Total Population' to the midwest data.###
###문제6. 성인 인구와 전체 인구를 나타내는 변수를 찾고, midwest 데이터에 '전체 인구 대비 미성년 인구 백분율' 변수를 추가하세요.###
?midwest
str(mw)
#전체인구 = poptotal (total로 변경)
#성인인구 = popadults
mw$popunderage <- ((mw$total-mw$popadults) / mw$total) * 100
mw

###Q7. Print the percentage of the minor(underage) population in the top 5 county(regions) with the highest percentage of underage populations.###
###문제7. 미성년 인구 백분율이 가장 높은 상위 5개 county(지역)의 미성년 인구 백분율을 출력하세요.###
mw %>%
  select(county, popunderage) %>%
  arrange(desc(popunderage)) %>%
  head(5)

###Q8. Large is 40% or more, middle is 30~40%, small is less than 30%. Add a minor(underage) percentage rating variable based on this classification criterion, and find out how many regions are in each rating.###
###문제8. large는 40% 이상, middle은 30~40%, small 은 30% 미만. 이 분류 기준에 따라 미성년 비율 등급 변수를 추가하고, 각 등급에 몇 개의 지역이 있는지 알아보세요.###
mw$grade_test <- ifelse(mw$popunderage >= 40, "large", ifelse(mw$popunderage >= 30, "middle", "small"))
table(mw$grade_test)

###Q9. Print the state and county(region) of the bottom 10 regions of the Asian population percentage.###
###문제9. 아시아 인구 백분율 하위 10개 지역의 state(주), county(지역)을 출력하세요.
mw %>%
  arrange(percasian)%>%
  select(state, county)%>%
  head(10)

