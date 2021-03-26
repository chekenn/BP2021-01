###Q1. mpg 데이터의  fl 변수가 어떤 변수인지 알아보고, 각  범주명이 어떤 것이 있는지 빈도표를 만들어 알아보세요.###
###Q1. Find out which variable is the fl variable in mpg data, and create a frequency table to find out what each category name has.###
library(ggplot2)
mpg <- as.data.frame(ggplot2::mpg)
mpg
?mpg #fl은 fuel type
str(mpg) 
class(mpg$fl)#fl의 변수타입은 chr
table(mpg$fl)


###Q2. fl_price.csv 파일을 fuel이라는 데이터프레임으로 불러와서 이 데이터를 이용해 mpg 데이터에 price_fl 변수를 추가하세요 (데이터합치기)###
###Q2. Load the fl_price.csv file into a data frame called fuel and use this data to add the variable price_fl to the mpg data (Combine data)###
library(readxl)
fuel <- as.data.frame(read.csv("fl_price.csv"))
library(dplyr)
mpg_2 <- left_join(mpg, fuel, by = "fl")
mpg_2

###Q3. price_fl 변수가 잘 추가됐는지 확인하기 위해 model, fl, price_fl 변수를 추출해 앞부분 5행을 출력해 보세요.###
###Q3. To check if the price_fl variable has been successfully added, extract the model, fl, and price_fl variables and print the first 5 rows.###
head(mpg_2$model,5)
head(mpg_2$fl, 5)
head(mpg_2$price_fl, 5)

###Q4. mpg 데이터에 결측치가 있는지 확인해보고, mpg 데이터의 hwy의 변수 중 65, 124, 131, 153, 212 행을 결측치로 할당하세요.###
###Q4. Check if there are missing values in the mpg data, and assign rows 65, 124, 131, 153, and 212 of the hwy variables of the mpg data as missing values. ###
table(is.na(mpg_2))
mpg_2[c(65, 124, 131, 153, 212), "hwy"] <- NA
mpg_2$hwy


###Q5. drv변수와 hwy변수에 결측치가 몇 개 있는지 알아보세요.###
###Q5. Find out how many missing values are in the drv and hwy variables.###
table(is.na(mpg_2$drv))
table(is.na(mpg_2$hwy))


###Q6. hwy변수의 결측치를 제외하고, 어떤 구동 방식의 고속도로 연비의 평균이 높은지 알아보세요.###
###Q6. Excluding the missing value of the hwy variable, find out which driving method the average of the hwy variable is high.###
mpg_2 %>%
  filter(!is.na(hwy)) %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy))

###Q7. drv 변수 중 10, 14, 58, 93행에 존재할 수 없는 값 "k"를 할당하고, cty 변수 중 29, 43, 129, 203행에 극단적으로 크거나 작은 값으로 3, 4, 39, 42를 할당하세요.###
###Q7. In drv variables, rows 10, 14, 58, 93 that cannot exist are assigned "k", and cty variables are assigned extremely large or small values of 3, 4, 39, 42 to rows 29, 43, 129, and 203 Try it. ###
mpg_2[c(65, 124, 131, 153, 212), "drv"] <- NA
mpg_2$hwy






###Q8. drv 변수에 이상치가 있는지 확인하고, 이상치를 결측치 처리하세요. 결측치 처리 시에 %in%를 활용해서 코드를 간결하게 만들어 보세요.###
###Q8. Check if there are outliers in the drv variable, and handle outliers as missing values. When dealing with missing values, use %in% to simplify your code.###


###Q9. 상자 그림을 이용해 cty에 이상치가 있는지 확인 하고, 상자 그림 통계치를 이용해 정상 범위를 벗어난 값을 결측 처리한 후 다시 상자 그림을 만들어 이상치가 사라졌는지 확인하세요.###
###Q9. Check if there are outliers in the cty using the box plot, and after processing the missing values out of the normal range using the box plot statistics, create a box plot again to see if the outlier disappears.###