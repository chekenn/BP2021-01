###code1
options(warn = -1) #warning
options(scipen = 10000) #지수표기를 숫자표기로 바꾸는 옵션
options(repr.plot.width = 13.5, repr.plot.height = 9) #가로세로지정

#library 확인
library(tidyverse) #include ggplot2, dplyr, tidy
library(scales) #n Graphical scales map data to aesthetics, and provide methods for automatically determining breaks and labels foraxes and legends.
library(reshape) #Flexibly restructure and aggregate data using #cast/melt
library(RColorBrewer) #color
library(ggthemes) #Some extra themes, geoms, and scales for 'ggplot2'
library(gridExtra)#Provides a number of user-level functions to work with `grid','graphics'
library(ggrepel)#Provides text and label geoms for 'ggplot2' that help to avoid overlapping text labels
library(lubridate) # date와 time을 더욱 다루기 쉽게 

#데이터 불러오기
data <- read_csv('archive/PatientInfo.csv')
#data <- data[,-c(4,6,14)]

#data$age <- 2020-data$birth_year
data$Date_Conf <- as.Date(data$confirmed_date) #as.Date : 날짜형 변수로
data$Date_Rec <- as.Date(data$released_date)
#data$Date_Death <- as.Date(data$deceased_date, format = "%m/%d/%y") #format 지정
data$DaysToRec <- as.numeric(data$Date_Rec-data$Date_Conf) #as.numeric : 숫자형 변수로
#data$DaysToDea <- data$Date_Death-data$Date_Conf

#data <- data[,c(1,3,7,10,12,13,5,19:21)]
data <- data[,c(1,2,6,7,8,9,3,15,16,17)]
#""공백엔 NA 결측치 할당
data$sex[data$sex==""] <- NA
data$infection_case[data$infection_case==""] <- NA
names(data) <- c("ID", "Sex", "Region", "Infection_reason", "Infected_by", "Contact_number", "Age", "Date_Confirm", "Date_Recovered", "Days_To_Recover")
#범위 지정 후 해당 범위는 0 처리
data$Contact_number[data$Contact_number %in% c("-", "1000000772", "1000000796")] <- 0
data$Contact_number <- as.numeric(data$Contact_number)

###code2
#sample뽑기
Head <- data[sample(1:nrow(data),5), ] 
#order정렬
Head <- Head[order(Head$ID),]
Head

###code3
data[,-1] %>% 
  is.na %>% #NA 결측값만
  melt %>%
  ggplot(data = ., aes(X2, X1))+
  geom_raster(aes(fill = value))+
  #geom_raster is a high performance special case for when all the tiles are the same size.
  coord_flip()+
  #coord_flip()은 x축과 y축의 구성을 뒤집어 표현하라는 명령어
  scale_fill_brewer(palette = "Paired", labels = c("Present","Missing"))+
  #scales 패키지의 scale_fill_brewer 함수를 이용해 그래프의 색상을 변경
  theme(axis.text.x  = element_text(angle=45, vjust=0.5))+ 
  labs(x = "", y = "Number of observation", fill = "Type", title = "Number of missing cases", subtitle = "by Variable (n = 5165)")+ 
  theme_fivethirtyeight()+
  #base_size:base font size / base_family:base font family
  theme(axis.title = element_text(size = 15.5, face = "bold"), axis.text = element_text(size = 15), legend.position="bottom", 
        legend.direction='horizontal', legend.title = element_text(size = 15.5), legend.text = element_text(size = 15.5),
        plot.background = element_rect(fill = "#fff6ed"), axis.line = element_line(size = 0.4, colour = "grey10"),
        legend.background = element_rect(fill = "#fff6ed"), plot.subtitle = element_text(size = 15), panel.background = element_rect(fill = "white"))
#nrow(data)

###code4
data %>%
  filter(year(Date_Confirm) > 2019) %>%
  group_by(Date_Confirm) %>%
  summarise(x = n()) %>%
  ggplot(., aes(Date_Confirm, x))+
  geom_line(size = 1.6, alpha = 0.8, col = "gray65")+
  geom_smooth(method = "loess", color = "firebrick3", size = 2.2, formula = y ~ x, fill = "firebrick4", alpha = 0.32)+
  #geom_point(size = 2.3, alpha = 0.7, col = "gray10")+
  scale_y_continuous(limits = c(0,150))+
  #y축의 continuous한 숫자에 대한 scale 변경
  labs(title = "Number of new infected people", subtitle = "by Date (n = 5165)", 
       y = "Number of infected", x = "")+
  scale_x_date(date_labels = "%b %d", date_breaks = "32 days")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(size = 15.5, face = "bold"), axis.text = element_text(size = 15), plot.background = element_rect(fill = "#fff6ed"), 
        axis.line = element_line(size = 0.4, colour = "grey10"), plot.subtitle = element_text(size = 15),
        #배경설정
        panel.background = element_rect(fill = "white"))
#nrow(data)

###code5
Table <- as.data.frame(table(data$Infection_reason))
names(Table) <- c("Reason", "n")

Table <- Table[between(Table$n, 40, 100000),]

ggplot(Table, aes(reorder(Reason, +n),n))+
  geom_bar(stat = "identity", colour = "black", fill = "darkolivegreen3", alpha = 0.9)+
  coord_flip()+
  geom_text(aes(label = n, y = n+60), size = 6.2)+
  scale_y_continuous(limits = c(0, max(Table$n)*1.11))+
  labs(title = "Way of infection", subtitle = "For reasons where more than 40 cases (n = 3829)", 
       y = "Number of people", x = "Reason")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(size = 15.5, face = "bold"), axis.text.y = element_text(size = 14), axis.text.x = element_text(size = 15),
        plot.background = element_rect(fill = "#fff6ed"), axis.line = element_line(size = 0.4, colour = "grey10"),
        legend.background = element_rect(fill = "#fff6ed"), plot.subtitle = element_text(size = 15), panel.background = element_rect(fill = "white"))
#sum(Table$n)

###code6
Table3 <- as.data.frame(table(data$Region))
names(Table3) <- c("Region", "n")
Table3 <- Table3[Table3$Region!="",] #공백이아니라면
#between(x, lower, upper, incbounds=TRUE, NAbounds=TRUE, check=FALSE)
#x %between% y
#inrange(x, lower, upper, incbounds=TRUE)
#x %inrange% y
Table3 <- Table3[between(Table3$n, 80, 100000),]

ggplot(Table3, aes(reorder(Region, +n),n))+
  geom_bar(stat = "identity", colour = "black", fill = "mediumpurple3", alpha = 0.9)+
  coord_flip()+
  geom_text(aes(label = n, y = n+17), size = 6.2)+
  scale_y_continuous(limits = c(0, max(Table3$n)*1.07))+
  labs(title = "Region where infection was detected", subtitle = "For regions where more than 80 cases (n = 1845)", 
       y = "Number of people", x = "Region")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(size = 15.5, face = "bold"), axis.text.y = element_text(size = 15), axis.text.x = element_text(size = 15),
        plot.background = element_rect(fill = "#fff6ed"), axis.line = element_line(size = 0.4, colour = "grey10"),
        legend.background = element_rect(fill = "#fff6ed"), plot.subtitle = element_text(size = 15), panel.background = element_rect(fill = "white"))
#sum(Table3$n)

###code7
Table3 <- Table3[order(Table3$n, decreasing = T),]
Table4 <- data %>%
  filter(year(Date_Confirm) > 2019) %>%
  filter(Region %in% Table3[1:4,1]) %>%
  group_by(Date_Confirm, Region) %>%
  summarise(x = n())
ggplot(Table4, aes(Date_Confirm, x))+
  geom_line(size = 1.5, alpha = 0.8, col = "gray65")+
  geom_smooth(method = "loess", color = "firebrick3", size = 1.9, formula = y ~ x, fill = "firebrick4", alpha = 0.32)+
  #facet_wrap : facet_wrap() 함수를 사용하여 면 분할을 구현
  facet_wrap(.~Region)+
  scale_y_continuous(limits = c(0,50))+
  labs(title = "Number of new infected people in most popular regions", subtitle = "by Date and Region (n = 1047)", 
       y = "Number of infected (up to 50 per day)", x = "")+
  scale_x_date(date_labels = "%b %d", date_breaks = "50 days")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(size = 15.5, face = "bold"), axis.text = element_text(size = 15), plot.background = element_rect(fill = "#fff6ed"),
        #strip.text.x : theme (strip)
        #strip은 한 plot내 둘 이상의 panel을 가진 그래프의 디자인을 변경할 때 활용
        strip.text.x = element_text(size = 14, face = "bold"), axis.line = element_line(size = 0.4, colour = "grey10"),
        plot.subtitle = element_text(size = 15), panel.background = element_rect(fill = "white"), strip.background = element_rect(fill = "#fff6ed"))
#sum(Table4$x)

###code8
data_chart_age <- data %>%
  group_by(Age, Sex) %>%
  filter(Age != "") %>% 
  filter(is.na(Sex)==F) %>%
  summarise(Count = n())

ggplot(data_chart_age, aes(Sex, Age))+
  geom_tile(aes(fill = Count), colour = "gray15")+
  #discrete는 이산형 / continuous와 반대
  scale_y_discrete(limit = c("100s", "90s", "80s", "70s", "60s", "50s", "40s", "30s", "20s", "10s", "0s"))+
  #scale_fill_distriller : give either divergent or sequential colours
  scale_fill_distiller(palette = "Spectral")+
  #geom_text(aes(label = paste0(round(Count/sum(data_chart_age$Count) * 100,1), "%")), size = 5)+
  geom_text(aes(label = Count), size = 5.5)+
  guides(fill = guide_colorbar(title.position = "top", barwidth = 1.3, barheight = 12))+
  labs(title = "Age of infected people", subtitle = "by Sex (n = 3782)", 
       y = "Age (group)", x = "Sex", fill = "Cases")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(size = 15.5, face = "bold"), axis.text = element_text(size = 15), legend.position="right",
        legend.title = element_text(size = 15),legend.text = element_text(size = 14.5), legend.direction='vertical', 
        plot.background = element_rect(fill = "#fff6ed"), axis.line = element_line(size = 0.4, colour = "grey10"), 
        legend.background = element_rect(fill = "#fff6ed"), plot.subtitle = element_text(size = 15), panel.background = element_rect(fill = "white"))
#sum(data_chart_age$Count)

###code9
ggplot(data, aes(Days_To_Recover/7))+
  #geom_density : 밀도함수 생성
  geom_density(alpha = 0.75, position = "stack", size = 0.8,  col = "gray20", fill = "deepskyblue2")+
  scale_x_continuous(limits = c(0,9.3), breaks = seq(0,9,1))+
  labs(title = "Time to cure since the virus was found", subtitle = "in Days (n = 1587)", 
       y = "Density", x = "Time to cure (in weeks)")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(size = 15.5, face = "bold"), axis.text = element_text(size = 15), panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "#fff6ed"), axis.line = element_line(size = 0.4, colour = "grey10"),
        legend.background = element_rect(fill = "#fff6ed"), plot.subtitle = element_text(size = 15))
#nrow(data)-sum(is.na(data$Days_To_Recover))

###code10
Table2 <- as.data.frame(table(data$Infected_by))
names(Table2) <- c("ID", "n")
Table2$ID <- as.factor(Table2$ID)
ggplot(Table2, aes(n))+
  geom_boxplot(outlier.size = 3.5, outlier.shape = 20, lwd = 0.8, fatten = 2, fill = "khaki", alpha = 0.9)+
  labs(title = "", subtitle = "", y = "", x = "")+
  coord_flip()+
  scale_x_continuous(limits = c(1,31), breaks = seq(0,30,5))+
  labs(title = "Number of people infected from one person", subtitle = "Only for people who have infected others (n = 5165)", 
       x = "Number of people infected from one person (up to 30)")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(size = 15, face = "bold"), axis.text = element_text(size = 15), panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "#fff6ed"), axis.line = element_line(size = 0.4, colour = "grey10"),
        legend.background = element_rect(fill = "#fff6ed"), axis.text.x = element_text(size = 0), plot.subtitle = element_text(size = 15))
#sum(Table2$n)

###code11
data %>%
  filter(Contact_number < 100000) %>%
  ggplot(., aes(Contact_number, Age))+
  geom_boxplot(outlier.size = 1.7, outlier.shape = 20, lwd = 0.6, fatten = 1.2, fill = "sienna2", alpha = 0.8)+
  #x는 연속형 y는 이산형
  scale_x_continuous(trans = "log10")+
  scale_y_discrete(limit = c("90s", "80s", "70s", "60s", "50s", "40s", "30s", "20s", "10s", "0s"))+
  labs(title = "Number of contacts and age of infected", subtitle = "For people with known age and number of contacts (n = 791)", 
       x = "Number of contacts (logarithmic scale)", y = "Age (group)")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(size = 15, face = "bold"), axis.text = element_text(size = 15), panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "#fff6ed"), axis.line = element_line(size = 0.4, colour = "grey10"),
        legend.background = element_rect(fill = "#fff6ed"), plot.subtitle = element_text(size = 15))
#sum(table(data$Age, data$Contact_number))

###code12
data[data$Sex %in% c("female", "male"), ] %>%
  ggplot(., aes(x = Sex, y = Contact_number, fill = Sex))+
  #geom_violin : violin형태의 함수 생성
  geom_violin(alpha = 0.8, col = "black")+
  #x는 범주형(성별) y는 연속형
  scale_y_continuous(limits = c(0,50), breaks = seq(0,50,10))+
  scale_x_discrete(labels = c("Female","Male"))+
  scale_fill_brewer(palette = "Set1")+
  labs(title = "Number of contacts of infected people", subtitle = "by Sex (n = 4043)", 
       x = "Sex", y = "Number of contacts (up to 50)", fill = "Sex")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(size = 15, face = "bold"), axis.text = element_text(size = 15), legend.position="none", 
        legend.direction='horizontal',plot.background = element_rect(fill = "#fff6ed"), plot.subtitle = element_text(size = 15),
        axis.line = element_line(size = 0.4, colour = "grey10"), panel.background = element_rect(fill = "white"))
#nrow(data[data$Sex %in% c("female", "male"), ])

