#패키지 설치
install.packages("tidyverse")
install.packages("magrittr")
install.packages("scales")
install.packages("sf")
install.packages("lubridate")
install.packages("ggraph")
install.packages("igraph")
install.packages("tidygraph")
install.packages("ggthemes")
install.packages("ggExtra")

library(tidyverse)
library(magrittr)
library(scales)
library(sf)
library(lubridate)
library(ggraph)
library(igraph)
library(tidygraph)
library(ggthemes)
library(ggExtra)

#patient 파일 불러오기
patient <- read_csv('archive/PatientInfo.csv')
print(dim(patient))
patient %>% head

###1###
#히트맵(column별 present vs missing)
patient %>% 
  is.na %>% #결측값 확인
  as_tibble %>% # same as 'melt' in reshape
  mutate(rows = row_number()) %>% 
  gather(key = columns, value = value, -rows) %>% 
  ggplot(aes(x = columns, y = rows)) +
  geom_raster(aes(fill = value)) + #히트맵(Heat map)
  geom_text(#그래프위에 글씨 삽입
    data = . %>% group_by(columns) %>% summarise(n = sum(value == T)),
    aes(y = nrow(patient), label = n),
    vjust = -1, # hjust(가로), vjust(세로) 속성을 통해 글씨 위치 조정
    size = 5,
    color = 'red',
    fontface = 'bold'
  ) +
  #그래프에 세로선 추가하기 : geom_vline()
  geom_vline(xintercept = seq(1, ncol(patient)) + .5, linetype = 'dashed') +
  #scale
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(
    limits = c(0, nrow(patient)),
    expand = expand_scale(mult = c(0, .1)),
    breaks = seq(0, nrow(patient), 1000)
  ) +
  scale_fill_grey(
    name = '',
    labels = c('present', 'missing')
  ) +
  labs(x = 'Columns', y = 'Rows') +
  theme(
    text = element_text(size = 15, face = 'bold'),
    axis.text.x  = element_text(angle = 90, vjust = 0.5)
  )


###2###
#시계열 그래프 그리기
patient %>% 
  filter(!is.na(confirmed_date)) %>% 
  group_by(confirmed_date) %>% 
  ggplot() +
  geom_line(
    aes(x = confirmed_date, y = ..count.., group = 1, color = '1'),
    stat = 'count',
    size = 3,
    show.legend = F
  ) +
  #line
  geom_line(
    aes(x = confirmed_date, y = sum(..count..) / patient %>% select(confirmed_date) %>% n_distinct(na.rm = T), group = 1, color = '2'), #na.rm = T 결측값 제거
    linetype = 'dashed',
    stat = 'count',
    size = 2,
    show.legend = F
  ) +
  #point
  geom_point(
    data = . %>%
      group_by(confirmed_date) %>% 
      summarise(count  = n()) %>% 
      arrange(desc(count)) %>% 
      slice(1),
    aes(x = confirmed_date, y = count),
    color = 'red',
    shape = 21,
    stroke = 3,
    size = 6
  ) +
  geom_text(
    data = . %>%
      group_by(confirmed_date) %>% 
      summarise(count  = n()) %>% 
      arrange(desc(count)) %>% 
      slice(1),
    aes(x = confirmed_date, y = count, label = paste(confirmed_date, ': \n "', count, '"', sep = '')),
    vjust = -.5,
    size = 6,
    fontface = 'bold'
  ) +
  #scale
  scale_x_date(
    expand = expand_scale(mult = c(0, 0.02)),
    date_breaks = '1 week', #x축 date 간격은 1주일
    labels = function(x) format(x, '%b %d')
  ) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .2))) +
  labs(x = 'Date', y = 'The number of confirmed patients') +
  theme(text = element_text(size = 15, face = 'bold'))


###3###
#major infection reason 파악하기
patient %>% 
  group_by(infection_case) %>% 
  summarise(count = n()) %>% 
  #na.omit : 결측치 제거
  na.omit %>% 
  #reorder
  #reorder(정렬하고 싶은 변수(factor형태여야함), 연속형 데이터, function)
  ggplot(aes(x = reorder(infection_case, count))) +
  #한 점과 다른 점 사이를 선으로 연결 / arrow 함수
  geom_segment(aes(xend = ..x.., y = count, yend = 0), linetype = 'dashed') +
  geom_point(
    aes(y = count + 15, color = reorder(infection_case, count)), 
    shape = 21,
    size = 8, 
    stroke = 3, 
    show.legend = F
  ) +
  geom_text(aes(y = count + 15, label = count), size = 3, fontface = 'bold') +
  scale_y_continuous(
    expand = expand_scale(mult = c(0, .1)),
  ) +
  #x축과 y축의 구성을 뒤집어 표현하라는 명령어
  coord_flip() +
  theme(text = element_text(size = 15, face = 'bold')) +
  guides(color = guide_legend(title = NULL)) +
  labs(x = 'infection reason', y = 'the number of patients')
