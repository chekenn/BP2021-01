#SPSS 데이터 파일(.sav)을 가져오기 위한 패키지 설치 및 로드
#install.packages("foreign")
library(foreign)
library(ggplot2)
library(dplyr)
library(readxl)
#데이터 불러오기
raw_welfare <- read.spss(file = "Koweps_hpda15_2020_beta1.sav",
                         to.data.frame=T, reencode="UTF-8")
View(head(raw_welfare))

###문제1 데이터 검토하기
welfare <- raw_welfare
head(welfare)
tail(welfare)

View(welfare)
dim(welfare)

str(welfare)
summary(welfare)

###문제2 변수명 변경하기
names(welfare)
welfare <- dplyr::rename(
  welfare,
  sex = h15_g3, # 성별
  birth = h15_g4, # 태어난 연도
  marriage = h15_g10, # 혼인 상태
  education = h15_g6, # 교육수준
  disability = h15_g9, # 장애등급
  health = h15_med2, # 건강상태
  income = p1502_8aq1, # 일한달의 월 평균 임금
  code_job = h15_eco9,#직종
  code_region = h15_reg7#7개 권역별 지역구분
)
View(head(welfare))

###문제3
#3.1 변수 검토 및 전처리 
#3.1.1.성별 변수의 타입을 파악하고, 빈도표를 만들어 각 범주에 몇 명이 있는지 확인
class(welfare$sex)
table(welfare$sex)

#3.1.2. 코드북을 참고하여, 모름/무응답이 있는 경우 이상치로 간주하고 이상치 제외
#(이상치가 없더라도 이상치 확인, 결측 처리, 결측치 확인)
welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex)
table(is.na(welfare$sex))

#3.1.3. 성별 항목에 이름을 부여하고(1:”male”, 2:”female”), 전처리에서 사용하는 빈도막대 그래프로 확인
welfare$sex <- ifelse(welfare$sex == 1, "male","female")
table(welfare$sex)
qplot(welfare$sex)

#3.1.4. 월급 변수의 타입을 파악하고, 요약 통계량을 확인
class(welfare$income)
summary(welfare$income)

#3.1.5. 전처리에서 사용하는 빈도막대그래프로 월급 변수를 확인
qplot(welfare$income)+ xlim(0,1500)

#3.1.6. 코드북을 참고하여, 정상 범위 밖의 이상치를 결측 처리 후 결측치 확인
welfare$income <- ifelse(welfare$income %in% c(0,9999),NA,welfare$income)
table(is.na(welfare$income))

#3.2. 분석하기
#3.2.1. 성별 월급 평균표 sex_income 만들기
sex_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income = mean(income))
sex_income

#3.2.2. 평균표를 이용하여 막대 그래프로 표현
ggplot(
  data = sex_income,
  aes(x=sex, y=mean_income)
) + geom_col()

#3.2.3. 그래프 해석
#남성의 mean_income은 352정도 여성의 mean_income은 190정도이다. 
#남성이 여성에 비해 소득 평균이 높음을 알 수 있다. 

###문제4
#4.1. 변수 검토 및 전처리
#4.1.1. 태어난 연도 변수의 타입을 파악하고, 요약 통계량을 확인, 전처리에서 사용하는 빈도막대그래프로 확인
class(welfare$birth)
table(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)

#4.1.2. 코드북을 참고하여, 모름/무응답이 있는 경우 이상치로 간주하고 이상치 제외
#(이상치가 없더라도 이상치 확인, 결측 처리, 결측치 확인)
welfare$birth <- ifelse(welfare$birth==9999, NA,welfare$birth)
table(is.na(welfare$birth))

#4.1.3. 나이 파생변수 생성을 위해 태어난 연도 변수를 조사 연도인 2020년에서 뺀 뒤 1 을 더해서 나이 변수 만들기
welfare$age <- 2020-welfare$birth + 1

#4.1.4. 나이 변수의 요약 통계량을 확인, 전처리에서 사용하는 빈도막대그래프로 확인
summary(welfare$age)
qplot(welfare$age)

#4.2. 분석하기
#4.2.1. 나이별 월급 평균표 age_income 만들기
age_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income))
age_income

#4.2.2. 평균표를 이용하여 선 그래프로 표현
ggplot(data =  age_income, 
       aes(x= age, y= mean_income)) +geom_line()

#4.2.3. 그래프 해석
#40~60세 나이대에서 소득평균값이 가장 높다는 것을 알 수 있다. 60세 이후 소득평균값이 급격이 감소하여 급격한 기울기의 변화를 볼 수 있다. 
#특이한 점은 80대 후반 부근에서 소득값이 다른 부근에 비해 높다는 점이다. 

###문제5
#5.1. 변수 검토 및 전처리
#5.1.1. 연령대 파생변수 ageg를 다음 기준에 맞게 생성
#(초년”young”: 30세 미만, 중년”middle”: 30~59세, 노년“old”: 60세 이상)
welfare <- welfare %>%
  mutate(ageg = ifelse(age<30,"young",ifelse(age <= 59, "middle", "old")))

#5.1.2. 연령대 변수를 빈도표 및 전처리에서 사용하는 막대그래프로 확인
table(welfare$ageg)
qplot(welfare$ageg)

#5.2. 분석하기
#5.2.1. 연령대별 월급 평균표 ageg_income 만들기
ageg_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income = mean(income))
ageg_income

#5.2.2. 평균표를 이용하여 막대 그래프로 표현
ggplot(data =  ageg_income, 
       aes(x= ageg, y= mean_income)) +geom_col()

#5.2.3. 막대 그래프가 초년, 중년, 노년의 나이 순으로 정렬되도록 설정 (scale_x_dixcrete() 활용)
ggplot(data =  ageg_income, 
       aes(x= ageg, y= mean_income)) +geom_col()+
  scale_x_discrete(limits = c("young", "middle","old"))

#5.2.4. 그래프 해석
#30~59세인 중년층의 평균 소득값이 가장 크다는 것을 알 수 있다. 
#중년 > 초년 > 노년 순으로 평균 소득값이 높다. 

###문제6
#6.2. 분석하기
#6.2.1. 연령대별 및 성별에 따른 월급 평균표 sex_income 만들기
sex_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg,sex) %>% 
  summarise(mean_income = mean(income))
sex_income

#6.2.2. 평균표를 이용하여 막대 그래프로 표현(성별에 따라 다른 색으로 표현되도록)
ggplot(data=sex_income, aes(x=ageg, y= mean_income, fill= sex))+geom_col()

#6.2.3. 막대 그래프가 초년, 중년, 노년의 나이 순으로 정렬되도록 설정(scale_x_dixcrete() 활용)
ggplot(data=sex_income, aes(x=ageg, y= mean_income, fill= sex))+geom_col()+
scale_x_discrete(limits = c("young", "middle","old"))

#6.2.4. 한 막대에 성별이 표현된 것을 분리하여 표현되도록 설정(position 파라미터 활용)
ggplot(data=sex_income, aes(x=ageg, y= mean_income, fill= sex))+
  scale_x_discrete(limits = c("young", "middle","old"))+
  geom_col(position="dodge")
  
#6.2.5. 그래프 해석
#중년남성의 소득 평균값이 가장 높음을 알 수 있다. 
#반면, 노년여성의 소득 평균값이 가장 낮다. 
#성별과 나이가 소득에 영향을 미치는 변수라는 사실을 확인할 수 있다. 

###문제7
#7.2. 분석하기
#7.2.1. 연령별 및 성별에 따른 월급 평균표 sex_age 만들기
sex_age <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age, sex) %>% 
  summarise(mean_income = mean(income))
sex_age

#7.2.2. 평균표를 이용하여 선 그래프로 표현(성별에 따라 다른 색으로 표현되도록)
ggplot(data=sex_age, aes(x=age, y= mean_income, col= sex))+geom_line()

#7.2.3. 그래프 해석
#전 연령에 걸쳐서 남성의 소득평균값이 여성에 비해 높다는 사실을 확인할 수 있다. 

###문제8
#8.1. 변수 검토 및 전처리
#8.1.1. 직종 변수의 타입을 파악하고, 빈도표를 만들어 각 범주에 몇 명이 있는지 확인
class(welfare$code_job)
table(welfare$code_job)

#8.1.2. 코드북의 두번째 시트를 list_job이라는 데이터프레임(티블)로 불러와서 앞 행을 확인하고, 몇 개의 행과 열로 되어 있는지 확인
list_job<-read_excel("Koweps_Codebook.xlsx", col_names=T, sheet=2)
head(list_job)
dim(list_job)

#8.1.3. 공통으로 들어 있는 직종 변수를 기준으로 welfare와 list_job을 결합
welfare <- left_join(welfare, list_job, id="code_job")

#8.1.4. welfare에서 직종 변수와 직업 변수(job)를 10개 행만 출력하여 잘 결합됐는지 확인
welfare %>%
  filter(!is.na(code_job)) %>%
  select(code_job, job) %>%
head(10)

#8.2. 분석하기
#8.2.1. 직업별 월급 평균표 job_income 만들기(직종 변수가 아닌 직업 변수 사용)
job_income <- welfare %>%
  filter(!is.na(job)&!is.na(income)) %>%
  group_by(job) %>%
  summarise(mean_income=mean(income))
head(job_income)

#8.2.2. 평균표를 월급 기준(mean_income)으로 내림차순으로 정렬하고 상위 10개를 추출하여 top10에 할당
top10<-job_income %>%
  arrange(desc(mean_income)) %>%
  head(10)
top10

#8.2.3. 추출된 평균표를 이용하여 막대 그래프로 표현(직업이 월급 기준 큰 순서로 정렬되도록 설정(reorder 활용), coord_flip()을 사용하여 그래프 회전)
ggplot(data=top10, aes(x=reorder(job, mean_income), y=mean_income))+geom_col()+coord_flip()+ylim(0,850)

#8.2.4. 이번에는 평균표를 월급 기준으로 하위 10개를 추출하여 bottom10에 할당
bottom10<-job_income %>%
  arrange(mean_income) %>%
  head(10)
bottom10

#8.2.5. 추출된 평균표를 이용하여 막대 그래프로 표현(직업이 월급 기준 적은 순서로 정렬되도록 설정(reorder 활용), coord_flip()을 사용하여 그래프 회전, y축을 위의 그래프와 동일한 범주로 표시되도록 설정)
ggplot(data=bottom10, aes(x=reorder(job, -mean_income), y=mean_income))+geom_col()+coord_flip()+ylim(0,850)

#8.2.6. 두 그래프 해석
#상위 10개의 월급평균값을 살펴보면 금융/의료/법률/컴퓨터 하드웨어 등 전문직과 관련된 직업임을 알 수 있다. 반대로, 하위 10개의 직업을 살펴보면 서비스직/예술가와 같은 직업임을 알 수 있다. 소득값 또한 상위 10개의 직업과 하위 10개의 직업 간의 간격이 크다는 것을 알 수 있다. 

###문제9
#9.2. 분석하기
#9.2.1. 각 성별로 직업별 빈도를 구해 상위 10개를 추출하여 job_male과 job_female에 할당
job_male<-welfare %>%
  filter(!is.na(job)&sex=="male") %>%
  group_by(job) %>%
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  head(10)
job_male

job_female<-welfare %>%
  filter(!is.na(job)&sex=="female") %>%
  group_by(job) %>%
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  head(10)
job_female

#9.2.2. 앞에서 만든 두 빈도표를 이용해 막대 그래프로 각각 표현(한 빈도표로 한 그래프)(x축에 빈도순으로 직업이 정렬되도록 하여(reorder 활용), 그래프 회전 coord_flip() 사용)
#남성
ggplot(data=job_male, aes(x=reorder(job, n), y=n))+geom_col()+coord_flip()

#여성
ggplot(data=job_female, aes(x=reorder(job, n), y=n))+geom_col()+coord_flip()

#9.2.3. 그래프 해석
#남과 여 모두 작물 재배 종사자가 가장 높은 임금 평균값을 보이고 있다. 여자의 경우 남자에 비해서 상위 10개의 직업군 중 서비스 종사자를 포함한 단순 종사자가 많다는 것을 알 수있다.
#그에 반해, 남자의 경우 자동차/건설/제조와 관련된 직업군을 상위 10개의 직업군에서 확인할 수 있다. 

###문제10
#10.1. 변수 검토 및 전처리
#10.1.1. 혼인 상태 변수의 타입을 파악하고, 빈도표를 만들어 각 범주에 몇 명이 있는지 확인
class(welfare$marriage)
table(welfare$marriage)

#10.1.2. 코드북을 참고하여, 파생변수 group_marriage를 생성(1: “marriage”, 3: “divorce”, 그 외에는 결측 처리) 후 빈도표로 확인
welfare$group_marriage <- ifelse(welfare$marriage == 1, "marriage",
                                 ifelse(welfare$marriage == 3, "divorce", NA))

table(welfare$group_marriage)

#10.1.3. 결측치를 빈도표로 확인하고, 전처리에서 사용하는 막대 그래프로 확인
table(is.na(welfare$group_marriage))
qplot(welfare$group_marriage)

#10.2. 분석하기
#10.2.1. 연령대별 이혼율 표 만들기
#(문제5에서 만든 ageg 변수 활용, 파생변수로 각 그룹별 빈도의 합 tot_group 변수와 각 그룹별 빈도를 빈도의 합 변수로 나눠 100을 곱한 이혼율 변수를 소수점 한자리 까지 반올림하여 이혼율 pct 변수 생성하여 ageg_marriage에 할당)
ageg_marriage <- welfare %>%
  group_by(group_marriage, ageg) %>%
  summarise(n=n())%>%
  mutate(tot_group=sum(n)) %>%
  mutate(pct=round(n/tot_group*100,1))
head(ageg_marriage)

#10.2.2. 연령대별 이혼율 그래프 만들기
#(앞에서 만든 표에서 초년을 제외하고, 이혼을 나타내는 값만 추출하여 ageg_divorce에 할당)(표를 이용하여 막대 그래프로 표현)
ageg_divorce <- ageg_marriage %>%
  filter(group_marriage=="divorce") %>%
  filter(ageg!="young")
  
ageg_divorce

ggplot(data=ageg_divorce, aes(x=group_marriage, y=pct, fill=ageg))+
  geom_col() + coord_flip()

#10.2.3. 그래프 해석
#이혼에서 중년과 노년의 비율이 비슷하게 나타남을 확인할 수 있다. 

###문제11
#11.1. 변수 검토 및 전처리
#11.1.1. 7개 권역별 지역구분 변수의 타입을 파악하고, 빈도표를 만들어 각 범주에 몇 명이 있는지 확인
class(welfare$code_region)
table(welfare$code_region)

#11.1.2. 코드북을 참고하여, 지역 코드 목록 list_region 만들기(data.frame 매서드를 이용하여 코드북의 1부터 7까지의 code_region 칼럼과 각 지역명을 region 칼럼에 입력)
list_region <- data.frame(code_region=c(1:7),
                          region=c("서울",
                                   "수도권(인천/경기)",
                                   "부산/경남/울산",
                                   "대구/경북",
                                   "대전/충남",
                                   "강원/충북",
                                   "광주/전남/전북/제주도"))
list_region

#11.1.3. 공통으로 들어 있는 code_region 변수를 기준으로 welfare와 list_region 결합하여 지역명 변수 추가
welfare <- left_join(welfare, list_region, id="code_region")

#11.1.4. 잘 결합됐는지 확인하기 위해 welfare에 code_region과 region 칼럼을 앞 6개 행 확인
welfare %>%
  select(code_region, region) %>%
head(6)

#11.2. 분석하기
#11.2.1. 지역별 연령대 비율표 만들기(문제5에서 만든 ageg 변수 활용, 파생변수로 각 그룹별 빈도의 합 tot_group 변수와 각 그룹별 빈도를 빈도의 합 변수로 나눠 100을 곱한 비율 변수를 소수점 두자리 까지 반올림하여 비율 pct 변수 생성하여 region_ageg에 할당)
region_ageg <- welfare %>%
  group_by(region, ageg) %>%
  summarise(n=n())%>%
  mutate(tot_group=sum(n)) %>%
  mutate(pct=round(n/tot_group*100,2))
head(region_ageg)

#11.2.2. 지역별 연령대 비율 그래프 만들기(앞에서 만든 표를 이용하여 막대 그래프로 표현)(지역과 비율을 x축과 y축으로 표현하고, 색으로 연령대를 다르게 표현되도록 하며, 그래프를 coord_flip()을 이용하여 회전)
ggplot(data=region_ageg, aes(x=region, y=pct, fill=ageg))+
  geom_col()+coord_flip()

#11.2.3. 노년층 비율 높은 순으로 막대 정렬하기 
#앞에서 만든 표를 노년만 추출하여 비율 순(오름차순)으로 정렬하여 list_order_old에 할당 
#이 표를 이용하여 지역명 칼럼만 order에 할당 지역명이 노년층 비율 순으로 정렬된 order 변수를 활용해 앞의 그래프 코드에 scale_x_discrete()를 추가하여 limits 파라미터에 변수를 지정하여 노년층 비율이 높은 순으로 정렬
list_order_old <- region_ageg %>%
  filter(ageg=="old")%>%
  arrange(pct)
list_order_old

order <- list_order_old$region
order

ggplot(data=region_ageg, aes(x=region, y=pct, fill=ageg))+
  geom_col() + coord_flip() + scale_x_discrete(limits = order)

#11.2.4. 연령대 순으로 막대 색깔 나열하기 연령대 변수의 타입과 범주를 확인하고, 범주형 타입이 아니라면 범주형 데이터 타입으로 변형 이때 범주의 순서가 나이순(노년, 중년, 초년 순)으로 정렬되도록 설정 다시 앞 문제의 그래프 코드로 표현
class(region_ageg$ageg) #문자열 타입
levels(region_ageg$ageg) #범주의 순서가 없음

region_ageg$ageg <- factor(region_ageg$ageg,
                           level=c("old","middle","young"))

ggplot(data=region_ageg, aes(x=region, y=pct, fill=ageg))+
  geom_col()+coord_flip()+scale_x_discrete(limits = order)

#11.2.5. 그래프 해석
#노년층은 초년+중년에 비해 비수도권에 많이 거주하고 있음을 알 수 있다. 
#경제활동인구가 수도권에 집중하고 있는 현상을 파악할 수 있다. 

###문제12 교육수준과 월급의 관계
class(welfare$education)
table(welfare$education)

welfare$education <- ifelse(welfare$education == 99, NA, welfare$education)
table(is.na(welfare$education))

table(welfare$education)
qplot(welfare$education)

list_education <- data.frame(education=c(1:9),
                          educationstr=c("미취학(만7세미만)",
                                      "무학(만7세이상)",
                                      "초등학교",
                                      "중학교",
                                      "고등학교",
                                      "전문대학",
                                      "대학교",
                                      "대학원(석사)",
                                      "대학원(박사)"))
list_education

welfare <- left_join(welfare, list_education, id="education")

welfare %>%
  select(education, educationstr) %>%
  head

education_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(educationstr) %>% 
  summarise(mean_income = mean(income))
education_income

ggplot(data =  education_income, 
       aes(x= reorder(educationstr, -mean_income), y= mean_income)) +geom_col()

#분석결과
#학력이 높을수록 소득평균값이 높다는 것을 알 수 있다. 
#즉, 학력과 월급은 유의미한 양의 상관관계가 있다. 

###문제13 장애등급과 월급의 관계
class(welfare$disability)
table(welfare$disability)

welfare$disability <- ifelse(welfare$disability == 9, NA, welfare$disability)
table(is.na(welfare$disability))

table(welfare$disability)
qplot(welfare$disability)

list_disability <- data.frame(disability=c(0:3),
                             disabilitystr=c("비해당(비장애인)",
                                            "장애정도가 심한 장애인",
                                            "장애정도가 심하지 않은 장애인",
                                            "비등록 장애인(보훈처등록장애인포함)"))
list_disability

welfare <- left_join(welfare, list_disability, id="disability")

welfare %>%
  select(disability, disabilitystr) %>%
  head

disability_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(disabilitystr) %>% 
  summarise(mean_income = mean(income))
disability_income

ggplot(data = disability_income, 
       aes(x= reorder(disabilitystr, -mean_income), y= mean_income)) +geom_col()

#분석결과
#장애인의 경우 평균소득이 낮다는 것을 확인할 수 있다. 이를 통해 장애인을 위한 국가적 정책이 필요함을 알 수 있다. 


###문제14 건강상태와 이혼율의 관계
class(welfare$health)
table(welfare$health)

welfare$health <- ifelse(welfare$health == 9, NA, welfare$health)
table(is.na(welfare$health))

table(welfare$health)
qplot(welfare$health)

list_health <- data.frame(health=c(1:5),
                          healthstr=c("아주 건강하다",
                                   "건강한 편이다",
                                   "보통이다",
                                   "건강하지 않은 편이다",
                                   "건강이 아주 안 좋다"))
list_health

welfare <- left_join(welfare, list_health, id="health")

welfare %>%
  select(health, healthstr) %>%
  head

health_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>%
  group_by(healthstr, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 1))
health_marriage

ggplot(data=health_marriage, aes(x=group_marriage, y=pct, fill=healthstr))+
  geom_col()+coord_flip()

ggplot(data=health_marriage, aes(x=healthstr, y=pct, fill=group_marriage))+
  geom_col()+coord_flip()

#분석결과
#divorce에서 상대적으로 다른 건강상태에 비해 '건강하지 않은 편이다'와 '건강이 아주 안 좋다'의 비율이 높은 편이다. 즉, 이혼율과 건강상태 간의 유의미한 상관관계가 있음을 추론해볼 있다.  


