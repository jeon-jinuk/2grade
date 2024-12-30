0913 수업 필기 #3주차
library(tidyverse)
library(readxl)
library(dplyr)
library(magrittr)
library(ggplot2)


getwd()
checkout=read.csv("checkout.csv")

nrow(checkout) #행 obs

ncol(checkout) #열 variable

head(checkout) #데이터의 앞부분
tail(checkout) #데이터의 뒷부분

str(checkout) # 변수 파악 struture데이터의구조

# factor : 범주형 변수
# 혈액형 조사 5명
btype=c("A","A","B","O","O")

table(btype) #빈도표

str(btype)
#factor 질적변수 
btype=factor(c("A","A","B","O","O"), levels = c("A","B","AB","O"))
table(btype)
str(btype)


my_check = read_xlsx("my_data.xlsx", sheet=1)
nrow(my_check)
ncol(my_check)
str(my_check)
head(my_check)
tail(my_check)

my_check$id = factor(my_check$id)

my_check_2 = read_xlsx("my_data.xlsx", sheet=2)
exam = read_xlsx("my_data.xlsx", sheet=3)
my_check_2
my_check_2$cust_id= factor(my_check_2$cust_id)
my_check_2$prod_name= factor(my_check_2$prod_name)
#########_------------------------------------------------------------------
#파이프연산
# ~~ 를 ~~ 한다/
# ~~ 를 ~~ 하고 ~~ 하고 ~~ 한다. # 주로 데이터 %>% ___하고 %>% ______ 하고 %>% _____ 한다. # shift + ctrl + M
###########--------------------------------------------------------
head(my_check)
head(my_check,2)

my_check %>% head
my_check %>% tail

# filter : 행을 선택한다. 조건에 맞는 행을 선택한다. 행을 ---------------
my_check
view(my_check)
m1 = filter(my_check, my_check$id==1)
m1
filter(my_check, amount > 1000)
filter(my_check, prod_name == "apple")
filter(my_check, prod_name == "apple" & id==1)
#####

m_1 = my_check %>% filter(id==1)
my_check %>% filter(amount > 1000)
my_check %>% filter(id==1) %>% filter(prod_name =="apple")

my_check %>% filter(id==1 & prod_name =="apple")
#속한다면 들어간다면 %in%
my_check %>% filter(prod_name %in% c("apple", "egg"))
my_check %>% filter(prod_name =="apple" | prod_name == "egg")

my_check %>% filter(id==4 | id==5)
my_check %>% filter(id %in% c(4,5))

#중복되지 않는
my_check %>% distinct(id) #변수 id만 가져와서 중복되지 않게
my_check %>% distinct(prod_name)

my_check %>%
  filter(grepl("a", prod_name)) %>%
  distinct(prod_name) 

my_check %>% filter(between(price,50,100))
my_check %>% filter(between(price,50,100) & between(amount,200,500))

my_check %>% distinct(prod_name) %>%
  filter(grepl("a", prod_name))
my_check %>% filter(grepl("a", prod_name))
my_check %>% filter(grepl("a", prod_name
                          
# 집계함수(sum(), mean(), n(), ..) =======================================                    
# summarise안에 집계함수
# 새로운 변수 생성할 수 있음
my_check %>% summarise(sum(amount))
my_check %>% summarise(n()) #행의수
r = my_check %>% summarise(tot=sum(amount))                         
r
my_check %>% summarise(n_distinct(id))
my_check %>% summarise(n=n(), n_id=n_distinct(id))
#n 행의수 id  고객의수
my_check %>% summarise(n()/n_distinct(id))
my_check %>% summarise(avg=n()/n_distinct(id))


                          
#연습문제 1
# 1-1. my_check_2의 앞 6행 출력한다. 
my_check_2 %>% head(6)
# 1-2. my_check_2의 뒤 4행 출력한다. tail 사용 

my_check_2 %>% tail(4)
#######-------------------------------------

#연습문제 2. filter() 사용하세요!!
# my_check_2  자료를 이용한다. 
# 2-1. 고객번호가 3번인 사람의 자료만 출력
my_check_2 %>% filter(cust_id==3)
# 2-2. 고객번호가 3번 혹은 5번인 사람의 자료만 출력
my_check_2 %>% filter(cust_id==4 | cust_id==5)
# 2-3. hat 을 주문한 기록 출력
my_check_2 %>% filter(prod_name =="hat") 
# 2-4. hat  혹은  socks를 주문한 고객번호
my_check_2 %>% filter(prod_name =="hat" | prod_name == "sock")
# 2-5. qty가 2개인 주문
my_check_2 %>% filter(qty==2)
# my_check 이용 
# 2-6.과일 이름 출력, 중복 안됨
my_check %>% distinct(prod_name)
# 2-7. "r" 을 포함하는 과일 이름
my_check %>%
  filter(grepl("r", prod_name)) %>%
  distinct(prod_name) 
# 2-8. 중복되지 않는 고객번호
my_check %>% distinct(id)
# 2-9. price가 150이상이고 number 가 3이상인 주문
my_check %>% filter(150<=price) & (3<=number )
# 2-10. (caramel 혹은 apple주문) 중에서 number가 3이상인 주문
my_check %>% filter(prod_name %in% c("apple", "caramel")) & (3<=number)
# 2-11. price가 50 이상 200이하이고, number가 5이하인 주문
my_check %>% filter(between(price,50,200) & (number<=5)
# 2-12. prod_name 이 apple 혹은 caramel이고, price가 300 이상인 주문의 횟수
my_check %>% filter(prod_name %in% c("apple", "caramel")) & (300<=price)

# 연습문제 3. summarise() 사용하세요!!!
# my_check
# 3-1. 주문한 금액(amount)의 총 합
my_check %>% summarise(sum(amount))
# 3-2. 주문 개수(number)의 총 합
my_check %>% summarise(sum(number))
# 3-3. 주문 건 수는 ? 
my_check %>% summarise(n()) 
# 3-4. 고객의 수는 ?
my_check %>% summarise(n_distinct(id))
# 3-5. 고객 한 명당 평균 주문 횟수는 ?
my_check %>% summarise(Mean_id=n()/n_distinct(id))
# 3-6. 주문 금액(amount)의 평균은? 
my_check %>% summarise(Mean_amount=sum(amount)/n())
# my_check_2
# 3-7. 주문량(qty)의 합은?
my_check_2 %>% summarise(sum(qty))
# 3-8. 고객의 수는 ?
my_check_2 %>% summarise(n_distinct(cust_id))
# 3-9. 단가(unit_price)의 평균은?
my_check_2 %>% summarise(sum(unit_price)/sum(qty))
# 3-10. 주문량(qty)의 평균은?
my_check_2 %>% summarise(sum(qty)/n())

                          
                          
                          
