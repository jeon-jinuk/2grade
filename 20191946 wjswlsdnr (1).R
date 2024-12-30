#7주차
library(tidyverse)
library(readxl)
library(ggplot2)
my_check = read_xlsx("C:/Users/admin/Desktop/Data_viz/my_data.xlsx",sheet = 1)
my_check_2 = read_xlsx("C:/Users/admin/Desktop/Data_viz/my_data.xlsx",sheet = 2)
CarSales = read.csv("C:/Users/admin/Desktop/Data_viz/CarSales.csv")
checkout2 = read.csv("C:/Users/admin/Desktop/Data_viz/checkout2.csv")
##=====================================================================
# ~~별로(범주형변수) ~~(양적변수)의 분포를 표현하고 비교
# 6 가지 방법으로 분포를 알아본다. # boxplot(violin) 과 histogram(density)는 x, y 가 다름
# id별로 주문합의 분포
#1. boxplot
# ~~별로에 사용되는 변수가 범주형이 아니면 원하는 그림을 그리지 못한다. # 항상 확인하세요. 
my_check = read_xlsx("data/my_data.xlsx", sheet=1)
# id별로 그림을 그리고 싶은데
ggplot(my_check, aes(x=id, y=amount)) + geom_boxplot()
#id가 범주형으로 지정이 되어있지 않아서
str(my_check)

my_check$id = factor(my_check$id)
my_check$prod_name = factor(my_check$prod_name)

ggplot(my_check, aes(x=id, y=amount)) + geom_boxplot()

# id가 4,5인 고객만으로 연습
data45 <- my_check %>%
  filter(id %in% c(4,5))

ggplot(data45, aes(x=id, y=amount)) +
  geom_boxplot() 
  
ggplot(data45, aes(x=id, y=amount)) +
  geom_boxplot() +
  geom_jitter(height = 0, width = 0.01)
#boxplot의 단점 요약해서 보여주기떄문에 상세히 볼수 x jitter(흔들다)를 쓰면 알 수 있다 

#2. Multi violin chart boxplot 보다 진화한차트
ggplot(data45, aes(x=id, y=amount)) +
  geom_violin()+
  geom_jitter(height = 0, width = 0.01)

# 그림 설명 : 5번 고객의 800근방에 주문액이 많이 몰려있다 주문액의 분포가 더 넓고, 평균과 중앙값이 더 크다. 
#4번 고객은 200~600 사이에 골고루 분포되어있다

#3. Multi density chart 히스토그램을 곡선으로 만듬
# x 축이 양적변수, group 사용
ggplot(data45, aes(x=amount, group=id)) +
  geom_density(alpha=0.6)

#group 대신 color 혹은 fill 가능
ggplot(data45, aes(x=amount, color=id)) +
  geom_density(alpha=0.4)

ggplot(data45, aes(x=amount, fill=id)) +
  geom_density(alpha=0.4)

#4. ridgeline plot
library(ggridges)
ggplot(data45, aes(x = amount , y = id )) +
  geom_density_ridges2()

ggplot(data45, aes(x = amount , y = id, fill=id )) +
  geom_density_ridges2(alpha=0.3)
#alpha=선명도
# 4번 고객의 주문액은 480정도에 많이 있고, 5번 고객은 700정도에 많이 있다. 1500 이상도 있는데
# 4번고객은 1000 이상은 상당히 희박하다. 

#5. Small Multiple with facet_wrap()
ggplot(data=data45, aes(x=amount, group=id)) +
  geom_density(adjust=1) + # adjust = binwidth
  facet_wrap(~id) #한장에 하나씩 id기준으로

# 전체중에서 차지하는 비중 비교
#6. stacked area plot
# group=id는 fill=id가 있으니 생략가능
ggplot(data=data45, aes(x=amount, group=id, fill=id)) +
  geom_density(alpha=0.6,adjust=2, position="identity") # adjust = binwidth

ggplot(data=data45, aes(x=amount, fill=id)) +
  geom_density(alpha=0.3,adjust=2, position="fill")
# 800 을 기준으로 그 이하는 4번 고객이 많고 그 이상을 5번 고객이 많다. 
#비율을 바교해볼떄
ggplot(data=data45, aes(x=amount, fill=id)) +
 geom_density(alpha=0.3,adjust=1, position="stack")
#stack=쌓다 
##=====================================================================
# id별로 주문합의 분포

#1. boxplot
ggplot(my_check, aes(x=id, y=amount)) +
  geom_boxplot() +
  geom_jitter(height = 0, width = 0.01)

#2. Multi violin chart
ggplot(my_check, aes(x=id, y=amount)) +
  geom_violin()

#3. Multi density chart
# x 축이 양적변수, group 사용
ggplot(my_check, aes(x=amount, group=id)) +
  geom_density(alpha=0.6)
#group 대신 color 혹은 fill 가능
ggplot(my_check, aes(x=amount, color=id)) +
  geom_density(alpha=0.4)

ggplot(my_check, aes(x=amount, fill=id)) +
  geom_density(alpha=0.4)

#4. ridgeline plot
library(ggridges)
ggplot(my_check, aes(x = amount , y = id )) +
  geom_density_ridges2()

ggplot(my_check, aes(x = amount , y = id, fill=id )) +
  geom_density_ridges2()

#5. Small Multiple with facet_wrap()
ggplot(data=my_check, aes(x=amount, group=id)) +
  geom_density(adjust=1) + # adjust = binwidth
  facet_wrap(~id)

# 전체 중에서 차지하는 비중 비교
#6. stacked area plot
# group=id는 fill=id가 있으니 생략가능
ggplot(data=my_check, aes(x=amount, group=id, fill=id)) +
  geom_density(alpha=0.6,adjust=2, position="identity") # adjust = binwidth

ggplot(data=my_check, aes(x=amount, fill=id)) +
  geom_density(alpha=0.3,adjust=2, position="fill")

ggplot(data=my_check, aes(x=amount, fill=id)) +
  geom_density(alpha=0.3,adjust=1, position="stack")
#################################


##########-----------------------------------------
# 상품별로 주문액의 분포를 상자그림을 통해 볼 수 있다. 동시에 각 값을 표시
ggplot(my_check, aes(x=prod_name, y=amount)) +
  geom_boxplot()+
  geom_jitter(height = 0, width = 0.1)
# 주문액의 평균은 caramel이 가장 크고, apple이 가장 작다.
# 그리고 분산은 caramel이 가장 크고 donut이 가장 적다.
# 주문액의 최고는 caramel이고 최소는 apple, banana 이다.

# 과제 11-4
# 상품별로 주문액의 분포를 violin, density,ridgeline plot,
# Small Multiple with facet_wrap, stacked area plot 으로 표현하고 설명하시오. ##########==================================================
str(my_check)
my_check$prod_name = factor(my_check$prod_name)
#violin
ggplot(my_check, aes(x=prod_name, y=amount)) +
  geom_violin()+
  geom_jitter(height = 0, width = 0.01)
#설명:  바나나는 600쯤에 분포가 몰려있다. 그리고 도넛의 분산이 작아서 점으로 표시된다. 사과는 600에서 분포가 급격히 없어진다
# 주문액의 최고는 카라멜, 달걀, 바나나, 사과, 도넛순이다.  
#density
ggplot(my_check, aes(x=amount, color=prod_name)) +
  geom_density(alpha=0.6)
#설명: 바나나가 다른 상품들에 비해 600에 유독 많이 분포하고 있고 변화가 심하다 
#ridgeline plot
ggplot(my_check, aes(x = amount , y = prod_name )) +
  geom_density_ridges2()
#설명: 사과는 0과 500사이에 제일 많이 분포해있고 바나나는 500~1000사이에 많이있다 분산은 카라멜이 제일 크다 작은것은 도넛이다
# 달걀은 1000~1500사이에 분포가 크고 500~1000사이도 있다
#Small Multiple with facet_wrap
ggplot(data=my_check, aes(x=amount, group=prod_name)) +
  geom_density(adjust=1) +
  facet_wrap(~prod_name)
#설명: 도넛은 표본이 적어 그래프가 그려지지 않는다 그리고 등락의 폭은 바나나가 제일 크다  
#stacked area plot
ggplot(data=my_check, aes(x=amount, group=prod_name, fill=prod_name)) +
  geom_density(alpha=0.6,adjust=2, position="identity")
#설명: 분산은 카라멜이 가장 크고 도넛이 가장 적다 
# 바나나과 카라멜 판매액의 분포
d3 <- my_check %>%
  filter(prod_name %in% c("banana", "caramel"))
d3%>%
  ggplot(aes(x=prod_name , y=amount)) +
  geom_boxplot()+ geom_jitter(height = 0, width = 0.01)
d3 %>%
  ggplot(aes(x=prod_name , y=amount))+
  geom_violin(aes(fill=prod_name))+ geom_jitter(height = 0, width = 0.01)
# 과제 11-5
# 바나나과 카라멜 판매액의 분포를 density, ridgeline plot, Small Multiple with facet_wrap,
# stacked area plot 로 표현하고 설명하시오.

#density
ggplot(d3, aes(x=amount, color=prod_name)) +
  geom_density(alpha=0.6)
#설명: 둘중 카라멜의 평균이 크다
#ridgeline plot
ggplot(d3, aes(x = amount , y = prod_name )) +
  geom_density_ridges2()
#설명:둘중 카라맬의 분산이 제일 크다
#Small Multiple with facet_wrap
ggplot(data=d3, aes(x=amount, group=prod_name)) +
  geom_density(adjust=1)+
facet_wrap(~prod_name)
#설명: 둘중 판매액의 최고는 카라멜이다
#stacked area plot
ggplot(data=d3, aes(x=amount, group=prod_name, fill=prod_name)) +
  geom_density(alpha=0.6,adjust=2, position="identity")


# 과제 11-6
# my_check_2 데이터의 socks, pants, hat의 주문액 분포를
# density, ridgeline plot, stacked area plot으로 표현하고 설명하시오.
d4 <- my_check_2 %>%
  filter(prod_name %in% c("socks", "pants", "hat"))

#density
ggplot(d4, aes(x=amount, color=prod_name)) +
  geom_density(alpha=0.6)
#ridgeline plot
ggplot(d4, aes(x = amount , y = prod_name )) +
  geom_density_ridges2()
#stacked area plot
ggplot(data=d4, aes(x=amount, group=prod_name, fill=prod_name)) +
  geom_density(alpha=0.6,adjust=2, position="identity")


###########_---------------------------------------------

###########+
# mtcars 데이터 사용 ++++++++++++++++++++++++++++++++++++++
str(mtcars)

# cyl별로 mpg의 평균
mtcars %>% group_by(cyl) %>% summarise(average=mean(mpg)) %>%
  ggplot(aes(cyl, average)) +
  geom_col(color="blue", fill=rgb(0.5,0.,0,0.4))

mtcars %>% group_by(cyl) %>% summarise(average=mean(mpg)) %>%
  ggplot(aes(cyl, average)) +
  geom_point()+
  geom_segment( aes(x=cyl, xend=cyl, y=0, yend=average))
# cyl별로 mpg의 분포를 살펴본다. 
p1 <- ggplot(mtcars, aes(factor(cyl), mpg))
p1 + geom_boxplot()+
  geom_jitter(height = 0, width = 0.01)
# 실린더가 많을 수록 연비가 낮음을 알 수 있고, 연비의 분산도 적다. 
p1 + geom_violin(aes(fill=cyl)) +
geom_jitter(height = 0, width = 0.1)
# 실린더가 1일때의 연비는 차종에 따라 23~34 사이에 고르게 분포되어있고. 
# 3일때의 연비는 차종에 따라 15근방이 많이 몰려있다. 
# 2일때의 연비는 차종에 따라 18~22사이에 고르게 분포되어 있다. 
ggplot(mtcars, aes(x=mpg, color=factor(cyl), fill=factor(cyl))) +
geom_density(alpha=0.4, adjust=2)
# 실린더가 1일때의 연비는 차종에 따라 25를 10~35사이에 넓게 퍼져있고
# 3일때의 연비는 차종에 따라 15근방을 중심으로 10~23사이에 대칭적으로 분포되어 있다. 
# 2일때의 연비는 차종에 따라 20을 중심으로 13과 27사이에 대칭적으로 분포되어 있다.

# 과제 11-7
# mtcars 에서 gear별 wt의 분포를 그린 후 설명하시오. 
p1 <- ggplot(mtcars, aes(factor(gear), wt))
p1 + geom_boxplot()+
  geom_jitter(height = 0, width = 0.01)

#설명:gear이 높을수록 wt의 값이 낮아진다
# 과제 11-8
# mtcars 에서 gear별 hp의 평균을 구하시오. 이를 그래프로 표현하시오.
mtcars %>% group_by(gear) %>% summarise(average=mean(hp)) %>%
  ggplot(aes(gear, average)) +
  geom_col(color="blue", fill=rgb(0.5,0.,0,0.4))
#설명: 기어 4가 평균이 제일 낮고 5가 제일 높다
#################==================================================
# 과제 11-9
###################################
# my_check_2 를 이용하세요. 가장 표현을 잘하는 그래프 1개만 그리세요. 
# 1). 고객별 주문액의 합을 구하고 그래프로 표현하시오.
my_check_2$cust_id = factor(my_check_2$cust_id)
ggplot(my_check_2, aes(x = amount , y = cust_id )) +
  geom_density_ridges2()
# 2). 상품별 주문액의 합을 구하고 그래프로 표현하시오.
my_check_2$prod_name = factor(my_check_2$prod_name)
ggplot(my_check_2, aes(x = amount , y = prod_name )) +
  geom_density_ridges2()
# 3). 고객별 주문액의 평균을 구하고 그래프로 표현하시오. 
my_check_2 %>% group_by(cust_id) %>% summarise(average=mean(amount)) %>%
  ggplot(aes(cust_id, average)) +
  geom_col(color="blue", fill=rgb(0.5,0.,0,0.4))
# 4). 상품별 주문액의 평균을 구하고 그래프로 표현하시오. ##########-----------------------
my_check_2 %>% group_by(prod_name) %>% summarise(average=mean(amount)) %>%
  ggplot(aes(prod_name, average)) +
  geom_col(color="blue", fill=rgb(0.5,0.,0,0.4))
# CarSales 데이터
# 고객이름별 주문액의 합을 구하고 그래프로 표현하시오. 
# 단, 주문액 합이 가장 많은 10명의 고객만. 

d = CarSales %>% group_by(customerName) %>%
summarise(total=sum(quantityOrdered* priceEach)) %>%
  arrange(desc(total)) %>%
  head(10)

# 같은 결과.
d= CarSales %>% group_by(customerName) %>%
summarise(total=sum(quantityOrdered* priceEach)) %>%
  slice_max(total, n=10)

d %>% ggplot(aes(x=customerName, y=total)) +
  geom_col()
# 자동으로 x 축의 크기 순(알파벳 순)
# hjust수평 자리 맞추기를 vjust제어하고 수직 자리 맞추기를 제어합니다.
d %>% ggplot(aes(x=customerName, y=total)) +
geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# 주문액의 크기 순으로 (x축의 순서가 이름인데 이것을 주문액으로 변경)
d %>% ggplot(aes(x=fct_reorder(customerName, total), 
                 y=total)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# 주문액의 크기 내림 차순으로
d %>% ggplot(aes(x=fct_rev(fct_reorder(customerName, total)), 
                 y=total)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))





