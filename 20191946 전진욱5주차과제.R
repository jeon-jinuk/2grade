#빈도를 나타내는 변수를 만든 후 geom_col)사용하여 막대그래프 그림
#빈도를 나타내는 변수는 summarisein)을 사용
#5주차
library(tidyverse)
library(readxl)
library(dplyr)
library(magrittr)
library(ggplot2)

my_check = read_xlsx("C:\\Users\\admin\\Desktop\\Data_viz\\my_data.xlsx",
my_check = read_xlsx("C:\\Users\\admin\\Desktop\\Data_viz\\my_data.xlsx")
my_check %>%#저장된 변수 확인 
group_by(id) %>%
summarise(n=n())

my_check %>%
group_by(id) %>% 
summarise(n=n()) %>% 
ggplot(aes(x=id, y=n)) + #x축, y축의 값 지정, 지정하지 않으면 순서대로 xy축
geom_col()

my_check %>%
group_by(id) %>% 
summarise(n=n())%>% 
ggplot(aes(x=id, y=n))+
geom_bar(stat="identity") #default, y 통계량을 사용한다.
                                                         
#3-1. 데이터 저장 후 그래프
d = my_check %>%
group_by(id) %>% 
summarise(n=n())
d           
           
           
d %>%
ggplot(aes(x=id, y=n})
geom_col()
                    
#number는 질적변수는 아니지만 막대그래프를 그릴 수 있다.
my_check %>%
ggplot(aes(number)) +
geom_bar()

#제품별 주문 횟수

my_check %>%
group_by(prod_name) %>%
summarise(num=n0)

my_check %>%
  group_by(prod_name) %>%
  summarise(num=n0) %>%
  ggplot(aes(prod_name, num)) +
  geom_col(color="blue", fill=rgb(0.5,0,0))

my_check$prod_name=factor(my_check$prod_name)

my_check %>%
  group_by(prod_name) %>%
  summarise(num=n0) %>%
  ggplotlaes(x=prod_name, y=num, fill=prod_name)) +
  geom_col(color="blue")+ 
  scale_fill_manual(values = c("#121212", "#FFFF00", "#FF0000", "#00FF77","cyan")) #그래프 그리기 전에 변수 형태를 확인하자. 특히 factor !!!!



my_check %>%
group_by(prod_name) %>%
summarise(num-n()) %>%
ggplot(aes(x=prod_name, y=num, fill=prod_name))+
geom_col(color="blue")+
scale_fill_manual (values = c(colors()[99], "#FFFF00",colors()[68], colors()[254], "cyan"))



data1 <- my_check%>%
  group_by(id)%>%
  summarise(n=n())

ggplot(data1,aes(x="",y=n,fill=id))+
  geom_col()
  
#파이그래프
ggplot(data1,aes(x="",y=n,fill=id))+
  geom_col(width=1) +
  coord_polar("y")+
  theme_void()


ggplot(data1,aes(x="",y=n,fill=id)) +
  geom_bar(stat="identity",width=1)

ggplot(data1,aes(x="",y=n,fill=id))+
  geom_col(width=1)+
  coord_polar("y")+
  theme_void()+
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),color = "white")

ggplot(data1,aes(x="",y=n,fill=id))+
  geom_col(width=1)+
  coord_polar("y")+
  theme_void()+
  geom_label(aes(label = n),
            color = "white",
            position = position_stack(vjust = 0.5),
            show.legend = FALSE)

data2 <- data.frame(
  group=c("B","C","D","E","A"),
  value=c(13,7,9,21,2)
)

ggplot(data2,aes(x="",y=value,fill=group))+
  geom_col(width=1,color="white")+
  coord_polar("y",start=0)+
  theme_void()+
  geom_label(aes(label = value),
             position = position_stack(vjust = 0.5),
  geom_label(aes(label = group),
             position = position_stack(vjust = 0.2), color="white"
             
df <- data.frame(value = c(10,30,32,28),
                 group = paste0("G",1:4))
hsize <- 4

df <- df %>%
  mutate(x = hsize)
#도넛
ggplot(df,aes(x = hsize,y = value,fill = group)) +
  geom_col() + 
  coord_polar(theta="y")+
  xlim(c(0.2,hsize+0.5))
#롤리팝
data <- my_check %>% group_by(id) %>% summarise(n=n())

ggplot(data,aes(x=id,y=n)) +
  geom_point()+
  geom_segment( aes(x=id,xend=id,y=0,yend=n))


ggplot(data,aes(x=id,y=n)) +
  geom_segment( aes(x=id,xend=id,y=0,yend=n))+
  geom_point( size=5, color="red", fill=alpha("orange",0.3),alpha=0.1,shape=11,stroke=5)
#circular barplot
ggplot(data,aes(x=as.factor(id), y=n))+
  geom_bar(stat="identity",fill=alpha("blue",0.3))+
  coord_polar()

library(wordcloud2)
head(demoFreq)
str(demoFreq)

wordcloud2(data=demoFreq,size=1.6)

df <- data.frame(w=c("time","happy","strong","money","fame"),cnt=c(25,9,11,4,39))
wordcloud2(data=df,size=1.6)

d <- my_check %>%
  group_by(id) %>%
  summarise(n=n())
str(d)
wordcloud2(data=d,size=1.6)


# 연습문제 8 --------------------------------------------------

my_check_2 = read_xlsx("C:\\Users\\admin\\Desktop\\Data_viz\\my_data.xlsx", sheet=2)
# my_check_2 이용

# geomc_col() 이용

# 8-1. 고객별 주문횟수의 막대그래프를 그리시오. 
my_check_2 %>%
  group_by(cust_id) %>%
  summarise(num=n()) %>%
  ggplot(aes(x=cust_id,y=num))+
  geom_col()
# 8-2. 상품별 주문횟수의 막대그래프를 그리시오. 
my_check_2 %>%
  group_by(prod_name) %>%
  summarise(num=n()) %>%
  ggplot(aes(x=prod_name,y=num))+
  geom_col()
# 8-3. 상품별 주문횟수의 막대그래프를 그리시오. 단, 막대 내부는 blue, 테두리는 yellow.
my_check_2 %>%
  group_by(prod_name) %>%
  summarise(num=n()) %>%
  ggplot(aes(prod_name, num)) +
  geom_col(color="yellow", fill=rgb(0,0,0.5))

# 8-4. 상품별 주문횟수의 막대그래프를 그리시오. 단, 상품별 막대의 색을 hexcode로
my_check_2 %>%
  group_by(prod_name) %>%
  summarise(num=n()) %>%
  ggplot(aes(x=prod_name, y=num, fill=prod_name)) +
  geom_col(color="blue")+ 
  scale_fill_manual(values = c("#F5F5DC", "#964B00", "#AA336A", "#8B4000","#F0F8FF","#AA332A")) 
 #그래프 그리기 전에 변수 형태를 확인하자. 특히 factor !!!!


#                (beige, brown2,deeppink, darkorange, aliceblue)색으로 지정하세요.

# 8-5. 상품별 주문횟수의 막대그래프를 그리시오. 단, 막대 내부는 black, 테두리는 red, 불투명도=0.5.
my_check_2 %>%
  group_by(prod_name) %>%
  summarise(num=n()) %>%
  ggplot(aes(prod_name, num)) +
  geom_col(color="red", fill=rgb(0,0,0,0.5))
##

###################################################################





##############_--------------------------------------------

## 연습문제 9 =====================

# 각 문제의 그림은 막대그래프, 파이차트, 도넛, circular barplot 그리고 

# 롤리팝, 워드클라우드 5개를 그립니다.

# 먼저 group_by, summarise, mutate 등을 이용하여 필요한 변수를 생성하시오.



# my_check_2 데이터를 이용하세요.

# 9-1. 각 고객의 주문 횟수를 나타내는 그래프
#막대그래프
my_check_2 %>%
  group_by(cust_id) %>%
  summarise(num=n()) %>%
  ggplot(aes(x=cust_id,y=num))+
  geom_col()
#파이차트
data1 <- my_check_2%>%
  group_by(cust_id)%>%
  summarise(num=n())

ggplot(data1,aes(x="",y=num,fill=cust_id))+
  geom_col(width=1) +
  coord_polar("y")+
  theme_void()

#도넛
data2 = my_check_2 %>%
  group_by(cust_id) %>%
  summarise(num=n())



hsize <- 3

data2 <- data2 %>%
  mutate(x = hsize)

ggplot(data2,aes(x = hsize, y =num, fill = cust_id)) +
  geom_col() + 
  coord_polar(theta="y")+
  xlim(c(0.2,hsize+0.5))
#circular barplot
my_check_2 %>%  group_by(cust_id) %>% summarise(n=n()) %>% 
  ggplot(aes(x = cust_id, y = n, fill = cust_id)) +
  geom_col(color = "white", lwd = 1, show.legend = FALSE) + 
  coord_polar() +  
  geom_label(aes(label = cust_id),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  geom_text(aes(label = n), color="white",
            position = position_stack(vjust = 0.2),
            show.legend = FALSE)
#롤리팝
data <- my_check_2 %>% group_by(cust_id) %>% summarise(num=n())

ggplot(data,aes(x=cust_id,y=num)) +
  geom_point()+
  geom_segment( aes(x=cust_id,xend=cust_id,y=0,yend=num))
#워드클라우드
d <- my_check_2 %>%
  group_by(cust_id) %>%
  summarise(num=n())
str(d)
wordcloud2(data=d,size=1.6)
# 9-2. 각 상품의 주문 횟수를 나타내는 그래프 
#막대그래프
my_check_2 %>%
  group_by(prod_name) %>%
  summarise(num=n()) %>%
  ggplot(aes(x=prod_name,y=num))+
  geom_col()
#파이차트
data1 <- my_check_2%>%
  group_by(prod_name)%>%
  summarise(num=n())

ggplot(data1,aes(x="",y=num,fill=prod_name))+
  geom_col(width=1) +
  coord_polar("y")+
  theme_void()

#도넛
data2 = my_check_2 %>%
  group_by(prod_name) %>%
  summarise(num=n())



hsize <- 3

data3 <- data3 %>%
  mutate(x = hsize)

ggplot(data2,aes(x = hsize, y =num, fill = prod_name)) +
  geom_col() + 
  coord_polar(theta="y")+
  xlim(c(0.2,hsize+0.5))
#circular barplot
my_check_2 %>%  group_by(prod_name) %>% summarise(n=n()) %>% 
  ggplot(aes(x = prod_name, y = n, fill = prod_name)) +
  geom_col(color = "white", lwd = 1, show.legend = FALSE) + 
  coord_polar() +  
  geom_label(aes(label = prod_name),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  geom_text(aes(label = n), color="white",
            position = position_stack(vjust = 0.2),
            show.legend = FALSE)
#롤리팝
data <- my_check_2 %>% group_by(prod_name) %>% summarise(num=n())

ggplot(data,aes(x=prod_name,y=num)) +
  geom_point()+
  geom_segment( aes(x=prod_name,xend=prod_name,y=0,yend=num))
#워드클라우드
d <- my_check_2 %>%
  group_by(prod_name) %>%
  summarise(num=n())
str(d)
wordcloud2(data=d,size=1.6)
# 

# CarSales 데이터를 이용하세요.
CarSales = read_xlsx("C:\\Users\\admin\\Desktop\\Data_viz\\CarSales")
# 9-3. 각 고객(customerName)의 주문 횟수를 나타내는 그래프
#막대그래프
CarSales %>%
  group_by(customerName) %>%
  summarise(num=n()) %>%
  ggplot(aes(x=customerName,y=num))+
  geom_col()
#파이차트
data4 <- CarSales%>%
  group_by(customerName)%>%
  summarise(num=n())

ggplot(data4,aes(x="",y=num,fill=customerName))+
  geom_col(width=1) +
  coord_polar("y")+
  theme_void()

#도넛
data3 = CarSales %>%
  group_by(customerName) %>%
  summarise(num=n())



hsize <- 3

data3 <- data3 %>%
  mutate(x = hsize)

ggplot(data3,aes(x = hsize, y =num, fill = customerName)) +
  geom_col() + 
  coord_polar(theta="y")+
  xlim(c(0.2,hsize+0.5))
#circular barplot
CarSales %>%  group_by(customerName) %>% summarise(n=n()) %>% 
  ggplot(aes(x = customerName, y = n, fill =customerName)) +
  geom_col(color = "white", lwd = 1, show.legend = FALSE) + 
  coord_polar() +  
  geom_label(aes(label = customerName),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  geom_text(aes(label = n), color="white",
            position = position_stack(vjust = 0.2),
            show.legend = FALSE)
#롤리팝
data <- CarSales %>% group_by(customerName) %>% summarise(num=n())

ggplot(data,aes(x=customerName,y=num)) +
  geom_point()+
  geom_segment( aes(x=customerName,xend=customerName,y=0,yend=num))
#워드클라우드
d <- CarSales %>%
  group_by(customerName) %>%
  summarise(num=n())
str(d)
wordcloud2(data=d,size=1.6)
# 9-4. 각 제품(productName)의 주문 횟수를 나타내는 그래프 

#막대그래프
CarSales %>%
  group_by(productName) %>%
  summarise(num=n()) %>%
  ggplot(aes(x=productName,y=num))+
  geom_col()
#파이차트
data5 <- CarSales%>%
  group_by(productName)%>%
  summarise(num=n())

ggplot(data5,aes(x="",y=num,fill=productName))+
  geom_col(width=1) +
  coord_polar("y")+
  theme_void()

#도넛
data6 = CarSales %>%
  group_by(productName) %>%
  summarise(num=n())



hsize <- 3

data7 <- data7 %>%
  mutate(x = hsize)

ggplot(data7,aes(x = hsize, y =num, fill = productName)) +
  geom_col() + 
  coord_polar(theta="y")+
  xlim(c(0.2,hsize+0.5))
#circular barplot
CarSales %>%  group_by(productName) %>% summarise(n=n()) %>% 
  ggplot(aes(x = productName, y = n, fill =productName)) +
  geom_col(color = "white", lwd = 1, show.legend = FALSE) + 
  coord_polar() +  
  geom_label(aes(label = productName),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  geom_text(aes(label = n), color="white",
            position = position_stack(vjust = 0.2),
            show.legend = FALSE)
#롤리팝
data <- CarSales %>% group_by(productName) %>% summarise(num=n())

ggplot(data,aes(x=productName,y=num)) +
  geom_point()+
  geom_segment( aes(x=productName,xend=productName,y=0,yend=num))
#워드클라우드
d <- CarSales %>%
  group_by(productName) %>%
  summarise(num=n())
str(d)
wordcloud2(data=d,size=1.6)

# 9-5. year=substring(CarSales$orderDate,1,4)를 이용하여 
#    년도별 주문횟수를 나타내는 그래프
CarSales %>%
  year=substring(CarSales$orderDate,1,4)
  group_by(year) %>%
  summarise(num=n()) %>%
  ggplot(aes(x=year,y=num))+
  geom_col()
#파이차트
data6 <- CarSales%>%
  group_by(year)%>%
  summarise(num=n())

ggplot(data6,aes(x="",y=num,fill=year))+
  geom_col(width=1) +
  coord_polar("y")+
  theme_void()

#도넛
data8 = CarSales %>%
  group_by(year) %>%
  summarise(num=n())



hsize <- 3

data8 <- data8 %>%
  mutate(x = hsize)

ggplot(data8,aes(x = hsize, y =num, fill = year)) +
  geom_col() + 
  coord_polar(theta="y")+
  xlim(c(0.2,hsize+0.5))
#circular barplot
CarSales %>%  group_by(year) %>% summarise(n=n()) %>% 
  ggplot(aes(x = year, y = n, fill =year)) +
  geom_col(color = "white", lwd = 1, show.legend = FALSE) + 
  coord_polar() +  
  geom_label(aes(label = year),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  geom_text(aes(label = n), color="white",
            position = position_stack(vjust = 0.2),
            show.legend = FALSE)
#롤리팝
data <- CarSales %>% group_by(year) %>% summarise(num=n())

ggplot(data,aes(x=year,y=num)) +
  geom_point()+
  geom_segment( aes(x=year,xend=year,y=0,yend=num))
#워드클라우드
d <- CarSales %>%
  group_by(year) %>%
  summarise(num=n())
str(d)
wordcloud2(data=d,size=1.6)
 

# 9-6. 각 직원(employeeName)이 담당한 주문 횟수를 나타내는 그래프
#막대그래프
CarSales %>%
  group_by(employeeName) %>%
  summarise(num=n()) %>%
  ggplot(aes(x=employeeName,y=num))+
  geom_col()
#파이차트
data10 <- CarSales%>%
  group_by(employeeName)%>%
  summarise(num=n())

ggplot(data10,aes(x="",y=num,fill=employeeName))+
  geom_col(width=1) +
  coord_polar("y")+
  theme_void()

#도넛
data11 = CarSales %>%
  group_by(employeeName) %>%
  summarise(num=n())



hsize <- 3

data11 <- data11 %>%
  mutate(x = hsize)

ggplot(data11,aes(x = hsize, y =num, fill = employeeName)) +
  geom_col() + 
  coord_polar(theta="y")+
  xlim(c(0.2,hsize+0.5))
#circular barplot
CarSales %>%  group_by(employeeName) %>% summarise(n=n()) %>% 
  ggplot(aes(x = employeeName, y = n, fill =employeeName)) +
  geom_col(color = "white", lwd = 1, show.legend = FALSE) + 
  coord_polar() +  
  geom_label(aes(label = employeeName),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  geom_text(aes(label = n), color="white",
            position = position_stack(vjust = 0.2),
            show.legend = FALSE)
#롤리팝
data13 <- CarSales %>% group_by(employeeName) %>% summarise(num=n())

ggplot(data13,aes(x=employeeName,y=num)) +
  geom_point()+
  geom_segment( aes(x=employeeName,xend=employeeName,y=0,yend=num))
#워드클라우드
d <- CarSales %>%
  group_by(employeeName) %>%
  summarise(num=n())
str(d)
wordcloud2(data=d,size=1.6)




