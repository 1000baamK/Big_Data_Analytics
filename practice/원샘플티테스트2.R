#1 데이터 불러오기
sl <- read.csv("seoul.csv",
               header = F,
               sep = ",")

View(sl)
str(sl)

#열 이름 추가
colnames(sl) = "평가"

#2 통계량 확인
library(psych)
describe(sl)

#3 그래프그리기
par(mfrow=c(1,2))


boxplot(sl$평가,
        ylab="price",
        main="평가에 대한 박스플롯")
hist(sl$평가, breaks=10, col="red",
     xlab="price", ylab="frequency",
     ylim = c(0, 100), xlim = c(0,100),
     main="평가에 대한 히스토그램")

#4 통계분석
t.test(sl$평가, 
       alternative = c("two.sided"),
       mu = 86,
       conf.level = 0.95)

# 귀무기각->대립채택

#5 데이터 검증
mu=78.67
se=0.37
data = rnorm(1000, mu, se)
data <- sort(data)
data

# 검증된 데이터로 그래프그리기
par(mfrow=c(1,1))
plot(data, dnorm(data, mu, se), type='l', 
     main="평가의 평균 가격 검정",
     xlim=c(70,90))
abline(v=mu, col="green", lty=5)
abline(v=mu+1.96*se, col="blue", lty=5)
abline(v=mu-1.96*se, col="blue", lty=5)
abline(v=86, col="red", lty=5)


