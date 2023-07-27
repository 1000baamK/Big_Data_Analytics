#작년 자동차 경유값 평균 : 1561.45
#작년도 평균 경유값으로 1561.459가 나왔다 이번년도도 비슷한지 해보자
#1 데이터 불러오기
ds <- read.csv("oneSample.csv",
               header = F,
               sep = ",")

View(ds)
str(ds)

#열 이름 추가
colnames(ds) = "경유"

#2 통계량 확인
library(psych)
describe(ds)

#3 그래프그리기(수치형이므로 => 박스, 히스토그램)
par(mfrow=c(1,2))

boxplot(ds$경유,
        ylab="price",
        main="경유에 대한 박스플롯")
hist(ds$경유, breaks=10, col="red",
     xlab="price", ylab="frequency",
     ylim = c(0,80), xlim = c(1400,2200),
     main="경유에 대한 히스토그램")

#4 통계분석
t.test(ds,
       alternative = c("two.sided"),
       mu = 1561.45,
       conf.level = 0.95)

#결론 : 순천 주유소 86곳은 전국 주유소의 합리적인 평균가격보다 낮다.  귀무기각->대립채택

#5 데이터 검증
mu=1561.45
se=16.1
data = rnorm(1000, mu, se)
data <- sort(data)
data

# 검증된 데이터로 그래프그리기
par(mfrow=c(1,1))
plot(data, dnorm(data, mu, se), type='l', 
     main="경유의 평균 가격 검정",
     xlim=c(1500,1850))
abline(v=mu, col="green", lty=5)
abline(v=mu+1.96*se, col="blue", lty=5)
abline(v=mu-1.96*se, col="blue", lty=5)
abline(v=1843.89, col="red", lty=5)


