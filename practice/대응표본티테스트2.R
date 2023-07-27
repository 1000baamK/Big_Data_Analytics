#데이터 불러오기

sh <- read.csv("쇼핑몰구매횟수.csv",
               header = T,
               sep = ",")

str(sh)
View(sh)

#기본 통계치 확인
library(psych)
describe(sh)

sh$point <- c(sh$post - sh$pre)
describe(sh$point)
#그래프
layout(matrix(c(1, 2, 3, 3), 2, 2, byrow=TRUE))
hist(sh$pre, main="광고 이후",col=15,
     ylim=c(0,70), xlab="score",
     density=30, angle=60)
hist(sh$post, main="광고 이후", col=18,
     ylim=c(0,70), xlab="score",
     density=30, angle=60)
boxplot(sh$point, main="변화 점수", col="cyan")

par(mfrow=c(1,1))

#통계분석(대응표본티테스트)
t.test(sh$post, sh$pre,
       alternative = c("two.sided"),
       paired = TRUE,
       conf.level = 0.95)

#통계분석 데이터 그래프
mu = 0
se = 0.16
inter = qt(0.025, df = 29)
data <- rnorm(1000, mu, se)
data <- sort(data)
plot(data, dnorm(data, mu, se),
     main=" ",
     type="l",
     xlim=c(-2,2), xlab=" ", ylab=" ")
abline(v=mu, col="green", lty=5)
abline(v=mu+inter*se, col="blue", lty=5) 
abline(v=mu-inter*se, col="blue", lty=5)
abline(v=0.76, col="red", lty=5)

