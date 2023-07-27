#데이터 불러오기

hp <- read.csv("hp.csv",
               header = T,
               sep = ",")

str(hp)
View(hp)

#기본 통계치 확인
library(psych)
describe(hp[4:5])
describe(hp$point)

#그래프
layout(matrix(c(1, 2, 3, 3), 2, 2, byrow=TRUE))
hist(hp$before, main="재활치료 전 상태점수",col=15,
     ylim=c(0,70), xlab="score",
     density=30, angle=60)
hist(hp$after, main="재활치료 후 상태점수", col=18,
     ylim=c(0,70), xlab="score",
     density=30, angle=60)
boxplot(hp$point, main="변화 점수", col="cyan")

par(mfrow=c(1,1))

#통계분석(대응표본티테스트)
t.test(hp$after, hp$before,
       alternative = c("two.sided"),
       paired = TRUE,
       conf.level = 0.95)

#통계분석 데이터 그래프
mu = 0
se = 0.65
inter = qt(0.025, df = 400)
data <- rnorm(1000, mu, se)
data <- sort(data)
plot(data, dnorm(data, mu, se),
     main=" ",
     type="l",
     xlim=c(-10,10), xlab=" ", ylab=" ")
abline(v=mu, col="green", lty=5)
abline(v=mu+inter*se, col="blue", lty=5) 
abline(v=mu-inter*se, col="blue", lty=5)
abline(v=8.81, col="red", lty=5)
