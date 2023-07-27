#1 데이터 불러오기
stf <- read.csv("satisfaction.csv",
                 header = T,
                 sep = ",")

str(stf)

attach(stf)


#2 기술통계량 확인
# tapply(satisfaction, gender, summary)

library(psych)
describeBy(satisfaction, gender, mat=T)

#3 그래프그리기
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
boxplot(satisfaction ~ gender, col=c(3,7), main="만족도에 대한 박스플롯")
hist(satisfaction[gender=="male"], col=3, main="남성 만족도", xlab="satisfaction", xlim=c(40, 80), ylim=c(0,15), density=30, angle=60)
hist(satisfaction[gender=="female"], col=7, main="여성 만족도", xlab="satisfaction", xlim=c(40, 80),ylim=c(0,15), density=30, angle=60)

#4 통계 분석

var.test(satisfaction ~ gender, data = stf)

t.test(satisfaction ~ gender,
       data = stf,
       alternative = c("two.sided"),
       var.equal = TRUE,
       conf.level = 0.95)


detach(stf)

par(mfrow=c(1,1))

# 통계 결과 그래프
x = 64.84
se = 0.58
data <- rnorm(1000, x, se)
data <- sort(data)
plot(data, dnorm(data, x, se), type="l", col=3, main="만족도 정규분포표", xlab=" ", ylab=" ",
     xlim=c(50, 80), ylim=c(0,0.8))
abline(v=x, col=3, lty=3)


par(new=TRUE)

x = 65.84
se = 0.66
data <- rnorm(1000, x, se)
data <- sort(data)
plot(data, dnorm(data, x, se), type="l", col=7, xlab=" ", ylab=" ",
     xlim=c(50, 80), ylim=c(0,0.8))
abline(v=x, col=7, lty=3)

