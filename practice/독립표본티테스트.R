#1 데이터 불러오기
gs80 <- read.csv("80.csv",
                 header = F,
                 sep = ",")

View(gs80)
str(gs80)

gs80$V1 <- factor(gs80$V1)

str(gs80)

colnames(gs80) <- paste(c("array","price"))
attach(gs80)


#2 기술통계량 확인
tapply(price, array, summary)

library(psych)
describeBy(price, array, mat=T)

#3 그래프그리기
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
boxplot(price ~ array, col=c(3,7), main="경유값에 대한 박스플롯")
hist(price[array=="GwangJu"], col=3, main="광주 경유값", xlab="price", xlim=c(1800,2100), ylim=c(0,30), density=30, angle=60)
hist(price[array=="SunCheon"], col=7, main="순천 경유값", xlab="price", xlim=c(1800,2100),ylim=c(0,30), density=30, angle=60)
gs80
#4 통계 분석

var.test(price ~ array, data = gs80)

t.test(price ~ array,
       data = gs80,
       alternative = c("two.sided"),
       var.equal = TRUE,
       conf.level = 0.95)


detach(gs80)

par(mfrow=c(1,1))

# 통계 결과 그래프
x = 1923.75
se = 3.77
data <- rnorm(1000, x, se)
data <- sort(data)
plot(data, dnorm(data, x, se), type="l", col=3, main="경유값 정규분포표", xlab=" ", ylab=" ",
     xlim=c(1900, 1950), ylim=c(0, 0.12))
abline(v=x, col=3, lty=3)


par(new=TRUE)

x = 1922.11
se = 4.27
data <- rnorm(1000, x, se)
data <- sort(data)
plot(data, dnorm(data, x, se), type="l", col=7, xlab=" ", ylab=" ",
     xlim=c(1900, 1950), ylim=c(0, 0.12))
abline(v=x, col=7, lty=3)

