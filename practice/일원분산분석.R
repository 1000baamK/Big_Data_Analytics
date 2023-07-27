#데이터 불러오기
sns <- read.csv("4sns.csv",
                header=T,
                na.strings=".")

View(sns)
str(sns)
sns$group <- factor(sns$group)

str(sns)

attach(sns)
detach(sns)
#기술통계치
library(psych)
describeBy(score,group, mat=T)

table(is.na(sns)) # 결측치 확인
sns <- na.omit(sns) #결측치 제거

str(sns)


##ggplot
library(ggplot2)

ggplot(sns, aes(x=score, fill=group, color=group))+
  geom_density(position="identity", alpha=0.1, size=1)+
  ggtitle(" ")+
  theme(title = element_text(color="darkblue", size=15))

ggplot(sns, aes(x=score, fill=group))+
  geom_histogram(binwidth=1, color="black")+
  facet_grid( ~ group)+
  theme_bw()+
  ggtitle(" ")+
  theme(title = element_text(color="darkblue", size=15))+
  coord_cartesian(xlim=c(0,5))+
  theme(legend.position = "none")

ggplot(sns, aes(x=score, y=group, fill=group))+
  geom_boxplot()+
  ggtitle(" ")+
  theme(title = element_text(color="darkblue", size=15))+
  theme(legend.position = "none")
  
#등분산 검정()
bartlett.test(score ~ group, sns)

#leveneTest
install.packages("car")
library(car)
leveneTest(score,group)


#아노바분석
snsResult <- aov(score ~ group, sns)
str(snsResult)
View(snsResult)
summary(snsResult)

TukeyHSD(snsResult)

#install.packages("agricolae")
library(agricolae)

# 사후 검정
# group=TRUE: 그룹으로 묶어서 표시, FALSE: 1:1로 비교
# console=TRUE: 결과를 화면에 표시
duncan.test(snsResult, "group", group=T, console = TRUE)
scheffe.test(snsResult, "group", group=F, console = TRUE)


# 05.통계결과 그래프
par(mfrow=c(2,1))
tukeyPlot <- TukeyHSD(snsResult) # 그룹간 차이 비교
plot(tukeyPlot)
duncanPlot <- duncan.test(snsResult, "group")
plot(duncanPlot, ylim=c(0,6))


par(mfrow=c(1,1))
par(new=F)
x=3.48 # 정규분포로 표시(facebook)
se=0.16
data <-rnorm(1000, x, se)
data <- sort(data)
plot(data, dnorm(data, x, se), col="red",type='l', 
     main="SNS 평균 만족도", 
     xlim=c(2, 5), ylim=c(0,3), xlab=" ", ylab=" ")
abline(v=x, col="red", lty=3)

par(new=T) # 그래프를 겹쳐서 표현하기

x=4.02 # 정규분포로 표시(instagram)
se=0.14
data <-rnorm(1000, x, se)
data <- sort(data)
plot(data, dnorm(data, x, se), type='l', col="green",
     xlim=c(2, 5), ylim=c(0,3), xlab=" ", ylab=" ")
abline(v=x, col="green", lty=3)

par(new=T)
x=3.7 # 정규분포로 표시(tiktok)
se=0.16
data <-rnorm(1000, x, se)
data <- sort(data)
plot(data, dnorm(data, x, se), type='l', col="blue",
     xlim=c(2, 5), ylim=c(0,3), xlab=" ", ylab=" ")
abline(v=x, col="blue", lty=3)

par(new=T)
x=3.3 # 정규분포로 표시(twitter)
se=0.17
data <-rnorm(1000, x, se)
data <- sort(data)
plot(data, dnorm(data, x, se), type='l', col="purple",
     xlim=c(2, 5), ylim=c(0,3), xlab=" ", ylab=" ")
abline(v=x, col="purple", lty=3)
legend("topright", legend = c("페이스북","인스타그램","틱톡", "트위터"),
       fill = c("red","green","blue","purple"))



detach(sns)

