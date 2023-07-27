# 데이터 불러오기
army <- read.csv("army2.csv",
                 header=T,
                 na.strings=".")

View(army)

str(army)

# 데이터 결측치 확인
table(is.na(army))


attach(army)

# 02.기본통계치 확인: describe(psych패키지 이용)
library(psych)
describe(army)

# 상관관계 그래프
pairs.panels(army)

# 03.다중 회귀분석
# 전체변수 일괄입력
# backword: 변수제거
# forward: 변수추가
# stepwise: backward와 forwoard 동시
# AIC (Akaike information criterion), BIC (Bayesian ...)

# 전체변수 일괄입력
library(car)
armyModel <- lm(weight ~ .,data=army)
anova(armyModel)
summary(armyModel)
# 다중공선성
vif(armyModel) # 10이상이면 문제 발생

# 표준화 회귀계수
# install.packages("lm.beta")
library(lm.beta)
lmBeta <- lm.beta(armyModel)
summary(lmBeta)

# backword: 변수제거
##armyModel1 <- lm(weight ~ ., army)##id 제거를 위해서 만든명령줄
summary(armyModel1) 
armyModel1 <- lm(weight ~ .-id, army)
View(armyModel1)
summary(armyModel1)
armyModel1 <- step(armyModel, 
                   direction = "backward", 
                   trace = T)

# forword: 변수제거
armyModel2 <- lm(weight ~1, army)
summary(armyModel2) 
armyModel2 <- step(armyModel2, direction = "forward", 
                   scope =(weight ~ design+info+comm+op+fb),
                   trace = T)

# 04. 회귀분석 가정 검정
# 정규성: Nomal Q-Q
# 선형성: Residuals vs Fitted
# 등분산성: Scale-Location
# 이상치검정 : Residuals vs Leverage(cook's distance) 4/n-k-1
# 2sd 이상(이하)일 경우, 

opar <- par(no.readonly = TRUE)
par(mfrow=c(2,2))
plot(armyModel)
par(opar)

# 잔차의 정규분포 검정 
shapiro.test(armyModel$residuals)

## 학사과정에서는 여기서 끝~~


#이상치 검정, sd, hat, d 통합검정
library(car)
influencePlot(armyModel, id.method="identify")

detach(army)


# 부록: 모든 경우의 수 고려
install.packages("leaps")
library(leaps)

leap <- regsubsets(weight ~ ., army, nbest = 5)   # size당 5개의 최적 모형 저장
summary(leap)
plot(leap)

