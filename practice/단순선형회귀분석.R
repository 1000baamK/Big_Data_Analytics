#데이터 불러오기

es <- read.csv("es.csv",
               header=T,
               na.strings=".")

View(es)

str(es)

table(is.na(es))

attach(es)

#기술통꼐치 확인

library(psych)
describe(es)

pairs.panels(es)


# 02.단순회귀분석

plot(salary ~ exp, data=es)
abline(lm(salary ~ exp, data = es), col = "red", lty = 4)
#회귀분석
esModel <- lm(salary ~ exp, data = es)
#회귀 검정
anova(esModel)

#분석후에 해석이 쉽도록 요약설명해주세요
summary(esModel)

# 03. 회귀분석 가정 검정
# 정규성: Nomal Q-Q
# 선형성: Residuals vs Fitted
# 등분산성: Scale-Location
# 이상치검정 : Residuals vs Leverage(cook's distance) 4/n-k-1


par(mfrow=c(2,2))
plot(esModel)
par(mfrow=c(1,1))


# 잔차의 정규분포 검정 
shapiro.test(esModel$residuals)

#이상치 검정, sd, hat, d 통합검정
library(car)
influencePlot(esModel, id.method="identify")

detach(es)
