#### 로지스틱 회귀분석(Regression) #####

# 01.데이터 불러오기 
am <- read.csv("army2.csv", 
                     header=TRUE, 
                     na.strings = "."
)

#데이터보기
View(am)

#데이터 속성 보기기
str(am)

#데이터 전처리
table(is.na(am))

#객체 연결(간편한 조작을 위해)
attach(am)

# 02.기본통계치 확인: describe(psych패키지 이용)
library(psych)
describe(am)
#상관분석 돌리기
pairs.panels(am)

# 03.로지스틱 회귀분석
amModel <- glm(weight ~ chest.size+height+waist+head+foot.length, data=am)
amModel
#회귀분석의 결정 -> 분산분석(아노바)
anova(amModel)
summary(amModel)

#오즈비 만드는 공식
odd <- exp(cbind(OR = coef(amModel), confint(amModel)))
round(odd, 3)

detach(am)