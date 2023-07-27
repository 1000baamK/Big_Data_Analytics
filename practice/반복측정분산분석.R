# 데이터 불러오기

gd <- read.csv("mathgrade.csv",
               header=T,
               na.strings=".")

View(gd)
str(gd)

gd$time <- factor(gd$time,
                  levels=c(1:3),
                  labels=c("전", "3달", "6달"))

str(gd)

#기본 통계치 확인
library(psych)
attach(gd)
describeBy(score, time, mat=T)

#박스 플롯 그래프
library(ggplot2)
ggplot(gd, aes(x=time, y=score, fill=time))+
  geom_boxplot()+
  theme_bw()+
  ggtitle("<성적>")+
  theme(title=element_text(color="black", size=15))


# 04.통계분석
# 구형성(sphericity)검정: Mauchly’s test. 

library(car)
gdMatrix <- cbind(score[time=="전"], score[time=="3달"], score[time=="6달"])
gdModelLm <- lm(gdMatrix ~ 1)
timeF <- factor(c("전","3달","6달"))
options(contrasts=c("contr.sum", "contr.poly"))
gdResultMt <- Anova(gdModelLm, idata=data.frame(timeF), #Anova 대문자
                     idesign=~timeF, type="III")
summary(gdResultMt, multivariate=F)

# ANOVA 분석
gdResult <- aov(score ~ time+Error(id/time), data=gd)
summary(gdResult)

# 사후검정(Multicamparison test) - t-value 포함
# install.packages("multcomp")
library(multcomp)
resultLm <- lm(score ~ time)
TukeyResult <- glht(resultLm, linfct=mcp(time='Tukey'))
summary(TukeyResult)

# 통계결과 그래프
plot(TukeyResult, xlim=c(0,20))

detach(gd)

# 부록: 사후검정(기본함수사용): Tukey HSD
TukeyResult <- aov(score ~ time, data=gd)
TukeyHSD(TukeyResult)
plot(TukeyHSD(TukeyResult))

c(1,2,3,4)+c(5,6,7,8,9)
