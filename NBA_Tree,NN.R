# 패키지 로드
install.packages("gridExtra")
install.packages("pastecs")
install.packages("gmodels")
install.packages("janitor")
install.packages("skimr")
library(gridExtra) # 그리드 레이아웃
library(pastecs) # 자세한 요약 통계
library(ggplot2) # 시각화
library(gmodels) # 분할표 만들기
library(caret) # 특징 선택 알고리즘
library(randomForest) # 랜덤 포레스트 알고리즘
install.packages("e1071")
install.packages("randomForest")
library(e1071) #혼동행렬
library(tidyverse)
library(readxl)
library(janitor)
library(dplyr)
library(skimr)
library(ggridges)
library(cowplot)
library(caret)
library(car)
install.packages("ggridges")
install.packages("cowplot")
library(stats)
library(reshape)
install.packages("prediction")
library(prediction)

nba.df<- read.table(choose.files(), header=T)
class(nba.df)
head(nba.df)
summary(nba.df)
str(nba.df)

# NA 값을 가진 데이터 프레임이 있는지 확인
sum(is.na(nba.df))
# NA 값을 삭제했을 때 총 레코드가 줄었는지 확인
sum(complete.cases(nba.df))

# 데이터 변환
to.factors <- function(df, variables) {
  for (variable in variables) {
    df[[variable]] <- as.factor(df[[variable]])
  }
  return(df)
}

#데이터 변환 변수 선택
categorical.vars <- c('WIN')


# 데이터형 변환
nba.df <- to.factors(df = nba.df, variables = categorical.vars) 
str(nba.df)

nba.df <- nba.df %>%
  clean_names()



#information value 영향을 많이 주는 변수 찾기
library(Information)
install.packages("Information")


nba_iv_df <- nba.df %>%
  mutate(win=as.integer(win)-1)

nba_iv <- create_infotables(data=nba_iv_df, y="win", bins=10, parallel=TRUE)
nba_iv$Summary %>%
  mutate(Variable = fct_reorder(Variable, IV)) %>% 
  ggplot(aes(x=Variable, y=IV))+
  geom_col()+
  coord_flip()

nba_iv
#top 6개만
nba_iv$Summary %>% 
  top_n(6, wt=IV)

#로지스틱1 (VIF제거), 예측모형 생성
index_train <- createDataPartition(nba.df$win,p=0.6,list = FALSE)
train_df <- nba.df[index_train, ]
test_df  <- nba.df[-index_train, ]

str(train_df)
str(test_df)
#iv 0.8짜리 제거한 모형

nba_glm <- glm(win~ .-blka-FGM-X3PA-FTM-FTA-OREB-DREB-AST-TOV-STL, family = "binomial", data = train_df)
summary(nba_glm)
car::vif(nba_glm)

#변수선택(단계적선택)
nba_glm_step <- step(nbanomal_glm, direction="backward")


#예측모형성능
install.packages("tidypredict")
install.packages("stringr")
library(tidypredict)
library(stringr)


#iv0.2제거(VIF만) CASE 1
test_df1 <- test_df %>%
  tidypredict_to_column(credit_glm) %>%
  mutate(pred_credit.rating = ifelse(fit>0.5, "1", "0")%>% as.factor)
confusionMatrix(table(test_df1$credit.rating, test_df1$pred_credit.rating))
with(credit_glm, pchisq(null.deviance - deviance, 
                        +df.null - df.residual, lower.tail = FALSE)) #카이제곱검정

p <- predict(credit_glm, newdata = test_df1, type = "response")
pr <- prediction(p, test_df1$credit.rating)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
auc <- performance(pr, measure="auc")
auc
plot(prf)
lines(x=c(0,1),y=c(0,1),col="red")






#######iv 0.2 제거(단계적선택 CASE2)
fit.all2 = glm(credit.rating~. - bank.credits-occupation-telephone-residence.duration-dependents,family = binomial,data=train_df) #로지스틱 회귀 분석
#로지스틱 회귀 분석
fit.all3 = glm(credit.rating~. ,family = binomial,data=train_df) #로지스틱 회귀 분

fit.step2 = step(fit.all2, direction='forward')
fit.step22 = step(fit.all2, direction='backward')
fit.step3 = step(fit.all3, direction='backward')
fit.step33 = step(fit.all3, direction='forward')
fit.step3$anova
summary(fit.step22)

test_df2 <- test_df %>%
  tidypredict_to_column(fit.step3) %>%
  mutate(pred_credit.rating = ifelse(fit>0.5, "1", "0")%>% as.factor)
confusionMatrix(table(test_df2$credit.rating, test_df2$pred_credit.rating))

p2 <- predict(fit.step3, newdata = test_df2, type = "response")
pr2 <- prediction(p2, test_df2$credit.rating)
prf2 <- performance(pr2, measure = "tpr", x.measure = "fpr")
plot(prf2)
auc2 <- performance(pr2, measure="auc")
auc2
lines(x=c(0,1),y=c(0,1),col="red")

with(fit.step22, pchisq(null.deviance - deviance, 
                        +df.null - df.residual, lower.tail = FALSE)) #카이제곱검정


#오즈비구하기
exp(coef(fit.step2))
anova(fit.all2, test="Chisq")



#우도함수를 이용한 회귀계수 구간추정
confint(fit.step)
#표준오차를 이용한 회귀계수 구간추정
confint.default(fit.step)


#적합도 검정
#로지스틱 모형 적합도 검정 : 카이제곱검정
with(fit.step2, null.deviance - deviance) #영모형 이탈도에 대한 제안된 모형 이탈도의감소
with(fit.step2, df.null - df.residual) #자유도 계산
with(fit.step2, pchisq(null.deviance - deviance, 
                       +df.null - df.residual, lower.tail = FALSE)) #카이제곱검정

#예측모형활용
cr_dt1<- read.table(choose.files(), header=T)

to.factors <- function(df, variables) {
  for (variable in variables) {
    df[[variable]] <- as.factor(df[[variable]])
  }
  return(df)
}

categorical.vars <- c('customer.id','credit.rating', 'account.balance', 'previous.credit.payment.status',
                      'credit.purpose', 'savings', 'employment.duration', 'installment.rate',
                      'marital.status', 'guarantor', 'residence.duration', 'current.assets',
                      'other.credits', 'apartment.type', 'bank.credits', 'occupation',
                      'dependents', 'telephone', 'foreign.worker')
cr_dt1 <- to.factors(df = cr_dt1, variables = categorical.vars)

str(cr_dt1)
index_train <- createDataPartition(cr_dt$credit.rating, p = 0.7, list = FALSE)
train_df1 <- cr_dt1[index_train, ]
test_df1  <- cr_dt1[-index_train, ]
credit_glm1 <- glm(credit.rating ~ ., family = "binomial", data = train_df1)
test_df1 <- test_df1 %>%
  tidypredict_to_column(credit_glm1) %>% 
  mutate(pred_credit.rating = ifelse(fit > 0.5, "Yes", "No") %>% as.factor)

total <- merge(test_df3$fit, test_df3$credit.rating, key='customer.id' ) 

total