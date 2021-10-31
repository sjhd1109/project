library(MASS)
library(car)
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
install.packages("tidypredict")
install.packages("stringr")
library(tidypredict)
library(stringr)
library(CRAN)
install.packages("CRAN")
install.packages("prediction")
library(prediction)
install.packages("pROC")
library(pROC)
install.packages("ROCR")
library(ROCR)


#1.로지스틱 (단계적선택, IV2개)
setwd("C:\\Rtest")
multiple <- read.csv("multiple.csv")

str(multiple)

to.factors <- function(df, variables) {
  for (variable in variables) {
    df[[variable]] <- as.factor(df[[variable]])
  }
  return(df)
}

#데이터 변환 변수 선택
categorical.vars <- c('WIN')


# 데이터형 변환
multiple1 <- to.factors(df = multiple, variables = categorical.vars) 
str(multiple1)


#information value 영향을 많이 주는 변수 찾기
library(Information)
install.packages("Information")


nba_iv_df <- multiple1 %>%
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

#trian 모델 (trian, test)

index_train <- createDataPartition(multiple1$WIN,p=0.6,list = FALSE)
train_df <- multiple1[index_train, ]
test_df  <- multiple1[-index_train, ]

#모델(iv제거) train 나누고 해야하는데..

glm <- glm(WIN~., family = "binomial", data = multiple1)
vif(glm)
model_glm1 <-update(glm, .~. -PTS)
vif(model_glm1)
model_glm2 <-update(model_glm1, .~. -X3PM)
vif(model_glm2)
model_glm3 <-update(model_glm2, .~. -FGA)
vif(model_glm3)
model_glm4 <-update(model_glm3, .~. -FTA)
vif(model_glm4)   #10이상 값없음, 5미만값

#모델(IV값으로 상위 7개)
nba_glm <- glm(WIN~+BLKA+BLK+DREB+STL+AST+FGM+PFD, family = "binomial", data=train_df)
nba_glm
summary(nba_glm)
vif(nba_glm)

#모델(단계적변수선택)
nba_step_glm <- glm(WIN~.-PTS-X3PM-FGA-FTA, family="binomial", train_df)
nbabest_step_glm <- step(nba_step_glm, direction = 'both')
summary(nbabest_step_glm)
vif(nbabest_step_glm)

#예측모형성능
library(rms)
lrm(nba_glm)
#iv CASE 1 0.68?......
test_df1 <- test_df %>%
  tidypredict_to_column(nba_glm) %>%
  mutate(pred_WIN = ifelse(fit>0.5, "1", "0")%>% as.factor)
confusionMatrix(table(test_df1$WIN, test_df1$pred_WIN))
with(nba_glm, pchisq(null.deviance - deviance, 
                     +df.null - df.residual, lower.tail = FALSE)) #카이제곱검정

p <- predict(nba_glm, newdata = test_df1, type = "response")
pr <- prediction(p, test_df1$WIN)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
auc <- performance(pr, measure="auc")
auc
plot(prf)
lines(x=c(0,1),y=c(0,1),col="red")

#단계적선택

test_df1 <- test_df %>%
  tidypredict_to_column(nbabest_step_glm) %>%
  mutate(pred_WIN = ifelse(fit>0.5, "1", "0")%>% as.factor)
confusionMatrix(table(test_df1$WIN, test_df1$pred_WIN))
with(nbabest_step_glm, pchisq(null.deviance - deviance, 
                              +df.null - df.residual, lower.tail = FALSE)) #카이제곱검정

p1 <- predict(nbabest_step_glm, newdata = test_df1, type = "response")
pr1 <- prediction(p1, test_df1$WIN)
prf1 <- performance(pr1, measure = "tpr", x.measure = "fpr")
auc1 <- performance(pr1, measure="auc")
plot(prf1)
lines(x=c(0,1),y=c(0,1),col="red")
