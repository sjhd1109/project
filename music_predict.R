setwd("C:\\Rtest")

# Import data
spotify <- read.csv("spotify_final(6398).csv")
spotify

# check missing value
anyNA(spotify)

# inspect data
library(dplyr)
glimpse(spotify)

names(spotify)
colnames(spotify)[1]<-"track"
names(spotify)

spotify_clean <- spotify[!duplicated(spotify[, c('track','artist')]), ] %>%
  mutate(target = as.factor(target)) %>% 
  select(-uri) %>% 
  select(18,3:17) %>% 
  mutate(target = recode(target, "1" = "Hit",
                         "0" = "Flop"))
table(is.na(spotify))

table(is.na(spotify_clean))

spotify_clean<-na.omit(spotify_clean)
table(is.na(spotify_clean))
anyNA(spotify_clean)
table(spotify_clean$target)

# data cleaning and manipulation
##density
library(tidyr)
library(ggplot2)
library(digest)
spotify_clean %>% 
  select(target, names(.)[c(2:16)]) %>% 
  pivot_longer(2:16) %>% 
  ggplot(aes(x = value)) + 
  geom_density(aes(color = target)) +
  facet_wrap(~name, ncol = 3, scales="free") +
  labs(title = "Audio Features - Hit or Flop", x = "", y = "density")+      theme_minimal() + theme(axis.text.y = element_blank())

##corrlation matrix
library(GGally)
ggcorr(NULL, cor_matrix = cor(spotify_clean[,c(2:16)]),
       label = T, geom = "blank", hjust = 0.90) +
  geom_point(size = 10, aes(color = coefficient > 0, 
                            alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)

# dropping unncessary variables
spotify_clean_model <- spotify_clean %>% 
  select(-loudness, -sections, -instrumentalness)

#Classification Model
#Cross Balidation

RNGkind(sample.kind = "Rounding")
set.seed(100)

idx <- sample(nrow(spotify_clean_model), nrow(spotify_clean_model)*0.8)
spotify_train<- spotify_clean_model[idx,]
spotify_test <- spotify_clean_model[-idx,]

library(recipes)
spotify_recipe <- recipe(target~., spotify_train) %>% 
  step_scale(all_numeric()) %>%
  prep()

spotify_train <- juice(spotify_recipe)
spotify_test <- bake(spotify_recipe, spotify_test) 

prop.table(table(spotify_train$target))

prop.table(table(spotify_test$target))

# model building
library(rpart)
spotify_dt <- rpart(target ~ ., spotify_train)
#plot
library(rattle)
fancyRpartPlot(spotify_dt, sub = NULL)
#plot
plot(spotify_dt);text(spotify_dt)

library(tree)
library(partykit)
library(rpart.plot)
library(klaR)
as.party(spotify_dt)
plot(as.party(spotify_dt))
prp(spotify_dt)
summary(spotify_dt)

library(randomForest)
library(xgboost)
library(Matrix)
library(partykit)        
library(gbm)                     # boosting
library(lightgbm)               # boosting
library(plot3D)

source("pred_2Dplot.r")

tmp.raprt.result<-rpart(target~danceability+energy+key+mode+speechiness+acousticness+liveness+valence+tempo+duration_ms+time_signature+chorus_hit, data=spotify_train)
pred_2Dplot(tmp.raprt.result)

###end


# model fitting
pred_dt <- predict(spotify_dt, newdata = spotify_test, type = "class")
prob_dt <- predict(spotify_dt, newdata = spotify_test, type = "prob")

# result
library(dplyr)
detach("package:MASS", unload=TRUE) 
remove.packages("dplyr")
install.packages("dplyr")

spotify_dt_table <- select(spotify_test,target) %>%
  bind_cols(target_pred = pred_dt) %>% 
  bind_cols(target_eprob = round(prob_dt[,1],4)) %>% 
  bind_cols(target_pprob = round(prob_dt[,2],4))


# performance evaluation - confusion matrix
library(yardstick)
library(purrr)
library(tidyr)
spotify_dt_table%>% 
  conf_mat(target, target_pred) %>% 
  autoplot(type = "heatmap")

spotify_dt_table %>%
  summarise(
    accuracy = accuracy_vec(target, target_pred),
    sensitivity = sens_vec(target, target_pred),
    specificity = spec_vec(target, target_pred),
    precision = precision_vec(target, target_pred))


# ROC
spotify_dt_roc <- data.frame(prediction=round(prob_dt[,1],4),
                             trueclass=as.numeric(spotify_dt_table$target=="Flop"))
spotify_dt_roc


spotify_dt_roc <- ROCR::prediction(spotify_dt_roc$prediction,
                                   spotify_dt_roc$trueclass) 
library(ROCR)
auc_ROCR_dt <- performance(spotify_dt_roc, measure = "auc")
auc_ROCR_dt <- auc_ROCR_dt@y.values[[1]]
final_dt <- spotify_dt_table %>%
  summarise(accuracy = accuracy_vec(target, target_pred),
            sensitivity = sens_vec(target, target_pred),
            specificity = spec_vec(target, target_pred),
            precision = precision_vec(target, target_pred)) %>% 
  cbind(AUC = auc_ROCR_dt)

plot(performance(spotify_dt_roc, "tpr", "fpr"),
     main = "ROC")
abline(a = 0, b = 1)


# model building
RNGkind(sample.kind = "Rounding")
set.seed(417)
library(caret)
ctrl <- trainControl(method="repeatedcv", number=4, repeats=4) # k-fold cross validation
forest <- train(target ~ ., data=spotify_train, method="rf", trControl = ctrl)
forest

forest1 <- randomForest(target ~ ., ntree = 100, importance = TRUE, data = spotify_train)
forest1

varImp(forest)
plot(forest1)
head(forest1$err.rate)
importance(forest1)
varImpPlot(forest1)
forest$finalModel

# model fitting
pred_forest <- predict(forest, newdata = spotify_test, type = "raw")
prob_forest <- predict(forest, newdata = spotify_test, type = "prob")

# result
spotify_forest_table <- select(spotify_test, target) %>%
  bind_cols(target_pred = pred_forest) %>% 
  bind_cols(target_eprob = round(prob_forest[,1],4)) %>% 
  bind_cols(target_pprob = round(prob_forest[,2],4))

# performance evaluation - confusion matrix
spotify_forest_table %>% 
  conf_mat(target, target_pred) %>% 
  autoplot(type = "heatmap")

spotify_forest_table %>%
  summarise(
    accuracy = accuracy_vec(target, target_pred),
    sensitivity = sens_vec(target, target_pred),
    specificity = spec_vec(target, target_pred),
    precision = precision_vec(target, target_pred))

# ROC
spotify_forest_roc <- data.frame(prediction=round(prob_forest[,1],4),
                                 trueclass=as.numeric(spotify_forest_table$target=="Flop"))
spotify_forest_roc


spotify_forest_roc <- ROCR::prediction(spotify_forest_roc$prediction,
                                       spotify_forest_roc$trueclass) 

# ROC curve
plot(performance(spotify_forest_roc, "tpr", "fpr"),main = "ROC")
abline(a = 0, b = 1)

# AUC
auc_ROCR_f <- performance(spotify_forest_roc, measure = "auc")
auc_ROCR_f <- auc_ROCR_f@y.values[[1]]
final_f <- spotify_forest_table %>%
  summarise(
    accuracy = accuracy_vec(target, target_pred),
    sensitivity = sens_vec(target, target_pred),
    specificity = spec_vec(target, target_pred),
    precision = precision_vec(target, target_pred)) %>% 
  cbind(AUC = auc_ROCR_f)

rbind("Decision Tree" = final_dt, "Random Forest" = final_f)

#boost


y = spotify_train[,13]
y1 = spotify_test[,13]
y_train <- ifelse(y$target == "Hit",1,0)
y_test <- ifelse(y1$target == "Hit",1,0)


x_train      =  model.matrix(~.+0, data=spotify_train[,-13])
x_test       =  data.frame(model.matrix(~.+0, data=spotify_test[,-13]))




bst <- xgboost(    data          = x_train, 
                   label         = y_train,
                   nrounds       = 100,
                   num_class     = 2
)
prd0 <- predict( bst,  as.matrix(x_test) )
table(y_test, prd0)


install.packages("e1071")
library(e1071)
confusionMatrix(f_y_test, f_prd0)

f_prd0 <- as.factor(prd0)
f_y_test <- as.factor(y_test)
