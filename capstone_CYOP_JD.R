#loading the needed libraries for performing desired computations

library(dslabs)
library(tidyverse)
library(caret)
library(randomForest)
library(MASS)


#SOCCER############################################################################################

# loading the data from the harddrive
data<-read.csv("C:\\Users\\duschmaj\\Desktop\\datascience\\capstone_final\\matches.csv")

#select relevant predictors
data <- data %>% filter(game_status=="FT") %>% summarize(
  home_score, 
  away_score, 
  home_possessionPct, 
  home_shotsSummary, 
  away_shotsSummary, 
  home_wonCorners, 
  away_wonCorners, 
  home_saves, 
  away_saves)

#make possession numeric / split shots and shots on target
data <- data %>% mutate(home_possessionPct=as.numeric(str_replace(data$home_possessionPct, "[%]", ""))) 

data <- data %>% 
  mutate(home_shots=as.numeric(str_extract(home_shotsSummary,"[0-9]*"))) %>% 
  mutate(away_shots=as.numeric(str_extract(away_shotsSummary,"[0-9]*"))) %>% 
  mutate(home_shots_ontarget=str_remove(home_shotsSummary,"^[0-9]*\\s\\(")) %>%
  mutate(home_shots_ontarget=as.numeric(str_remove(home_shots_ontarget, "\\)"))) %>% 
  mutate(away_shots_ontarget=str_remove(away_shotsSummary,"^[0-9]*\\s\\(")) %>%
  mutate(away_shots_ontarget=as.numeric(str_remove(away_shots_ontarget, "\\)"))) 
data <- data[,-which(names(data) %in% c("home_shotsSummary", "away_shotsSummary"))]


# decide if the hometeam (h) or the awayteam (a) won, or if there was a draw (d)
data <- data %>% 
  mutate(winner=ifelse(home_score>away_score, "h", ifelse(home_score==away_score, "d", "a"))) %>% 
  mutate(winner=as.factor(winner))

# Remove scores as they should not be used as predictors
data <- data[,-which(names(data) %in% c("home_score", "away_score"))]

#remove 0% possessionPct because this is indication that there is missing data.
data <- data %>% filter(home_possessionPct !=0) 

#remove rows with Na's
data <- na.omit(data)

# Creating a validation set for final model validation
set.seed(1) 
test_index <- createDataPartition(y = data$winner, times = 1, p = 0.1, list = FALSE)
soccer <- data[-test_index,]
soccer_validation <- data[test_index,]

# DATA ANALYSIS

summary(soccer)

soccer %>% ggplot(aes(winner)) + geom_bar()

soccer %>% ggplot(aes(home_shots, home_shots_ontarget, col=winner)) + geom_point()
soccer %>% ggplot(aes(away_shots, away_shots_ontarget, col=winner)) + geom_point()

soccer %>% ggplot(aes(winner, home_possessionPct)) + geom_boxplot()
soccer %>% ggplot(aes(winner, home_shots)) + geom_boxplot()
soccer %>% ggplot(aes(winner, home_shots_ontarget)) + geom_boxplot()
soccer %>% ggplot(aes(winner, home_wonCorners)) + geom_boxplot()
soccer %>% ggplot(aes(winner, home_saves)) + geom_boxplot()

soccer %>% ggplot(aes(winner, away_shots)) + geom_boxplot()
soccer %>% ggplot(aes(winner, away_shots_ontarget)) + geom_boxplot()
soccer %>% ggplot(aes(winner, away_wonCorners)) + geom_boxplot()
soccer %>% ggplot(aes(winner, away_saves)) + geom_boxplot()


# Creating train and test set for model creation
set.seed(13041984)
test_index <- createDataPartition(y = soccer$winner, times = 1, p = 0.1, list = FALSE)
train <- soccer[-test_index,]
test <- soccer[test_index,]

table(test$winner)

#MODEL BUILDING
# guess
random_result <- test %>% mutate(pred=sample(c("h","a","d"), nrow(test), replace=TRUE)) %>% summarize(accuracy=mean(winner==pred))

results <- data.frame(what="Soccer: random guessing", accuracy=random_result)
results

# all home wins
all_h <- test %>% mutate(pred=replicate(nrow(test), "h")) %>% summarize(accuracy=mean(winner==pred))
results <- bind_rows(results, data.frame(what="Soccer: All home wins", accuracs=all_h))
results

# knn
fit_knn <- train(winner ~ .,  method = "knn",  
                 tuneGrid = data.frame(k = seq(5, 70, 5)), data = train)

ggplot(fit_knn, highlight=TRUE)

knn_result <- test %>% mutate(pred=predict(fit_knn, test)) %>% summarize(accuracy=mean(winner==pred))
results<- bind_rows(results, data.frame(what="Soccer: KNN", accuracy=knn_result))
results

confusionMatrix(predict(fit_knn, test), test$winner)[2]

#QDA
train %>% ggplot(aes(sample=home_shots)) + geom_qq() + geom_qq_line()

fit_qda <- train(winner ~ ., method = "qda", data = train)

qda_result <- test %>% mutate(pred=predict(fit_qda, test)) %>% summarize(accuracy=mean(winner==pred))
results <- bind_rows(results, data.frame(what="Soccer: QDA", accuracy=qda_result))
results

confusionMatrix(predict(fit_qda, test), test$winner)[2]

#LDA
fit_lda <- train(winner ~ ., method = "lda", data = train)

lda_result <- test %>% mutate(pred=predict(fit_lda, test)) %>% summarize(accuracy=mean(winner==pred))
results <- bind_rows(results, data.frame(what="Soccer: LDA", accuracy=lda_result))
results

confusionMatrix(predict(fit_lda, test), test$winner)[2]


# rpart
fit_rpart <- train(winner ~ ., method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     data = train)
ggplot(fit_rpart, highlight = TRUE)
plot(fit_rpart$finalModel)
text(fit_rpart$finalModel)

rpart_result <- test %>% mutate(pred=predict(fit_rpart, test)) %>% summarize(accuracy=mean(winner==pred))
results <- bind_rows(results, data.frame(what="Soccer: rpart", accuracy=rpart_result))
results

confusionMatrix(predict(fit_rpart, test), test$winner)[2]


# random forest

fit_randomforest <- train(winner ~ .,
                    method = "Rborist",
                    tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                    data = train)

ggplot(fit_randomforest, highlight=TRUE)

randomforest_result <- test %>% mutate(pred=predict(fit_randomforest, test)) %>% summarize(accuracy=mean(winner==pred))
results <- bind_rows(results, data.frame(what="Soccer: Random Forest", accuracy=randomforest_result))
results

confusionMatrix(predict(fit_randomforest, test), test$winner)[2]


# Ensemble
ensemble <- data.frame(QDA= predict(fit_qda, test),
                       LDA= predict(fit_lda, test),
                       RF = predict(fit_randomforest, test))

read_out <- function(x){names(which.max(table(t(ensemble[x,]))))}
ensemble_pred <- sapply(seq(1,nrow(ensemble)), read_out)
ensemble_result <- test %>% cbind(ensemble_pred) %>% summarize(accuracy=mean(winner==ensemble_pred))
results <- bind_rows(results, data.frame(what="Soccer: Ensemble", accuracy=ensemble_result))
results
confusionMatrix(as.factor(ensemble_pred), test$winner)[2]


# FINAL VALIDATION

ensemble_final <- data.frame(KNN= predict(fit_knn, soccer_validation),
                       QDA= predict(fit_qda, soccer_validation),
                       LDA= predict(fit_lda, soccer_validation),
                       rpart=predict(fit_rpart, soccer_validation),
                       RF = predict(fit_randomforest, soccer_validation))

read_out <- function(x){names(which.max(table(t(ensemble_final[x,]))))}
ensemble_final_pred <- sapply(seq(1,nrow(ensemble_final)), read_out)

ensemble_final_result <- soccer_validation %>% cbind(ensemble_final_pred) %>% summarize(accuracy=mean(winner==ensemble_final_pred))
ensemble_final_result

lda_final <- mean(predict(fit_lda, soccer_validation)==soccer_validation$winner)
lda_final



##FOOTBALL####################################################################################################

# loading the data from the harddrive
gs2010 <- read.csv("C:\\Users\\duschmaj\\Desktop\\datascience\\capstone_final\\game_stats_2010.csv")
gs2011 <- read.csv("C:\\Users\\duschmaj\\Desktop\\datascience\\capstone_final\\game_stats_2011.csv")
gs2012 <- read.csv("C:\\Users\\duschmaj\\Desktop\\datascience\\capstone_final\\game_stats_2012.csv")
gs2013 <- read.csv("C:\\Users\\duschmaj\\Desktop\\datascience\\capstone_final\\game_stats_2013.csv")
gs2014 <- read.csv("C:\\Users\\duschmaj\\Desktop\\datascience\\capstone_final\\game_stats_2014.csv")
gs2015 <- read.csv("C:\\Users\\duschmaj\\Desktop\\datascience\\capstone_final\\game_stats_2015.csv")
gs2016 <- read.csv("C:\\Users\\duschmaj\\Desktop\\datascience\\capstone_final\\game_stats_2016.csv")
gs2017 <- read.csv("C:\\Users\\duschmaj\\Desktop\\datascience\\capstone_final\\game_stats_2017.csv")
gs2018 <- read.csv("C:\\Users\\duschmaj\\Desktop\\datascience\\capstone_final\\game_stats_2018.csv")
gs2019 <- read.csv("C:\\Users\\duschmaj\\Desktop\\datascience\\capstone_final\\game_stats_2019.csv")
data_nfl <- rbind(gs2010,gs2011,gs2012,gs2013,gs2014,gs2015,gs2016,gs2017,gs2018,gs2019)

#select relevant predictors
data_nfl <- data_nfl %>% summarize(
  H.RushAtt, 
  H.RushYards, 
  H.PassYards, 
  H.Turnover,
  H.Score,
  A.RushAtt, 
  A.RushYards, 
  A.PassYards, 
  A.Turnover,
  A.Score,
  )

# decide if the hometeam (h) or the awayteam (a) won, or if there was a draw (d)
data_nfl <- data_nfl %>% 
  mutate(winner=ifelse(H.Score>A.Score, "h", ifelse(H.Score==A.Score, "d", "a")))

# Remove scores as they should not be used as predictors
data_nfl <- data_nfl[,-which(names(data_nfl) %in% c("H.Score", "A.Score"))]

#remove rows with Na's
data_nfl <- na.omit(data_nfl)

#remove draws as they are less than 0.3% of entries
table(data_nfl$winner)
table(data_nfl$winner)/nrow(data_nfl)
data_nfl <- data_nfl %>% filter(winner != "d") %>% mutate(winner=as.factor(winner))


# Creating a validation set for final model validation
set.seed(240289) 
test_index <- createDataPartition(y = data_nfl$winner, times = 1, p = 0.1, list = FALSE)
football <- data_nfl[-test_index,]
football_validation <- data_nfl[test_index,]

# DATA ANALYSIS

summary(football)

football %>% ggplot(aes(winner)) + geom_bar()

football %>% ggplot(aes(H.RushAtt, H.RushYards, col=winner)) + geom_point()
football %>% ggplot(aes(A.RushAtt, A.RushYards, col=winner)) + geom_point()

football %>% ggplot(aes(winner, H.RushAtt)) + geom_boxplot() 
football %>% ggplot(aes(winner, H.RushYards)) + geom_boxplot()
football %>% ggplot(aes(winner, H.PassYards)) + geom_boxplot()
football %>% ggplot(aes(winner, H.Turnover)) + geom_boxplot()

football %>% ggplot(aes(winner, A.RushAtt)) + geom_boxplot()
football %>% ggplot(aes(winner, A.RushYards)) + geom_boxplot()
football %>% ggplot(aes(winner, A.PassYards)) + geom_boxplot()
football %>% ggplot(aes(winner, A.Turnover)) + geom_boxplot()

# Creating train and test set for model creation
set.seed(020816)
test_index <- createDataPartition(y = football$winner, times = 1, p = 0.1, list = FALSE)
train_nfl <- football[-test_index,]
test_nfl <- football[test_index,]

table(test_nfl$winner)

#MODEL BUILDING
# guess
random_result_nfl <- test_nfl %>% mutate(pred=sample(c("h","a","d"), nrow(test_nfl), replace=TRUE)) %>% summarize(accuracy=mean(winner==pred))

results <- bind_rows(results, data.frame(what="Footall: random guessing", accuracy=random_result_nfl))
results


# all home wins
all_h_nfl <- test_nfl %>% mutate(pred=replicate(nrow(test_nfl), "h")) %>% summarize(accuracy=mean(winner==pred))
results <- bind_rows(results, data.frame(what="Football: All home wins", accuracy=all_h_nfl))
results

# knn
fit_knn_nfl <- train(winner ~ .,  method = "knn", 
                 tuneGrid = data.frame(k = seq(1, 100, 5)), 
                 data = train_nfl)
ggplot(fit_knn_nfl, highlight=TRUE)
knn_result_nfl <- test_nfl %>% mutate(pred=predict(fit_knn_nfl, test_nfl)) %>% summarize(accuracy=mean(winner==pred))

results<- bind_rows(results, data.frame(what="Football: KNN", accuracy=knn_result_nfl))
results

confusionMatrix(predict(fit_knn_nfl, test_nfl), test_nfl$winner)[2]

#QDA
fit_qda_nfl <- train(winner ~ ., method = "qda", data = train_nfl)

qda_result_nfl <- test_nfl %>% mutate(pred=predict(fit_qda_nfl, test_nfl)) %>% summarize(accuracy=mean(winner==pred))
results <- bind_rows(results, data.frame(what="Football: QDA", accuracy=qda_result_nfl))
results

confusionMatrix(predict(fit_qda_nfl, test_nfl), test_nfl$winner)[2]

#LDA
fit_lda_nfl <- train(winner ~ ., method = "lda", data = train_nfl)

lda_result_nfl <- test_nfl %>% mutate(pred=predict(fit_lda_nfl, test_nfl)) %>% summarize(accuracy=mean(winner==pred))
results <- bind_rows(results, data.frame(what="Football: LDA", accuracy=lda_result_nfl))
results

confusionMatrix(predict(fit_lda_nfl, test_nfl), test_nfl$winner)[2]


# rpart
fit_rpart_nfl <- train(winner ~ ., method = "rpart",
                   tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                   data = train_nfl)
ggplot(fit_rpart_nfl, highlight=TRUE)
plot(fit_rpart_nfl$finalModel)
text(fit_rpart_nfl$finalModel)

rpart_result_nfl <- test_nfl %>% mutate(pred=predict(fit_rpart_nfl, test_nfl)) %>% summarize(accuracy=mean(winner==pred))
results <- bind_rows(results, data.frame(what="Football: rpart", accuracy=rpart_result_nfl))
results

confusionMatrix(predict(fit_rpart_nfl, test_nfl), test_nfl$winner)[2]


# random forest

fit_randomforest_nfl <- train(winner ~ .,
                          method = "Rborist",
                          tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                          data = train_nfl)

ggplot(fit_randomforest_nfl, highlight=TRUE)

randomforest_result_nfl <- test_nfl %>% mutate(pred=predict(fit_randomforest_nfl, test_nfl)) %>% summarize(accuracy=mean(winner==pred))
results <- bind_rows(results, data.frame(what="Football: Random Forest", accuracy=randomforest_result_nfl))
results

confusionMatrix(predict(fit_randomforest_nfl, test_nfl), test_nfl$winner)[2]


# Ensemble

ensemble_nfl <- data.frame(QDA= predict(fit_qda_nfl, test_nfl),
                           LDA= predict(fit_lda_nfl, test_nfl),
                           RF = predict(fit_randomforest_nfl, test_nfl))

read_out <- function(x){names(which.max(table(t(ensemble_nfl[x,]))))}
ensemble_pred_nfl <- sapply(seq(1,nrow(ensemble_nfl)), read_out)
ensemble_result_nfl <- test_nfl %>% cbind(ensemble_pred_nfl) %>% summarize(accuracy=mean(winner==ensemble_pred_nfl))
results <- bind_rows(results, data.frame(what="Football: Ensemble", accuracy=ensemble_result_nfl))
results

confusionMatrix(as.factor(ensemble_pred_nfl), test_nfl$winner)[2]


# FINAL VALIDATION

ensemble_final_nfl <- data.frame(QDA= predict(fit_qda_nfl, football_validation),
                                 LDA= predict(fit_lda_nfl, football_validation),
                                 RF = predict(fit_randomforest_nfl, football_validation))

read_out <- function(x){names(which.max(table(t(ensemble_final_nfl[x,]))))}
ensemble_final_pred_nfl <- sapply(seq(1,nrow(ensemble_final_nfl)), read_out)

ensemble_final_result_nfl <- football_validation %>% cbind(ensemble_final_pred_nfl) %>% summarize(accuracy=mean(winner==ensemble_final_pred_nfl))
ensemble_final_result_nfl

