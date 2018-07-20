# Mckinsey_code

library(data.table)
library(tidyverse)
library(dplyr)

setwd("D:/Projects/Mckinsey/DATA")

train <- fread("train_data.csv",sep = ",",header = TRUE)
test <- fread("test_data.csv",sep = ",",header = TRUE)

test$renewal <- NA
all <- rbind(train,test)

Income_tr <- mean(Income) + 2 * sd(Income)
all <- mutate(all,Income=ifelse(Income > Income_tr,mean(Income, na.rm=TRUE), Income))

#Completeness of the data
sapply(all, function(x) {sum(is.na(x))})
attach(all)

all <- mutate(all,`Count_3-6_months_late`=ifelse(is.na(`Count_3-6_months_late`), 
                                                  median(`Count_3-6_months_late`, na.rm=TRUE), `Count_3-6_months_late`))

all <- mutate(all,`Count_6-12_months_late`=ifelse(is.na(`Count_6-12_months_late`), 
                                                   median(`Count_6-12_months_late`, na.rm=TRUE), `Count_6-12_months_late`))

all <- mutate(all,`Count_more_than_12_months_late`=ifelse(is.na(`Count_more_than_12_months_late`), 
                                                           median(`Count_more_than_12_months_late`, na.rm=TRUE), `Count_more_than_12_months_late`))

all <- mutate(all,application_underwriting_score=ifelse(is.na(application_underwriting_score), 
                                                          mean(application_underwriting_score, na.rm=TRUE), application_underwriting_score))

sapply(all, function(x) {sum(is.na(x))})

train_df <- all[!is.na(all$renewal),]
test_df <- all[is.na(all$renewal),]

model <- glm (renewal ~ .-id, data = train_df, family = binomial)
summary(model)

predict <- predict(model, type = 'response')

#confusion matrix
table(train_df$renewal, predict > 0.5)

#ROCR Curve
library(ROCR)
ROCRpred <- prediction(predict, train_df$renewal)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

library(pROC)
roc_obj <- roc(train_df$renewal, predict)
auc(roc_obj)

#plot glm
library(ggplot2)
ggplot(train_df, aes(x=Income, y=renewal)) + geom_point() + 
  stat_smooth(method="glm", family="binomial", se=FALSE)

predict_test <- predict(model, test_df, type = 'response')
out <- cbind(train_df,predict)
write.csv(out,file = "Out1.csv")
