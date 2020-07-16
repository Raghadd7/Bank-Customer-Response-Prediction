getOption("max.print") #  max print 
options(scipen=99)  # prevents printing scientific notations.
write.csv(loan_data_balanced1,"~/Desktop/loan-campaign/loan_data_final.CSV") #saving data after balancing
write.csv(woe_data_balanced,"~/Desktop/loan-campaign/woe_data_balanced.CSV") #saving data after balancing WoE encoded 
write.csv(loan_data_encoded1h_MM,"~/Desktop/loan-campaign/loan_data_encoded1h_MM.CSV") # saving data after normalizing 
write.csv(woe_data,"~/Desktop/loan-campaign/woe_data.CSV") #saving data after WoE encoding

#reading loan campaign data
loan_data <- read.csv("~/Desktop/loan-campaign/PL_XSELL.CSV",strip.white = TRUE)

#removing variables
loan_data$CUST_ID  <- NULL
loan_data$random  <- NULL
loan_data$AGE_BKT <- NULL
loan_data$ACC_OP_DATE <- NULL 
loan_data$AMT_OTH_BK_ATM_USG_CHGS  <- NULL
loan_data$NO_OF_IW_CHQ_BNC_TXNS  <- NULL
loan_data$NO_OF_OW_CHQ_BNC_TXNS  <- NULL

#removing duplicates
#install.packages("tidyverse")
library(tidyverse)
loan_data <- loan_data %>% distinct()

#removing "other" value in gender 
loan_data <- loan_data[!(loan_data$GENDER == "O"),]
#xtabs(~ loan_data$TARGET + loan_data$GENDER, data = loan_data)
loan_data$GENDER <- forcats::fct_drop(loan_data$GENDER)
xtabs(~ loan_data$TARGET + loan_data$GENDER, data = loan_data)

prop.table(table(loan_data$TARGET))

# changing data type to factor
loan_data$FLG_HAS_CC <- as.factor(loan_data$FLG_HAS_CC)
loan_data$FLG_HAS_ANY_CHGS <- as.factor(loan_data$FLG_HAS_ANY_CHGS)
loan_data$FLG_HAS_NOMINEE <- as.factor(loan_data$FLG_HAS_NOMINEE)
loan_data$FLG_HAS_OLD_LOAN <- as.factor(loan_data$FLG_HAS_OLD_LOAN)

xtabs(~ loan_data$TARGET + loan_data$GENDER, data = loan_data)
xtabs(~ loan_data$TARGET + loan_data$OCCUPATION, data = loan_data)
xtabs(~ loan_data$TARGET + loan_data$ACC_TYPE, data = loan_data)
xtabs(~ loan_data$TARGET + loan_data$LEN_OF_RLTN_IN_MNTH, data = loan_data)

####################################
#data visualization 
library(ggplot2)

#scatter polt of data before and after balancing 
loan_data_encoded1h$TARGET <- as.factor(loan_data_encoded1h$TARGET)
loan_data_balanced1$TARGET <- as.factor(loan_data_balanced1$TARGET)
#Original dataset
ggplot(data = loan_data_encoded1h,
       mapping = aes(x =TOT_NO_OF_L_TXNS, y =BALANCE , color = TARGET)) +
   geom_point(alpha = .7,
              size = 3) +
   #scale_colour_brewer(palette = "BuGn") +
   scale_color_manual(values = c( "#c5e3ea", "#99D8C9"))+
   labs(y = "Balance", 
        fill = "Target",
        x = "Total number of transactions",
        title = "Original dataset")+
   theme_minimal()

#Modified dataset
ggplot(data = loan_data_balanced1,
       mapping = aes(x =TOT_NO_OF_L_TXNS, y =BALANCE , color = TARGET)) +
   geom_point(alpha = .7,
              size = 4) +
   scale_color_manual(values = c( "#c5e3ea", "#99D8C9"))+
   labs(y = "Balance", 
        fill = "Target",
        x = "Total number of transactions",
        title = "Modified dataset")+
   theme_minimal()

#numerical data visualization 
ggplot(data = loan_data, aes(x = AGE))+
   geom_histogram(fill = alpha("#D8ECF0", alpha = 0.7) , binwidth = 5, colour = "black")+
   geom_vline(aes(xintercept = median(AGE)), linetype = "dashed")+
   theme_minimal() +
   scale_color_brewer(palette="BuGn")
ggplot(data = loan_data, aes(x = LEN_OF_RLTN_IN_MNTH))+
   geom_histogram(fill = alpha("#BBE3D9", alpha = 0.7) , binwidth = 5, colour = "black")+
   geom_vline(aes(xintercept = median(LEN_OF_RLTN_IN_MNTH)), linetype = "dashed")+
   theme_minimal() +
   scale_color_brewer(palette="BuGn")
ggplot(data = loan_data, aes(x = HOLDING_PERIOD))+
   geom_histogram(fill = alpha("#8DD2A9", alpha = 0.7) , binwidth = 5, colour = "black")+
   geom_vline(aes(xintercept = median(HOLDING_PERIOD)), linetype = "dashed")+
   theme_minimal() +
   scale_color_brewer(palette="BuGn")
ggplot(data = loan_data, aes(x = NO_OF_L_CR_TXNS))+
   geom_histogram(fill = alpha("#F9D69F", alpha = 0.7) , binwidth = 5, colour = "black")+
   geom_vline(aes(xintercept = median(NO_OF_L_CR_TXNS)), linetype = "dashed")+
   theme_minimal() +
   scale_color_brewer(palette="BuGn")
ggplot(data = loan_data, aes(x = NO_OF_L_DR_TXNS))+
   geom_histogram(fill = alpha("#D1B3DD", alpha = 0.7) , binwidth = 5, colour = "black")+
   geom_vline(aes(xintercept = median(NO_OF_L_DR_TXNS)), linetype = "dashed")+
   theme_minimal() +
   scale_color_brewer(palette="BuGn")
ggplot(data = loan_data, aes(x = TOT_NO_OF_L_TXNS))+
   geom_histogram(fill = alpha("#F9E79F", alpha = 0.7) , binwidth = 5, colour = "black")+
   geom_vline(aes(xintercept = median(TOT_NO_OF_L_TXNS)), linetype = "dashed")+
   theme_minimal() +
   scale_color_brewer(palette="BuGn")

#categorical data visualization 
library(scales)

#gender 
ggplot(loan_data, 
       aes(x = factor(GENDER,
                      levels = c("F","M"),
                      labels = c("Female","Male")),
           fill = factor(TARGET, 
                         levels = c("0", "1"),
                         labels = c("non-responders", 
                                    "Responders" 
                         )))) + 
   geom_bar(position = "dodge") +
   scale_fill_brewer(palette = "BuGn") +
   labs(y = "Count", 
        fill = "Target",
        x = "Gender") +
   theme_minimal()

#occupation
ggplot(loan_data, 
       aes(x = factor(OCCUPATION,
                      levels = c("PROF","SAL","SELF-EMP","SENP"),
                      labels = c("PROF","SAL","SELF-EMP","SENP")),
           fill = factor(TARGET, 
                         levels = c("0", "1"),
                         labels = c("non-responders", 
                                    "Responders" 
                         )))) + 
   geom_bar(position = "dodge") +
   scale_fill_brewer(palette = "BuGn") +
   labs(y = "Count", 
        fill = "Target",
        x = "Occupation") +
   theme_minimal()

#FLG_HAS_CC
ggplot(loan_data, 
       aes(x = factor(FLG_HAS_CC,
                      levels = c("0","1"),
                      labels = c("No","Yes")),
           fill = factor(TARGET, 
                         levels = c("0", "1"),
                         labels = c("non-responders", 
                                    "Responders" 
                         )))) + 
   geom_bar(position = "dodge") +
   scale_fill_brewer(palette = "BuGn") +
   labs(y = "Count", 
        fill = "Target",
        x = "Has Credit Card") +
   theme_minimal()


#####################################

#categorical variables encoding

#1 one-hot-encoding 
library(dataPreparation)
loan_data_encoding <- build_encoding(loan_data, cols = c("GENDER", "OCCUPATION","ACC_TYPE",
                                                         "FLG_HAS_CC","FLG_HAS_ANY_CHGS",
                                                         "FLG_HAS_NOMINEE","FLG_HAS_OLD_LOAN"), 
                                                          verbose = TRUE)

loan_data_encoded1h <- one_hot_encoder(loan_data, encoding = loan_data_encoding, drop = TRUE)

#unbalanced data 
library(dataPreparation)
loan_data_encoded1h_v2 <- loan_data_encoded1h
loan_data_encoded1h_v2$GENDER.M = NULL
loan_data_encoded1h_v2$OCCUPATION.SENP = NULL
loan_data_encoded1h_v2$ACC.TYPE.SA = NULL
loan_data_encoded1h_v2$FLG.HAS.CC.1 = NULL
loan_data_encoded1h_v2$FLG.HAS.ANY.CHGS.1= NULL
loan_data_encoded1h_v2$FLG.HAS.NOMINEE.1= NULL
loan_data_encoded1h_v2$FLG.HAS.OLD.LOAN.1= NULL

loan_data_encoded1h_v2$NO_OF_NET_DR_TXNS <- NULL
loan_data_encoded1h_v2$NO_OF_L_DR_TXNS <- NULL
loan_data_encoded1h_v2$AVG_AMT_PER_CHQ_TXN<- NULL
loan_data_encoded1h_v2$AVG_AMT_PER_MOB_TXN <- NULL
loan_data_encoded1h_v2$NO_OF_L_CR_TXNS<-NULL

loan_data_encoded1h_v2$AMT_ATM_DR <- NULL
loan_data_encoded1h_v2$AMT_BR_CSH_WDL_DR <- NULL
loan_data_encoded1h_v2$AMT_CHQ_DR<- NULL
loan_data_encoded1h_v2$AMT_NET_DR <- NULL
loan_data_encoded1h_v2$AMT_MOB_DR <- NULL
loan_data_encoded1h_v2$AMT_L_DR<- NULL


#2 WoE data encoding 
library(scorecard)
bins = woebin(loan_data, y ='TARGET')
woe_data <- woebin_ply(loan_data, bins)

##############
woe_data_v2 <- woe_data
woe_data_v2$AMT_MIN_BAL_NMC_CHGS_woe <- NULL
woe_data_v2$NO_OF_MOB_DR_TXNS_woe <- NULL
woe_data_v2$AMT_MOB_DR_woe <- NULL
woe_data_v2$AVG_AMT_PER_MOB_TXN_woe <- NULL

#data balancing 
library(imbalance)
loan_data_balanced1 <- oversample(loan_data_encoded1h, method = "ADASYN", classAttr = "TARGET")
woe_data_balanced <- oversample(woe_data, method = "ADASYN", classAttr = "TARGET")

############### balanced data
loan_data_balanced1_v2 <- loan_data_balanced1 
loan_data_balanced1_v2$GENDER.M <- NULL
loan_data_balanced1_v2$OCCUPATION.SENP <-NULL
loan_data_balanced1_v2$ACC.TYPE.SA <-NULL
loan_data_balanced1_v2$FLG.HAS.CC.1 <- NULL
loan_data_balanced1_v2$FLG.HAS.ANY.CHGS.1 <- NULL
loan_data_balanced1_v2$FLG.HAS.NOMINEE.1 <-NULL
loan_data_balanced1_v2$FLG.HAS.OLD.LOAN.1 <-NULL

loan_data_balanced1_v2$NO_OF_NET_DR_TXNS <- NULL
loan_data_balanced1_v2$NO_OF_L_DR_TXNS <- NULL
loan_data_balanced1_v2$AVG_AMT_PER_MOB_TXN <- NULL
loan_data_balanced1_v2$AVG_AMT_PER_CHQ_TXN<- NULL

loan_data_balanced1_v2$NO_OF_L_CR_TXNS<-NULL
loan_data_balanced1_v2$AMT_BR_CSH_WDL_DR <- NULL
loan_data_balanced1_v2$AMT_L_DR <- NULL
loan_data_balanced1_v2$AMT_MOB_DR <- NULL
loan_data_balanced1_v2$AMT_NET_DR <- NULL
loan_data_balanced1_v2$AMT_CHQ_DR <- NULL
loan_data_balanced1_v2$AMT_ATM_DR <- NULL

############### balanced WoE encoded data 
woe_data_balanced_v2 <- woe_data_balanced
woe_data_balanced_v2$AMT_MIN_BAL_NMC_CHGS_woe <- NULL
woe_data_balanced_v2$NO_OF_MOB_DR_TXNS_woe <- NULL
woe_data_balanced_v2$AMT_MOB_DR_woe <- NULL
woe_data_balanced_v2$AVG_AMT_PER_MOB_TXN_woe <- NULL



   #min-max normalization 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) } 
loan_data_balanced1_MM <- as.data.frame(lapply(loan_data_balanced1[,2:42], normalize))
loan_data_balanced1_MM$TARGET <- loan_data_balanced1$TARGET

write_csv(loan_data_balanced1_MM,"~/Desktop/loan-campaign/loan_data_balanced_MM.CSV")

loan_data_encoded1h_MM <- as.data.frame(lapply(loan_data_encoded1h[,2:42], normalize))
loan_data_encoded1h_MM$TARGET <- loan_data_encoded1h$TARGET


########### unbalanced normalized data 
loan_data_encoded1h_MM_v2 <- loan_data_encoded1h_MM
loan_data_encoded1h_MM_v2$GENDER.M = NULL
loan_data_encoded1h_MM_v2$OCCUPATION.SENP = NULL
loan_data_encoded1h_MM_v2$ACC.TYPE.SA = NULL
loan_data_encoded1h_MM_v2$FLG.HAS.CC.1 = NULL
loan_data_encoded1h_MM_v2$FLG.HAS.ANY.CHGS.1= NULL
loan_data_encoded1h_MM_v2$FLG.HAS.NOMINEE.1= NULL
loan_data_encoded1h_MM_v2$FLG.HAS.OLD.LOAN.1= NULL

loan_data_encoded1h_MM_v2$NO_OF_NET_DR_TXNS <- NULL
loan_data_encoded1h_MM_v2$NO_OF_L_DR_TXNS <- NULL
loan_data_encoded1h_MM_v2$AVG_AMT_PER_CHQ_TXN<- NULL
loan_data_encoded1h_MM_v2$AVG_AMT_PER_MOB_TXN <- NULL
loan_data_encoded1h_MM_v2$NO_OF_L_CR_TXNS<-NULL

loan_data_encoded1h_MM_v2$AMT_ATM_DR <- NULL
loan_data_encoded1h_MM_v2$AMT_BR_CSH_WDL_DR <- NULL
loan_data_encoded1h_MM_v2$AMT_CHQ_DR<- NULL
loan_data_encoded1h_MM_v2$AMT_NET_DR <- NULL
loan_data_encoded1h_MM_v2$AMT_MOB_DR <- NULL
loan_data_encoded1h_MM_v2$AMT_L_DR<- NULL


#balanced normalized data
loan_data_balanced1_MM_v2 <- loan_data_balanced1_MM
loan_data_balanced1_MM_v2$GENDER.M <- NULL
loan_data_balanced1_MM_v2$OCCUPATION.SENP <-NULL
loan_data_balanced1_MM_v2$ACC.TYPE.SA <-NULL
loan_data_balanced1_MM_v2$FLG.HAS.CC.1 <- NULL
loan_data_balanced1_MM_v2$FLG.HAS.ANY.CHGS.1 <- NULL
loan_data_balanced1_MM_v2$FLG.HAS.NOMINEE.1 <-NULL
loan_data_balanced1_MM_v2$FLG.HAS.OLD.LOAN.1 <-NULL

loan_data_balanced1_MM_v2$NO_OF_NET_DR_TXNS <- NULL
loan_data_balanced1_MM_v2$NO_OF_L_DR_TXNS <- NULL
loan_data_balanced1_MM_v2$AVG_AMT_PER_MOB_TXN <- NULL
loan_data_balanced1_MM_v2$AVG_AMT_PER_CHQ_TXN<- NULL

loan_data_balanced1_MM_v2$NO_OF_L_CR_TXNS<-NULL
loan_data_balanced1_MM_v2$AMT_BR_CSH_WDL_DR <- NULL
loan_data_balanced1_MM_v2$AMT_L_DR <- NULL
loan_data_balanced1_MM_v2$AMT_MOB_DR <- NULL
loan_data_balanced1_MM_v2$AMT_NET_DR <- NULL
loan_data_balanced1_MM_v2$AMT_CHQ_DR <- NULL
loan_data_balanced1_MM_v2$AMT_ATM_DR <- NULL



#splitting the balanced data to 70% trainig set and 30% test set
library(caret)

set.seed(10)

trainDataIndex <- createDataPartition(loan_data_balanced1_v2$TARGET, p=0.70, list = F)
trainData <-loan_data_balanced1_v2[trainDataIndex, ]
testData <-loan_data_balanced1_v2[-trainDataIndex, ]

table(trainData$TARGET)
table(testData$TARGET)
prop.table(table(trainData$TARGET))
prop.table(table(testData$TARGET))

trainData$TARGET <- as.factor(trainData$TARGET)
testData$TARGET <- as.factor(testData$TARGET)


#logistic regression model on one-hot-encoded balanced data
logitmod <- glm(TARGET ~ ., 
                family = "binomial", data=trainData)
summary(logitmod)

predlog <- predict(logitmod, newdata = testData, type = "response")

library(InformationValue)
optimalCutoff(testData$TARGET, predlog)[1]

y_pred_num <- ifelse(predlog > 0.5, 1, 0)
accuracy <- table(testData$TARGET, y_pred_num)
sum(diag(accuracy))/sum(accuracy)

predlog <- plogis(predict(logitmod, testData))

car::vif(logitmod)
max(car::vif(logitmod))

cfmlog <- caret::confusionMatrix(data=as.factor(y_pred_num), reference=testData$TARGET,positive="1")
cfmlog$byClass


#logistic regression model with interactions on one-hot-encoded balanced data
logitmod <- glm(TARGET ~     HOLDING_PERIOD*OCCUPATION.SAL+
                         FLG.HAS.OLD.LOAN.0*OCCUPATION.SAL+
                          NO_OF_CHQ_DR_TXNS*OCCUPATION.SAL+
                               
                         AGE+BALANCE+ SCR+                    
                         LEN_OF_RLTN_IN_MNTH+TOT_NO_OF_L_TXNS+NO_OF_BR_CSH_WDL_DR_TXNS+
                         NO_OF_ATM_DR_TXNS+NO_OF_MOB_DR_TXNS+AVG_AMT_PER_ATM_TXN+AVG_AMT_PER_CSH_WDL_TXN+
                         AVG_AMT_PER_NET_TXN+AMT_MIN_BAL_NMC_CHGS+GENDER.F+
                         OCCUPATION.PROF+OCCUPATION.SELF.EMP+ACC.TYPE.CA+            
                         FLG.HAS.ANY.CHGS.0+FLG.HAS.NOMINEE.0 +FLG.HAS.CC.0     
                         ,
                family = "binomial", data=trainData)

summary(logitmod)
predlog <- predict(logitmod, newdata = testData, type = "response")
library(InformationValue)
optimalCutoff(testData$TARGET, predlog)[1]
y_pred_num <- ifelse(predlog > 0.5, 1, 0)
accuracy <- table(testData$TARGET, y_pred_num)
sum(diag(accuracy))/sum(accuracy)

caret::confusionMatrix(data=as.factor(y_pred_num), reference=testData$TARGET,positive="1")$byClass
####################
trainDataIndex <- createDataPartition(loan_data_encoded1h_v2$TARGET, p=0.70, list = F)
trainData <-loan_data_encoded1h_v2[trainDataIndex, ]
testData <-loan_data_encoded1h_v2[-trainDataIndex, ]

table(trainData$TARGET)
table(testData$TARGET)
prop.table(table(trainData$TARGET))
prop.table(table(testData$TARGET))

trainData$TARGET <- as.factor(trainData$TARGET)
testData$TARGET <- as.factor(testData$TARGET)

#logistic regression model on one-hot-encoded imbalanced data 
logitmod <- glm(TARGET ~ ., 
                family = "binomial", data=trainData)
summary(logitmod)

predlog <- predict(logitmod, newdata = testData, type = "response")

library(InformationValue)
optimalCutoff(testData$TARGET, predlog)[1]

y_pred_num <- ifelse(predlog > 0.5, 1, 0)
accuracy <- table(testData$TARGET, y_pred_num)
sum(diag(accuracy))/sum(accuracy)

predlog <- plogis(predict(logitmod, testData))

car::vif(logitmod)
max(car::vif(logitmod))

cfmlog <- caret::confusionMatrix(data=as.factor(y_pred_num), reference=testData$TARGET,positive="1")
cfmlog$byClass



########### 
trainDataIndex <- createDataPartition(woe_data_balanced_v2$TARGET, p=0.70, list = F)
trainData <-woe_data_balanced_v2[trainDataIndex, ]
testData <-woe_data_balanced_v2[-trainDataIndex, ]

table(trainData$TARGET)
table(testData$TARGET)
prop.table(table(trainData$TARGET))
prop.table(table(testData$TARGET))

trainData$TARGET <- as.factor(trainData$TARGET)
testData$TARGET <- as.factor(testData$TARGET)

#logistic regression model on balanced WoE encoded data
logitmod <- glm(TARGET ~ ., 
                family = "binomial", data=trainData)
summary(logitmod)

predlog <- predict(logitmod, newdata = testData, type = "response")

library(InformationValue)
optimalCutoff(testData$TARGET, predlog)[1]

y_pred_num <- ifelse(predlog > 0.5, 1, 0)
accuracy <- table(testData$TARGET, y_pred_num)
sum(diag(accuracy))/sum(accuracy)

predlog <- plogis(predict(logitmod, testData))

car::vif(logitmod)
max(car::vif(logitmod))

cfmlog <- caret::confusionMatrix(data=as.factor(y_pred_num), reference=testData$TARGET,positive="1")
cfmlog$byClass

#logistic regression model on WoE encoded data with interactions 
logitmod <- glm(TARGET ~ HOLDING_PERIOD_woe*OCCUPATION_woe+
                         ACC_TYPE_woe*OCCUPATION_woe+
                         FLG_HAS_OLD_LOAN_woe*OCCUPATION_woe+
                   
                   AGE_woe+ BALANCE_woe+SCR_woe +LEN_OF_RLTN_IN_MNTH_woe+NO_OF_L_CR_TXNS_woe+NO_OF_L_DR_TXNS_woe+
                   TOT_NO_OF_L_TXNS_woe+NO_OF_BR_CSH_WDL_DR_TXNS_woe+ NO_OF_ATM_DR_TXNS_woe+NO_OF_NET_DR_TXNS_woe+
                   NO_OF_CHQ_DR_TXNS_woe+AMT_ATM_DR_woe+            
                   AMT_BR_CSH_WDL_DR_woe+AMT_CHQ_DR_woe+AMT_NET_DR_woe+              
                   AMT_L_DR_woe+    
                   AVG_AMT_PER_ATM_TXN_woe+AVG_AMT_PER_CSH_WDL_TXN_woe+AVG_AMT_PER_CHQ_TXN_woe+     
                   AVG_AMT_PER_NET_TXN_woe+GENDER_woe+               
                   FLG_HAS_CC_woe+        
                   FLG_HAS_ANY_CHGS_woe+FLG_HAS_NOMINEE_woe
                ,
                family = "binomial", data=trainData)
summary(logitmod)

predlog <- predict(logitmod, newdata = testData, type = "response")
library(InformationValue)
optimalCutoff(testData$TARGET, predlog)[1]
y_pred_num <- ifelse(predlog > 0.5, 1, 0)
accuracy <- table(testData$TARGET, y_pred_num)
sum(diag(accuracy))/sum(accuracy)

cfmlog <- caret::confusionMatrix(data=as.factor(y_pred_num), reference=testData$TARGET,positive="1")
cfmlog$byClass

####################
trainDataIndex <- createDataPartition(woe_data_v2$TARGET, p=0.70, list = F)
trainData <-woe_data_v2[trainDataIndex, ]
testData <-woe_data_v2[-trainDataIndex, ]

table(trainData$TARGET)
table(testData$TARGET)
prop.table(table(trainData$TARGET))
prop.table(table(testData$TARGET))

trainData$TARGET <- as.factor(trainData$TARGET)
testData$TARGET <- as.factor(testData$TARGET)

#logistic regression model on imbalanced WoE encoded data 
logitmod <- glm(TARGET ~ ., 
                family = "binomial", data=trainData)
summary(logitmod)

predlog <- predict(logitmod, newdata = testData, type = "response")

library(InformationValue)
optimalCutoff(testData$TARGET, predlog)[1]

y_pred_num <- ifelse(predlog > 0.5, 1, 0)
accuracy <- table(testData$TARGET, y_pred_num)
sum(diag(accuracy))/sum(accuracy)

predlog <- plogis(predict(logitmod, testData))

car::vif(logitmod)
max(car::vif(logitmod))

cfmlog <- caret::confusionMatrix(data=as.factor(y_pred_num), reference=testData$TARGET,positive="1")
cfmlog$byClass


####################
trainDataIndex <- createDataPartition(loan_data_balanced1_MM_v2$TARGET, p=0.70, list = F)
trainData <-loan_data_balanced1_MM_v2[trainDataIndex, ]
testData <-loan_data_balanced1_MM_v2[-trainDataIndex, ]

table(trainData$TARGET)
table(testData$TARGET)
prop.table(table(trainData$TARGET))
prop.table(table(testData$TARGET))

trainData$TARGET <- as.factor(trainData$TARGET)
testData$TARGET <- as.factor(testData$TARGET)

#logistic regression model on balanced normalized data 
logitmod <- glm(TARGET ~ ., 
                family = "binomial", data=trainData)
summary(logitmod)

predlog <- predict(logitmod, newdata = testData, type = "response")

library(InformationValue)
optimalCutoff(testData$TARGET, predlog)[1]

y_pred_num <- ifelse(predlog > 0.5, 1, 0)
accuracy <- table(testData$TARGET, y_pred_num)
sum(diag(accuracy))/sum(accuracy)

predlog <- plogis(predict(logitmod, testData))

car::vif(logitmod)
max(car::vif(logitmod))

cfmlog <- caret::confusionMatrix(data=as.factor(y_pred_num), reference=testData$TARGET,positive="1")
cfmlog$byClass

#logstic regression model on balanced normalized data with interactions 
logitmod <- glm(TARGET ~     HOLDING_PERIOD*OCCUPATION.SAL+
                   FLG.HAS.OLD.LOAN.0*OCCUPATION.SAL+
                   NO_OF_CHQ_DR_TXNS*OCCUPATION.SAL+
                   
                   AGE+BALANCE+ SCR+                    
                   LEN_OF_RLTN_IN_MNTH+TOT_NO_OF_L_TXNS+NO_OF_BR_CSH_WDL_DR_TXNS+
                   NO_OF_ATM_DR_TXNS+NO_OF_MOB_DR_TXNS+AVG_AMT_PER_ATM_TXN+AVG_AMT_PER_CSH_WDL_TXN+
                   AVG_AMT_PER_NET_TXN+AMT_MIN_BAL_NMC_CHGS+GENDER.F+
                   OCCUPATION.PROF+OCCUPATION.SELF.EMP+ACC.TYPE.CA+            
                   FLG.HAS.ANY.CHGS.0+FLG.HAS.NOMINEE.0 +FLG.HAS.CC.0     
                ,
                family = "binomial", data=trainData)

summary(logitmod)
predlog <- predict(logitmod, newdata = testData, type = "response")
library(InformationValue)
optimalCutoff(testData$TARGET, predlog)[1]
y_pred_num <- ifelse(predlog > 0.5, 1, 0)
accuracy <- table(testData$TARGET, y_pred_num)
sum(diag(accuracy))/sum(accuracy)

caret::confusionMatrix(data=as.factor(y_pred_num), reference=testData$TARGET,positive="1")$byClass

#######################
trainDataIndex <- createDataPartition(loan_data_encoded1h_MM_v2$TARGET, p=0.70, list = F)
trainData <loan_data_encoded1h_MM_v2[trainDataIndex, ]
testData <loan_data_encoded1h_MM_v2[-trainDataIndex, ]

table(trainData$TARGET)
table(testData$TARGET)
prop.table(table(trainData$TARGET))
prop.table(table(testData$TARGET))

trainData$TARGET <- as.factor(trainData$TARGET)
testData$TARGET <- as.factor(testData$TARGET)


#logstic regression model on imbalanced normalized data 
logitmod <- glm(TARGET ~ ., 
                family = "binomial", data=trainData)
summary(logitmod)

predlog <- predict(logitmod, newdata = testData, type = "response")

library(InformationValue)
optimalCutoff(testData$TARGET, predlog)[1]

y_pred_num <- ifelse(predlog > 0.5, 1, 0)
accuracy <- table(testData$TARGET, y_pred_num)
sum(diag(accuracy))/sum(accuracy)

predlog <- plogis(predict(logitmod, testData))

car::vif(logitmod)
max(car::vif(logitmod))

cfmlog <- caret::confusionMatrix(data=as.factor(y_pred_num), reference=testData$TARGET,positive="1")
cfmlog$byClass





######################
#Random forest 
set.seed(100)
trainDataIndex <- createDataPartition(loan_data_balanced1$TARGET, p=0.70, list = F)
trainData <- loan_data_balanced1[trainDataIndex, ]
testData <- loan_data_balanced1[-trainDataIndex, ]

table(trainData$TARGET)
table(testData$TARGET)

trainData$TARGET <- as.factor(trainData$TARGET)
testData$TARGET <- as.factor(testData$TARGET)

library(randomForest)
set.seed(217)
rf2 <- randomForest(
   TARGET ~ . ,
   data=trainData, 
   improtance = TRUE
   #localImp = TRUE
)


predrf = predict(rf2, newdata=testData)
accuracy <- table(predrf, testData$TARGET)
sum(diag(accuracy))/sum(accuracy)
table(testData$TARGET, predrf)
predrf2 = predict(rf2, newdata=testData, type= "prob")

predrf22 <- ifelse(predrf2[,2] > 0.5, 1, 0)
accuracy <- as.matrix(table(Acctual= testData$TARGET, Predicted=predrf22))
sum(diag(accuracy))/sum(accuracy)

caret::confusionMatrix(data=as.factor(predrf22), reference=testData$TARGET,positive="1")
caret::confusionMatrix(data=as.factor(predrf22), reference=testData$TARGET,positive="1")$byClass
caret::precision(data = as.factor(predrf22), reference = testData$TARGET, relevant=levels(testData$TARGET)[1])
caret::recall(data = as.factor(predrf22), reference = testData$TARGET,relevant=levels(testData$TARGET)[1])
caret::F_meas(data =as.factor(predrf22), reference = testData$TARGET,relevant=levels(testData$TARGET)[1])
cfm <- caret::confusionMatrix(data=as.factor(predrf22), reference=testData$TARGET,positive="1")


# Extracts and plot variable importance (Mean Decrease in Gini Index)
varImpPlot(rf2)
importance = importance(rf2)
varImportance = data.frame(Variables = row.names(importance),
                           Importance =round(importance[, "MeanDecreaseGini"],2))
rankImportance=varImportance%>%mutate(Rank=paste('#',dense_rank(desc(Importance))))
ggplot(rankImportance,aes(x=reorder(Variables,Importance),
                          y=Importance,fill=Importance))+ 
   geom_bar(stat='identity') + 
   geom_text(aes(x = Variables, y = 0.5, label = Rank),
             hjust=0, vjust=0.55, size = 4, colour = 'white') +
   labs(x = 'Variables') +
   coord_flip() + 
   theme_classic()

#finding and measuaring fearture interactions & importance
features <- trainData %>% select( FLG.HAS.CC.0,OCCUPATION.SAL,AMT_L_DR,                
                                  OCCUPATION.PROF,AMT_BR_CSH_WDL_DR,AVG_AMT_PER_CSH_WDL_TXN, 
                                  OCCUPATION.SENP,BALANCE,HOLDING_PERIOD,          
                                  NO_OF_L_CR_TXNS,FLG.HAS.OLD.LOAN.1,     
                                  SCR,GENDER.F,AGE,                  
                                  LEN_OF_RLTN_IN_MNTH,ACC.TYPE.SA,NO_OF_BR_CSH_WDL_DR_TXNS,
                                  FLG.HAS.ANY.CHGS.0,FLG.HAS.OLD.LOAN.0,AVG_AMT_PER_NET_TXN,     
                                  AMT_CHQ_DR,AMT_NET_DR,              
                                  GENDER.M,NO_OF_CHQ_DR_TXNS,NO_OF_L_DR_TXNS,         
                                  OCCUPATION.SELF.EMP,FLG.HAS.NOMINEE.1,AMT_ATM_DR) 


X = trainData[,-c(1)]
predictor <- Predictor$new(rf2,data=X,y=trainData$TARGET)

imp = FeatureImp$new(predictor, loss = "ce")
plot(imp)

interact <- Interaction$new(predictor)
plot(interact)
interact2 <- Interaction$new(predictor, feature = "OCCUPATION.SAL")
plot(interact2)

##################
trainDataIndex <- createDataPartition(loan_data_encoded1h$TARGET, p=0.70, list = F)
trainData <- loan_data_encoded1h[trainDataIndex, ]
testData <- loan_data_encoded1h[-trainDataIndex, ]

table(trainData$TARGET)
table(testData$TARGET)

trainData$TARGET <- as.factor(trainData$TARGET)
testData$TARGET <- as.factor(testData$TARGET)

#rf on unbalanced data
set.seed(21)
rf2_unbal <- randomForest(
   TARGET ~ . ,
   data=trainData, 
   improtance = TRUE
   #localImp = TRUE
)
predrf2_unbal = predict(rf2_unbal, newdata=testData, type="prob")
predrf22_unbal <- ifelse(predrf2_unbal[,2] > 0.5, 1, 0)

accuracy <- table(testData$TARGET, predrf22_unbal)
sum(diag(accuracy))/sum(accuracy)

library(e1071)
caret::confusionMatrix(data=as.factor(predrf22_unbal), reference=testData$TARGET, positive = "1")$byClass

#ROC curve
library(pROC)
ROC_rf <- roc(testData$TARGET, predrf2[,2])
ROC_rf_unbal <- roc(testData$TARGET, predrf2_unbal[,2])

ROC_lr <- roc(testData$TARGET, predlog)
ROC_lr_withI <- roc(testData$TARGET, predlog)
ROC_lr_unbal <- roc(testData$TARGET, predlog)

ROC_lr_WoE <-  roc(testData$TARGET, predlog)
ROC_lr_WoE_withI <- roc(testData$TARGET, predlog)
ROC_lr_WoE_unbal <- roc(testData$TARGET, predlog)

ROC_lr_MM <- roc(testData$TARGET, predlog)
ROC_lr_MM_withI <- roc(testData$TARGET, predlog)
ROC_lr_MM_unbal <- roc(testData$TARGET, predlog)

# Area Under Curve (AUC) for each ROC curve 
ROC_rf_auc <- auc(ROC_rf)
ROC_rf_unbal_auc(ROC_rf_unbal)

ROC_lr_auc <- auc(ROC_lr)
ROC_lr_withI_auc <- auc(ROC_lr_withI)
ROC_lr_unbal_auc <- auc(ROC_lr_unbal)

ROC_lr_WoE_auc <- auc(ROC_lr_WoE)
ROC_lr_WoE_withI_auc <- auc(ROC_lr_WoE_withI)
ROC_lr_WoE_unbal_auc <- auc(ROC_lr_WoE_unbal)

ROC_lr_MM_auc <- auc(ROC_lr_MM)
ROC_lr_MM_withI_auc <- auc(ROC_lr_MM_withI)
ROC_lr_MM_unbal_auc <- auc(ROC_lr_MM_unbal)


#ploting ROC curve 

options(scipen=99)
ggroc(ROC_lr, legacy.axes = TRUE,col= "#009999") + 
   labs(x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") + 
   theme_minimal() +
   geom_abline(slope = 1, intercept = 0, size = 0.5)

###################


#plotting confusion matrix 
library(ggplot2)
library(scales)

ggplotConfusionMatrix <- function(m){
   mytitle <- paste("Accuracy", percent_format()(m$overall[1]),
                    "Kappa", percent_format()(m$overall[2]))
   p <-
      ggplot(data = as.data.frame(m$table) ,
             aes(x = Reference, y = Prediction)) +
      geom_tile(aes(fill = log(Freq)), colour = "white") +
      scale_fill_gradient(low = "white", high = "#99D8C9") +
      geom_text(aes(x = Reference, y = Prediction, label = Freq)) +
      theme(legend.position = "none") +
      ggtitle(mytitle)
   return(p)
}

ggplotConfusionMatrix(cfmlog)
ggplotConfusionMatrix(cfm)




