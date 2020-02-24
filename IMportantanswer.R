View(NHANES)
str(NHANES)
#10000 obs. of  76 variables

# To check if the variables are NA in  more than 8000 records 
colSums(is.na(NHANES)) >8000
# Length, HeadCirc, BMICatUnder20yrs, UrineVol2, UrineFlow2, DiabetesAge,
#THHrsDayChild, CompHrsdayChild, AgeFirstarij, AgeRegMarij, PregnantNow


#2) Build a classification tree to predict SleepTrouble

class(NHANES$SleepTrouble) # factor 
levels(NHANES$SleepTrouble) # 2 levels--> No and Yes 
table(is.na(NHANES$SleepTrouble)) #2228 NA
table(NHANES$SleepTrouble) #No  Yes 
#5799 1973 


target<- NHANES[!is.na(NHANES$SleepTrouble), ]
str(target)


target2<-target[!is.na(target$SleepHrsNight), ]
str(target2)

target3<-target2[!is.na(target2$Depressed),]
str(target3)
#6690 variables


class(NHANES$SleepHrsNight) #integer
NHANES$SleepHrsNight
levels(as.factor((NHANES$SleepHrsNight)))
# 2 3 4 5 6 7 8 9 10 11 12
table(is.na(NHANES$SleepHrsNight)) #FALSE  TRUE 
#7755  2245
#2245 variables NA

# Since we have to build a classification tree, we must consider "more" or "less" levels for 
#SleepHrsNight



attach(NHANES)
Drama<- ifelse(SleepHrsNight >=7 ,"More","Less" )
NHANES<- data.frame(NHANES,Drama)

View(NHANES)
colnames(NHANES)[77]<- "SleepHrsResult"


class(NHANES$Depressed) # factor
levels(NHANES$Depressed) #none several most

table(is.na(NHANES$Depressed))

#FALSE  TRUE 
#6673  3327 
# 3327 NA

table(NHANES$Depressed)

#None Several    Most 
#5246    1009     418


View(NHANES)
str(NHANEs)



# Gini index is used for split
# Considering one rpart tree with entire data and one with train and test data




library(rpart)
set.seed(1234)
decis_tree<-rpart(SleepTrouble ~ SleepHrsNight+ Depressed, data = NHANES, parms = list(split = "gini"), control = rpart.control(minsplit = 0, minbucket = 0, cp = 0.01))
library(rpart.plot)
rpart.plot(decis_tree)
print(decis_tree)

install.packages("dplyr")
library(dplyr)

#70% training data, 30% testing data
NHANESindex <- sample(2, nrow(NHANES), replace = T, prob = c(0.7,0.3))
Training_NHANES <- NHANES[NHANESindex == 1, ] %>% filter(is.na(SleepTrouble)==F)
Testing_NHANES <- NHANES[NHANESindex == 2, ]%>% filter(is.na(SleepTrouble)==F)


decis_tree_rpartTraining<- rpart(SleepTrouble~SleepHrsNight+Depressed, data = Training_NHANES, parms = list(split = "gini"), control = rpart.control(minsplit = 0, minbucket = 0, cp = 0.01))
rpart.plot(decis_tree_rpartTraining)
print(decis_tree_rpartTraining)

# Prediction cases assumption
PredictNHANES<-predict(decis_tree_rpartTraining, Testing_NHANES, type="class")

MatrixNHANEStable<-table(Testing_NHANES$SleepTrouble, PredictNHANES)
MatrixNHANEStable
#    No    Yes
#No  1685   28
#Yes  534   55



#2)
print(decis_tree)
summary(decis_tree)

# Gini split is used.
# SleepHrsNight>=5.5, Depressed_None, Depressed =Several.Most are termina nodes
#

#3. 
table(NHANES$SleepTrouble)
#No  Yes 
#5799 1973

prop.table(table(NHANES$SleepTrouble))
# No     Yes 
#0.74614 0.25386 

#25% of proportion of NHANEs has sleepTrouble.
#1973 people have sleep trouble


#4
# To split the data into training set 75% and test set 25%
# Sampling should be done and 1,2 will be randomly allocated
#Replacement should be true

set.seed(1234)

Aindex<- sample(2,nrow(NHANES),replace= TRUE, prob=c(0.75,0.25))

train_nhanes<- NHANES[Aindex==1,] %>% filter(is.na(SleepTrouble)==F)
test_nhanes<- NHANES[Aindex==2,]%>% filter(is.na(SleepTrouble)==F)

decis_tree_trainnhanes <- rpart(SleepTrouble~SleepHrsNight+Depressed, data = train_nhanes, parms = list(split = "gini"),control = rpart.control(minsplit = 0, minbucket = 0, cp = 0.01))

rpart.plot(decis_tree_trainnhanes)

print(decis_tree_trainnhanes)

predictnhanes1<-predict(decis_tree_trainnhanes,test_nhanes)
predictnhanes1 # all probabilities of yes and no


predict_quest2<-predict(decis_tree_trainnhanes, newdata = test_nhanes, type = "prob")
predict_quest2


# confusion matrix for cut off point 0.5

conmat<-table(predict_quest2[,2] >= 0.5,test_nhanes$SleepTrouble)
conmat
row.names(conmat) <- c("No","Yes")
#        No    Yes
#No      1440  420
#Yes      26   57

# other calculations for conmat with 0.5 cut off
#True positive rate (sensitivity)

sensitivityconmat<-conmat[4]/(conmat[4] + conmat[3])
sensitivityconmat  #0.1194969


# True negative rate (specificity)

specificityconmat <- conmat[1]/(conmat[1] + conmat[2])
specificityconmat #0.9822647


# False positive rate
falsepositiverateconmat<- 1-specificityconmat
falsepositiverateconmat #0.01773533

# False negative rate


falsenegativerateconmat <- 1-sensitivityconmat
falsenegativerateconmat # 0.8805031



# Accuracy
accuracyconmat<- (conmat[1] + conmat[4])/sum(conmat)
accuracyconmat  #0.7704581



# confusion matrix for cut off point 0.25 ( as for people percentage with sleep trouble)

conmat2<-table(predict_quest2[,2] >= 0.25,test_nhanes$SleepTrouble)
conmat2
row.names(conmat2) <- c("No","Yes")
#        No    Yes
#No      1339  356
#Yes      127   121

# other calculations for conmat with 0.25 cut off
#True positive rate (sensitivity)

sensitivityconmat2<-conmat2[4]/(conmat2[4] + conmat2[3])
sensitivityconmat2  #0.2536688


# True negative rate (specificity)

specificityconmat2 <- conmat2[1]/(conmat2[1] + conmat2[2])
specificityconmat2  #0.9133697


# False positive rate
falsepositiverateconmat2<- 1-specificityconmat2
falsepositiverateconmat2 #0.08663029

# False negative rate


falsenegativerateconmat2 <- 1-sensitivityconmat2
falsenegativerateconmat2 #0.7463312



# Accuracy
accuracyconmat2<- (conmat2[1] + conmat2[4])/sum(conmat2)
accuracyconmat2  #0.7514153

#As cut off point changes, the values changes accordingly

#e) To create ROC curve 

install.packages("ROCR")
library(ROCR)


# 70% and 30% training and testing data Trainning_NHANES, TEsting_NHANEs, decision tree: decis_tree_rpartTraining

newpredict<- predict(decis_tree_rpartTraining,Testing_NHANES)
newpredict


newpredict2<-prediction(newpredict[,2],Testing_NHANES$SleepTrouble)
















