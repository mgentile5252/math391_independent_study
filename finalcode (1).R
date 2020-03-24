## LOAD DATA
test_path<-"./repositories/data-analysis-ii/final/testing1.csv"
train_path<-"./repositories/data-analysis-ii/final/training1.csv"
dat_test<-read.csv(test_path, header = TRUE,stringsAsFactors=FALSE)
dat_test$culture_values<-as.factor(dat_test$culture_values)
dat_full<-read.csv(train_path, header = TRUE,stringsAsFactors=FALSE)
dat_full$culture_values<-as.factor(dat_full$culture_values)
library("dplyr")
dat_train<-sample_n(dat_full, nrow(dat_full) * 0.25)
dat_train$culture_values<-as.factor(dat_train$culture_values)

## PRELIMINARY TESTING
names(dat_train)
head(dat_train)

# MOOD MEDIAN TESTS
library(RVAideMemoire)
inputs=rep(NA,115-34)
pvals=rep(0,115-34)
for (i in seq(34,115)) {
  inputs[i-33] = names(dat_train)[i]
  pvals[i-33] = mood.medtest(dat_train[,i]~culture_values,data=dat_train)[5]
}
dat_stats<-data.frame(Inputs=inputs)
dat_stats$P_Values<-pvals
dat_stats<-dat_stats[order(as.numeric(dat_stats$P_Values)),]

## MEAN BOOTSTRAP FUNCTION
library(boot)
meanfun <- function(data, i){
  d <- data[i]
  return(mean(d, na.rm=TRUE))   
}


# BOX PLOTS - modular
par(mfrow=c(1,3))
for (i in seq(35,115)) {
  ## subset data
  temp = dat_train[,i]
  title = names(dat_train)[i]
  for (i in seq(0,2)) {
    ## subset by predictor
    group_idx <- which(temp == i)
    subset <- dat_train[group_idx,]
    
    ## calculate group mean
    subset_mean <- mean(as.numeric(subset$culture_values), na.rm=TRUE)
    
    ## bootstrap mean
    bo <- boot(as.numeric(subset$culture_values), statistic=meanfun, R=100)
    bo.ci <- boot.ci(bo, conf=0.95, type="basic")
    
    ## plot boxplots
    boxplot(as.numeric(subset$culture_values), main=paste(title,i, sep=" "),ylab="Culture Value")
    abline(h = subset_mean, col="blue", lwd=2)
    abline(h = bo.ci$basic[4], col="blue", lty=2, lwd=2)
    abline(h = bo.ci$basic[5], col="blue", lty=2, lwd=2)
  }
}


#######################################################
######### ORDINAL LOGISTIC REGRESSION  ################ 
#######################################################
library("MASS")
##input_form<-paste(unlist(mood_tests$Inputs), collapse='+')
mod<-polr(culture_values~flexible.processes + performance.general + inclusivity.general + safety + red.tape + female.friendly + 
bureaucracy + performance.process + goals + available.resources + integrity + CUSTOMER + time.horizon + perks + quality.colleagues + 
tech.innovation + innovation.process + simplicity + voice.of.the.employee + benefits + COLLABORATION + learning + prioritize + 
candid.discussions + PERFORMANCE + execution.general + culture.general + cross.unit + mission +
collaboration.general + PC + intensity.positive + INCLUSIVITY + feedback + job.security + disabled.friendly + friendly + empowerment + 
fairness + not.agist + challenging.work + engaged.employees + EXECUTION + high.performers + senior.leaders + digital + ambition + 
strategy.general  + RESPECT + INTEGRITY + operational.excellence + connections + risk.taking + AGILITY + minority.friendly + 
work.life.balance + accountability + integrate.acquisitions + honesty + speed + INNOVATION + project.management + agility.general + 
communicate.strategy + ENGAGEMENT + trust + compensation + innovative.products + strategic.consistency + creativity + 
religious.tolerance + illegal + lgbt.friendly + intensity.negative + stretch.goals + underperformers + change.ready + 
innovation.general,data=dat_train,Hess=TRUE)
summary(mod)

## compute predictions
pred<-predict(mod,dat_test)
ord.acc <- calcAccuracy("Ord. Full",pred,dat_test$culture_values)

## find best predictors
mod.coeff <- coef(mod)[order(abs(coef(mod)))]

## ORDERED LOGISTIC REGERSSION
mod.red<-polr(culture_values~religious.tolerance+PC+culture.general+RESPECT+integrity+feedback+
minority.friendly+intensity.positive+trust+illegal+mission+voice.of.the.employee+honesty+lgbt.friendly,
data=dat_train,Hess=TRUE)
summary(mod.red)

## compute predictions
pred.red<-predict(mod.red,dat_test)
mod.red.acc <- calcAccuracy("Ord. Reduced 1",pred.red, dat_test$culture_values)


## ORDERED LOGISTIC REGERSSION 2
mod.red2<-polr(culture_values~intensity.negative +  illegal + religious.tolerance + ENGAGEMENT 
               +speed + honesty + minority.friendly + connections + integrity
               + RESPECT + senior.leaders + EXECUTION + challenging.work + not.agist
               + disabled.friendly + job.security + intensity.positive + mission + voice.of.the.employee
               + inclusivity.general,
              data=dat_train,Hess=TRUE)
summary(mod.red2)

## compute predictions
pred.red2<-predict(mod.red2,dat_test)
mod.red.acc2 <- calcAccuracy("Ord. Reduced 2",pred.red2, dat_test$culture_values)


#######################################################
#################### LDA + QDA ######################## 
#######################################################
library("MASS")
mod.lda<-lda(culture_values~flexible.processes + performance.general + inclusivity.general + safety + red.tape + female.friendly + 
               bureaucracy + performance.process + goals + available.resources + integrity + CUSTOMER + time.horizon + perks + quality.colleagues + 
               tech.innovation + innovation.process + simplicity + voice.of.the.employee + benefits + COLLABORATION + learning + prioritize + 
               candid.discussions + PERFORMANCE + execution.general + culture.general + cross.unit + mission +
               collaboration.general + PC + intensity.positive + INCLUSIVITY + feedback + job.security + disabled.friendly + friendly + empowerment + 
               fairness + not.agist + challenging.work + engaged.employees + EXECUTION + high.performers + senior.leaders + digital + ambition + 
               strategy.general  + RESPECT + INTEGRITY + operational.excellence + connections + risk.taking + AGILITY + minority.friendly + 
               work.life.balance + accountability + integrate.acquisitions + honesty + speed + INNOVATION + project.management + agility.general + 
               communicate.strategy + ENGAGEMENT + trust + compensation + innovative.products + strategic.consistency + creativity + 
               religious.tolerance + illegal + lgbt.friendly + intensity.negative + stretch.goals + underperformers + change.ready + 
               innovation.general,data=dat_full)
lda.pred<-predict(mod.lda, dat_test)$class
lda.acc<-calcAccuracy("LDA", lda.pred, dat_test$culture_values)


## QDA
mod.qda<-qda(culture_values~flexible.processes + performance.general + inclusivity.general + safety + red.tape + female.friendly + 
               bureaucracy + performance.process + goals + available.resources + integrity + CUSTOMER + time.horizon + perks + quality.colleagues + 
               tech.innovation + innovation.process + simplicity + voice.of.the.employee + benefits + COLLABORATION + learning + prioritize + 
               candid.discussions + PERFORMANCE + execution.general + culture.general + cross.unit + mission +
               collaboration.general + PC + intensity.positive + INCLUSIVITY + feedback + job.security + disabled.friendly + friendly + empowerment + 
               fairness + not.agist + challenging.work + engaged.employees + EXECUTION + high.performers + senior.leaders + digital + ambition + 
               strategy.general  + RESPECT + INTEGRITY + operational.excellence + connections + risk.taking + AGILITY + minority.friendly + 
               work.life.balance + accountability + integrate.acquisitions + honesty + speed + INNOVATION + project.management + agility.general + 
               communicate.strategy + ENGAGEMENT + trust + compensation + innovative.products + strategic.consistency + creativity + 
               religious.tolerance + illegal + lgbt.friendly + intensity.negative + stretch.goals + underperformers + change.ready + 
               innovation.general,data=dat_full)
qda.pred<-predict(mod.qda, dat_test)$class
qda.acc<-calcAccuracy("QDA",qda.pred, dat_test$culture_values)



#######################################################
#################### KNN Models ####################### 
#######################################################


#######################################################
###################### TREES ########################## 
#######################################################
library(rpart)
library(rpart.plot)
mod.tree <- rpart(culture_values ~ intensity.negative +  illegal + religious.tolerance 
              +speed + honesty + minority.friendly + connections + integrity + ENGAGEMENT
              + RESPECT + senior.leaders + EXECUTION + challenging.work + not.agist
              + disabled.friendly + job.security + intensity.positive + mission + voice.of.the.employee
              + inclusivity.general, data = dat_train, na.action=na.exclude, method = "class")
rpart.plot(mod.tree)
pred.tree <- predict(mod.tree,dat_test,type="class")
tree.acc<-calcAccuracy("Tree",pred.tree, dat_test$culture_values)

library("randomForest")
mod.forest = randomForest(culture_values~flexible.processes + performance.general + inclusivity.general + safety + red.tape + female.friendly + 
                            bureaucracy + performance.process + goals + available.resources + integrity + CUSTOMER + time.horizon + perks + quality.colleagues + 
                            tech.innovation + innovation.process + simplicity + voice.of.the.employee + benefits + COLLABORATION + learning + prioritize + 
                            candid.discussions + PERFORMANCE + execution.general + culture.general + cross.unit + mission +
                            collaboration.general + PC + intensity.positive + INCLUSIVITY + feedback + job.security + disabled.friendly + friendly + empowerment + 
                            fairness + not.agist + challenging.work + engaged.employees + EXECUTION + high.performers + senior.leaders + digital + ambition + 
                            strategy.general  + RESPECT + INTEGRITY + operational.excellence + connections + risk.taking + AGILITY + minority.friendly + 
                            work.life.balance + accountability + integrate.acquisitions + honesty + speed + INNOVATION + project.management + agility.general + 
                            communicate.strategy + ENGAGEMENT + trust + compensation + innovative.products + strategic.consistency + creativity + 
                            religious.tolerance + illegal + lgbt.friendly + intensity.negative + stretch.goals + underperformers + change.ready + 
                            innovation.general,data=dat_train, importance=TRUE, na.action=na.exclude)
mod.forest
pred.forest = predict(mod.forest,dat_test)
plot(pred.forest,dat_test$culture_values)
forest.acc<-calcAccuracy("Forest",pred.forest, dat_test$culture_values)



#######################################################
###################### SVMs  ########################## 
#######################################################
library(e1071)
mod.svm = svm(culture_values~flexible.processes + performance.general + inclusivity.general + safety + red.tape + female.friendly + 
                bureaucracy + performance.process + goals + available.resources + integrity + CUSTOMER + time.horizon + perks + quality.colleagues + 
                tech.innovation + innovation.process + simplicity + voice.of.the.employee + benefits + COLLABORATION + learning + prioritize + 
                candid.discussions + PERFORMANCE + execution.general + culture.general + cross.unit + mission +
                collaboration.general + PC + intensity.positive + INCLUSIVITY + feedback + job.security + disabled.friendly + friendly + empowerment + 
                fairness + not.agist + challenging.work + engaged.employees + EXECUTION + high.performers + senior.leaders + digital + ambition + 
                strategy.general  + RESPECT + INTEGRITY + operational.excellence + connections + risk.taking + AGILITY + minority.friendly + 
                work.life.balance + accountability + integrate.acquisitions + honesty + speed + INNOVATION + project.management + agility.general + 
                communicate.strategy + ENGAGEMENT + trust + compensation + innovative.products + strategic.consistency + creativity + 
                religious.tolerance + illegal + lgbt.friendly + intensity.negative + stretch.goals + underperformers + change.ready + 
                innovation.general,data=dat_test, kernel="linear", cost=10, scale=FALSE)
svm.pred <- predict(mod.svm, dat_test, na.action = na.exclude)
svm.acc <- calcAccuracy("SVC",svm.pred, dat_test$culture_values)



#######################################################
################### ACCURACIES  ####################### 
#######################################################
acc<-ord.acc
acc<-rbind(acc,mod.red.acc)
acc<-rbind(acc,mod.red.acc2)
acc<-rbind(acc,lda.acc)
acc<-rbind(acc,tree.acc)
acc<-rbind(acc,forest.acc)
acc<-rbind(acc,svm.acc)
par(mfrow=c(1,2))
barplot(acc$Accuracy, names.arg=acc$Model.Type,ylim=c(0,0.75),col=c("blue","red"),ylab="Accuracy",main="Model Accuracy")
barplot(acc$Adjacent, names.arg=acc$Model.Type,ylim=c(0,0.75),col=c("blue","red"),ylab="Adjacency", main="Model Adjacency")

#######################################################
##################### EFFECTS  ######################## 
#######################################################
library("effects")
mod.coeff
par(mfrow=c(2,2))
plot(effect("minority.friendly",mod.red),multiline=TRUE,rug=FALSE)
plot(effect("religious.tolerance",mod.red),multiline=TRUE,rug=FALSE)
plot(effect("RESPECT",mod.red),multiline=TRUE,rug=FALSE)
plot(effect("PC",mod.red),multiline=TRUE,rug=FALSE)
plot(effect("voice.of.the.employee",mod.red),multiline=TRUE,rug=FALSE)

#######################################################
################ Helper Functions  #################### 
#######################################################
calcAccuracy <- function(model_name,pred,y){
  na_idx <- which(is.na(y))
  ## percentage correct
  acc <- mean(as.numeric(pred == y), na.rm=TRUE)
  ## percentage adjacent
  pred_dist <- as.numeric(pred[-na_idx]) - as.numeric(y[-na_idx])
  adj_acc <- mean(as.numeric(abs(pred_dist) < 2),na.rm=TRUE)
  print(paste("adjacent accuracy: ", adj_acc))
  print(paste("accuracy: ", acc))
  ## prep return val
  x<-data.frame(model_name,acc,adj_acc)
  names(x)=c("Model.Type", "Accuracy", "Adjacent")
  return(x)
}
  
  
