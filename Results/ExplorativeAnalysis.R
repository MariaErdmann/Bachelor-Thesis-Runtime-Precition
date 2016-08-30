# Explorative Analysis
source("~/Bachelor-Thesis-Runtime-Prediction/Results/helperFunctions.R")
source("~/Bachelor-Thesis-Runtime-Prediction/Results/formulas.R")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/finalresult.RData")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/ResultsDDesc.RData")
load("~/Bachelor-Thesis-Runtime-Prediction/class.dsets.RData")


library(xlsx)
library(xtable)
library(plyr)
library(ggplot2)
library(reshape)

# Run time and mmce overall and for each classifier

traintime.summary = ddply(big.res, "lrn.id", function(x) round(summary(x$timetrain.test.mean, digits = 6),4))
testtime.summary = ddply(big.res, "lrn.id", function(x) round(summary(x$timepredict.test.mean),4))
timeboth.summary = ddply(big.res, "lrn.id", function(x) round(summary(x$timeboth.test.mean),4))

traintime.mean = ddply(big.res, "lrn.id", function(x) round(mean(x$timetrain.test.mean),3))
testtime.mean = ddply(big.res, "lrn.id", function(x) round(mean(x$timepredict.test.mean),3))
timeboth.mean = ddply(big.res, "lrn.id", function(x) round(mean(x$timeboth.test.mean),3))

time.mean.all = merge(traintime.mean, testtime.mean, by = "lrn.id")
time.mean.all2 = merge(time.mean.all, timeboth.mean, by = "lrn.id")
colnames(time.mean.all2) = c("classifier", "traintime", "testtime", "timeboth")
time.mean.all2 = rbind(time.mean.all2, c("total", colMeans(time.mean.all2[2:4])))

# Boxplots
df = data.frame(res.ddesc$timetrain.test.mean, res.ddesc$lrn.id)
df.2 = melt(df, var.ids = names(df)[1])
upper.limit.train <- quantile(res.ddesc[res.ddesc$lrn.id == "gbm", "timetrain.test.mean"])[4] 
lower.limit.train <- quantile(res.ddesc[res.ddesc$lrn.id == "naiveBayes", "timetrain.test.mean"])[2]-1.5*IQR(res.ddesc[res.ddesc$lrn.id == "rpart","timepredict.test.mean"])
p1.timetrain = ggplot(df.2, aes(x=res.ddesc.lrn.id, y = value)) + geom_boxplot() + 
  coord_cartesian(ylim = c(lower.limit.train,upper.limit.train)) +
  labs(x = "classifier", y = "training time")


df.predict = data.frame(res.ddesc$timepredict.test.mean, classifier = res.ddesc$lrn.id)
df2.predict = melt(df.predict, var.ids = classifier)
upper.limit.pred <- quantile(res.ddesc[res.ddesc$lrn.id == "naiveBayes","timepredict.test.mean"])[4]
lower.limit.pred <- quantile(res.ddesc[res.ddesc$lrn.id == "rpart","timepredict.test.mean"])[4]-1.5*IQR(res.ddesc[res.ddesc$lrn.id == "rpart","timepredict.test.mean"])
p2.timepredict = ggplot(df2.predict, aes(x=classifier, y = value)) + geom_boxplot() + 
  coord_cartesian(ylim = c(lower.limit.pred,upper.limit.pred)) +
  labs(x = "classifier", y = "prediction time")

ggplot(big.res, aes(lrn.id, timepredict.test.mean)) + geom_boxplot()
ggplot(big.res, aes(lrn.id, timeboth.test.mean)) + geom_boxplot()

# histograms and kernel density all in one
ggplot(big.res, aes(x=timetrain.test.mean)) + 
  geom_histogram(aes(y=..density..),    
    binwidth=10,
    colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + facet_wrap(~lrn.id, nrow = 3)

# Can't be visualised in a compact and reasonable way
# Histograms seperatedly
hist(big.res$timepredict.test.mean)
hist(big.res[big.res$lrn.id == "gbm",]$timepredict.test.mean)
hist(big.res[big.res$lrn.id == "glmnet",]$timepredict.test.mean)
hist(big.res[big.res$lrn.id == "naiveBayes",]$timepredict.test.mean)
hist(big.res[big.res$lrn.id == "nnet",]$timepredict.test.mean)
hist(big.res[big.res$lrn.id == "ranger",]$timepredict.test.mean)
hist(big.res[big.res$lrn.id == "rpart",]$timepredict.test.mean)


# Scatterplots
ggplot(res.ddesc, aes(x = NumberOfInstances, y = timetrain.test.mean)) + geom_point()
ggplot(res.ddesc, aes(x = NumberOfFeatures, y = timetrain.test.mean)) + geom_point()
ggplot(res.ddesc, aes(x = NumberOfClasses, y = timetrain.test.mean)) + geom_point()
ggplot(res.ddesc, aes(x = MajorityClassSize, y = timetrain.test.mean)) + geom_point()


# mmce
round(mean(big.res$mmce.test.mean, na.rm = T),3)
mmce.summary = ddply(big.res, "lrn.id", function(x) summary(x$mmce.test.mean)[1:6])
big.mmce.gbm = subset(res.ddesc, lrn.id == "gbm" & mmce.test.mean > 0.9)
big.mmce.naiveBayes = subset(res.ddesc, lrn.id == "naiveBayes" & mmce.test.mean > 0.9)
big.mmce.nnet = subset(res.ddesc, lrn.id == "nnet" & mmce.test.mean > 0.9)
big.mmce.rpart = subset(res.ddesc, lrn.id == "rpart" & mmce.test.mean > 0.9)

# Visuealisation of mmce
mmce.hist.vlinedata = ddply(big.res, "lrn.id", summarize, MmceMean = mean(mmce.test.mean, na.rm = TRUE))

big.res2 = big.res
big.res2$mmce = big.res2$mmce.test.mean
big.res2$classifier = big.res2$lrn.id

ggplot(big.res2, aes(x=mmce)) + 
  geom_histogram(aes(y=..density..),    
    binwidth=.05,
    colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  facet_wrap(~lrn.id, nrow = 3) +
  geom_vline(aes(xintercept=MmceMean),  mmce.hist.vlinedata, col="red", linetype = "dashed")

# Analysis of dataset properties
dataset.chars = rbind(
  NumberOfInstances = round(summary(class.dsets$NumberOfInstances),2),
  NumberOfFeatures = round(summary(class.dsets$NumberOfFeatures),2),
  NumberOfClasses = round(summary(class.dsets$NumberOfClasses),2),
  MajorityClassSize = round(summary(class.dsets$MajorityClassSize),2))

#### Feature Analysis ####

#### Ranger ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rangerdfmean.RData")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rangerdf.RData")

# Performance measures (timetrain, timepredict, timeboth, mmce) summary
ranger.summary = getTimeSummary(ranger.df.mean)

# Summary of predictors
getFeautureSummary(data = ranger.df.mean, preds = ranger.preds.test)
getFeautureSummaryTable(data = ranger.df.mean, preds = ranger.preds.test)


#### rpart ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rpartdfmean.RData")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rpartdf.RData")

rpart.summary = getTimeSummary(rpart.df.mean)

# Summary of predictors
getFeautureSummary(data = rpart.df.mean, preds = rpart.preds.test)
getFeautureSummaryTable(data = rpart.df.mean, preds = rpart.preds.test)


#### gbm ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/gbmdfmean.RData")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/gbmdf.RData")

gbm.summary = getTimeSummary(gbm.df.mean)
# gbm has 140 missing values in mmce, and after summarising replications 24 missings:
mis = big.res[is.na(big.res$mmce),]
mis2 = gbm.df.mean[is.na(gbm.df.mean$mmce.test.mean),]

getFeautureSummary(data = gbm.df.mean, preds = gbm.preds.test)
feat.summary = getFeautureSummaryTable(data = gbm.df.mean, preds = gbm.preds.test)

#### glmnet ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/glmnetdfmean.RData")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/glmnetdf.RData")

glmnet.summary = getTimeSummary(glmnet.df.mean)

getFeautureSummary(glmnet.df.mean, preds = glmnet.preds.test)
getFeautureSummaryTable(glmnet.df.mean, preds = glmnet.preds.test)


#### naiveBayes ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/naiveBayesdfmean.RData")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/naiveBayesdf.RData")

naiveBayes.summary = getTimeSummary(naiveBayes.df.mean)

getFeautureSummary(data = naiveBayes.df.mean, preds = naiveBayes.preds.test)
feat.summary.naiveBayes = getFeautureSummaryTable(data = naiveBayes.df.mean, preds = naiveBayes.preds.test)


#### nnet ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/nnetdfmean.RData")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/nnetdf.RData")

nnet.summary = getTimeSummary(nnet.df.mean)

getFeautureSummary(data = nnet.df.mean, preds = nnet.preds.test)
getFeautureSummaryTable(data = nnet.df.mean, preds = nnet.preds.test)


################################################################################

#### Analysis of Variability ####
# Analysis of variability can be either done with looking at the variation coeff.
# (set var.coef = TRUE) or the standard deviation (set var.coef = FALSE).
var.coef = TRUE

load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/ranger_jobid2.RData")

if (!var.coef) {
  sd.ranger = createDataFrameWithSd(data = ranger.df.new)
} else {
  sd.ranger = createDataFrameWithCv(data = ranger.df.new)
}

# traintime
ranger.var = c(mean.train.time = mean(big.res[big.res$lrn.id == "ranger",]$timetrain.test.mean,na.rm= T),summary(sd.ranger$timetrain.sd))
sd.ranger.by.task = ddply(sd.ranger, "name", function(x) round(summary(x$timetrain.sd)[1:7],3))
arrange(sd.ranger.by.task, desc(Mean))
arrange(sd.ranger.by.task, desc(Min.))
mean.ranger.by.task = ddply(ranger.df.mean, "name", function(x) round(summary(x$timetrain.test.mean)[1:7], 3))
arrange(mean.ranger.by.task, desc(Mean))

# testtime
ranger.var.pred = c(mean.prediction.time = mean(big.res[big.res$lrn.id == "ranger",]$timepredict.test.mean, na.rm = T),summary(sd.ranger$timepredict.sd))
sd.ranger.pred.by.task = ddply(sd.ranger, "name", function(x) round(summary(x$timepredict.sd)[1:7], 3))
arrange(sd.ranger.pred.by.task, desc(Mean))
arrange(sd.ranger.by.task, desc(Min.))
mean.pred.ranger.by.task = ddply(ranger.df.mean, "name", function(x) round(summary(x$timepredict.test.mean)[1:7],3))
arrange(mean.pred.ranger.by.task, desc(Mean))

# mmce
ranger.var.mmce = c(mean.mmce = mean(big.res[big.res$lrn.id == "ranger",]$mmce.test.mean),summary(sd.ranger$mmce.sd))
sd.ranger.mmce.by.task = ddply(sd.ranger, "name", function(x) round(summary(x$mmce.sd)[1:7], 3))
arrange(sd.ranger.mmce.by.task, desc(Mean))
arrange(sd.ranger.by.task, desc(Min.))
mean.mmce.ranger.by.task = ddply(ranger.df.mean, "name", function(x) round(summary(x$mmce.test.mean)[1:7],3))
arrange(mean.mmce.ranger.by.task, desc(Mean))

#### rpart ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rpart_jobid2.RData")

if (!var.coef) {
  rpart.sd = createDataFrameWithSd(data = rpart.df.new)
} else {
  rpart.sd = createDataFrameWithCv(data = rpart.df.new)
}

# train time
rpart.var = c(mean.train.time = mean(rpart.df.mean$timetrain.test.mean), summary(rpart.sd$timetrain.sd))
sd.rpart.by.task = ddply(rpart.sd, "name", function(x) round(summary(x$timetrain.sd),3))
arrange(sd.rpart.by.task, desc(Mean))
mean.rpart.by.task = ddply(rpart.df.mean, "name", function(x) round(summary(x$timetrain.test.mean),3))
arrange(mean.rpart.by.task, desc=(Mean))

# test time
rpart.var.pred = c(mean.prediction.time = mean(rpart.df.mean$timepredict.test.mean), summary(rpart.sd$timepredict.sd))
sd.rpart.pred.by.task = ddply(rpart.sd, "name", function(x) round(summary(x$timepredict.sd),3))
arrange(sd.rpart.pred.by.task, desc(Mean))
mean.rpart.pred.by.task = ddply(rpart.df.mean, "name", function(x) round(summary(x$timepredict.test.mean),3))
arrange(mean.rpart.pred.by.task, desc=(Mean))

# mmce
# test time
rpart.var.mmce = c(mean.mmce = mean(rpart.df.mean$mmce.test.mean), summary(rpart.sd$mmce.sd))
sd.rpart.mmce.by.task = ddply(rpart.sd, "name", function(x) {
  x = na.omit(x$mmce.sd)
  y = round(summary(x),3)
}) 
arrange(sd.rpart.mmce.by.task, desc(Mean))
mean.rpart.mmce.by.task = ddply(rpart.df.mean, "name", function(x) round(summary(x$mmce.test.mean),3))
arrange(mean.rpart.mmce.by.task, desc=(Mean))


#### gbm ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/gbm_jobid2.RData")

if (!var.coef) {
  sd.gbm = createDataFrameWithSd(gbm.df.new)
} else {
  sd.gbm = createDataFrameWithCv(data = gbm.df.new)
}

# train time
gbm.var = c(mean.train.time = mean(big.res[big.res$lrn.id == "gbm", ]$timetrain.test.mean), summary(sd.gbm$timetrain.sd))
sd.gbm.by.task = ddply(sd.gbm, "name", function(x) round(summary(x$timetrain.sd)[1:7],3))
arrange(sd.gbm.by.task, desc(Mean))
mean.gbm.by.task = ddply(gbm.df.mean, "name", function(x) round(summary(x$timetrain.test.mean)[1:7],3))
arrange(mean.gbm.by.task, desc(Mean))

# prediction time
gbm.var.pred = c(mean.prediction.time = mean(big.res[big.res$lrn.id == "gbm", ]$timepredict.test.mean), summary(sd.gbm$timepredict.sd))
sd.gbm.pred.by.task = ddply(sd.gbm, "name", function(x) round(summary(x$timepredict.sd)[1:7],3))
arrange(sd.gbm.pred.by.task, desc(Mean))
mean.gbm.pred.by.task = ddply(gbm.df.mean, "name", function(x) round(summary(x$timepredict.test.mean)[1:7],3))
arrange(mean.gbm.pred.by.task, desc(Mean))

# mmce
gbm.var.mmce = c(mean.mmce.time = mean(big.res[big.res$lrn.id == "gbm", ]$mmce.test.mean, na.rm = T), summary(sd.gbm$mmce.sd))
sd.gbm.mmce.by.task = ddply(sd.gbm, "name", function(x) round(summary(x$mmce.sd)[1:7],3))
arrange(sd.gbm.mmce.by.task, desc(Mean))
mean.gbm.mmce.by.task = ddply(gbm.df.mean, "name", function(x) round(summary(x$mmce.test.mean)[1:7],3))
arrange(mean.gbm.mmce.by.task, desc(Mean))

#### glmnet ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/glmnet_jobid2.RData")

if (!var.coef) {
  sd.glmnet = createDataFrameWithSd(data = glmnet.df.new)
} else {
  sd.glmnet = createDataFrameWithCv(data = glmnet.df.new)
}

# train time
glmnet.var = c(mean.train.time = mean(big.res[big.res$lrn.id == "glmnet",]$timetrain.test.mean), summary(sd.glmnet$timetrain.sd))
sd.glmnet.by.task = ddply(sd.glmnet, "name", function(x) round(summary(x$timetrain.sd)[1:7],3))
arrange(sd.glmnet.by.task, desc(Mean))
mean.glmnet.by.task = ddply(glmnet.df.mean, "name", function(x) round(summary(x$timetrain.test.mean)[1:7],2))
arrange(mean.glmnet.by.task, desc(Mean))

# prediction time
glmnet.var.pred = c(mean.prediction.time = mean(big.res[big.res$lrn.id == "glmnet",]$timepredict.test.mean), summary(sd.glmnet$timepredict.sd))
sd.glmnet.pred.by.task = ddply(sd.glmnet, "name", function(x) round(summary(x$timepredict.sd)[1:7],3))
arrange(sd.glmnet.pred.by.task, desc(Mean))
mean.glmnet.pred.by.task = ddply(glmnet.df.mean, "name", function(x) round(summary(x$timepredict.test.mean)[1:7],2))
arrange(mean.glmnet.pred.by.task, desc(Mean))

# mmce
glmnet.var.mmce = c(mean.mmce = mean(big.res[big.res$lrn.id == "glmnet",]$mmce.test.mean), summary(sd.glmnet$mmce.sd))
sd.glmnet.mmce.by.task = ddply(sd.glmnet, "name", function(x) round(summary(x$mmce.sd)[1:7],3))
arrange(sd.glmnet.mmce.by.task, desc(Mean))
mean.glmnet.mmce.by.task = ddply(glmnet.df.mean, "name", function(x) round(summary(x$mmce.test.mean)[1:7],2))
arrange(mean.glmnet.mmce.by.task, desc(Mean))

#### naiveBayes ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/naiveBayes_jobid2.RData")

if (!var.coef) {
  sd.naiveBayes = createDataFrameWithSd(data = naiveBayes.df.new)
} else {
  sd.naiveBayes = createDataFrameWithCv(data = naiveBayes.df.new)
}

# train time
naiveBayes.var = c(mean.train.time = mean(big.res[big.res$lrn.id == "naiveBayes",]$timetrain.test.mean),summary(sd.naiveBayes$timetrain.sd))
sd.naiveBayes.by.task = ddply(sd.naiveBayes, "name", function(x) round(summary(x$timetrain.sd)[1:7],2))
arrange(sd.naiveBayes.by.task, desc(Mean))
mean.naiveBayes.by.task = ddply(naiveBayes.df.mean, "name", function(x) round(summary(x$timetrain.test.mean)[1:7],2))
arrange(mean.naiveBayes.by.task, desc(Mean))

# prediction time
naiveBayes.var.pred = c(mean.prediction.time = mean(big.res[big.res$lrn.id == "naiveBayes",]$timepredict.test.mean),summary(sd.naiveBayes$timepredict.sd))
sd.naiveBayes.pred.by.task = ddply(sd.naiveBayes, "name", function(x) round(summary(x$timepredict.sd)[1:7],2))
arrange(sd.naiveBayes.pred.by.task, desc(Mean))
mean.naiveBayes.pred.by.task = ddply(naiveBayes.df.mean, "name", function(x) round(summary(x$timepredict.test.mean)[1:7],2))
arrange(mean.naiveBayes.pred.by.task, desc(Mean))

# mmce
naiveBayes.var.mmce = c(mean.mmce= mean(big.res[big.res$lrn.id == "naiveBayes",]$mmce.test.mean),summary(sd.naiveBayes$mmce.sd))
sd.naiveBayes.mmce.by.task = ddply(sd.naiveBayes, "name", function(x) round(summary(x$mmce.sd)[1:7],2))
arrange(sd.naiveBayes.mmce.by.task, desc(Mean))
mean.naiveBayes.mmce.by.task = ddply(naiveBayes.df.mean, "name", function(x) round(summary(x$mmce.test.mean)[1:7],2))
arrange(mean.naiveBayes.mmce.by.task, desc(Mean))


#### nnet #### 
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/nnet_jobid2.RData")

if(!var.coef) {
  sd.nnet = createDataFrameWithSd(data = nnet.df.new)
} else {
  sd.nnet = createDataFrameWithCv(data = nnet.df.new)
}

# time train
nnet.var = c(mean.train.time = mean(big.res[big.res$lrn.id == "nnet",]$timetrain.test.mean), summary(sd.nnet$timetrain.sd))
sd.nnet.by.task = ddply(sd.nnet, "name", function(x) round(summary(x$timetrain.sd)[1:7],3))
arrange(sd.nnet.by.task, desc(Mean))
mean.nnet.by.task = ddply(nnet.df.mean, "name", function(x) round(summary(x$timetrain.test.mean)[1:7],3))
arrange(mean.nnet.by.task, desc(Mean))

# prediction train
nnet.var.pred = c(mean.prediction.time = mean(big.res[big.res$lrn.id == "nnet",]$timepredict.test.mean), summary(sd.nnet$timepredict.sd))
sd.nnet.pred.by.task = ddply(sd.nnet, "name", function(x) round(summary(x$timepredict.sd)[1:7],3))
arrange(sd.nnet.pred.by.task, desc(Mean))
mean.nnet.pred.by.task = ddply(nnet.df.mean, "name", function(x) round(summary(x$timepredict.test.mean)[1:7],3))
arrange(mean.nnet.pred.by.task, desc(Mean))

# mmce
nnet.var.mmce = c(mean.mmce = mean(big.res[big.res$lrn.id == "nnet",]$mmce.test.mean), summary(sd.nnet$mmce.sd))
sd.nnet.mmce.by.task = ddply(sd.nnet, "name", function(x) round(summary(x$mmce.sd)[1:7],3))
arrange(sd.nnet.mmce.by.task, desc(Mean))
mean.nnet.mmce.by.task = ddply(nnet.df.mean, "name", function(x) round(summary(x$mmce.test.mean)[1:7],3))
arrange(mean.nnet.mmce.by.task, desc(Mean))

# Variability of timetrain summary
var.traintime = rbind(gbm = gbm.var[1:7], glmnet = glmnet.var[1:7],naiveBayes = naiveBayes.var[1:7],nnet = nnet.var[1:7],ranger = ranger.var[1:7],rpart = rpart.var[1:7])

sd.df.boxplot = rbind(
data.frame(classifier = rep("ranger", length(sd.ranger.by.task$Mean)), mean.sd = sd.ranger.by.task$Mean),
data.frame(classifier = rep("rpart", length(sd.rpart.by.task$Mean)), mean.sd = sd.rpart.by.task$Mean),
data.frame(classifier = rep("gbm", length(sd.gbm.by.task$Mean)), mean.sd = sd.gbm.by.task$Mean),
data.frame(classifier = rep("glmnet", length(sd.glmnet.by.task$Mean)), mean.sd = sd.glmnet.by.task$Mean),
data.frame(classifier = rep("naiveBayes",length(sd.naiveBayes.by.task$Mean)), mean.sd = sd.naiveBayes.by.task$Mean),
data.frame(classifier = rep("nnet", length(sd.nnet.by.task$Mean)), mean.sd = sd.nnet.by.task$Mean))

ggplot(sd.df.boxplot, aes(classifier, mean.sd)) + 
geom_boxplot() +
stat_summary(fun.y = "mean", geom = "point", shape= 23, size= 1, fill= "white") +
ylab("cv of train time")  

# test time
var.testtime = rbind(gbm = gbm.var.pred[1:7], glmnet = glmnet.var.pred[1:7],naiveBayes = naiveBayes.var.pred[1:7],nnet = nnet.var.pred[1:7],ranger = ranger.var.pred[1:7],rpart = rpart.var.pred[1:7])

sd.df.boxplot.pred = rbind(data.frame(classifier = rep("ranger", length(sd.ranger.pred.by.task$Mean)), mean.sd = sd.ranger.pred.by.task$Mean),
  data.frame(classifier = rep("rpart", length(sd.rpart.pred.by.task$Mean)), mean.sd = sd.rpart.pred.by.task$Mean),
  data.frame(classifier = rep("gbm", length(sd.gbm.pred.by.task$Mean)), mean.sd = sd.gbm.pred.by.task$Mean),
  data.frame(classifier = rep("glmnet", length(sd.glmnet.pred.by.task$Mean)), mean.sd = sd.glmnet.pred.by.task$Mean),
  data.frame(classifier = rep("naiveBayes",length(sd.naiveBayes.pred.by.task$Mean)), mean.sd = sd.naiveBayes.pred.by.task$Mean),
  data.frame(classifier = rep("nnet", length(sd.nnet.pred.by.task$Mean)), mean.sd = sd.nnet.pred.by.task$Mean))

ggplot(sd.df.boxplot.pred, aes(classifier, mean.sd)) + 
  geom_boxplot() +
  stat_summary(fun.y = "mean", geom = "point", shape= 23, size= 1, fill= "white")+
  ylab("cv of prediction time")

# mmce
var.mmce = rbind(gbm = gbm.var.mmce[1:7], glmnet = glmnet.var.mmce[1:7],naiveBayes = naiveBayes.var.mmce[1:7],nnet = nnet.var.mmce[1:7],ranger = ranger.var.pred[1:7],rpart = rpart.var.mmce[1:7])
round(mean(var.mmce[,5]), 3)

sd.df.boxplot.mmce = rbind(data.frame(classifier = rep("ranger", length(sd.ranger.mmce.by.task$Mean)), mean.sd = sd.ranger.mmce.by.task$Mean),
  data.frame(classifier = rep("rpart", length(sd.rpart.mmce.by.task$Mean)), mean.sd = sd.rpart.mmce.by.task$Mean),
  data.frame(classifier = rep("gbm", length(sd.gbm.mmce.by.task$Mean)), mean.sd = sd.gbm.mmce.by.task$Mean),
  data.frame(classifier = rep("glmnet", length(sd.glmnet.mmce.by.task$Mean)), mean.sd = sd.glmnet.mmce.by.task$Mean),
  data.frame(classifier = rep("naiveBayes",length(sd.naiveBayes.mmce.by.task$Mean)), mean.sd = sd.naiveBayes.mmce.by.task$Mean),
  data.frame(classifier = rep("nnet", length(sd.nnet.mmce.by.task$Mean)), mean.sd = sd.nnet.mmce.by.task$Mean))

ggplot(sd.df.boxplot.mmce, aes(classifier, mean.sd)) + 
  geom_boxplot() +
  stat_summary(fun.y = "mean", geom = "point", shape= 23, size= 1, fill= "white") +
  ylab("cv of mmce")
