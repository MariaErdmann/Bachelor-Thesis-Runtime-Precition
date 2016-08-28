library(mboost)
library(randomForest)
library(xlsx)
library(ggplot2)
library(gridExtra)
library(grid)
library(xtable)
library(reshape)
source("~/Bachelor-Thesis-Runtime-Prediction/Results/formulas.R")
source("~/Bachelor-Thesis-Runtime-Prediction/Results/helperFunctions.R")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/nnetdfgam_test.RData")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/nnetdfgam_pred.RData")
nnet.df.mean$skip = as.factor(nnet.df.mean$skip)
nnet.df.gam.pred$skip = as.factor(nnet.df.gam.pred$skip)
nnet.df.gam.test$skip = as.factor(nnet.df.gam.test$skip)

set.seed(334)
opar = par()


#### RESPONSE = TRAIN-TIME ####

#### glmboost ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/nnet_glmboost_test.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_nnet_glmboost_test.rda")

# Appropriate mstop
mstop(cvm.nnet.glmboost.test) #99880
plot(cvm.nnet.glmboost.test)

# Coefficinet Paths
par(mar=c(5.1,4.1,4.1,10.1))
plot(nnet.glmboost.test, off2int = TRUE, main = "Analysis of training time on nnet
  - inclusion of intercept")

par(mar=c(5.1,4.1,4.1,10.1))
plot(nnet.glmboost.test,
  ylim = range(coef(nnet.glmboost.test, which = 2:29)),
  main = "Analysis of training time on nnet - \n adjusted y-scale")

# Coefficients
coefs.nnet.glmboost = data.frame(coef(nnet.glmboost.test, which = ""))
colnames(coefs.nnet.glmboost) = "coefficients"
save(coefs.nnet.glmboost, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/nnet/coefs_nnet_glmboost.rda")

# Variables that were not chosen into the model
not.selected.nnet.glm = setdiff(variable.names(nnet.glmboost.test), variable.names(nnet.glmboost.test, usedonly = TRUE))

# save predictions
predictions.glmboost.test = predict(nnet.glmboost.test, newdata = nnet.df.mean, type = "response")
save(predictions.glmboost.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/nnet/predictions_glmboost_test.rda")

# ggplot partial effects
p1.test = createCombinedPartialEffectPlots(input = nnet.df.mean$ObsForTrain,
  model = nnet.glmboost.test, which = "ObsForTrain", xlab = "Number of instances")
p2.test = createCombinedPartialEffectPlots(input = nnet.df.mean$NumberOfFeatures,
  model = nnet.glmboost.test, which = "NumberOfFeatures", xlab = "Number of features")
p3.test = createCombinedPartialEffectPlots(input = nnet.df.mean$NumberOfClasses,
  model = nnet.glmboost.test, which = "NumberOfClasses", xlab = "Number of classes")
p4.test = createCombinedPartialEffectPlots(input = nnet.df.mean$MajorityClassSize,
  model = nnet.glmboost.test, which = "MajorityClassSize", xlab = "Majority class size")
p5.test = createCombinedPartialEffectPlots(input = nnet.df.mean$size,
  model = nnet.glmboost.test, which = "size", xlab = "size")
p6.test = createCombinedPartialEffectPlots(input = nnet.df.mean$decay,
  model = nnet.glmboost.test, which = "decay", xlab = "decay")
p7.test = createCombinedPartialEffectPlots(input = nnet.df.mean$maxit,
  model = nnet.glmboost.test, which = "maxit", xlab = "maxit")
p8.test = createCombinedPartialEffectPlots(input = nnet.df.mean$skip,
  model = nnet.glmboost.test, which = "skip", xlab = "skip")
p8.test = p8.test + geom_errorbar(aes(y=part.effect, ymax=part.effect, ymin=part.effect), linetype="dashed", size = 1)
multiplot(p1.test, p2.test, p3.test, p4.test,p5.test,p6.test,p7.test, p8.test, cols = 2)


#### gamboost ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/nnet_gamboost_test.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_nnet_gamboost_test.rda")

# Appropriate mstop
mstop(cvm.nnet.gamboost.test) #99880
plot(cvm.nnet.gamboost.test)

# Save predictions
predictions_gamboost_test_nnet = predict(nnet.gamboost.test, newdata = nnet.df.gam.test, type = "response")
save(predictions_gamboost_test_nnet, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/nnet/predictions_gamboost_test_nnet.rda")

# Variables, not selected by the model
not.selected.nnet.gam.test= setdiff(variable.names(nnet.gamboost.test), variable.names(nnet.gamboost.test, usedonly = TRUE))

# ggplots: Single and combined partial effects
p1.gam.test = createSinglePartialEffectPlots(input = nnet.df.gam.test$ObsForTrain, 
  model = nnet.gamboost.test, which = "bols(ObsForTrain", xlab = "Number of instances")
p2.gam.test = createSinglePartialEffectPlots(input = nnet.df.gam.test$ObsForTrain, 
  model = nnet.gamboost.test, which = "bbs(ObsForTrain", xlab = "Number of instances")
p3.gam.test = createCombinedPartialEffectPlots(input = nnet.df.gam.test$ObsForTrain,
  model = nnet.gamboost.test, which = "ObsForTrain", xlab = "Number of instances")
multiplot(p1.gam.test, p2.gam.test, p3.gam.test, cols = 3)

p4.gam.test = createSinglePartialEffectPlots(input = nnet.df.gam.test$NumberOfFeatures, 
  model = nnet.gamboost.test, which = "bols(NumberOfFeatures", xlab = "Number of features")
p5.gam.test = createSinglePartialEffectPlots(input = nnet.df.gam.test$NumberOfFeatures, 
  model = nnet.gamboost.test, which = "bbs(NumberOfFeatures", xlab = "Number of features")
p6.gam.test = createCombinedPartialEffectPlots(input = nnet.df.gam.test$NumberOfFeatures,
  model = nnet.gamboost.test, which = "NumberOfFeatures", xlab = "Number of features")
multiplot(p4.gam.test, p5.gam.test, p6.gam.test, cols = 3)

p7.gam.test = createSinglePartialEffectPlots(input = nnet.df.gam.test$NumberOfClasses, 
  model = nnet.gamboost.test, which = "bols(NumberOfClasses", xlab = "Number of classes")
p8.gam.test = createSinglePartialEffectPlots(input = nnet.df.gam.test$NumberOfClasses, 
  model = nnet.gamboost.test, which = "bbs(NumberOfClasses", xlab = "Number of classes")
p9.gam.test = createCombinedPartialEffectPlots(input = nnet.df.gam.test$NumberOfClasses,
  model = nnet.gamboost.test, which = "NumberOfClasses", xlab = "Number of classes")
multiplot(p7.gam.test, p8.gam.test, p9.gam.test, cols = 3)

p10.gam.test = createSinglePartialEffectPlots(input = nnet.df.gam.test$MajorityClassSize, 
  model = nnet.gamboost.test, which = "bols(MajorityClassSize", xlab = "Majority class size")
p11.gam.test = createSinglePartialEffectPlots(input = nnet.df.gam.test$MajorityClassSize, 
  model = nnet.gamboost.test, which = "bbs(MajorityClassSize", xlab = "Majority class size")
p12.gam.test = createCombinedPartialEffectPlots(input = nnet.df.gam.test$MajorityClassSize,
  model = nnet.gamboost.test, which = "MajorityClassSize", xlab = "Majority class size")
multiplot(p10.gam.test, p11.gam.test, p12.gam.test, cols = 3)

p13.gam.test = createSinglePartialEffectPlots(input = nnet.df.gam.test$size, 
  model = nnet.gamboost.test, which = "bols(size", xlab = "size")
p14.gam.test = createSinglePartialEffectPlots(input = nnet.df.gam.test$size, 
  model = nnet.gamboost.test, which = "bbs(size", xlab = "size")
p15.gam.test = createCombinedPartialEffectPlots(input = nnet.df.gam.test$size,
  model = nnet.gamboost.test, which = "size", xlab = "size")
multiplot(p13.gam.test, p14.gam.test, p15.gam.test, cols = 3)

p16.gam.test = createSinglePartialEffectPlots(input = nnet.df.gam.test$decay, 
  model = nnet.gamboost.test, which = "bols(decay", xlab = "decay")
p17.gam.test = createSinglePartialEffectPlots(input = nnet.df.gam.test$decay, 
  model = nnet.gamboost.test, which = "bbs(decay", xlab = "decay")
p18.gam.test = createCombinedPartialEffectPlots(input = nnet.df.gam.test$decay,
  model = nnet.gamboost.test, which = "decay", xlab = "decay")
multiplot(p16.gam.test, p17.gam.test, p18.gam.test, cols = 3)

p19.gam.test = createSinglePartialEffectPlots(input = nnet.df.gam.test$maxit, 
  model = nnet.gamboost.test, which = "bols(maxit", xlab = "maxit")
p20.gam.test = createSinglePartialEffectPlots(input = nnet.df.gam.test$maxit, 
  model = nnet.gamboost.test, which = "bbs(maxit", xlab = "maxit")
p21.gam.test = createCombinedPartialEffectPlots(input = nnet.df.gam.test$maxit,
  model = nnet.gamboost.test, which = "maxit", xlab = "maxit")
multiplot(p19.gam.test, p20.gam.test, p21.gam.test, cols = 3)

p24.gam.test = createCombinedPartialEffectPlots(input = nnet.df.gam.test$skip,
  model = nnet.gamboost.test, which = "skip", xlab = "skip")
p24.gam.test = p24.gam.test + geom_errorbar(aes(y=part.effect, ymax=part.effect, ymin=part.effect), linetype="dashed", size = 1)

#### glm ####

nnet.glm.test = glm(nnet.glm.test.fmla, data = nnet.df.mean, family = Gamma(link = "log"))
step.nnet.glm.test = step(nnet.glm.test)
step.nnet.glm.test$anova
not.sel.glm.test = c("size", "skipTRUE", "I(MajorityClassSize^2)", "log(maxit)")
coefs.nnet.glm.test = summary(nnet.glm.test)$coefficients[,c(1,4)]
coefs.nnet.glm.test[not.sel.glm.test,] = rep(NA, length(not.sel.glm.test))
coefs.nnet.glm.test[complete.cases(coefs.nnet.glm.test), ] = summary(step.nnet.glm.test)$coefficients[,c(1,4)]
save(coefs.nnet.glm.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/nnet/coefs_nnet_glm_test.rda")

load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/nnet/coefs_nnet_glmboost.rda")
coefs.test = cbind(coefs.nnet.glmboost, coefs.nnet.glm.test)
save(coefs.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/nnet/coefs_test.rda")

predictions_glm_test = predict(step.nnet.glm.test, newdata = nnet.df.mean,type = "response")
save(predictions_glm_test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/nnet/predictions_glm_test.rda")

#### random forest ####
nnet.df.mean2 = nnet.df.mean
nnet.df.mean2$NumberOfInstances = nnet.df.mean2$ObsForTrain

nnet.rF.test = randomForest(timetrain.test.mean ~ NumberOfInstances +
    NumberOfFeatures + NumberOfClasses + MajorityClassSize + size + 
    skip + decay + maxit, data = nnet.df.mean2, importance = TRUE)

# save predictions
predictions.rF.test = predict(nnet.rF.test, newdata = nnet.df.mean2, type = "response")
save(predictions.rF.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/nnet/predictions_rF_test.rda")

plot(nnet.rF.test)

varUsed(nnet.rF.test, by.tree=FALSE, count=TRUE)

# Variable importance
imp.test = importance(nnet.rF.test)
varImpPlot(nnet.rF.test, main = NULL)

# Partial dependence plot
impvar.test <- rownames(imp.test)[order(imp.test[, 1], decreasing=TRUE)]
op <- par(mfrow=c(2, 4))
for (i in seq_along(impvar.test)) {
  partialPlot(nnet.rF.test, nnet.df.mean, impvar.test[i],
    xlab=impvar.test[i],ylim=c(0, 80), main = "")
}
par(op)


#### PREDICTION TIME ####

#### glmboost ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/nnet_glmboost_pred.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_nnet_glmboost_pred.rda")

# Appropriate mstop
mstop(cvm.nnet.glmboost.pred) # 97595
plot(cvm.nnet.glmboost.pred)

# Coefficinet Paths
par(mar=c(5.1,4.1,4.1,10.1))
plot(nnet.glmboost.pred, off2int = TRUE, main = "Analysis of prediction time on nnet
  - inclusion of intercept")

par(mar=c(5.1,4.1,4.1,10.1))
plot(nnet.glmboost.test,
  ylim = range(coef(nnet.glmboost.test, which = 2:23)),
  main = "Analysis of prediction on nnet - \n adjusted y-scale")

# Coefficients
coefs.nnet.glmboost.pred = data.frame(coef(nnet.glmboost.pred))
colnames(coefs.nnet.glmboost.pred) = "coefficients"

# Variables that were not chosen into the model
not.selected.nnet.glm.pred = setdiff(variable.names(nnet.glmboost.pred), variable.names(nnet.glmboost.pred, usedonly = TRUE))
coefs.nnet.glmboost.pred[not.selected.nnet.glm.pred,] = rep(NA, length(not.selected.nnet.glm.pred))

save(coefs.nnet.glmboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/coefs_nnet_glmboost_pred.rda")

# save predictions
predictions.glmboost.pred = predict(nnet.glmboost.pred, newdata = nnet.df.mean, type = "response")
save(predictions.glmboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/predictions_glmboost_pred.rda")


# ggplot partial effects
p1.pred = createCombinedPartialEffectPlots(input = nnet.df.mean$ObsForPred,
  model = nnet.glmboost.pred, which = "ObsForPred", xlab = "Number of instances")
p2.pred = createCombinedPartialEffectPlots(input = nnet.df.mean$NumberOfFeatures,
  model = nnet.glmboost.pred, which = "NumberOfFeatures", xlab = "Number of features")
p3.pred = createCombinedPartialEffectPlots(input = nnet.df.mean$NumberOfClasses,
  model = nnet.glmboost.pred, which = "NumberOfClasses", xlab = "Number of classes")
p4.pred = createCombinedPartialEffectPlots(input = nnet.df.mean$MajorityClassSize,
  model = nnet.glmboost.pred, which = "MajorityClassSize", xlab = "Majority class size")
p5.pred = createCombinedPartialEffectPlots(input = nnet.df.mean$size,
  model = nnet.glmboost.pred, which = "size", xlab = "size")
p6.pred = createCombinedPartialEffectPlots(input = nnet.df.mean$decay,
  model = nnet.glmboost.pred, which = "decay", xlab = "decay")
p7.pred = createCombinedPartialEffectPlots(input = nnet.df.mean$maxit,
  model = nnet.glmboost.pred, which = "maxit", xlab = "maxit")
p8.pred = createCombinedPartialEffectPlots(input = nnet.df.mean$skip,
  model = nnet.glmboost.pred, which = "skip", xlab = "skip")
p8.pred = p8.pred + geom_errorbar(aes(y=part.effect, ymax=part.effect, ymin=part.effect), linetype="dashed", size = 1)
multiplot(p1.pred, p2.pred, p3.pred, p4.pred,p5.pred,p6.pred,p7.pred, p8.pred, cols = 2)

#### gamboost ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/nnet_gamboost_pred.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_nnet_gamboost_pred.rda")

mstop(cvm.nnet.gamboost.pred) # 100 000
not.selected.nnet.gam.pred = setdiff(variable.names(nnet.gamboost.pred), variable.names(nnet.gamboost.pred, usedonly = TRUE))
# all variables selected

# save predictions
predictions.gam.pred = predict(nnet.gamboost.pred, newdata = nnet.df.gam.pred, type = "response")
save(predictions.gam.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/nnet/predictions_gam_pred.rda")

# ggplot for single and combined effects
# ggplot
p1.gam.pred = createSinglePartialEffectPlots(input = nnet.df.gam.pred$ObsForPred, 
  model = nnet.gamboost.pred, which = "bols(ObsForPred", xlab = "Number of instances")
p2.gam.pred = createSinglePartialEffectPlots(input = nnet.df.gam.pred$ObsForPred, 
  model = nnet.gamboost.pred, which = "bbs(ObsForPred", xlab = "Number of instances")
p3.gam.pred = createCombinedPartialEffectPlots(input = nnet.df.gam.pred$ObsForPred,
  model = nnet.gamboost.pred, which = "ObsForPred", xlab = "Number of instances")
multiplot(p1.gam.pred, p2.gam.pred, p3.gam.pred, cols = 3)

p4.gam.pred = createSinglePartialEffectPlots(input = nnet.df.gam.pred$NumberOfFeatures, 
  model = nnet.gamboost.pred, which = "bols(NumberOfFeatures", xlab = "Number of features")
p5.gam.pred = createSinglePartialEffectPlots(input = nnet.df.gam.pred$NumberOfFeatures, 
  model = nnet.gamboost.pred, which = "bbs(NumberOfFeatures", xlab = "Number of features")
p6.gam.pred = createCombinedPartialEffectPlots(input = nnet.df.gam.pred$NumberOfFeatures,
  model = nnet.gamboost.pred, which = "NumberOfFeatures", xlab = "Number of features")
multiplot(p4.gam.pred, p5.gam.pred, p6.gam.pred, cols = 3)

p7.gam.pred = createSinglePartialEffectPlots(input = nnet.df.gam.pred$NumberOfClasses, 
  model = nnet.gamboost.pred, which = "bols(NumberOfClasses", xlab = "Number of classes")
p8.gam.pred = createSinglePartialEffectPlots(input = nnet.df.gam.pred$NumberOfClasses, 
  model = nnet.gamboost.pred, which = "bbs(NumberOfClasses", xlab = "Number of classes")
p9.gam.pred = createCombinedPartialEffectPlots(input = nnet.df.gam.pred$NumberOfClasses,
  model = nnet.gamboost.pred, which = "NumberOfClasses", xlab = "Number of classes")
multiplot(p7.gam.pred, p8.gam.pred, p9.gam.pred, cols = 3)

p10.gam.pred = createSinglePartialEffectPlots(input = nnet.df.gam.pred$MajorityClassSize, 
  model = nnet.gamboost.pred, which = "bols(MajorityClassSize", xlab = "Majority class size")
p11.gam.pred = createSinglePartialEffectPlots(input = nnet.df.gam.pred$MajorityClassSize, 
  model = nnet.gamboost.pred, which = "bbs(MajorityClassSize", xlab = "Majority class size")
p12.gam.pred = createCombinedPartialEffectPlots(input = nnet.df.gam.pred$MajorityClassSize,
  model = nnet.gamboost.pred, which = "MajorityClassSize", xlab = "Majority class size")
multiplot(p10.gam.pred, p11.gam.pred, p12.gam.pred, cols = 3)

p13.gam.pred = createSinglePartialEffectPlots(input = nnet.df.gam.pred$size, 
  model = nnet.gamboost.pred, which = "bols(size", xlab = "size")
p14.gam.pred = createSinglePartialEffectPlots(input = nnet.df.gam.pred$size, 
  model = nnet.gamboost.pred, which = "bbs(size", xlab = "size")
p15.gam.pred = createCombinedPartialEffectPlots(input = nnet.df.gam.pred$size,
  model = nnet.gamboost.pred, which = "size", xlab = "size")
multiplot(p13.gam.pred, p14.gam.pred, p15.gam.pred, cols = 3)

p16.gam.pred = createSinglePartialEffectPlots(input = nnet.df.gam.pred$decay, 
  model = nnet.gamboost.pred, which = "bols(decay", xlab = "decay")
p17.gam.pred = createSinglePartialEffectPlots(input = nnet.df.gam.pred$decay, 
  model = nnet.gamboost.pred, which = "bbs(decay", xlab = "decay")
p18.gam.pred = createCombinedPartialEffectPlots(input = nnet.df.gam.pred$decay,
  model = nnet.gamboost.pred, which = "decay", xlab = "decay")
multiplot(p16.gam.pred, p17.gam.pred, p18.gam.pred, cols = 3)

p19.gam.pred = createSinglePartialEffectPlots(input = nnet.df.gam.pred$maxit, 
  model = nnet.gamboost.pred, which = "bols(maxit", xlab = "maxit")
p20.gam.pred = createSinglePartialEffectPlots(input = nnet.df.gam.pred$maxit, 
  model = nnet.gamboost.pred, which = "bbs(maxit", xlab = "maxit")
p21.gam.pred = createCombinedPartialEffectPlots(input = nnet.df.gam.pred$maxit,
  model = nnet.gamboost.pred, which = "maxit", xlab = "maxit")
multiplot(p19.gam.pred, p20.gam.pred, p21.gam.pred, cols = 3)

p24.gam.pred = createCombinedPartialEffectPlots(input = nnet.df.gam.pred$skip,
  model = nnet.gamboost.pred, which = "skip", xlab = "skip")
p24.gam.pred = p24.gam.pred + geom_errorbar(aes(y=part.effect, ymax=part.effect, ymin=part.effect), linetype="dashed", size = 1)

#### glm ####
nnet.glm.pred = glm(nnet.glm.pred.fmla, data = nnet.df.mean, family = Gamma(link = "log"))
step.nnet.glm.pred = step(nnet.glm.pred)
step.nnet.glm.pred$anova
not.sel.glm.pred = c("log(size)", "log(maxit)", "I(maxit^2)", "I(decay^2)",
  "sqrt(ObsForPred)", "I(MajorityClassSize^2)", "sqrt(maxit)") 
coefs.nnet.glm.pred = summary(nnet.glm.pred)$coefficients[,c(1,4)]
coefs.nnet.glm.pred[not.sel.glm.pred, ] = rep(NA, length(not.sel.glm.pred))
coefs.nnet.glm.pred[complete.cases(coefs.nnet.glm.pred),] = summary(step.nnet.glm.pred)$coefficients[,c(1,4)]
save(coefs.nnet.glm.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/nnet/coefs_nnet_glm_pred.rda")

# save coefficients
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/coefs_nnet_glmboost_pred.rda")
coefs.pred = cbind(coefs.nnet.glmboost.pred, coefs.nnet.glm.pred)
save(coefs.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/nnet/coefs_pred.rda")

# save predictions
predictions_glm_pred = predict(step.nnet.glm.pred, newdata = nnet.df.mean,type = "response")
save(predictions_glm_pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/nnet/nnet_predictions_glm_pred.rda")


#### random forest ####
nnet.df.mean3 = nnet.df.mean
nnet.df.mean3$NumberOfInstances = nnet.df.mean3$ObsForPred

nnet.rF.pred = randomForest(timepredict.test.mean ~ NumberOfInstances +
    NumberOfFeatures + NumberOfClasses + MajorityClassSize + size + skip + decay
  + maxit, data = nnet.df.mean3, importance = TRUE)

predictions.rF.pred = predict(nnet.rF.pred, newdata = nnet.df.mean3, type = "response")
save(predictions.rF.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/nnet/predictions_rF_pred.rda")

plot(nnet.rF.pred)

varUsed(nnet.rF.pred, by.tree=FALSE, count=TRUE)

# Variable importance
imp.pred = importance(nnet.rF.pred)
varImpPlot(nnet.rF.pred, main = NULL)

# Partial dependence plot
impvar.pred <- rownames(imp.pred)[order(imp.pred[, 1], decreasing=TRUE)]
op <- par(mfrow=c(2, 4))
for (i in seq_along(impvar.pred)) {
  partialPlot(nnet.rF.pred, nnet.df.mean, impvar.pred[i],
    xlab=impvar.pred[i],ylim=c(0, 0.1), main = "")
}
par(op)


#### Comparing models ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/nnet/coefs_test.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/nnet/coefs_pred.rda")
# Create dataframe with estimated coefficients for glmboost and glm on training 
# time and prediction time
both.coefs = cbind(coefs.test, coefs.pred)
save(both.coefs, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/nnet_coefs_both.rda")

# RMSE and RAE
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/nnet/predictions_glmboost_test.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/nnet/predictions_gamboost_test_nnet.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/nnet/predictions_glm_test.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/nnet/predictions_rF_test.rda")

load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/predictions_glmboost_pred.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/nnet/predictions_gam_pred.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/nnet/nnet_predictions_glm_pred.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/nnet/predictions_rF_pred.rda")

test.errors = calculateErrorsForComparison2(time = "training", glmboost.pred = predictions.glmboost.test,
  glmboost.data = nnet.df.mean, gamboost.pred = predictions_gamboost_test_nnet,
  gamboost.data = nnet.df.gam.test, glm.pred = predictions_glm_test,
  glm.data = nnet.df.mean, rF.pred = predictions.rF.test, rF.data = nnet.df.mean2)

pred.errors = calculateErrorsForComparison2(time = "training", glmboost.pred = predictions.glmboost.pred,
  glmboost.data = nnet.df.mean, gamboost.pred = predictions.gam.pred,
  gamboost.data = nnet.df.gam.pred, glm.pred = predictions_glm_pred,
  glm.data = nnet.df.mean, rF.pred = predictions.rF.pred, rF.data = nnet.df.mean3)

# obs vs. fitted
df = data.frame(timetrain = nnet.df.mean$timetrain.test.mean,
  glmboost = predictions.glmboost.test,
  gamboost = predictions_gamboost_test_nnet,
  glm = predictions_glm_test,
  randomForest = predictions.rF.test)
df2 = melt(df, id.vars = 1)
colnames(df2)[2] = "model"

df.pred = data.frame(timepredict = nnet.df.mean$timepredict.test.mean,
  glmboost = predictions.glmboost.pred,
  gamboost = predictions.gam.pred,
  glm = predictions_glm_pred,
  randomForest = predictions.rF.pred)
df.pred2 = melt(df.pred, id.vars = 1)
colnames(df.pred2)[2] = "model"

p1 =ggplot(data = df2, aes(timetrain,value,colour=model))+
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  scale_colour_manual(values=c("red","blue", "black", "green"))+
  labs(x="Observed training time",y="Fitted values") +
  theme(legend.position="none")

p2 =ggplot(data = df.pred2, aes(timepredict,value,colour=model))+
  geom_point(position=position_jitter(w=0.04,h=0.02),size=1.8) + 
  geom_abline(intercept = 0, slope = 1) +
  scale_colour_manual(values=c("red","blue", "black", "green"))+
  labs(x="Observed prediction time",y="Fitted values") +
  theme(legend.position="bottom")

grid_arrange_shared_legend(p1,p2,ncol=2)
