library(mboost)
library(rpart)
library(randomForest)
library(ggplot2)
library(xtable)
library(gridExtra)
library(grid)
library(reshape)
source("~/Bachelor-Thesis-Runtime-Prediction/Results/formulas.R")
source("~/Bachelor-Thesis-Runtime-Prediction/Results/helperFunctions.R")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rpartdfgam_pred.RData")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rpartdfgam_test.RData")
set.seed(334)
opar = par()

#### TRAINING TIME ####

#### glmboost ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/rpart_glmboost_test.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_rpart_glmboost_test.rda")

# Appropriate mstop
mstop(cvm.rpart.glmboost.test) # 47240
plot(cvm.rpart.glmboost.test)

par(mar=c(5.1,4.1,4.1,10.1))
plot(rpart.glmboost.test, off2int = TRUE, main = "Analysis of training time on rpart
  - inclusion of intercept")

par(mar=c(5.1,4.1,4.1,10.1))
plot(rpart.glmboost.test,
  ylim = range(coef(rpart.glmboost.test, which = 2:33)),
  main = "Analysis of training time on rpart - \n adjusted y-scale")
par(opar)

# Coefficients
coefs.rpart.glm = data.frame(coef(rpart.glmboost.test, which = ""))
colnames(coefs.rpart.glm) = "coefficients"
# Variables that were not chosen into the model
n.sel.glmboost.test = setdiff(variable.names(rpart.glmboost.test), variable.names(rpart.glmboost.test, usedonly = TRUE))
# sqrt(maxdepth)
coefs.rpart.glm[n.sel.glmboost.test, ] = rep(NA, length(n.sel.glmboost.test))

# save predictions
predictions.glmboost.test = predict(rpart.glmboost.test, newdata = rpart.df.mean,
  type = "response")
save(predictions.glmboost.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/rpart/predictions_glmboost_test.rda")

# ggplot: partial effects
p1.test = createCombinedPartialEffectPlots(input = rpart.df.mean$ObsForTrain,
  model = rpart.glmboost.test, which = "ObsForTrain", xlab = "Number of instances")
p2.test = createCombinedPartialEffectPlots(input = rpart.df.mean$NumberOfFeatures,
  model = rpart.glmboost.test, which = "NumberOfFeatures", xlab = "Number of features")
p3.test = createCombinedPartialEffectPlots(input = rpart.df.mean$NumberOfClasses,
  model = rpart.glmboost.test, which = "NumberOfClasses", xlab = "Number of classes")
p4.test = createCombinedPartialEffectPlots(input = rpart.df.mean$MajorityClassSize,
  model = rpart.glmboost.test, which = "MajorityClassSize", xlab = "Majority class size")
p5.test = createCombinedPartialEffectPlots(input = rpart.df.mean$maxdepth,
  model = rpart.glmboost.test, which = "maxdepth", xlab = "maxdepth")
p6.test = createCombinedPartialEffectPlots(input = rpart.df.mean$minsplit.trafo,
  model = rpart.glmboost.test, which = "minsplit.trafo", xlab = "minsplit")
p7.test = createCombinedPartialEffectPlots(input = rpart.df.mean$minbucket.trafo,
  model = rpart.glmboost.test, which = "minbucket.trafo", xlab = "minbucket")
p8.test = createCombinedPartialEffectPlots(input = rpart.df.mean$cp.trafo,
  model = rpart.glmboost.test, which = "cp.trafo", xlab = "cp")
mp.glmboost.test = multiplot(p1.test, p2.test, p3.test, p4.test,p5.test,p6.test,p7.test,p8.test, cols = 2)


#### gamboost ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_rpart_gamboost_test.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/rpart_gamboost_test.rda")

# mstop
mstop(cvm.rpart.gamboost.test) # 99973
plot(cvm.rpart.gamboost.test, main = NULL, cex.axis = 0.3, ylab = "Risk")

# Plot partial prediction plots
plot(rpart.gamboost.test)

# Coefficients
coef(rpart.gamboost.test)
coef.lin.eff.gamboost.test = round(data.frame(unlist(coef(rpart.gamboost.test, which = "bols"))),3)

# Not selected
not.selected.rpart.gam.test = setdiff(variable.names(rpart.gamboost.test), variable.names(rpart.gamboost.test, usedonly = TRUE))

# save predictions
predictions.gamboost.test = predict(rpart.gamboost.test, newdata = rpart.df.gam.test, type = "response")
save(predictions.gamboost.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/rpart/predictions_gamboost_test.rda")

# ggplot
p1.gam.test = createSinglePartialEffectPlots(input = rpart.df.gam.test$ObsForTrain, 
  model = rpart.gamboost.test, which = "bols(ObsForTrain", xlab = "Number of instances")
p2.gam.test = createSinglePartialEffectPlots(input = rpart.df.gam.test$ObsForTrain, 
  model = rpart.gamboost.test, which = "bbs(ObsForTrain", xlab = "Number of instances")
p3.gam.test = createCombinedPartialEffectPlots(input = rpart.df.gam.test$ObsForTrain,
  model = rpart.gamboost.test, which = "ObsForTrain", xlab = "Number of instances")
multiplot(p1.gam.test, p2.gam.test, p3.gam.test, cols = 3)

p4.gam.test = createSinglePartialEffectPlots(input = rpart.df.gam.test$NumberOfFeatures, 
  model = rpart.gamboost.test, which = "bols(NumberOfFeatures", xlab = "Number of features")
p5.gam.test = createSinglePartialEffectPlots(input = rpart.df.gam.test$NumberOfFeatures, 
  model = rpart.gamboost.test, which = "bbs(NumberOfFeatures", xlab = "Number of features")
p6.gam.test = createCombinedPartialEffectPlots(input = rpart.df.gam.test$NumberOfFeatures,
  model = rpart.gamboost.test, which = "NumberOfFeatures", xlab = "Number of features")
multiplot(p4.gam.test, p5.gam.test, p6.gam.test, cols = 3)

p7.gam.test = createSinglePartialEffectPlots(input = rpart.df.gam.test$NumberOfClasses, 
  model = rpart.gamboost.test, which = "bols(NumberOfClasses", xlab = "Number of classes")
p8.gam.test = createSinglePartialEffectPlots(input = rpart.df.gam.test$NumberOfClasses, 
  model = rpart.gamboost.test, which = "bbs(NumberOfClasses", xlab = "Number of classes")
p9.gam.test = createCombinedPartialEffectPlots(input = rpart.df.gam.test$NumberOfClasses,
  model = rpart.gamboost.test, which = "NumberOfClasses", xlab = "Number of classes")
multiplot(p7.gam.test, p8.gam.test, p9.gam.test, cols = 3)

p10.gam.test = createSinglePartialEffectPlots(input = rpart.df.gam.test$MajorityClassSize, 
  model = rpart.gamboost.test, which = "bols(MajorityClassSize", xlab = "Majority class size")
p11.gam.test = createSinglePartialEffectPlots(input = rpart.df.gam.test$MajorityClassSize, 
  model = rpart.gamboost.test, which = "bbs(MajorityClassSize", xlab = "Majority class size")
p12.gam.test = createCombinedPartialEffectPlots(input = rpart.df.gam.test$MajorityClassSize,
  model = rpart.gamboost.test, which = "MajorityClassSize", xlab = "Majority class size")
multiplot(p10.gam.test, p11.gam.test, p12.gam.test, cols = 3)

p13.gam.test = createSinglePartialEffectPlots(input = rpart.df.gam.test$maxdepth, 
  model = rpart.gamboost.test, which = "bols(maxdepth", xlab = "maxdepth")
p14.gam.test = createSinglePartialEffectPlots(input = rpart.df.gam.test$maxdepth, 
  model = rpart.gamboost.test, which = "bbs(maxdepth", xlab = "maxdepth")
p15.gam.test = createCombinedPartialEffectPlots(input = rpart.df.gam.test$maxdepth,
  model = rpart.gamboost.test, which = "maxdepth", xlab = "maxdepth")
multiplot(p13.gam.test, p14.gam.test, p15.gam.test, cols = 3)

p16.gam.test = createSinglePartialEffectPlots(input = rpart.df.gam.test$minbucket.trafo, 
  model = rpart.gamboost.test, which = "bols(minbucket.trafo", xlab = "minbucket")
p17.gam.test = createSinglePartialEffectPlots(input = rpart.df.gam.test$minbucket.trafo, 
  model = rpart.gamboost.test, which = "bbs(minbucket.trafo", xlab = "minbucket")
p18.gam.test = createCombinedPartialEffectPlots(input = rpart.df.gam.test$minbucket.trafo,
  model = rpart.gamboost.test, which = "minbucket.trafo", xlab = "minbucket")
multiplot(p16.gam.test, p17.gam.test, p18.gam.test, cols = 3)

p19.gam.test = createSinglePartialEffectPlots(input = rpart.df.gam.test$minsplit.trafo, 
  model = rpart.gamboost.test, which = "bols(minsplit.trafo", xlab = "minsplit")
p20.gam.test = createSinglePartialEffectPlots(input = rpart.df.gam.test$minsplit.trafo, 
  model = rpart.gamboost.test, which = "bbs(minsplit.trafo", xlab = "minsplit")
p21.gam.test = createCombinedPartialEffectPlots(input = rpart.df.gam.test$minsplit.trafo,
  model = rpart.gamboost.test, which = "minsplit.trafo", xlab = "minsplit")
multiplot(p19.gam.test, p20.gam.test, p21.gam.test, cols = 3)

p22.gam.test = createSinglePartialEffectPlots(input = rpart.df.gam.test$cp.trafo, 
  model = rpart.gamboost.test, which = "bols(cp.trafo", xlab = "cp")
p23.gam.test = createSinglePartialEffectPlots(input = rpart.df.gam.test$cp.trafo, 
  model = rpart.gamboost.test, which = "bbs(cp.trafo", xlab = "cp")
p24.gam.test= createCombinedPartialEffectPlots(input = rpart.df.gam.test$cp.trafo,
  model = rpart.gamboost.test, which = "cp.trafo", xlab = "cp")
multiplot(p22.gam.test, p23.gam.test, p24.gam.test, cols = 3)


#### glm ####

glm.rpart.test = glm(rpart.glm.test.fmla, data = rpart.df.mean, family = Gamma(link = "log"))

# creating coefficient frame
step.glm.rpart.test = step(glm.rpart.test, direction = "both")
step.glm.rpart.test$anova
step.glm.rpart.test$anova[,1]
n.sel.glm.test = c("NumberOfClasses", "I(NumberOfClasses^2)", "I(minbucket.trafo^2)",
  "I(minsplit.trafo^2)" ,"sqrt(maxdepth)", "log(ObsForTrain)", "log(MajorityClassSize)",
  "log(maxdepth)", "log(minsplit.trafo)")
coefs.glm.test.rpart = summary(glm.rpart.test)$coefficients[, c(1,4)]
coefs.glm.test.rpart[n.sel.glm.test, ] = rep(NA, length(n.sel.glm.test))
coefs.glm.test.rpart[complete.cases(coefs.glm.test.rpart), ] = summary(step.glm.rpart.test)$coefficients[ ,c(1,4)]

# Create dataframe with estimated coefficients for glmboost and glm on training time
coefs.train = cbind(coefs.rpart.glm, coefs.glm.test.rpart)
round(coefs.train[,2] - coefs.train[,1],5)

# save predictions
predictions.glm.test = predict(step.glm.rpart.test, newdata = rpart.df.mean, type = "response")
save(predictions.glm.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/rpart/predictions_glm_test.rda")

#### randomForest ####
# use ugly hack
rpart.df.mean2 = rpart.df.mean
rpart.df.mean2$NumberOfInstances = rpart.df.mean2$ObsForTrain
rpart.df.mean2$cp = rpart.df.mean2$cp.trafo
rpart.df.mean$minbucket = rpart.df.mean2$minbucket.trafo
rpart.df.mean2$minsplit = rpart.df.mean2$minsplit.trafo

rpart.rF.test = randomForest(timetrain.test.mean ~ MajorityClassSize +
    + NumberOfClasses + NumberOfFeatures + NumberOfInstances + maxdepth +
    cp + minsplit + minbucket, data = rpart.df.mean, importance = TRUE)
plot(rpart.rF.test)

# save predictions
predictions.rF.test = predict(rpart.rF.test, newdata = rpart.df.mean2, type = "response")
save(predictions.rF.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/rpart/predictions_rF_test.rda")

varUsed(rpart.rF.test, by.tree=FALSE, count=TRUE)

# Variable importance
imp.test = importance(rpart.rF.test)
varImpPlot(rpart.rF.test, main = NULL)

# Partial dependence plot
impvar.test <- rownames(imp.test)[order(imp.test[, 1], decreasing=TRUE)]
op <- par(mfrow=c(2, 4))
for (i in seq_along(impvar.test)) {
  partialPlot(rpart.rF.test, rpart.df.mean, impvar.test[i],
    xlab=impvar.test[i],ylim=c(0.05, 0.25), main = "")
}
par(op)

#### PREDICTION TIME ####

#### glmboost ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_rpart_glmboost_pred.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/rpart_glmboost_pred.rda")

# Appropriate mstop
mstop(cvm.rpart.glmboost.pred) # 99997
plot(cvm.rpart.glmboost.pred)

# Coefficinet Paths
par(mar=c(5.1,4.1,4.1,10.1))
plot(rpart.glmboost.pred, off2int = TRUE, main = "Analysis of prediction time on rpart
  - inclusion of intercept")

par(mar=c(5.1,4.1,4.1,10.1))
plot(rpart.glmboost.pred,
  ylim = range(coef(rpart.glmboost.pred, which = 2:33)),
  main = "Analysis of prediction time on rpart - \n adjusted y-scale")
par(opar)

# Coefficients
coefs.rpart.glmboost.pred = data.frame(coef(rpart.glmboost.pred, which = ""))
colnames(coefs.rpart.glmboost.pred) = "coefficients"
# Variables that were not chosen into the model
n.sel.glmboost.pred = setdiff(variable.names(rpart.glmboost.pred), variable.names(rpart.glmboost.pred, usedonly = TRUE))
coefs.rpart.glmboost.pred[n.sel.glmboost.pred, ] = rep(NA, length(n.sel.glmboost.pred))
save(coefs.rpart.glmboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/rpart/coef_glmboost_pred.rda")

# save predictions
predictions_glmboost_pred = predict(rpart.glmboost.pred, newdata = rpart.df.mean, type = "response")
save(predictions_glmboost_pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/rpart/predictions_glmboost_pred.rda")

# ggplot2
p1 = createCombinedPartialEffectPlots(input = rpart.df.mean$ObsForPred,
  model = rpart.glmboost.pred, which = "ObsForPred", xlab = "Number of instances")
p2 = createCombinedPartialEffectPlots(input = rpart.df.mean$NumberOfFeatures,
  model = rpart.glmboost.pred, which = "NumberOfFeatures", xlab = "Number of features")
p3 = createCombinedPartialEffectPlots(input = rpart.df.mean$NumberOfClasses,
  model = rpart.glmboost.pred, which = "NumberOfClasses", xlab = "Number of classes")
p4 = createCombinedPartialEffectPlots(input = rpart.df.mean$MajorityClassSize,
  model = rpart.glmboost.pred, which = "MajorityClassSize", xlab = "Majority class size")
p5 = createCombinedPartialEffectPlots(input = rpart.df.mean$maxdepth,
  model = rpart.glmboost.pred, which = "maxdepth", xlab = "maxdepth")
p6 = createCombinedPartialEffectPlots(input = rpart.df.mean$minsplit.trafo,
  model = rpart.glmboost.pred, which = "minsplit.trafo", xlab = "minsplit")
p7 = createCombinedPartialEffectPlots(input = rpart.df.mean$minbucket.trafo,
  model = rpart.glmboost.pred, which = "minbucket.trafo", xlab = "minbucket")
p8 = createCombinedPartialEffectPlots(input = rpart.df.mean$cp.trafo,
  model = rpart.glmboost.pred, which = "cp.trafo", xlab = "cp")

multiplot(p1, p2, p3, p4, cols=2)
multiplot(p5, p6, p7, p8, cols=2)
multiplot(p1, p2, p3, p4,p5,p6,p7,p8, cols = 2)

### gamboost ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_rpart_gamboost_pred.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/rpart_gamboost_pred.rda")

# mstop
mstop(cvm.rpart.gamboost.pred) #99964
plot(cvm.rpart.gamboost.pred)

# Partial prediction plots
plot(rpart.gamboost.pred)

# Selected frequencies
sel.freq.rpart.gam.pred = table(rpart.gamboost.pred$xselect()[1:mstop(rpart.gamboost.pred)])

# Coefficients
coef(rpart.gamboost.pred)
# Estimated coefficients for linear components
names(coef(rpart.gamboost.pred, which = "bols"))
lin.eff.rpart.gam.pred = round(data.frame(unlist(coef(rpart.gamboost.pred, which = "bols"))), 3)
colnames(lin.eff.rpart.gam.pred) = "coefficients"

# Not selected
not.selected.rpart.gam.pred = setdiff(variable.names(rpart.gamboost.pred), variable.names(rpart.gamboost.pred, usedonly = TRUE))

# Save predictions
predictions_gamboost_pred = predict(rpart.gamboost.pred, newdata = rpart.df.gam.pred, type = "response")
save(predictions_gamboost_pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/rpart/predictions_gamboost_pred.rda")

# ggplot
p1.gam.pred = createSinglePartialEffectPlots(input = rpart.df.gam.pred$ObsForPred, 
  model = rpart.gamboost.pred, which = "bols(ObsForPred", xlab = "Number of instances")
p2.gam.pred = createSinglePartialEffectPlots(input = rpart.df.gam.pred$ObsForPred, 
  model = rpart.gamboost.pred, which = "bbs(ObsForPred", xlab = "Number of instances")
p3.gam.pred = createCombinedPartialEffectPlots(input = rpart.df.gam.pred$ObsForPred,
  model = rpart.gamboost.pred, which = "ObsForPred", xlab = "Number of instances")
multiplot(p1.gam.pred, p2.gam.pred, p3.gam.pred, cols = 3)

p4.gam.pred = createSinglePartialEffectPlots(input = rpart.df.gam.pred$NumberOfFeatures, 
  model = rpart.gamboost.pred, which = "bols(NumberOfFeatures", xlab = "Number of features")
p5.gam.pred = createSinglePartialEffectPlots(input = rpart.df.gam.pred$NumberOfFeatures, 
  model = rpart.gamboost.pred, which = "bbs(NumberOfFeatures", xlab = "Number of features")
p6.gam.pred = createCombinedPartialEffectPlots(input = rpart.df.gam.pred$NumberOfFeatures,
  model = rpart.gamboost.pred, which = "NumberOfFeatures", xlab = "Number of features")
multiplot(p4.gam.pred, p5.gam.pred, p6.gam.pred, cols = 3)

p7.gam.pred = createSinglePartialEffectPlots(input = rpart.df.gam.pred$NumberOfClasses, 
  model = rpart.gamboost.pred, which = "bols(NumberOfClasses", xlab = "Number of classes")
p8.gam.pred = createSinglePartialEffectPlots(input = rpart.df.gam.pred$NumberOfClasses, 
  model = rpart.gamboost.pred, which = "bbs(NumberOfClasses", xlab = "Number of classes")
p9.gam.pred = createCombinedPartialEffectPlots(input = rpart.df.gam.pred$NumberOfClasses,
  model = rpart.gamboost.pred, which = "NumberOfClasses", xlab = "Number of classes")
multiplot(p7.gam.pred, p8.gam.pred, p9.gam.pred, cols = 3)

p10.gam.pred = createSinglePartialEffectPlots(input = rpart.df.gam.pred$MajorityClassSize, 
  model = rpart.gamboost.pred, which = "bols(MajorityClassSize", xlab = "Majority class size")
p11.gam.pred = createSinglePartialEffectPlots(input = rpart.df.gam.pred$MajorityClassSize, 
  model = rpart.gamboost.pred, which = "bbs(MajorityClassSize", xlab = "Majority class size")
p12.gam.pred = createCombinedPartialEffectPlots(input = rpart.df.gam.pred$MajorityClassSize,
  model = rpart.gamboost.pred, which = "MajorityClassSize", xlab = "Majority class size")
multiplot(p10.gam.pred, p11.gam.pred, p12.gam.pred, cols = 3)

p13.gam.pred = createSinglePartialEffectPlots(input = rpart.df.gam.pred$maxdepth, 
  model = rpart.gamboost.pred, which = "bols(maxdepth", xlab = "maxdepth")
p14.gam.pred = createSinglePartialEffectPlots(input = rpart.df.gam.pred$maxdepth, 
  model = rpart.gamboost.pred, which = "bbs(maxdepth", xlab = "maxdepth")
p15.gam.pred = createCombinedPartialEffectPlots(input = rpart.df.gam.pred$maxdepth,
  model = rpart.gamboost.pred, which = "maxdepth", xlab = "maxdepth")
multiplot(p13.gam.pred, p14.gam.pred, p15.gam.pred, cols = 3)

p16.gam.pred = createSinglePartialEffectPlots(input = rpart.df.gam.pred$minbucket.trafo, 
  model = rpart.gamboost.pred, which = "bols(minbucket.trafo", xlab = "minbucket")
p17.gam.pred = createSinglePartialEffectPlots(input = rpart.df.gam.pred$minbucket.trafo, 
  model = rpart.gamboost.pred, which = "bbs(minbucket.trafo", xlab = "minbucket")
p18.gam.pred = createCombinedPartialEffectPlots(input = rpart.df.gam.pred$minbucket.trafo,
  model = rpart.gamboost.pred, which = "minbucket.trafo", xlab = "minbucket")
multiplot(p16.gam.pred, p17.gam.pred, p18.gam.pred, cols = 3)

p19.gam.pred = createSinglePartialEffectPlots(input = rpart.df.gam.pred$minsplit.trafo, 
  model = rpart.gamboost.pred, which = "bols(minsplit.trafo", xlab = "minsplit")
p20.gam.pred = createSinglePartialEffectPlots(input = rpart.df.gam.pred$minsplit.trafo, 
  model = rpart.gamboost.pred, which = "bbs(minsplit.trafo", xlab = "minsplit")
p21.gam.pred = createCombinedPartialEffectPlots(input = rpart.df.gam.pred$minsplit.trafo,
  model = rpart.gamboost.pred, which = "minsplit.trafo", xlab = "minsplit")
multiplot(p19.gam.pred, p20.gam.pred, p21.gam.pred, cols = 3)

p22.gam.pred = createSinglePartialEffectPlots(input = rpart.df.gam.pred$cp.trafo, 
  model = rpart.gamboost.pred, which = "bols(cp.trafo", xlab = "cp")
p23.gam.pred = createSinglePartialEffectPlots(input = rpart.df.gam.pred$cp.trafo, 
  model = rpart.gamboost.pred, which = "bbs(cp.trafo", xlab = "cp")
p24.gam.pred = createCombinedPartialEffectPlots(input = rpart.df.gam.pred$cp.trafo,
  model = rpart.gamboost.pred, which = "cp.trafo", xlab = "cp")
multiplot(p22.gam.pred, p23.gam.pred, p24.gam.pred, cols = 3)


#### glm ####

glm.rpart.pred = glm(rpart.glm.pred.fmla, data = rpart.df.mean, family = Gamma(link = "log"))

# creating coefficient frame
step.glm.rpart.pred = step(glm.rpart.pred, direction = "both")
step.glm.rpart.pred$anova
n.sel.glm.pred = c("minsplit.trafo", "minbucket.trafo", "cp.trafo", "maxdepth",
  "I(minbucket.trafo^2)", "I(ObsForPred^2)", "I(maxdepth^2)", "I(cp.trafo^2)",
  "sqrt(cp.trafo)", "log(maxdepth)", "log(cp.trafo)", "log(ObsForPred)")
coefs.glm.pred = summary(glm.rpart.pred)$coefficients[,c(1,4)]
coefs.glm.pred[n.sel.glm.pred, ] = rep(NA, length(n.sel.glm.pred))
coefs.glm.pred[complete.cases(coefs.glm.pred), ] = summary(step.glm.rpart.pred)$coefficients[,c(1,4)]

# Create dataframe with estimated coefficients for glmboost and glm on prediction time
coefs.pred = cbind(coefs.rpart.glmboost.pred, coefs.glm.pred)
save(coefs.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/rpart/coefs_pred.rda")

# save predictions
predictions_glm_pred = predict(step.glm.rpart.pred, newdata = rpart.df.mean, type = "response")
save(predictions_glm_pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/rpart/predictions_glm_pred.rda")

# ggplot
p1.glm.pred = createPartialEffectsForGlm(input = rpart.df.mean$MajorityClassSize,
  model = glm.rpart.pred, linear.effect = NULL, quadratic.effect = "I(MajorityClassSize^2)",
  log.effect = "sqrt(MajorityClassSize)", xlab = "Majority class size")
p2.glm.pred = createPartialEffectsForGlm(input = rpart.df.mean$ObsForPred,
  model = glm.rpart.pred,
  linear.effect = "ObsForPred", quadratic.effect = NULL,
  log.effect = "sqrt(ObsForPred)", xlab = "Number of instances")
p3.glm.pred = createPartialEffectsForGlm(input = rpart.df.mean$NumberOfFeatures,
  model = glm.rpart.pred,
  linear.effect = "NumberOfFeatures", quadratic.effect = "I(NumberOfFeatures^2)",
  log.effect = "sqrt(NumberOfFeatures)", xlab = "Number of features")
p4.glm.pred = createPartialEffectsForGlm(input = rpart.df.mean$NumberOfClasses,
  model = glm.rpart.pred,
  linear.effect = "NumberOfClasses", quadratic.effect = "I(NumberOfClasses^2)",
  log.effect = NULL, xlab = "Number of classes")
p5.glm.pred = createPartialEffectsForGlm(input = rpart.df.mean$maxdepth,
  model = glm.rpart.pred,
  linear.effect = "maxdepth", quadratic.effect = NULL,
  log.effect = NULL, xlab = "maxdepth")
p6.glm.pred = createPartialEffectsForGlm(input = rpart.df.mean$minsplit.trafo,
  model = glm.rpart.pred,
  linear.effect = "minsplit.trafo", quadratic.effect = "I(minsplit.trafo^2)",
  log.effect = "sqrt(minsplit.trafo)", xlab = "minsplit")
p7.glm.pred = createPartialEffectsForGlm(input = rpart.df.mean$minbucket.trafo,
  model = glm.rpart.pred,
  linear.effect = "minbucket.trafo", quadratic.effect = "I(minbucket.trafo^2)",
  log.effect = "sqrt(minbucket.trafo)", xlab = "minbucket")
# cp has no effect
multiplot(p1.glm.pred, p2.glm.pred, p3.glm.pred, p4.glm.pred, cols = 2)
multiplot(p5.glm.pred, p6.glm.pred, p7.glm.pred, cols = 2)

#### randomForest ####
# use ugly hack 
rpart.df.mean3 = rpart.df.mean
rpart.df.mean3$NumberOfInstances = rpart.df.mean3$ObsForPred
rpart.df.mean3$cp = rpart.df.mean3$cp.trafo
rpart.df.mean3$minsplit = rpart.df.mean3$minsplit.trafo
rpart.df.mean3$minbucket = rpart.df.mean3$minbucket.trafo

rpart.rF.pred = randomForest(timepredict.test.mean ~ MajorityClassSize +
    + NumberOfClasses + NumberOfFeatures + NumberOfInstances + maxdepth +
    cp + minsplit + minbucket, data = rpart.df.mean3, importance = TRUE)

# save predictions
predictions.rF.pred = predict(rpart.rF.pred, newdata = rpart.df.mean3, type = "response")
save(predictions.rF.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/rpart/predictions_rF_pred.rda")

plot(rpart.rF.pred)

varUsed(rpart.rF.pred, by.tree=FALSE, count=TRUE)

# Variable importance
imp.pred = importance(rpart.rF.pred)
varImpPlot(rpart.rF.pred, main = NULL)

# Partial dependence plot
impvar.pred <- rownames(imp.pred)[order(imp.pred[, 1], decreasing=TRUE)]
op <- par(mfrow=c(2, 4))
for (i in seq_along(impvar.pred)) {
  partialPlot(rpart.rF.pred, rpart.df.mean3, impvar.pred[i],
    xlab=impvar.pred[i],ylim=c(0, 0.08), main = "")
}
par(op)


#### Compare models ####
# Create dataframe with estimated coefficients for glmboost and glm on training 
# time and prediction time
coefs.both = cbind(coefs.train, coefs.pred)

# RMSE and RAE
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/rpart/predictions_glmboost_test.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/rpart/predictions_gamboost_test.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/rpart/predictions_glm_test.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/rpart/predictions_rF_test.rda")

load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/rpart/predictions_glmboost_pred.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/rpart/predictions_gamboost_pred.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/rpart/predictions_glm_pred.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/rpart/predictions_rF_pred.rda")

errors.test = calculateErrorsForComparison2(time = "training", glmboost.pred = predictions.glmboost.test,
  glmboost.data = rpart.df.mean, gamboost.pred = predictions.gamboost.test,
  gamboost.data = rpart.df.gam.test, glm.pred = predictions.glm.test,
  glm.data = rpart.df.mean, rF.pred = predictions.rF.test, rF.data = rpart.df.mean2)

errors.pred = calculateErrorsForComparison2(time = "prediction", glmboost.pred = predictions_glmboost_pred,
  glmboost.data = rpart.df.mean, gamboost.pred = predictions_gamboost_pred,
  gamboost.data = rpart.df.gam.pred, glm.pred = predictions_glm_pred,
  glm.data = rpart.df.mean, rF.pred = predictions.rF.pred, rF.data = rpart.df.mean3)

# Obs vs. fitted
df = data.frame(traintime = rpart.df.mean$timetrain.test.mean,
  glmboost = predictions.glmboost.test,
  gamboost = predictions.gamboost.test,
  glm = predictions.glm.test,
  randomForest = predictions.rF.test)
df2 = melt(df, id.vars = 1)
colnames(df2)[2] = "model"

df.pred = data.frame(predicttime = rpart.df.mean$timepredict.test.mean,
  glmboost = predictions_glmboost_pred,
  gamboost = predictions_gamboost_pred,
  glm = predictions_glm_pred,
  randomForest = predictions.rF.pred)
df2.pred = melt(df.pred, id.vars = 1)
colnames(df2.pred)[2] = "model"


p1 =ggplot(data = df2, aes(traintime,value,colour=model))+
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  scale_colour_manual(values=c("red","blue", "black", "green"))+
  labs(x="Observed training time",y="Fitted values") +
  theme(legend.position="none")

p2 =ggplot(data = df2.pred, aes(predicttime,value,colour=model))+
  geom_point(position=position_jitter(w=0.04,h=0.02),size=1.8) + 
  geom_abline(intercept = 0, slope = 1) +
  scale_colour_manual(values=c("red","blue", "black", "green"))+
  labs(x="Observed prediction time",y="Fitted values") +
  theme(legend.position="bottom")

grid_arrange_shared_legend(p1,p2,ncol=2)
