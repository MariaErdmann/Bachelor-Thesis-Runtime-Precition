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
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rangerdfgam_pred.RData")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rangerdfgam_test.RData")
ranger.df.mean$respect.unordered.factors = as.factor(ranger.df.mean$respect.unordered.factors)
ranger.df.mean$replace = as.factor(ranger.df.mean$replace)
ranger.df.gam.test$respect.unordered.factors = as.factor(ranger.df.gam.test$respect.unordered.factors)
ranger.df.gam.test$replace = as.factor(ranger.df.gam.test$replace)
ranger.df.gam.pred$respect.unordered.factors = as.factor(ranger.df.gam.pred$respect.unordered.factors)
ranger.df.gam.pred$replace = as.factor(ranger.df.gam.pred$replace)
set.seed(334)
opar = par()

#### TRAINING TIME ####

#### glmboost ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/ranger_glmboost_test.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_ranger_glmboost_test.rda")

# mstop
mstop(cvm.ranger.glmboost.test) # 99999
plot(cvm.ranger.glmboost.test)

# Plot coefficient paths
par(mar=c(5.1,4.1,4.1,13.1))
plot(ranger.glmboost.test, off2int = TRUE, main = "Analysis of training time on ranger
  - inclusion of intercept")

par(mar=c(5.1,4.1,4.1,13.3))
plot(ranger.glmboost.test,
  ylim = range(coef(ranger.glmboost.test, which = 2:31)),
  main = "Analysis of training time on ranger - \n adjusted y-scale")

# Coefficients
coefs.ranger.glmboost = data.frame(coef(ranger.glmboost.test, which =))
colnames(coefs.ranger.glmboost) = "coefficients"
# Variables that were not chosen into the model
not.sel = setdiff(variable.names(ranger.glmboost.test), variable.names(ranger.glmboost.test, usedonly = TRUE))
# all variables selected
coefs.ranger.glmboost[not.sel,] = rep(NA, length(not.sel))
save(coefs.ranger.glmboost, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/Ranger/coefs_ranger_glmboost.rda")

# save predictions
predictions.glmboost.test = predict(ranger.glmboost.test, newdata = ranger.df.mean, type = "response")
save(predictions.glmboost.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/Ranger/predictions_ranger_glm_test.rda")

# ggplot partial effects
p1.test = createCombinedPartialEffectPlots(input = ranger.df.mean$ObsForTrain,
  model = ranger.glmboost.test, which = "ObsForTrain", xlab = "Number of instances")
p2.test = createCombinedPartialEffectPlots(input = ranger.df.mean$NumberOfFeatures,
  model = ranger.glmboost.test, which = "NumberOfFeatures", xlab = "Number of features")
p3.test = createCombinedPartialEffectPlots(input = ranger.df.mean$NumberOfClasses,
  model = ranger.glmboost.test, which = "NumberOfClasses", xlab = "Number of classes")
p4.test = createCombinedPartialEffectPlots(input = ranger.df.mean$MajorityClassSize,
  model = ranger.glmboost.test, which = "MajorityClassSize", xlab = "Majority class size")
p5.test = createCombinedPartialEffectPlots(input = ranger.df.mean$num.trees,
  model = ranger.glmboost.test, which = "num.trees", xlab = "num.trees")
p6.test = createCombinedPartialEffectPlots(input = ranger.df.mean$sample.fraction.trafo,
  model = ranger.glmboost.test, which = "sample.fraction.trafo", xlab = "sample.fraction")
p7.test = createCombinedPartialEffectPlots(input = ranger.df.mean$mtry.trafo,
  model = ranger.glmboost.test, which = "mtry.trafo", xlab = "mtry")
p8.test = createCombinedPartialEffectPlots(input = ranger.df.mean$replace,
  model = ranger.glmboost.test, which = "replace", xlab = "replace")
p8.test = p8.test + geom_errorbar(aes(y=part.effect, ymax=part.effect, ymin=part.effect), linetype="dashed", size = 1)
p9.test = createCombinedPartialEffectPlots(input = ranger.df.mean$respect.unordered.factors,
  model = ranger.glmboost.test, which = "respect.unordered.factors", xlab = "respect.unordered.factors")
p9.test = p9.test + geom_errorbar(aes(y=part.effect, ymax=part.effect, ymin=part.effect), linetype="dashed", size = 1)

multiplot(p1.test, p2.test, p3.test, p4.test,p5.test,p6.test,p7.test, p8.test, p9.test, cols = 3)

#### gamboost ####
#### no result ####


#### glm ####
glm.ranger.test = glm(ranger.glm.test.fmla, data = ranger.df.mean,
  family = Gamma(link = "log"))
# Error because of family
# changing to gaussian() makes it possible to fit a model, however
# stepwise-selection does not work

glm.ranger.test = glm(ranger.glm.test.fmla ,data = ranger.df.mean,
  family = gaussian())

# creating coefficient frame
step.glm.ranger.test = step(glm.ranger.test, direction = "both")
step.glm.ranger.test$anova

coefs.ranger.glm.test.gaussian = summary(glm.ranger.test)$coefficients[,c(1,4)]
not.sel.glm.test = c("I(mtry.trafo^2)", "num.trees", "log(sample.fraction.trafo)",
  "sqrt(sample.fraction.trafo)", "I(sample.fraction.trafo^2)", "log(num.trees)",
  "I(ObsForTrain^2)")
coefs.ranger.glm.test.gaussian[not.sel.glm.test, ] = rep(NA, length(not.sel.glm.test))
coefs.ranger.glm.test.gaussian[complete.cases(coefs.ranger.glm.test.gaussian),] = summary(step.glm.ranger.test)$coefficients[,c(1,4)]
save(coefs.ranger.glm.test.gaussian, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/Ranger/coefs_ranger_glm_test_gaussian.rda")

#### randomForest ####
ranger.df.mean2 = ranger.df.mean
ranger.df.mean2$NumberOfInstances = ranger.df.mean2$ObsForTrain
ranger.df.mean2$sample.fraction = ranger.df.mean2$sample.fraction.trafo
ranger.df.mean2$mtry = ranger.df.mean2$mtry.trafo

ranger.rF.test = randomForest(timetrain.test.mean ~ NumberOfInstances +
    NumberOfFeatures + NumberOfClasses + MajorityClassSize + num.trees
  + sample.fraction + mtry + replace + respect.unordered.factors,
  data = ranger.df.mean2, importance = TRUE)

# save predictions
predictions.rF.test = predict(ranger.rF.test, newdata = ranger.df.mean2, type = "response")
save(predictions.rF.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/Ranger/predictions_rF_test.rda")

plot(ranger.rF.test)

varUsed(ranger.rF.test, by.tree=FALSE, count=TRUE)

# Variable importance
imp.test = importance(ranger.rF.test)
varImpPlot(ranger.rF.test, main = NULL)
impvar.test <- rownames(imp.test)[order(imp.test[, 1], decreasing=TRUE)]
impvar.test = impvar.test[-1]
impvar.test = impvar.test[-8]
op <- par(mfrow=c(2, 4))
for (i in seq_along(impvar.test)) {
  partialPlot(ranger.rF.test, ranger.df.mean, impvar.test[i],
    xlab=impvar.test[i],ylim=c(0, 30), main = "")
}
par(op)


#### PREDICTION TIME ####

#### glmboost ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/ranger_glmboost_pred.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_ranger_glmboost_pred.rda")

# mstop
mstop(cvm.ranger.glmboost.pred) #3847
plot(cvm.ranger.glmboost.pred)

# Plot coefficient paths
par(mar=c(5.1,4.1,4.1,13.1))
plot(ranger.glmboost.pred, off2int = TRUE, main = "Analysis of prediction time on ranger
  - inclusion of intercept")

par(mar=c(5.1,4.1,4.1,13.3))
plot(ranger.glmboost.pred,
  ylim = range(coef(ranger.glmboost.pred, which = 2:31)),
  main = "Analysis of prediction time on ranger - \n adjusted y-scale")

par(mar=c(5.1,4.1,4.1,2.1))

# Coefficients
coefs.ranger.glmboost.pred = data.frame(coef(ranger.glmboost.pred, which = ""))
colnames(coefs.ranger.glmboost.pred) = "coefficients"
# Variables that were not chosen into the model
not.sel.glmboost.pred = setdiff(variable.names(ranger.glmboost.pred), variable.names(ranger.glmboost.pred, usedonly = TRUE))
# not selected: sample.fraction.trafo
coefs.ranger.glmboost.pred[not.sel.glmboost.pred,] = rep(NA, length(not.sel.glmboost.pred))
save(coefs.ranger.glmboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/Ranger/coefs_ranger_glmboost_pred.rda")

# save predictions
predictions.glmboost.pred = predict(ranger.glmboost.pred, newdata = ranger.df.mean, type = "response")
save(predictions.glmboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/Ranger/prediction_glm_pred.rda")

# ggplot partial effects
p1.pred = createCombinedPartialEffectPlots(input = ranger.df.mean$ObsForPred,
  model = ranger.glmboost.pred, which = "ObsForPred", xlab = "Number of instances")
p2.pred = createCombinedPartialEffectPlots(input = ranger.df.mean$NumberOfFeatures,
  model = ranger.glmboost.pred, which = "NumberOfFeatures", xlab = "Number of features")
p3.pred = createCombinedPartialEffectPlots(input = ranger.df.mean$NumberOfClasses,
  model = ranger.glmboost.pred, which = "NumberOfClasses", xlab = "Number of classes")
p4.pred = createCombinedPartialEffectPlots(input = ranger.df.mean$MajorityClassSize,
  model = ranger.glmboost.pred, which = "MajorityClassSize", xlab = "Majority class size")
p5.pred = createCombinedPartialEffectPlots(input = ranger.df.mean$num.trees,
  model = ranger.glmboost.pred, which = "num.trees", xlab = "num.trees")
p6.pred = createCombinedPartialEffectPlots(input = ranger.df.mean$sample.fraction.trafo,
  model = ranger.glmboost.pred, which = "sample.fraction.trafo", xlab = "sample.fraction")
p7.pred = createCombinedPartialEffectPlots(input = ranger.df.mean$mtry.trafo,
  model = ranger.glmboost.pred, which = "mtry.trafo", xlab = "mtry")
p8.pred = createCombinedPartialEffectPlots(input = ranger.df.mean$replace,
  model = ranger.glmboost.pred, which = "replace", xlab = "replace")
p8.pred = p8.pred + geom_errorbar(aes(y=part.effect, ymax=part.effect, ymin=part.effect), linetype="dashed", size = 1)
p9.pred = createCombinedPartialEffectPlots(input = ranger.df.mean$respect.unordered.factors,
  model = ranger.glmboost.pred, which = "respect.unordered.factors", xlab = "respect.unordered.factors")
p9.pred = p9.pred + geom_errorbar(aes(y=part.effect, ymax=part.effect, ymin=part.effect), linetype="dashed", size = 1)

multiplot(p1.pred, p2.pred, p3.pred, p4.pred,p5.pred,p6.pred,p7.pred,p8.pred, p9.pred, cols = 3)

#### gamboost ####
# Cholesky-Faktorisierung fehlgeschlagen

#### glm ####
ranger.glm.pred = glm(ranger.glm.pred.fmla, data = ranger.df.mean, family = Gamma(link = "log"))
step.ranger.glm.pred = step(ranger.glm.pred)
step.ranger.glm.pred$anova
coefs.glm.pred = summary(ranger.glm.pred)$coefficients[,c(1,4)]
not.sel.glm.pred = c("num.trees", "log(MajorityClassSize)", "I(sample.fraction.trafo^2)",
  "log(sample.fraction.trafo)", "sqrt(ObsForPred)", "replaceTRUE")
coefs.glm.pred[not.sel.glm.pred, ] = rep(NA, length(not.sel.glm.pred))
coefs.glm.pred[complete.cases(coefs.glm.pred),] = summary(step.ranger.glm.pred)$coefficients[,c(1,4)]
save(coefs.glm.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/Ranger/coefs_ranger_glm_pred.rda")

# save predictions
predictions.glm.pred = predict(step.ranger.glm.pred, newdata = ranger.df.mean, type = "response")
save(predictions.glm.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/Ranger/predictions_realglm_pred.rda")

#### randomForest ####
ranger.df.mean3 = ranger.df.mean
ranger.df.mean3$NumberOfInstances = ranger.df.mean3$ObsForPred
ranger.df.mean3$mtry = ranger.df.mean3$mtry.trafo
ranger.df.mean3$sample.fraction = ranger.df.mean3$sample.fraction.trafo

ranger.pred.rF = randomForest(timepredict.test.mean ~ NumberOfInstances +
    NumberOfFeatures + NumberOfClasses + MajorityClassSize + num.trees +
    mtry + sample.fraction + replace + respect.unordered.factors, data = ranger.df.mean3,
  importance = TRUE)

plot(ranger.pred.rF)

varUsed(ranger.pred.rF, by.tree=FALSE, count=TRUE)

# Variable importance
imp.pred = importance(ranger.pred.rF)
varImpPlot(ranger.pred.rF, main = NULL)

# Partial dependence plot
impvar.pred <- rownames(imp.pred)[order(imp.pred[, 1], decreasing=TRUE)]
impvar.pred = impvar.pred[-8]
impvar.pred = impvar.pred[-8]
op <- par(mfrow=c(2, 4))
for (i in seq_along(impvar.pred)) {
  partialPlot(ranger.pred.rF, ranger.df.mean, impvar.pred[i],
    xlab=impvar.pred[i],ylim=c(0, 1.2), main = "")
}
par(op)

# save predictions
predictions.rF.pred = predict(ranger.pred.rF, newdata = ranger.df.mean3, type = "response")
save(predictions.rF.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/Ranger/predictions_rF_pred.rda")

#### Comparison of models ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/Ranger/coefs_ranger_glmboost.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/Ranger/coefs_ranger_glmboost_pred.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/Ranger/coefs_ranger_glm_pred.rda")

# Create dataframe with estimated coefficients for glmboost and glm on training 
# time and prediction time
all.coefs = cbind(coefs.ranger.glmboost, coefs.ranger.glmboost.pred, coefs.glm.pred)

# RMSE and RAE
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/Ranger/prediction_glm_pred.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/Ranger/predictions_ranger_glm_test.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/Ranger/predictions_realglm_pred.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/Ranger/predictions_rF_test.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/Ranger/predictions_rF_pred.rda")

mse.test.1 = mean((ranger.df.mean$timetrain.test.mean - predictions.glmboost.test)^2)
mse.test.4 = mean((ranger.df.mean2$timetrain.test.mean - predictions.rF.test)^2)

rae.test.1 = sum(abs(ranger.df.mean$timetrain.test.mean - predictions.glmboost.test))/
    sum(abs(ranger.df.mean$timetrain.test.mean - mean(ranger.df.mean$timetrain.test.mean)))
rae.test.4 = sum(abs(ranger.df.mean2$timetrain.test.mean - predictions.rF.test))/
    sum(abs(ranger.df.mean2$timetrain.test.mean - mean(ranger.df.mean2$timetrain.test.mean)))

errors = rbind(glmboost = c(mse.test.1, sqrt(mse.test.1), rae.test.1),
  randomForest = c(mse.test.4, sqrt(mse.test.4), rae.test.4))
colnames(errors) = c("mse", "rmse", "rae")

mse.pred.1 = mean((ranger.df.mean$timepredict.test.mean - predictions.glmboost.pred)^2)
mse.pred.3 = mean((ranger.df.mean$timepredict.test.mean - predictions.glm.pred)^2)
mse.pred.4 = mean((ranger.df.mean3$timepredict.test.mean - predictions.rF.pred)^2)

rae.pred.1 = sum(abs(ranger.df.mean$timepredict.test.mean - predictions.glmboost.pred))/
    sum(abs(ranger.df.mean$timepredict.test.mean - mean(ranger.df.mean$timepredict.test.mean)))
rae.pred.3 = sum(abs(ranger.df.mean$timepredict.test.mean - predictions.glm.pred))/
    sum(abs(ranger.df.mean$timepredict.test.mean - mean(ranger.df.mean$timepredict.test.mean)))
rae.pred.4 = sum(abs(ranger.df.mean3$timepredict.test.mean - predictions.rF.pred))/
    sum(abs(ranger.df.mean3$timepredict.test.mean - mean(ranger.df.mean3$timepredict.test.mean)))

errors.pred = rbind(glmboost = c(mse.pred.1, sqrt(mse.pred.1), rae.pred.1),
  glm = c(mse.pred.3, sqrt(mse.pred.3), rae.pred.3),
  randomForest = c(mse.pred.4, sqrt(mse.pred.4), rae.pred.4))
colnames(errors.pred) = c("mse", "rmse", "rae")

# Obs vs fitted
df = data.frame(timetrain = ranger.df.mean$timetrain.test.mean,
  glmboost = predictions.glmboost.test,
  randomForest = predictions.rF.test)
df2 = melt(df, id.vars = 1)
colnames(df2)[2] = "model"

df.pred = data.frame(timepredict = ranger.df.mean$timepredict.test.mean,
  glmboost = predictions.glmboost.pred,
  glm = predictions.glm.pred,
  randomForest = predictions.rF.pred)
df.pred2 = melt(df.pred, id.vars = 1)
colnames(df.pred2)[2] = "model"

p1 =ggplot(data = df2, aes(timetrain,value,colour=model))+
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  scale_colour_manual(values=c("red", "green"))+
  labs(x="Observed training time",y="Fitted values")+
  theme(legend.position="bottom")
  

p2 =ggplot(data = df.pred2, aes(timepredict,value,colour=model))+
  geom_point(position=position_jitter(w=0.04,h=0.02),size=1.8) + 
  geom_abline(intercept = 0, slope = 1) +
  scale_colour_manual(values=c("red", "black", "green"))+
  labs(x="Observed prediction time",y="Fitted values") +
  theme(legend.position="bottom")

grid_arrange_shared_legend(p1,p2,ncol=2)
