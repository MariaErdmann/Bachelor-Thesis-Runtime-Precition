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
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/glmnetdfgam_pred.RData")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/glmnetdfgam_test.RData")

set.seed(334)
opar = par()

#### TRAINING TIME ####

#### glmboost ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/glmnet_glmboost_test.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_glmnet_glmboost_test.rda")

# Appropriate mstop
mstop(cvm.glmnet.glmboost.test) # 99998
plot(cvm.glmnet.glmboost.test)

# Coefficinet Paths
par(mar=c(5.1,4.1,4.1,10.1))
plot(glmnet.glmboost.test, off2int = TRUE, main = "Analysis of training time on glmnet
  - inclusion of intercept")

par(mar=c(5.1,4.1,4.1,10.1))
plot(glmnet.glmboost.test,
  ylim = range(coef(glmnet.glmboost.test, which = 2:21)),
  main = "Analysis of prediction time on glmnet - \n adjusted y-scale")

# Coefficients
coefs.glmnet.glmboost = data.frame(coef(glmnet.glmboost.test, which = ""))
colnames(coefs.glmnet.glmboost) = "coefficients"
save(coefs.glmnet.glmboost, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/glmnet/coefs_glmnet_glmboost.rda")

# Variables that were not chosen into the model
not.selected.glmnet.glm = setdiff(variable.names(glmnet.glmboost.test), variable.names(glmnet.glmboost.test, usedonly = TRUE))

# Predictions
predictions.glmboost.test = predict(glmnet.glmboost.test, newdata = glmnet.df.mean, type = "response")
save(predictions.glmboost.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/glmnet/predictions_glmboost_test.rda")

# ggplot: partial effects
p1.test = createCombinedPartialEffectPlots(input = glmnet.df.mean$ObsForTrain,
  model = glmnet.glmboost.test, which = "ObsForTrain", xlab = "Number of instances")
p2.test = createCombinedPartialEffectPlots(input = glmnet.df.mean$NumberOfFeatures,
  model = glmnet.glmboost.test, which = "NumberOfFeatures", xlab = "Number of features")
p3.test = createCombinedPartialEffectPlots(input = glmnet.df.mean$NumberOfClasses,
  model = glmnet.glmboost.test, which = "NumberOfClasses", xlab = "Number of classes")
p4.test = createCombinedPartialEffectPlots(input = glmnet.df.mean$MajorityClassSize,
  model = glmnet.glmboost.test, which = "MajorityClassSize", xlab = "Majority class size")
p5.test = createCombinedPartialEffectPlots(input = glmnet.df.mean$alpha,
  model = glmnet.glmboost.test, which = "alpha", xlab = "alpha")
multiplot(p1.test, p2.test, p3.test, p4.test,p5.test, cols = 2)


#### gamboost ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/glmnet_gamboost_test.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_glmnet_gamboost_test.rda")

# Appropriate mstop
mstop(cvm.glmnet.gamboost.test) #99992
plot(cvm.glmnet.gamboost.test, main = "", ylab = "Risk")

# Variables that were not chosen into the model
not.selected.glmnet.gam = setdiff(variable.names(glmnet.gamboost.test), variable.names(glmnet.gamboost.test, usedonly = TRUE))
# all varaibles selected

# Predictions
predictions.gamboost.test = predict(glmnet.gamboost.test, newdata = glmnet.df.gam.test, type = "response")
save(predictions.gamboost.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/glmnet/predictions_gamboost_test.rda")

# ggplot for single and combined effects
# ggplot
p1.gam.test = createSinglePartialEffectPlots(input = glmnet.df.gam.test$ObsForTrain, 
  model = glmnet.gamboost.test, which = "bols(ObsForTrain", xlab = "Number of instances")
p2.gam.test = createSinglePartialEffectPlots(input = glmnet.df.gam.test$ObsForTrain, 
  model = glmnet.gamboost.test, which = "bbs(ObsForTrain", xlab = "Number of instances")
p3.gam.test = createCombinedPartialEffectPlots(input = glmnet.df.gam.test$ObsForTrain,
  model = glmnet.gamboost.test, which = "ObsForTrain", xlab = "Number of instances")
multiplot(p1.gam.test, p2.gam.test, p3.gam.test, cols = 3)

p4.gam.test = createSinglePartialEffectPlots(input = glmnet.df.gam.test$NumberOfFeatures, 
  model = glmnet.gamboost.test, which = "bols(NumberOfFeatures", xlab = "Number of features")
p5.gam.test = createSinglePartialEffectPlots(input = glmnet.df.gam.test$NumberOfFeatures, 
  model = glmnet.gamboost.test, which = "bbs(NumberOfFeatures", xlab = "Number of features")
p6.gam.test = createCombinedPartialEffectPlots(input = glmnet.df.gam.test$NumberOfFeatures,
  model = glmnet.gamboost.test, which = "NumberOfFeatures", xlab = "Number of features")
multiplot(p4.gam.test, p5.gam.test, p6.gam.test, cols = 3)

p7.gam.test = createSinglePartialEffectPlots(input = glmnet.df.gam.test$NumberOfClasses, 
  model = glmnet.gamboost.test, which = "bols(NumberOfClasses", xlab = "Number of classes")
p8.gam.test = createSinglePartialEffectPlots(input = glmnet.df.gam.test$NumberOfClasses, 
  model = glmnet.gamboost.test, which = "bbs(NumberOfClasses", xlab = "Number of classes")
p9.gam.test = createCombinedPartialEffectPlots(input = glmnet.df.gam.test$NumberOfClasses,
  model = glmnet.gamboost.test, which = "NumberOfClasses", xlab = "Number of classes")
multiplot(p7.gam.test, p8.gam.test, p9.gam.test, cols = 3)

p10.gam.test = createSinglePartialEffectPlots(input = glmnet.df.gam.test$MajorityClassSize, 
  model = glmnet.gamboost.test, which = "bols(MajorityClassSize", xlab = "Majority class size")
p11.gam.test = createSinglePartialEffectPlots(input = glmnet.df.gam.test$MajorityClassSize, 
  model = glmnet.gamboost.test, which = "bbs(MajorityClassSize", xlab = "Majority class size")
p12.gam.test = createCombinedPartialEffectPlots(input = glmnet.df.gam.test$MajorityClassSize,
  model = glmnet.gamboost.test, which = "MajorityClassSize", xlab = "Majority class size")
multiplot(p10.gam.test, p11.gam.test, p12.gam.test, cols = 3)

p13.gam.test = createSinglePartialEffectPlots(input = glmnet.df.gam.test$alpha, 
  model = glmnet.gamboost.test, which = "bols(alpha", xlab = "alpha")
p14.gam.test = createSinglePartialEffectPlots(input = glmnet.df.gam.test$alpha, 
  model = glmnet.gamboost.test, which = "bbs(alpha", xlab = "alpha")
p15.gam.test = createCombinedPartialEffectPlots(input = glmnet.df.gam.test$alpha,
  model = glmnet.gamboost.test, which = "alpha", xlab = "alpha")
multiplot(p13.gam.test, p14.gam.test, p15.gam.test, cols = 3)


#### glm ####
glm.glmnet.test = glm(glmnet.glm.test.fmla, data = glmnet.df.mean, family = Gamma(link = "log"))
step.glm.glmnet.test = step(glm.glmnet.test, direction = "both")
step.glm.glmnet.test$anova
n.sel.glm.test = c("log(alpha)", "sqrt(alpha)", "I(ObsForTrain^2)", "I(MajorityClassSize^2)")
coefs.glm.test = summary(glm.glmnet.test)$coefficients[,c(1,4)]
coefs.glm.test[n.sel.glm.test, ] = rep(NA, length(n.sel.glm.test))
coefs.glm.test[complete.cases(coefs.glm.test),] = summary(step.glm.glmnet.test)$coefficients[,c(1,4)]
save(coefs.glm.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/glmnet/coefs_glm_test.rda")

test.coefs = cbind(coefs.glmnet.glmboost, coefs.glm.test)
save(test.coefs, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/glmnet/coefs_test.rda")

predictions.glm.test = predict(step.glm.glmnet.test, newdata = glmnet.df.mean, type = "response")
save(predictions.glm.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/glmnet/predictions_glm_test.rda")

#### randomForest ####
glmnet.df.mean2 = glmnet.df.mean
glmnet.df.mean2$NumberOfInstances = glmnet.df.mean2$ObsForTrain

glmnet.rF.test = randomForest(timetrain.test.mean ~ MajorityClassSize +
    + NumberOfClasses + NumberOfFeatures + NumberOfInstances + alpha,
  data = glmnet.df.mean, importance = TRUE)
plot(glmnet.rF.test)

# save predictions
predictions.rF.test = predict(glmnet.rF.test, newdata = glmnet.df.mean2, type = "response")
save(predictions.rF.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/glmnet/predictions_rF_test.rda")

# Variable importance
imp.test = importance(glmnet.rF.test)
varImpPlot(glmnet.rF.test, main = NULL)

# Partial dependence plot
impvar.test <- rownames(imp.test)[order(imp.test[, 1], decreasing=TRUE)]
op <- par(mfrow=c(2, 3))
for (i in seq_along(impvar.test)) {
  partialPlot(glmnet.rF.test, glmnet.df.mean, impvar.test[i],
    xlab=impvar.test[i],ylim=c(0.0, 6), main = "")
}
par(op)


#### PREDICTION TIME ####

#### glmboost ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/glmnet_glmboost_pred.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_glmnet_glmboost_pred.rda")

# Appropriate mstop
mstop(cvm.glmnet.glmboost.pred) # 99394
plot(cvm.glmnet.glmboost.pred)

# Coefficinet Paths
par(mar=c(5.1,4.1,4.1,10.1))
plot(glmnet.glmboost.pred, off2int = TRUE, main = "Analysis of prediction time on glmnet
  - inclusion of intercept")

par(mar=c(5.1,4.1,4.1,10.1))
plot(glmnet.glmboost.pred,
  ylim = range(coef(glmnet.glmboost.pred, which = 2:21)),
  main = "Analysis of prediction time on glmnet - \n adjusted y-scale")

# Coefficients
coefs.glmnet.glmboost.pred = data.frame(coef(glmnet.glmboost.pred, which = ""))
colnames(coefs.glmnet.glmboost.pred) = "coefficients"
save(coefs.glmnet.glmboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/glmnet/coefs_glmnet_glmboost_pred.rda")

# Variables that were not chosen into the model
not.selected.glmnet.glm.pred = setdiff(variable.names(glmnet.glmboost.pred), variable.names(glmnet.glmboost.pred, usedonly = TRUE))
# all variables were selected

# Predictions
predictions.glmboost.pred = predict(glmnet.glmboost.pred, newdata = glmnet.df.mean, type = "response")
save(predictions.glmboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/glmnet/predictions_glmboost_pred.rda")

# ggplot for combined effects
p1.pred = createCombinedPartialEffectPlots(input = glmnet.df.mean$ObsForPred,
  model = glmnet.glmboost.pred, which = "ObsForPred", xlab = "Number of instances")
p2.pred = createCombinedPartialEffectPlots(input = glmnet.df.mean$NumberOfFeatures,
  model = glmnet.glmboost.pred, which = "NumberOfFeatures", xlab = "Number of features")
p3.pred = createCombinedPartialEffectPlots(input = glmnet.df.mean$NumberOfClasses,
  model = glmnet.glmboost.pred, which = "NumberOfClasses", xlab = "Number of classes")
p4.pred = createCombinedPartialEffectPlots(input = glmnet.df.mean$MajorityClassSize,
  model = glmnet.glmboost.pred, which = "MajorityClassSize", xlab = "Majority class size")
p5.pred = createCombinedPartialEffectPlots(input = glmnet.df.mean$alpha,
  model = glmnet.glmboost.pred, which = "alpha", xlab = "alpha")
multiplot(p1.pred, p2.pred, p3.pred, p4.pred, p5.pred, cols = 2)

#### gamboost ####

load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/glmnet_gamboost_pred.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_glmnet_gamboost_pred.rda")

# Appropriate mstop
mstop(cvm.glmnet.gamboost.pred) #100000
plot(cvm.glmnet.gamboost.pred)

# Coefficinet Paths
plot(glmnet.gamboost.pred)

# Coefficients
coef(glmnet.gamboost.pred)

# Variables that were not chosen into the model
not.selected.glmnet.gam.pred = setdiff(variable.names(glmnet.gamboost.pred), variable.names(glmnet.gamboost.pred, usedonly = TRUE))
# all varaibles selected

# Predictions
predictions.gamboost.pred = predict(glmnet.gamboost.pred, newdata = glmnet.df.gam.pred, type = "response")
save(predictions.gamboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/glmnet/predictions_gamboost_pred.rda")

# ggplot for single and combined effects
p1.gam.pred = createSinglePartialEffectPlots(input = glmnet.df.gam.pred$ObsForPred, 
  model = glmnet.gamboost.pred, which = "bols(ObsForPred", xlab = "Number of instances")
p2.gam.pred = createSinglePartialEffectPlots(input = glmnet.df.gam.pred$ObsForPred, 
  model = glmnet.gamboost.pred, which = "bbs(ObsForPred", xlab = "Number of instances")
p3.gam.pred = createCombinedPartialEffectPlots(input = glmnet.df.gam.pred$ObsForPred,
  model = glmnet.gamboost.pred, which = "ObsForPred", xlab = "Number of instances")
multiplot(p1.gam.pred, p2.gam.pred, p3.gam.pred, cols = 3)

p4.gam.pred = createSinglePartialEffectPlots(input = glmnet.df.gam.pred$NumberOfFeatures, 
  model = glmnet.gamboost.pred, which = "bols(NumberOfFeatures", xlab = "Number of features")
p5.gam.pred = createSinglePartialEffectPlots(input = glmnet.df.gam.pred$NumberOfFeatures, 
  model = glmnet.gamboost.pred, which = "bbs(NumberOfFeatures", xlab = "Number of features")
p6.gam.pred = createCombinedPartialEffectPlots(input = glmnet.df.gam.pred$NumberOfFeatures,
  model = glmnet.gamboost.pred, which = "NumberOfFeatures", xlab = "Number of features")
multiplot(p4.gam.pred, p5.gam.pred, p6.gam.pred, cols = 3)

p7.gam.pred = createSinglePartialEffectPlots(input = glmnet.df.gam.pred$NumberOfClasses, 
  model = glmnet.gamboost.pred, which = "bols(NumberOfClasses", xlab = "Number of classes")
p8.gam.pred = createSinglePartialEffectPlots(input = glmnet.df.gam.pred$NumberOfClasses, 
  model = glmnet.gamboost.pred, which = "bbs(NumberOfClasses", xlab = "Number of classes")
p9.gam.pred = createCombinedPartialEffectPlots(input = glmnet.df.gam.pred$NumberOfClasses,
  model = glmnet.gamboost.pred, which = "NumberOfClasses", xlab = "Number of classes")
multiplot(p7.gam.pred, p8.gam.pred, p9.gam.pred, cols = 3)

p10.gam.pred = createSinglePartialEffectPlots(input = glmnet.df.gam.pred$MajorityClassSize, 
  model = glmnet.gamboost.pred, which = "bols(MajorityClassSize", xlab = "Majority class size")
p11.gam.pred = createSinglePartialEffectPlots(input = glmnet.df.gam.pred$MajorityClassSize, 
  model = glmnet.gamboost.pred, which = "bbs(MajorityClassSize", xlab = "Majority class size")
p12.gam.pred = createCombinedPartialEffectPlots(input = glmnet.df.gam.pred$MajorityClassSize,
  model = glmnet.gamboost.pred, which = "MajorityClassSize", xlab = "Majority class size")
multiplot(p10.gam.pred, p11.gam.pred, p12.gam.pred, cols = 3)

p13.gam.pred = createSinglePartialEffectPlots(input = glmnet.df.gam.pred$alpha, 
  model = glmnet.gamboost.pred, which = "bols(alpha", xlab = "alpha")
p14.gam.pred = createSinglePartialEffectPlots(input = glmnet.df.gam.pred$alpha, 
  model = glmnet.gamboost.pred, which = "bbs(alpha", xlab = "alpha")
p15.gam.pred = createCombinedPartialEffectPlots(input = glmnet.df.gam.pred$alpha,
  model = glmnet.gamboost.pred, which = "alpha", xlab = "alpha")
multiplot(p13.gam.pred, p14.gam.pred, p15.gam.pred, cols = 3)


#### glm ####
glm.glmnet.pred = glm(glmnet.glm.pred.fmla, data = glmnet.df.mean, family = Gamma(link = "log"))
step.glm.glmnet.pred = step(glm.glmnet.pred, direction = "both")
step.glm.glmnet.pred$anova
not.sel.glm.pred = c("log(NumberOfFeatures)", "I(ObsForPred^2)", "sqrt(MajorityClassSize)",
  "alpha", "I(alpha^2)")
coefs.glm.pred = summary(glm.glmnet.pred)$coefficients[,c(1,4)]
coefs.glm.pred[not.sel.glm.pred,] = rep(NA, length(not.sel.glm.pred))
coefs.glm.pred[complete.cases(coefs.glm.pred), ] = summary(step.glm.glmnet.pred)$coefficients[,c(1,4)]

# Create dataframe with estimated coefficients for glmboost and glm on prediction time
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/glmnet/coefs_glmnet_glmboost_pred.rda")
coefs.pred = cbind(coefs.glmnet.glmboost.pred,coefs.glm.pred)
save(coefs.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/glmnet/coefs_pred.rda")

# Save predictions
predictions.glm.pred = predict(step.glm.glmnet.pred, newdata = glmnet.df.mean, type = "response")
save(predictions.glm.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/glmnet/predictions_glm_pred.rda")

#### random Forest ####
glmnet.df.mean3 = glmnet.df.mean
glmnet.df.mean3$NumberOfInstances = glmnet.df.mean3$ObsForPred

glmnet.rF.pred = randomForest(timepredict.test.mean ~ MajorityClassSize +
    + NumberOfClasses + NumberOfFeatures + NumberOfInstances + alpha,
  data = glmnet.df.mean3, importance = TRUE)
plot(glmnet.rF.pred)

# save Predictions
predictions.rF.pred = predict(glmnet.rF.pred, newdata = glmnet.df.mean3, type = "response")
save(predictions.rF.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/glmnet/predictions_rF_pred.rda")

# Variable importance
imp.pred = importance(glmnet.rF.pred)
varImpPlot(glmnet.rF.pred, main = NULL)

# Partial dependence plot
impvar.pred <- rownames(imp.pred)[order(imp.pred[, 1], decreasing=TRUE)]
op <- par(mfrow=c(2, 3))
for (i in seq_along(imp.pred)) {
  partialPlot(glmnet.rF.pred, glmnet.df.mean, impvar.pred[i],
    xlab=impvar.pred[i],ylim=c(0.05, 0.18), main = "")
}
par(op)


#### Compare models ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/glmnet/coefs_test.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/glmnet/coefs_pred.rda")

# Create dataframe with estimated coefficients for glm and glmboost on training and
# prediction time 
all.coefs = cbind(test.coefs, coefs.pred)

# RMSE and RAE
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/glmnet/predictions_glmboost_test.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/glmnet/predictions_gamboost_test.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/glmnet/predictions_glm_test.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/glmnet/predictions_rF_test.rda")

errors.test = calculateErrorsForComparison2(time = "training", glmboost.pred = predictions.glmboost.test,
  glmboost.data = glmnet.df.mean, gamboost.pred = predictions.gamboost.test,
  gamboost.data = glmnet.df.gam.test, glm.pred = predictions.glm.test,
  glm.data = glmnet.df.mean, rF.pred = predictions.rF.test, rF.data = glmnet.df.mean2)

load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/glmnet/predictions_glmboost_pred.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/glmnet/predictions_gamboost_pred.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/glmnet/predictions_glm_pred.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/glmnet/predictions_rF_pred.rda")

errors.pred = calculateErrorsForComparison2(time = "prediction", glmboost.pred = predictions.glmboost.pred,
  glmboost.data = glmnet.df.mean, gamboost.pred = predictions.gamboost.pred,
  glmnet.df.gam.pred, glm.pred = predictions.glm.pred, glm.data = glmnet.df.mean,
  rF.pred = predictions.rF.pred, rF.data = glmnet.df.mean3)

# ggplot: obs vs fitted
df = data.frame(timetrain = glmnet.df.mean$timetrain.test.mean,
  glmboost = predictions.glmboost.test,
  gamboost = predictions.gamboost.test,
  glm = predictions.glm.test,
  randomForest = predictions.rF.test)
df2 = melt(df, id.vars = 1)
colnames(df2)[2] = "model"

df.pred = data.frame(timepredict = glmnet.df.mean$timepredict.test.mean,
  glmboost = predictions.glmboost.pred,
  gamboost = predictions.gamboost.pred,
  glm = predictions.glm.pred,
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
