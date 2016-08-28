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
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/gbmdfgam_test.RData")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/gbmdfgam_pred.RData")
set.seed(334)
opar = par()



#### TRAINING TIME ####

#### glmboost #####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/gbm_glmboost_test.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_gbm_glmboost_test.rda")

# Appropriate mstop
mstop(cvm.gbm.glmboost.test) # 40
plot(cvm.gbm.glmboost.test)

# Coefficinet Paths
par(mar=c(5.1,4.1,4.1,10.1))
plot(gbm.glmboost.test, off2int = TRUE, main = "Analysis of training time on gbm
  - inclusion of intercept")

par(mar=c(5.1,4.1,4.1,10.1))
plot(gbm.glmboost.test,
  ylim = range(coef(gbm.glmboost.test, which = 2:33)),
  main = "Analysis of training time on gbm - \n adjusted y-scale")

# Coefficients
coefs.gbm.glmboost = data.frame(round(coef(gbm.glmboost.test, which = ""),5))
colnames(coefs.gbm.glm) = "coefficients"

# Variables that were not chosen into the model
not.selected.gbm.glmboost = setdiff(variable.names(gbm.glmboost.test), variable.names(gbm.glmboost.test, usedonly = TRUE))
coefs.gbm.glmboost[not.selected.gbm.glm,] = rep(NA, length(not.selected.gbm.glm))
save(coefs.gbm.glmboost, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/gbm/coefs_gbm_glmboost.rda")

# ggplot partial effects
p1.test = createCombinedPartialEffectPlots(input = gbm.df.mean$ObsForTrain,
  model = gbm.glmboost.test, which = "ObsForTrain", xlab = "Number of instances")
p2.test = createCombinedPartialEffectPlots(input = gbm.df.mean$NumberOfFeatures,
  model = gbm.glmboost.test, which = "NumberOfFeatures", xlab = "Number of features")
p3.test = createCombinedPartialEffectPlots(input = gbm.df.mean$NumberOfClasses,
  model = gbm.glmboost.test, which = "NumberOfClasses", xlab = "Number of classes")
p4.test = createCombinedPartialEffectPlots(input = gbm.df.mean$MajorityClassSize,
  model = gbm.glmboost.test, which = "MajorityClassSize", xlab = "Majority class size")
p5.test = createCombinedPartialEffectPlots(input = gbm.df.mean$n.trees,
  model = gbm.glmboost.test, which = "n.trees", xlab = "n.trees")
p6.test = createCombinedPartialEffectPlots(input = gbm.df.mean$interaction.depth,
  model = gbm.glmboost.test, which = "interaction.depth", xlab = "interaction.depth")
p7.test = createCombinedPartialEffectPlots(input = gbm.df.mean$bag.fraction,
  model = gbm.glmboost.test, which = "bag.fraction", xlab = "bag.fraction")
p8.test = createCombinedPartialEffectPlots(input = gbm.df.mean$shrinkage.trafo,
  model = gbm.glmboost.test, which = "shrinkage.trafo", xlab = "shrinkage")

multiplot(p1.test, p2.test, p3.test, p4.test,p5.test,p6.test,p7.test, p8.test, cols = 2)


#### gamboost ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_gbm_gamboost_test.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/gbm_gamboost_test.rda")

# mstop
mstop(cvm.gbm.gamboost.test) # 212
plot(cvm.gbm.gamboost.test)

coef(gbm.gamboost.test)
not.selected.gbm.gam.test = setdiff(variable.names(gbm.gamboost.test), variable.names(gbm.gamboost.test, usedonly = TRUE))
# shrinkage.trafo

# ggplot for single and combined effects
# ggplot
p1.gam.test = createSinglePartialEffectPlots(input = gbm.df.gam.test$ObsForTrain, 
  model = gbm.gamboost.test, which = "bols(ObsForTrain", xlab = "Number of instances")
p2.gam.test = createSinglePartialEffectPlots(input = gbm.df.gam.test$ObsForTrain, 
  model = gbm.gamboost.test, which = "bbs(ObsForTrain", xlab = "Number of instances")
p3.gam.test = createCombinedPartialEffectPlots(input = gbm.df.gam.test$ObsForTrain,
  model = gbm.gamboost.test, which = "ObsForTrain", xlab = "Number of instances")
multiplot(p1.gam.test, p2.gam.test, p3.gam.test, cols = 3)

p4.gam.test = createSinglePartialEffectPlots(input = gbm.df.gam.test$NumberOfFeatures, 
  model = gbm.gamboost.test, which = "bols(NumberOfFeatures", xlab = "Number of features")
p5.gam.test = createSinglePartialEffectPlots(input = gbm.df.gam.test$NumberOfFeatures, 
  model = gbm.gamboost.test, which = "bbs(NumberOfFeatures", xlab = "Number of features")
p6.gam.test = createCombinedPartialEffectPlots(input = gbm.df.gam.test$NumberOfFeatures,
  model = gbm.gamboost.test, which = "NumberOfFeatures", xlab = "Number of features")
multiplot(p4.gam.test, p5.gam.test, p6.gam.test, cols = 3)

p7.gam.test = createSinglePartialEffectPlots(input = gbm.df.gam.test$NumberOfClasses, 
  model = gbm.gamboost.test, which = "bols(NumberOfClasses", xlab = "Number of classes")
p8.gam.test = createSinglePartialEffectPlots(input = gbm.df.gam.test$NumberOfClasses, 
  model = gbm.gamboost.test, which = "bbs(NumberOfClasses", xlab = "Number of classes")
p9.gam.test = createCombinedPartialEffectPlots(input = gbm.df.gam.test$NumberOfClasses,
  model = gbm.gamboost.test, which = "NumberOfClasses", xlab = "Number of classes")
multiplot(p7.gam.test, p8.gam.test, p9.gam.test, cols = 3)

p10.gam.test = createSinglePartialEffectPlots(input = gbm.df.gam.test$MajorityClassSize, 
  model = gbm.gamboost.test, which = "bols(MajorityClassSize", xlab = "Majority class size")
p11.gam.test = createSinglePartialEffectPlots(input = gbm.df.gam.test$MajorityClassSize, 
  model = gbm.gamboost.test, which = "bbs(MajorityClassSize", xlab = "Majority class size")
p12.gam.test = createCombinedPartialEffectPlots(input = gbm.df.gam.test$MajorityClassSize,
  model = gbm.gamboost.test, which = "MajorityClassSize", xlab = "Majority class size")
multiplot(p10.gam.test, p11.gam.test, p12.gam.test, cols = 3)

p13.gam.test = createSinglePartialEffectPlots(input = gbm.df.gam.test$n.trees, 
  model = gbm.gamboost.test, which = "bols(n.trees", xlab = "n.trees")
p14.gam.test = createSinglePartialEffectPlots(input = gbm.df.gam.test$n.trees, 
  model = gbm.gamboost.test, which = "bbs(n.trees", xlab = "n.trees")
p15.gam.test = createCombinedPartialEffectPlots(input = gbm.df.gam.test$n.trees,
  model = gbm.gamboost.test, which = "n.trees", xlab = "n.trees")
multiplot(p13.gam.test, p14.gam.test, p15.gam.test, cols = 3)

p16.gam.test = createSinglePartialEffectPlots(input = gbm.df.gam.test$interaction.depth, 
  model = gbm.gamboost.test, which = "bols(interaction.depth", xlab = "interaction.depth")
p17.gam.test = createSinglePartialEffectPlots(input = gbm.df.gam.test$interaction.depth, 
  model = gbm.gamboost.test, which = "bbs(interaction.depth", xlab = "interaction.depth")
p18.gam.test = createCombinedPartialEffectPlots(input = gbm.df.gam.test$interaction.depth,
  model = gbm.gamboost.test, which = "interaction.depth", xlab = "interaction.depth")
multiplot(p16.gam.test, p17.gam.test, p18.gam.test, cols = 3)

p19.gam.test = createSinglePartialEffectPlots(input = gbm.df.gam.test$bag.fraction, 
  model = gbm.gamboost.test, which = "bols(bag.fraction", xlab = "bag.fraction")
p20.gam.test = createSinglePartialEffectPlots(input = gbm.df.gam.test$bag.fraction, 
  model = gbm.gamboost.test, which = "bbs(bag.fraction", xlab = "bag.fraction")
p21.gam.test = createCombinedPartialEffectPlots(input = gbm.df.gam.test$bag.fraction,
  model = gbm.gamboost.test, which = "bag.fraction", xlab = "bag.fraction")
multiplot(p19.gam.test, p20.gam.test, p21.gam.test, cols = 3)


#### glm ####
glm.test.gbm = glm(gbm.glm.test.fmla, data = gbm.df.mean, family = Gamma(link = "log"))
step.glm.test.gbm = step(glm.test.gbm)
step.glm.test.gbm$anova
not.sel.glm.test = c("I(interaction.depth^2)", "log(interaction.depth)", "I(bag.fraction^2)", 
  "sqrt(ObsForTrain)", "sqrt(shrinkage.trafo)")
coefs.gbm.glm.test = summary(glm.test.gbm)$coefficients[,c(1,4)]
coefs.gbm.glm.test[not.sel.glm.test,] = rep(NA, length(not.sel.glm.test))
coefs.gbm.glm.test[complete.cases(coefs.gbm.glm.test),] = summary(step.glm.test.gbm)$coefficients[,c(1,4)]

# Create dataframe with estimated coefficients for glmboost and glm on training time
coefs.test = cbind(coefs.gbm.glmboost, coefs.gbm.glm.test)

# create partial effects for glm with ggplot2
p1.train.glm = createPartialEffectsForGlm(input = gbm.df.mean$ObsForTrain,
  step.glm.test.gbm, linear.effect = "ObsForTrain", 
  quadratic.effect = NULL, sqrt.effect = "sqrt(ObsForTrain)",
  log.effect = "log(ObsForTrain)",xlab = "Number of instances")
p2.train.glm = createPartialEffectsForGlm(input = gbm.df.mean$NumberOfFeatures,
  step.glm.test.gbm, linear.effect = "NumberOfFeatures", 
  quadratic.effect = "I(NumberOfFeatures^2)", sqrt.effect = "sqrt(NumberOfFeatures)",
  log.effect = "log(NumberOfFeatures)",xlab = "Number of features")
p3.train.glm = createPartialEffectsForGlm(input = gbm.df.mean$NumberOfClasses,
  step.glm.test.gbm, linear.effect = "NumberOfClasses", 
  quadratic.effect = "I(NumberOfClasses^2)", sqrt.effect = "sqrt(NumberOfClasses)",
  log.effect = "log(NumberOfClasses)",xlab = "Number of classes")
p4.train.glm = createPartialEffectsForGlm(input = gbm.df.mean$MajorityClassSize,
  step.glm.test.gbm, linear.effect = "MajorityClassSize", 
  quadratic.effect = "I(MajorityClassSize^2)", sqrt.effect = "sqrt(MajorityClassSize)",
  log.effect = "log(MajorityClassSize)",xlab = "Majority class size")
p5.train.glm = createPartialEffectsForGlm(input = gbm.df.mean$n.trees,
  step.glm.test.gbm, linear.effect = "n.trees", 
  quadratic.effect = "I(n.trees^2)", sqrt.effect = "sqrt(n.trees)",
  log.effect = "log(n.trees)",xlab = "n.trees")
p6.train.glm = createPartialEffectsForGlm(input = gbm.df.mean$interaction.depth,
  step.glm.test.gbm, linear.effect = "interaction.depth", 
  quadratic.effect = "I(interaction.depth^2)", sqrt.effect = "sqrt(interaction.depth)",
  log.effect = NULL ,xlab = "interaction.depth")
p7.train.glm = createPartialEffectsForGlm(input = gbm.df.mean$bag.fraction,
  step.glm.test.gbm, linear.effect = "bag.fraction", 
  quadratic.effect = "I(bag.fraction^2)", sqrt.effect = "sqrt(bag.fraction)",
  log.effect = NULL ,xlab = "bag.fraction")
p8.train.glm = createPartialEffectsForGlm(input = gbm.df.mean$shrinkage.trafo,
  step.glm.test.gbm, linear.effect = "shrinkage.trafo", 
  quadratic.effect = "I(shrinkage.trafo^2)", sqrt.effect = "sqrt(shrinkage.trafo)",
  log.effect = NULL ,xlab = "shrinkage")
multiplot(p1.train.glm, p2.train.glm, p3.train.glm, p4.train.glm, p5.train.glm,
  p6.train.glm, p7.train.glm, p8.train.glm, cols = 2)


#### random forest ####
gbm.df.mean2 = gbm.df.mean
gbm.df.mean2$shrinkage = gbm.df.mean2$shrinkage.trafo
gbm.df.mean2$NumberOfInstances = gbm.df.mean2$ObsForTrain

gbm.rF.test = randomForest(timetrain.test.mean ~ NumberOfInstances +
    NumberOfFeatures + NumberOfClasses + MajorityClassSize + n.trees + 
    interaction.depth + bag.fraction + shrinkage, data = gbm.df.mean2,
  importance = TRUE)

plot(gbm.rF.test)

varUsed(gbm.rF.test, by.tree=FALSE, count=TRUE)

# Variable Importance
imp.test = importance(gbm.rF.test)
varImpPlot(gbm.rF.test, main = NULL)

# Partial dependence plot
impvar.test <- rownames(imp.test)[order(imp.test[, 1], decreasing=TRUE)]
op <- par(mfrow=c(2, 4))
for (i in seq_along(impvar.test)) {
  partialPlot(gbm.rF.test, gbm.df.mean, impvar.test[i],
    xlab=impvar.test[i],ylim=c(30, 220), main = "")
}
par(op)

#### PREDICTION TIME ####

#### glmboost ####

load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/gbm_glmboost_pred.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_gbm_glmboost_pred.rda")

# Appropriate mstop
mstop(cvm.gbm.glmboost.pred) # 45
plot(cvm.gbm.glmboost.pred)

# Coefficinet Paths
par(mar=c(5.1,4.1,4.1,10.1))
plot(gbm.glmboost.pred, off2int = TRUE, main = "Analysis of training time on gbm
  - inclusion of intercept")

par(mar=c(5.1,4.1,4.1,10.1))
plot(gbm.glmboost.pred,
  ylim = range(coef(gbm.glmboost.test, which = 2:33)),
  main = "Analysis of training time on gbm - \n adjusted y-scale")

# Coefficients
coefs.gbm.glm.pred = data.frame(coef(gbm.glmboost.pred, which = ""))
colnames(coefs.gbm.glm.pred) = "coefficients"

# Variables that were not chosen into the model
not.selected.gbm.glm.pred = setdiff(variable.names(gbm.glmboost.pred), variable.names(gbm.glmboost.pred, usedonly = TRUE))
coefs.gbm.glm.pred[not.selected.gbm.glm.pred, ] = rep(NA, length(not.selected.gbm.glm.pred))

# ggplot partial effects
p1.test = createCombinedPartialEffectPlots(input = gbm.df.mean$ObsForPred,
  model = gbm.glmboost.pred, which = "ObsForPred", xlab = "Number of instances")
p2.test = createCombinedPartialEffectPlots(input = gbm.df.mean$NumberOfFeatures,
  model = gbm.glmboost.pred, which = "NumberOfFeatures", xlab = "Number of features")
p3.test = createCombinedPartialEffectPlots(input = gbm.df.mean$NumberOfClasses,
  model = gbm.glmboost.pred, which = "NumberOfClasses", xlab = "Number of classes")
p4.test = createCombinedPartialEffectPlots(input = gbm.df.mean$MajorityClassSize,
  model = gbm.glmboost.pred, which = "MajorityClassSize", xlab = "Majority class size")
p5.test = createCombinedPartialEffectPlots(input = gbm.df.mean$n.trees,
  model = gbm.glmboost.pred, which = "n.trees", xlab = "n.trees")
p6.test = createCombinedPartialEffectPlots(input = gbm.df.mean$interaction.depth,
  model = gbm.glmboost.pred, which = "interaction.depth", xlab = "interaction.depth")
p7.test = createCombinedPartialEffectPlots(input = gbm.df.mean$bag.fraction,
  model = gbm.glmboost.pred, which = "bag.fraction", xlab = "bag.fraction")
p8.test = createCombinedPartialEffectPlots(input = gbm.df.mean$shrinkage.trafo,
  model = gbm.glmboost.pred, which = "shrinkage.trafo", xlab = "shrinkage")

multiplot(p1.test, p2.test, p3.test, p4.test,p5.test,p6.test, p7.test, p8.test, cols = 2)


#### gamboost ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_gbm_gamboost_pred.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/gbm_gamboost_pred.rda")

# mstop
mstop(cvm.gbm.gamboost.pred) # 1006
plot(cvm.gbm.gamboost.pred)

coef(gbm.gamboost.pred)
not.selected.gbm.gam.pred = setdiff(variable.names(gbm.gamboost.pred), variable.names(gbm.gamboost.pred, usedonly = TRUE))
# intercept

# ggplot for single and combined effects
# ggplot
p1.gam.pred = createSinglePartialEffectPlots(input = gbm.df.gam.pred$ObsForPred, 
  model = gbm.gamboost.pred, which = "bols(ObsForPred", xlab = "Number of instances")
p2.gam.pred = createSinglePartialEffectPlots(input = gbm.df.gam.pred$ObsForPred, 
  model = gbm.gamboost.pred, which = "bbs(ObsForPred", xlab = "Number of instances")
p3.gam.pred = createCombinedPartialEffectPlots(input = gbm.df.gam.pred$ObsForPred,
  model = gbm.gamboost.pred, which = "ObsForPred", xlab = "Number of instances")
multiplot(p1.gam.pred, p2.gam.pred, p3.gam.pred, cols = 3)

p4.gam.pred = createSinglePartialEffectPlots(input = gbm.df.gam.pred$NumberOfFeatures, 
  model = gbm.gamboost.pred, which = "bols(NumberOfFeatures", xlab = "Number of features")
p5.gam.pred = createSinglePartialEffectPlots(input = gbm.df.gam.pred$NumberOfFeatures, 
  model = gbm.gamboost.pred, which = "bbs(NumberOfFeatures", xlab = "Number of features")
p6.gam.pred = createCombinedPartialEffectPlots(input = gbm.df.gam.pred$NumberOfFeatures,
  model = gbm.gamboost.pred, which = "NumberOfFeatures", xlab = "Number of features")
multiplot(p4.gam.pred, p5.gam.pred, p6.gam.pred, cols = 3)

p7.gam.pred = createSinglePartialEffectPlots(input = gbm.df.gam.pred$NumberOfClasses, 
  model = gbm.gamboost.pred, which = "bols(NumberOfClasses", xlab = "Number of classes")
p8.gam.pred = createSinglePartialEffectPlots(input = gbm.df.gam.pred$NumberOfClasses, 
  model = gbm.gamboost.pred, which = "bbs(NumberOfClasses", xlab = "Number of classes")
p9.gam.pred = createCombinedPartialEffectPlots(input = gbm.df.gam.pred$NumberOfClasses,
  model = gbm.gamboost.pred, which = "NumberOfClasses", xlab = "Number of classes")
multiplot(p7.gam.pred, p8.gam.pred, p9.gam.pred, cols = 3)

p10.gam.pred = createSinglePartialEffectPlots(input = gbm.df.gam.pred$MajorityClassSize, 
  model = gbm.gamboost.pred, which = "bols(MajorityClassSize", xlab = "Majority class size")
p11.gam.pred = createSinglePartialEffectPlots(input = gbm.df.gam.pred$MajorityClassSize, 
  model = gbm.gamboost.pred, which = "bbs(MajorityClassSize", xlab = "Majority class size")
p12.gam.pred = createCombinedPartialEffectPlots(input = gbm.df.gam.pred$MajorityClassSize,
  model = gbm.gamboost.pred, which = "MajorityClassSize", xlab = "Majority class size")
multiplot(p10.gam.pred, p11.gam.pred, p12.gam.pred, cols = 3)

p13.gam.pred = createSinglePartialEffectPlots(input = gbm.df.gam.pred$n.trees, 
  model = gbm.gamboost.pred, which = "bols(n.trees", xlab = "n.trees")
p14.gam.pred = createSinglePartialEffectPlots(input = gbm.df.gam.pred$n.trees, 
  model = gbm.gamboost.pred, which = "bbs(n.trees", xlab = "n.trees")
p15.gam.pred = createCombinedPartialEffectPlots(input = gbm.df.gam.pred$n.trees,
  model = gbm.gamboost.pred, which = "n.trees", xlab = "n.trees")
multiplot(p13.gam.pred, p14.gam.pred, p15.gam.pred, cols = 3)

p16.gam.pred = createSinglePartialEffectPlots(input = gbm.df.gam.pred$interaction.depth, 
  model = gbm.gamboost.pred, which = "bols(interaction.depth", xlab = "interaction.depth")
p17.gam.pred = createSinglePartialEffectPlots(input = gbm.df.gam.pred$interaction.depth, 
  model = gbm.gamboost.pred, which = "bbs(interaction.depth", xlab = "interaction.depth")
p18.gam.pred = createCombinedPartialEffectPlots(input = gbm.df.gam.pred$interaction.depth,
  model = gbm.gamboost.pred, which = "interaction.depth", xlab = "interaction.depth")
multiplot(p16.gam.pred, p17.gam.pred, p18.gam.pred, cols = 3)

p19.gam.pred = createSinglePartialEffectPlots(input = gbm.df.gam.pred$bag.fraction, 
  model = gbm.gamboost.pred, which = "bols(bag.fraction", xlab = "bag.fraction")
p20.gam.pred = createSinglePartialEffectPlots(input = gbm.df.gam.pred$bag.fraction, 
  model = gbm.gamboost.pred, which = "bbs(bag.fraction", xlab = "bag.fraction")
p21.gam.pred = createCombinedPartialEffectPlots(input = gbm.df.gam.pred$bag.fraction,
  model = gbm.gamboost.pred, which = "bag.fraction", xlab = "bag.fraction")
multiplot(p19.gam.pred, p20.gam.pred, p21.gam.pred, cols = 3)

p22.gam.pred = createSinglePartialEffectPlots(input = gbm.df.gam.pred$shrinkage.trafo, 
  model = gbm.gamboost.pred, which = "bols(shrinkage.trafo", xlab = "shrinkage")
p23.gam.pred = createSinglePartialEffectPlots(input = gbm.df.gam.pred$shrinkage.trafo, 
  model = gbm.gamboost.pred, which = "bbs(shrinkage.trafo", xlab = "shrinkage")
p24.gam.pred = createCombinedPartialEffectPlots(input = gbm.df.gam.pred$shrinkage.trafo,
  model = gbm.gamboost.pred, which = "shrinkage.trafo", xlab = "shrinkage")
multiplot(p22.gam.pred, p23.gam.pred, p24.gam.pred, cols = 3)


#### glm ####  

glm.pred.gbm = glm(gbm.glm.pred.fmla, data = gbm.df.mean, family = Gamma(link = "log"))
step.glm.pred.gbm = step(glm.pred.gbm)
step.glm.pred.gbm$anova
not.sel.glm.pred = c("interaction.depth", "I(shrinkage.trafo^2)", "sqrt(MajorityClassSize)",
  "log(bag.fraction)", "I(ObsForPred^2)", "shrinkage.trafo", "sqrt(shrinkage.trafo)",
  "log(MajorityClassSize)", "log(shrinkage.trafo)", "I(interaction.depth^2)",
  "sqrt(interaction.depth)")
coefs.glm.pred = summary(glm.pred.gbm)$coefficients[,c(1,4)]
coefs.glm.pred[not.sel.glm.pred, ] = rep(NA, length(not.sel.glm.pred))
coefs.glm.pred[complete.cases(coefs.glm.pred),] = summary(step.glm.pred.gbm)$coefficients[,c(1,4)]

# Create dataframe with estimated coefficients for glmboost and glm on prediction time
coefs.pred = cbind(coefs.gbm.glm.pred, coefs.glm.pred)

#### randomForest ####
gbm.df.mean3 = gbm.df.mean
gbm.df.mean3$shrinkage = gbm.df.mean3$shrinkage.trafo
gbm.df.mean3$NumberOfInstances = gbm.df.mean3$ObsForPred

gbm.rF.pred = randomForest(timepredict.test.mean ~ NumberOfInstances +
    NumberOfFeatures + NumberOfClasses + MajorityClassSize + n.trees + 
    interaction.depth + bag.fraction + shrinkage, data = gbm.df.mean3,
  importance = TRUE)

plot(gbm.rF.pred)

varUsed(gbm.rF.pred, by.tree=FALSE, count=TRUE)

# Variable importance
imp.pred = importance(gbm.rF.pred)
varImpPlot(gbm.rF.pred, main = NULL)

# Partial dependence plot
impvar.pred <- rownames(imp.pred)[order(imp.pred[, 1], decreasing=TRUE)]
op <- par(mfrow=c(2, 4))
for (i in seq_along(impvar.pred)) {
  partialPlot(gbm.rF.pred, gbm.df.mean, impvar.pred[i],
    xlab=impvar.pred[i],ylim=c(0.3, 2), main = "")
}
par(op)


#### Compare models ####
# Create dataframe with estimated coefficients for glm and glmboost on training and
# prediction time
coefs.all = cbind(coefs.test, coefs.pred)

# Calculate RMSE and RAE
errors.test = calculateErrorsForComparison(time = "training", glmboost = gbm.glmboost.test,
  glmboost.data = gbm.df.mean, gamboost = gbm.gamboost.test,
  gamboost.data = gbm.df.gam.test, glm = step.glm.test.gbm, glm.data = gbm.df.mean,
  rF = gbm.rF.test, rF.data = gbm.df.mean2)

errors.pred = calculateErrorsForComparison(time = "prediction", glmboost = gbm.glmboost.pred,
  glmboost.data = gbm.df.mean, gamboost = gbm.gamboost.pred,
  gamboost.data = gbm.df.gam.pred, glm = step.glm.pred.gbm, glm.data = gbm.df.mean,
  rF = gbm.rF.pred, rF.data = gbm.df.mean3)

# ggplot: obs vs fitted
plot(gbm.df.mean$timetrain.test.mean, predict(gbm.glmboost.test, newdata = gbm.df.mean,
  type = "response"), ylim = c(0,3000), col = "red")
points(gbm.df.mean$timetrain.test.mean, predict(step.glm.test.gbm, newdata = gbm.df.mean,
  type = "response"), col = "black")
points(gbm.df.mean$timetrain.test.mean, predict(gbm.gamboost.test, newdata = gbm.df.gam.test,
  type = "response"), col = "blue")
points(gbm.df.mean$timetrain.test.mean, predict(gbm.rF.test, newdata = gbm.df.mean,
  type = "response"), col = "green")
abline(0,1)


plot(gbm.df.mean$timepredict.test.mean, predict(gbm.glmboost.pred, newdata = gbm.df.mean,
  type = "response"), ylim = c(0,12), col = "red")
points(gbm.df.mean$timepredict.test.mean, predict(step.glm.pred.gbm, newdata = gbm.df.mean,
  type = "response"), col = "black")
points(gbm.df.mean$timepredict.test.mean, predict(gbm.gamboost.pred, newdata = gbm.df.gam.pred,
  type = "response"), col = "blue")
points(gbm.df.mean$timepredict.test.mean, predict(gbm.rF.pred, newdata = gbm.df.mean,
  type = "response"), col = "green")
abline(0,1)


# ggplot
df = data.frame(timetrain = gbm.df.mean$timetrain.test.mean,
  glmboost = as.numeric(predict(gbm.glmboost.test,type = "response", newdata = gbm.df.mean)),
  gamboost = as.numeric(predict(gbm.gamboost.test, newdata = gbm.df.gam.test, type = "response")),
  glm = as.numeric(predict(step.glm.test.gbm, newdata = gbm.df.mean,type = "response")),
  randomForest = as.numeric(predict(gbm.rF.test, newdata = gbm.df.mean2, type = "response")))
df2 = melt(df, id.vars = 1)
colnames(df2)[2] = "model"

df.pred = data.frame(timepredict = gbm.df.mean$timepredict.test.mean,
  glmboost = as.numeric(predict(gbm.glmboost.pred, type = "response", newdata = gbm.df.mean)),
  gamboost = as.numeric(predict(gbm.gamboost.pred, newdata = gbm.df.gam.pred, type = "response")),
  glm = as.numeric(predict(step.glm.pred.gbm, newdata = gbm.df.mean,type = "response")),
  randomForest = as.numeric(predict(gbm.rF.pred, newdata = gbm.df.mean3, type = "response")))
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
