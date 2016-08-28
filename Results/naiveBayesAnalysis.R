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
set.seed(334)
opar = par()


#### RESPONSE = TRAIN-TIME ####

#### glmboost ####

# Load model fit and mstop
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/naiveBayes_glmboost_test.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_naiveBayes_glmboost_test.rda")

# Appropriate mstop
mstop(cvm.naiveBayes.glmboost.test) #51778
plot(cvm.naiveBayes.glmboost.test)

# Coefficient Paths
par(mar=c(5.1,4.1,4.1,10.7))
plot(naiveBayes.glmboost.test, off2int = TRUE, main = "Analysis of training time on naiveBayes
  - inclusion of intercept")

par(mar=c(5.1,4.1,4.1,10.7))
plot(naiveBayes.glmboost.test,
  ylim = range(coef(naiveBayes.glmboost.test, which = 2:13)),
  main = "Analysis of training time on naiveBayes - \n adjusted y-scale")
par(opar)

# coefficients
freq.coef.naiveBayes.glm.test = cbind(table(naiveBayes.glmboost.test$xselect()),
  table(naiveBayes.glmboost.test$xselect())/mstop(cvm.naiveBayes.glmboost.test))

coefs.naiveBayes.glm = data.frame(coef(naiveBayes.glmboost.test, which = ""))
colnames(coefs.naiveBayes.glm) = "coefficients"

# Variables that were not chosen into the model
not.selected.naiveBayes.glm = setdiff(variable.names(naiveBayes.glmboost.test), variable.names(naiveBayes.glmboost.test, usedonly = TRUE))

# Single Effects
nme = names(coef(naiveBayes.glmboost.test))[2:13]
par(mfrow = c(2,2))
for (j in c(1,5,9)){
  for (i in j:(j+3)) {
    plot(naiveBayes.df.mean$timetrain.test.mean, predict(naiveBayes.glmboost.test, which = i),
      xlab = nme[i], ylab = "Fitted values")
  }
}

# Partial effects on the sum of effects for single covariates
plot(naiveBayes.df.mean$NumberOfFeatures, rowSums(predict(naiveBayes.glmboost.test, which = "NumberOfFeatures")))
plot(naiveBayes.df.mean$NumberOfClasses, rowSums(predict(naiveBayes.glmboost.test, which = "NumberOfClasses")))
plot(naiveBayes.df.mean$MajorityClassSize, rowSums(predict(naiveBayes.glmboost.test, which = "MajorityClassSize")))
plot(naiveBayes.df.mean$ObsForTrain, rowSums(predict(naiveBayes.glmboost.test, which = "ObsForTrain")))

# ggplot2
p1.pe.glm.test = createCombinedPartialEffectPlots(input = naiveBayes.df.mean$ObsForTrain,
  model = naiveBayes.glmboost.test, which = "ObsForTrain", xlab = "Number of instances")
p2.pe.glm.test = createCombinedPartialEffectPlots(input = naiveBayes.df.mean$NumberOfFeatures,
  model = naiveBayes.glmboost.test, which = "NumberOfFeatures", xlab = "Number of features")
p3.pe.glm.test = createCombinedPartialEffectPlots(input = naiveBayes.df.mean$NumberOfClasses,
  model = naiveBayes.glmboost.test, which = "NumberOfClasses", xlab = "Number of classes")
p4.pe.glm.test = createCombinedPartialEffectPlots(input = naiveBayes.df.mean$MajorityClassSize,
  model = naiveBayes.glmboost.test, which = "MajorityClassSize", xlab = "Majority class size")

multiplot(p1.pe.glm.test, p2.pe.glm.test, p3.pe.glm.test, p4.pe.glm.test, cols=2)


# Model with interaction
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/naiveBayes_glmboost_test.int.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_naiveBayes_glmboost_test.int.rda")

mstop(cvm.naiveBayes.glmboost.test.int) #14897

# Frequencies
freq.coef.naiveBayes.glm.test.int = cbind(table(naiveBayes.glmboost.test.int$xselect()),
  table(naiveBayes.glmboost.test.int$xselect())/mstop(cvm.naiveBayes.glmboost.test.int))

# Estimated coefficients
coefs.naiveBayes.glm.test.int = data.frame(coef(naiveBayes.glmboost.test.int, which = ""))
colnames(coefs.naiveBayes.glm.test.int) = "coefficients"
not.selected.naiveBayes.glmboost.int = setdiff(variable.names(naiveBayes.glmboost.test.int), variable.names(naiveBayes.glmboost.test.int, usedonly = TRUE))
coefs.naiveBayes.glm.test.int[not.selected.naiveBayes.glmboost.int, ] = rep(NA, length(not.selected.naiveBayes.glmboost.int))

# Plot partial effects
plot(naiveBayes.df.mean$timetrain.test.mean, rowSums(predict(naiveBayes.glmboost.test.int,
  which = "NumberOfFeatures")))
plot(naiveBayes.df.mean$timetrain.test.mean, rowSums(predict(naiveBayes.glmboost.test.int,
  which = "ObsForTrain")))
plot(naiveBayes.df.mean$timetrain.test.mean, rowSums(predict(naiveBayes.glmboost.test.int,
  which = "NumberOfClasses")))
plot(naiveBayes.df.mean$timetrain.test.mean, rowSums(predict(naiveBayes.glmboost.test.int,
  which = "MajorityClassSize")))

# ggplot2
df2.glm.test = data.frame(NumberOfFeatures =naiveBayes.df.mean$NumberOfFeatures, 
  part.effect = rowSums(predict(naiveBayes.glmboost.test.int, which = "NumberOfFeatures")))
df3.glm.test = data.frame(NumberOfClasses = naiveBayes.df.mean$NumberOfClasses,
  part.effect = rowSums(predict(naiveBayes.glmboost.test.int, which = "NumberOfClasses")))
df4.glm.test = data.frame(MajorityClassSize = naiveBayes.df.mean$MajorityClassSize,
  part.effect = rowSums(predict(naiveBayes.glmboost.test.int, which = "MajorityClassSize")))
df1.glm.test = data.frame(ObsForTrain = naiveBayes.df.mean$ObsForTrain,
  part.effect = rowSums(predict(naiveBayes.glmboost.test.int, which = "ObsForTrain")))

p1.pe.glm.test = ggplot(df1.glm.test, aes(x = ObsForTrain, y = part.effect)) + 
  geom_point(size = 0.5) + 
  scale_x_continuous(breaks = c(10000, 30000)) +
  labs (x = "Number of instances", y = "Parital effect")
p2.pe.glm.test = ggplot(df2.glm.test, aes(x = NumberOfFeatures, y = part.effect)) + 
  geom_point(size = 0.5) + 
  labs (x = "Number of features", y = "Parital effect")
p3.pe.glm.test = ggplot(df3.glm.test, aes(x = NumberOfClasses, y = part.effect)) + 
  geom_point(size = 0.5) + 
  labs (x = "Number of classes", y = "Parital effect")
p4.pe.glm.test = ggplot(df4.glm.test, aes(x = MajorityClassSize, y = part.effect)) + 
  geom_point(size = 0.5) + 
  labs (x = "Majority class size", y = "Parital effect")
multiplot(p1.pe.glm.test, p2.pe.glm.test, p3.pe.glm.test, p4.pe.glm.test, cols=2)


#### gamboost ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/naiveBayes_gamboost_test.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_naiveBayes_gamboost_test.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/naiveBayesdfgam_test.RData")

# mstop
mstop(cvm.naiveBayes.gamboost.test) # 46524
plot(cvm.naiveBayes.gamboost.test)

# Selection frequency
freq.coef.naiveBayes.gam.test = cbind(table(naiveBayes.gamboost.test$xselect()),
  table(naiveBayes.gamboost.test$xselect())/mstop(cvm.naiveBayes.gamboost.test))

# Estimated coefficients for the linear components
lin.eff.naiveBayes.gam.test = round(data.frame(unlist(coef(naiveBayes.gamboost.test, which = "bols"))), 3)

coef(naiveBayes.gamboost.test, which = "")
names(coef(naiveBayes.gamboost.test, which = ""))
not.selected.naiveBayes.gam = setdiff(variable.names(naiveBayes.gamboost.test), variable.names(naiveBayes.gamboost.test, usedonly = TRUE))

par(mfrow=c(3,3))
plot(naiveBayes.gamboost.test)
par(opar)

# ggplot
p1.gam.test = createSinglePartialEffectPlots(input = naiveBayes.df.gam.test$ObsForTrain, 
  model = naiveBayes.gamboost.test, which = "bols(ObsForTrain", xlab = "Number of instances")
p2.gam.test = createSinglePartialEffectPlots(input = naiveBayes.df.gam.test$ObsForTrain, 
  model = naiveBayes.gamboost.test, which = "bbs(ObsForTrain", xlab = "Number of instances")
p3.gam.test = createCombinedPartialEffectPlots(input = naiveBayes.df.gam.test$ObsForTrain,
  model = naiveBayes.gamboost.test, which = "ObsForTrain", xlab = "Number of instances")
multiplot(p1.gam.test, p2.gam.test, p3.gam.test, cols = 3)

p4.gam.test = createSinglePartialEffectPlots(input = naiveBayes.df.gam.test$NumberOfFeatures, 
  model = naiveBayes.gamboost.test, which = "bols(NumberOfFeatures", xlab = "Number of features")
p5.gam.test = createSinglePartialEffectPlots(input = naiveBayes.df.gam.test$NumberOfFeatures, 
  model = naiveBayes.gamboost.test, which = "bbs(NumberOfFeatures", xlab = "Number of features")
p6.gam.test = createCombinedPartialEffectPlots(input = naiveBayes.df.gam.test$NumberOfFeatures,
  model = naiveBayes.gamboost.test, which = "NumberOfFeatures", xlab = "Number of features")
multiplot(p4.gam.test, p5.gam.test, p6.gam.test, cols = 3)

p7.gam.test = createSinglePartialEffectPlots(input = naiveBayes.df.gam.test$NumberOfClasses, 
  model = naiveBayes.gamboost.test, which = "bols(NumberOfClasses", xlab = "Number of classes")
p8.gam.test = createSinglePartialEffectPlots(input = naiveBayes.df.gam.test$NumberOfClasses, 
  model = naiveBayes.gamboost.test, which = "bbs(NumberOfClasses", xlab = "Number of classes")
p9.gam.test = createCombinedPartialEffectPlots(input = naiveBayes.df.gam.test$NumberOfClasses,
  model = naiveBayes.gamboost.test, which = "NumberOfClasses", xlab = "Number of classes")
multiplot(p7.gam.test, p8.gam.test, p9.gam.test, cols = 3)

p10.gam.test = createSinglePartialEffectPlots(input = naiveBayes.df.gam.test$MajorityClassSize, 
  model = naiveBayes.gamboost.test, which = "bols(MajorityClassSize", xlab = "Majority class size")
p11.gam.test = createSinglePartialEffectPlots(input = naiveBayes.df.gam.test$MajorityClassSize, 
  model = naiveBayes.gamboost.test, which = "bbs(MajorityClassSize", xlab = "Majority class size")
p12.gam.test = createCombinedPartialEffectPlots(input = naiveBayes.df.gam.test$MajorityClassSize,
  model = naiveBayes.gamboost.test, which = "MajorityClassSize", xlab = "Majority class size")
multiplot(p10.gam.test, p11.gam.test, p12.gam.test, cols = 3)


# Model with interaction
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/naiveBayes_gamboost_test_int.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_naiveBayes_gamboost_test_int.rda")

# mstop
mstop(cvm.naiveBayes.gamboost.test.int)

# Estimated coefficients for linear components
lin.eff.naiveBayes.gamboost.int = round(data.frame(unlist(coef(naiveBayes.gamboost.test.int, which = "bols"))),4)

# Variables, that were excluded from the model
not.selected.naiveBayes.gam.int = setdiff(variable.names(naiveBayes.gamboost.test.int), variable.names(naiveBayes.gamboost.test.int, usedonly = TRUE))

# Visualisation
par(mfrow = c(1,2))
plot(naiveBayes.gamboost.test.int, which = "MajorityClassSize")
par(mfrow = c(1,1))
plot(naiveBayes.df.gam.test$MajorityClassSize, rowSums(predict(naiveBayes.gamboost.test.int, which = "MajorityClassSize")),
  ylab = "partial effect", xlab = "Majority Class Size")
# linear incresing, linearily decreasing, linearily increasing

par(mfrow = c(1,2))
plot(naiveBayes.gamboost.test.int, which = "NumberOfClasses")
par(mfrow = c(1,1))
plot(naiveBayes.df.gam.test$NumberOfClasses, rowSums(predict(naiveBayes.gamboost.test.int, which = "NumberOfClasses")),
  ylab = "partial effect", xlab = "Number of Classes")  

par(mfrow = c(1,3))
plot(naiveBayes.gamboost.test.int, which = "NumberOfFeatures")
par(mfrow = c(1,1))
plot(naiveBayes.df.gam.test$NumberOfFeatures, rowSums(predict(naiveBayes.gamboost.test.int, which = "NumberOfFeatures")),
  ylab = "partial effect", xlab = "Number of Features")  

par(mfrow = c(1,3))
plot(naiveBayes.gamboost.test.int, which = "ObsForTrain")
par(mfrow = c(1,1))
plot(naiveBayes.df.gam.test$ObsForTrain, rowSums(predict(naiveBayes.gamboost.test.int, which = "ObsForTrain")),
  ylab = "partial effect", xlab = "Number of instances used for training")  


#### glm ####

glm.naiveBayes.test = glm(naiveBayes.glm.test.fmla, data = naiveBayes.df.mean,
  family = Gamma(link = "log"))
step.glm.naiveBayes.test = step(glm.naiveBayes.test)
step.glm.naiveBayes.test$anova
# I(ObsForTrain^2), I(NumberOfClasses^2) not in model
coeffs.glm.naiveBayes.test = summary(step.glm.naiveBayes.test)$coefficients

# Create dataframe with estimated coefficients for glmboost and glm on training time
coeffs.glm.naiveBayes.test = rbind(
  coeffs.glm.naiveBayes.test[1:6, c(1,4)],
  quad.NumberOfClasses = c(NA, NA),
  quad.NumberOfFeatures = coeffs.glm.naiveBayes.test[7, c(1,4)],
  quad.NumberOfInstances = c(NA,NA),
  coeffs.glm.naiveBayes.test[8:15, c(1,4)]
)

coefs.test = cbind(coefs.naiveBayes.glm, coeffs.glm.naiveBayes.test)


# Model with interaction
int.fmla.test = as.formula(paste("timetrain.test.mean ~ ", paste(
  paste0(naiveBayes.preds.test, collapse = " + "),
  paste0("I(", naiveBayes.preds.test, "^2)", collapse = " + "),
  paste0("sqrt(", naiveBayes.preds.test, ")", collapse = " + "), 
  paste0("log(", naiveBayes.preds.test, ")", collapse = " + "),
  sep = " + ",
  paste0("ObsForTrain*NumberOfFeatures"))))

glm.naiveBayes.test.int = glm(int.fmla.test, data = naiveBayes.df.mean,
  family = Gamma(link = "log"))
step.glm.naiveBayes.test.int = step(glm.naiveBayes.test.int)
step.glm.naiveBayes.test.int$anova
n.sel.glm.test = c("I(MajorityClassSize^2)", "I(NumberOfClasses^2)", "sqrt(ObsForTrain)")
coeffs.glm.naiveBayes.test.int = summary(glm.naiveBayes.test.int)$coefficients[,c(1,4)]
coeffs.glm.naiveBayes.test.int[n.sel.glm.test,] = rep(NA, length(n.sel.glm.test))
coeffs.glm.naiveBayes.test.int[complete.cases(coeffs.glm.naiveBayes.test.int),] = summary(step.glm.naiveBayes.test.int)$coefficients[,c(1,4)]

coeffs.test.int = cbind(coefs.naiveBayes.glm.test.int, coeffs.glm.naiveBayes.test.int)

#### randomForest ####
# ugly hack to change the labels in varImpPlot but everything else needs
# a lot of extra work
naiveBayes.df.mean2 = naiveBayes.df.mean
naiveBayes.df.mean2$NumberOfInstances = naiveBayes.df.mean2$ObsForTrain

naiveBayes.rF = randomForest(timetrain.test.mean ~ MajorityClassSize +
    + NumberOfClasses + NumberOfFeatures + NumberOfInstances, data = naiveBayes.df.mean2,
  importance = TRUE)
plot(naiveBayes.rF)

# Variable importance
imp.test = importance(naiveBayes.rF)
varImpPlot(naiveBayes.rF, main = "")

# Partial dependence plot
impvar <- rownames(imp.test)[order(imp.test[, 1], decreasing=TRUE)]
op <- par(mfrow=c(2, 2))
for (i in seq_along(impvar)) {
  partialPlot(naiveBayes.rF, naiveBayes.df.mean, impvar[i], 
    xlab=impvar[i],ylim=c(0, 0.3), main = "")
}
par(op)


################################################################################


#### RESPONSE = PREDICTION TIME ####

#### glmboost ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_naiveBayes_glmboost_pred.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/naiveBayes_glmboost_pred.rda")

# mstop
mstop(cvm.naiveBayes.glmboost.pred) # 20
plot(cvm.naiveBayes.glmboost.pred)

# Coefficient paths
par(mar=c(5.1,4.1,4.1,10.1))
plot(naiveBayes.glmboost.pred, off2int = TRUE, main = "Analysis of prediction time on naiveBayes
  - inclusion of intercept")

par(mar=c(5.1,4.1,4.1,10.1))
plot(naiveBayes.glmboost.pred,
  ylim = range(coef(naiveBayes.glmboost.pred, which = 2:17)),
  main = "Analysis of prediction time on naive Bayes - \n adjusted y-scale")

# Estimated coefficients
coefs.naiveBayes.glmboost.pred = data.frame(coef(naiveBayes.glmboost.pred, which = ""))
not.selected.naiveBayes.glm.pred = setdiff(variable.names(naiveBayes.glmboost.pred), variable.names(naiveBayes.glmboost.pred, usedonly = TRUE))
coefs.naiveBayes.glmboost.pred[not.selected.naiveBayes.glm.pred,] = rep(NA, length(not.selected.naiveBayes.glm.pred))

# ggplot: combined parital effects
p1.pred = createCombinedPartialEffectPlots(input = naiveBayes.df.mean$ObsForPred,
  model = naiveBayes.glmboost.pred, which = "ObsForPred", xlab = "Number of instances")
p2.pred = createCombinedPartialEffectPlots(input = naiveBayes.df.mean$NumberOfFeatures,
  model = naiveBayes.glmboost.pred, which = "NumberOfFeatures", xlab = "Number of features")
p3.pred = createCombinedPartialEffectPlots(input = naiveBayes.df.mean$NumberOfClasses,
  model = naiveBayes.glmboost.pred, which = "NumberOfClasses", xlab = "Number of classes")
p4.pred = createCombinedPartialEffectPlots(input = naiveBayes.df.mean$MajorityClassSize,
  model = naiveBayes.glmboost.pred, which = "MajorityClassSize", xlab = "Majority class size")
multiplot(p1.pred, p2.pred, p3.pred, p4.pred, cols=2)

# Results with interaction
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_naiveBayes_glmboost_pred_int.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/naiveBayes_glmboost_pred_int.rda")

mstop(naiveBayes.glmboost.pred.int)
coefs.naiveBayes.glmboost.pred.int = data.frame(coef(naiveBayes.glmboost.pred.int, which = ""))
not.selected.naiveBayes.glm.pred.int = setdiff(variable.names(naiveBayes.glmboost.pred.int), variable.names(naiveBayes.glmboost.pred.int, usedonly = TRUE))
coefs.naiveBayes.glmboost.pred.int[not.selected.naiveBayes.glm.pred.int, ] = rep(NA, length(not.selected.naiveBayes.glm.pred.int))


#### gamboost ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_naiveBayes_gamboost_pred.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/naiveBayes_gamboost_pred.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/naiveBayesdfgam_pred.RData")

# mstop
mstop(cvm.naiveBayes.gamboost.pred) # 561
plot(cvm.naiveBayes.gamboost.pred)

# Selection frequencies
sel.freq.naiveBayes.gam.pred = table(naiveBayes.gamboost.pred$xselect()[1:mstop(cvm.naiveBayes.gamboost.pred)])

# Estimated coefficients
coef(naiveBayes.gamboost.pred, which = "")
names(coef(naiveBayes.gamboost.pred, which = ""))

names(coef(naiveBayes.gamboost.pred, which = "bols"))
lin.eff.naiveBayes.gam.pred = round(data.frame(unlist(coef(naiveBayes.gamboost.pred, which = "bols"))), 3)
colnames(lin.eff.naiveBayes.gam.pred) = "coefficients"

# Variavles that were excluded from the model
not.selected.naiveBayes.gam.pred = setdiff(variable.names(naiveBayes.gamboost.pred), variable.names(naiveBayes.gamboost.pred, usedonly = TRUE))

# Partial and combined partial effects (ggplot)
p1.gam.pred = createSinglePartialEffectPlots(input = naiveBayes.df.gam.pred$ObsForPred, 
  model = naiveBayes.gamboost.pred, which = "bols(ObsForPred", xlab = "Number of instances")
p2.gam.pred = createSinglePartialEffectPlots(input = naiveBayes.df.gam.pred$ObsForPred, 
  model = naiveBayes.gamboost.pred, which = "bbs(ObsForPred", xlab = "Number of instances")
p3.gam.pred = createCombinedPartialEffectPlots(input = naiveBayes.df.gam.pred$ObsForPred,
  model = naiveBayes.gamboost.pred, which = "ObsForPred", xlab = "Number of instances")
multiplot(p1.gam.pred, p2.gam.pred, p3.gam.pred, cols = 3)

p4.gam.pred = createSinglePartialEffectPlots(input = naiveBayes.df.gam.pred$NumberOfFeatures, 
  model = naiveBayes.gamboost.pred, which = "bols(NumberOfFeatures", xlab = "Number of features")
p5.gam.pred = createSinglePartialEffectPlots(input = naiveBayes.df.gam.pred$NumberOfFeatures, 
  model = naiveBayes.gamboost.pred, which = "bbs(NumberOfFeatures", xlab = "Number of features")
p6.gam.pred = createCombinedPartialEffectPlots(input = naiveBayes.df.gam.pred$NumberOfFeatures,
  model = naiveBayes.gamboost.pred, which = "NumberOfFeatures", xlab = "Number of features")
multiplot(p4.gam.pred, p5.gam.pred, p6.gam.pred, cols = 3)

p7.gam.pred = createSinglePartialEffectPlots(input = naiveBayes.df.gam.pred$NumberOfClasses, 
  model = naiveBayes.gamboost.pred, which = "bols(NumberOfClasses", xlab = "Number of classes")
p8.gam.pred = createSinglePartialEffectPlots(input = naiveBayes.df.gam.pred$NumberOfClasses, 
  model = naiveBayes.gamboost.pred, which = "bbs(NumberOfClasses", xlab = "Number of classes")
p9.gam.pred = createCombinedPartialEffectPlots(input = naiveBayes.df.gam.pred$NumberOfClasses,
  model = naiveBayes.gamboost.pred, which = "NumberOfClasses", xlab = "Number of classes")
multiplot(p7.gam.pred, p8.gam.pred, p9.gam.pred, cols = 3)

p10.gam.pred = createSinglePartialEffectPlots(input = naiveBayes.df.gam.pred$MajorityClassSize, 
  model = naiveBayes.gamboost.pred, which = "bols(MajorityClassSize", xlab = "Majority class size")
p11.gam.pred = createSinglePartialEffectPlots(input = naiveBayes.df.gam.pred$MajorityClassSize, 
  model = naiveBayes.gamboost.pred, which = "bbs(MajorityClassSize", xlab = "Majority class size")
p12.gam.pred = createCombinedPartialEffectPlots(input = naiveBayes.df.gam.pred$MajorityClassSize,
  model = naiveBayes.gamboost.pred, which = "MajorityClassSize", xlab = "Majority class size")
multiplot(p10.gam.pred, p11.gam.pred, p12.gam.pred, cols = 3)


# For comparison predicted effects of glmboost
par(mfrow = c(1,2))
x = seq(0, max(naiveBayes.df.gam.pred$MajorityClassSize) + 1000)
y = coef(naiveBayes.glmboost.pred, which = "sqrt(MajorityClassSize)")*log(x)
plot(x,y, main = "Estimated loglinear effect of \n MajorityClassSize on prediction time",
  xlab = "MajorityClassSize", ylab = "f(MajorityClassSize)", ylim = c(0,0.7))

x = seq(0, max(naiveBayes.df.gam.pred$ObsForPred) + 1000)
y = coef(naiveBayes.glmboost.pred, which = "sqrt(ObsForPred)")*log(x)
plot(x,y, main = "Estimated loglinear effect of \n ObsForPred on prediction time ",
  xlab = "ObsForPred", ylab = "f(ObsForPred)", ylim = c(0,0.7))

x = seq(0, max(naiveBayes.df.gam.pred$NumberOfClasses))
y = coef(naiveBayes.glmboost.pred, which = "I(NumberOfClasses^2)") * x^2 + 
  coef(naiveBayes.glmboost.pred, which = "sqrt(NumberOfClasses)") * log(x)
plot(x,y)

naiveBayes.df.gam.pred[naiveBayes.df.gam.pred$ObsForPred > 30000,]

# Model with interaction
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_naiveBayes_gamboost_pred_int.rda")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/naiveBayes_gamboost_pred_int.rda")

# mstop
mstop(cvm.naiveBayes.gamboost.pred.int) # 492

# Estimated coefficients of the linear components
lin.eff.naiveBayes.gam.pred.int = round(data.frame(unlist(coef(naiveBayes.gamboost.pred.int, which = "bols"))), 4)

# Variables that were not selected
not.selected.naiveBayes.gam.pred.int = setdiff(variable.names(naiveBayes.gamboost.pred.int), variable.names(naiveBayes.gamboost.pred.int, usedonly = TRUE))

# Partial effects
par(mfrow = c(5,2))
plot(naiveBayes.gamboost.pred.int)

load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/naiveBayesdfgam_pred.RData")
naiveBayes.df.gam.pred[naiveBayes.df.gam.pred$MajorityClassSize > 5000 & naiveBayes.df.gam.pred$NumberOfFeatures > 45 ,]

load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/naiveBayesdf.RData")
df = naiveBayes.df.mean[naiveBayes.df.mean$job.id %in% c(seq(353,363,1)), ]
range(df$ObsForTrain)

naiveBayes.df.gam.pred[naiveBayes.df.gam.pred$MajorityClassSize > 15000 ,]


#### glm ####
glm.naiveBayes.pred = glm(naiveBayes.glm.pred.fmla, data = naiveBayes.df.mean,
  family = Gamma(link = "log"))
step.glm.naiveBayes.pred = step(glm.naiveBayes.pred)
step.glm.naiveBayes.pred$anova
# log(NumberOfFeatures), log(NumberOfClasses)
n.sel.glm.pred = c("log(NumberOfFeatures)", "log(NumberOfClasses)")
prev.coef.glm.pred = summary(glm.naiveBayes.pred)$coefficients[,c(1,4)]
prev.coef.glm.pred[n.sel.glm.pred, ] = rep(NA, length(n.sel.glm.pred))
prev.coef.glm.pred[complete.cases(prev.coef.glm.pred),] = summary(step.glm.naiveBayes.pred)$coefficients[,c(1,4)]

coef.step.glm.naiveBayes.pred = prev.coef.glm.pred
 
# Create dataframe with estimated coefficients for glmboost and glm on prediction time
coefs.pred = cbind(coefs.naiveBayes.glmboost.pred, coef.step.glm.naiveBayes.pred)

# plot
p1.pred.glm = createPartialEffectsForGlm(input = naiveBayes.df.mean$ObsForPred,
  step.glm.naiveBayes.pred, linear.effect = "ObsForPred", 
  quadratic.effect = "I(ObsForPred^2)", sqrt.effect = "sqrt(ObsForPred)",
  log.effect = "log(ObsForPred)",xlab = "Number of instances")
p2.pred.glm = createPartialEffectsForGlm(input = naiveBayes.df.mean$NumberOfFeatures,
  step.glm.naiveBayes.pred, linear.effect = "NumberOfFeatures", 
  quadratic.effect = "I(NumberOfFeatures^2)", sqrt.effect = "sqrt(NumberOfFeatures)",
  log.effect = "log(ObsForPred)",xlab = "Number of features")
p3.pred.glm = createPartialEffectsForGlm(input = naiveBayes.df.mean$NumberOfClasses,
  step.glm.naiveBayes.pred, linear.effect = "NumberOfClasses", 
  quadratic.effect = "I(NumberOfClasses^2)", sqrt.effect = "sqrt(NumberOfClasses)",
  log.effect = "log(NumberOfClasses)",xlab = "Number of classes")
p4.pred.glm = createPartialEffectsForGlm(input = naiveBayes.df.mean$MajorityClassSize,
  step.glm.naiveBayes.pred, linear.effect = "MajorityClassSize", 
  quadratic.effect = "I(MajorityClassSize^2)", sqrt.effect = "sqrt(MajorityClassSize)",
  log.effect = "log(MajorityClassSize)",xlab = "Majority class size")
multiplot(p1.pred.glm, p2.pred.glm, p3.pred.glm, p4.pred.glm, cols = 2)


# Model with interaction

naiveBayes.glm.pred.fmla.int = 
  as.formula(paste("timepredict.test.mean ~ ", paste(
    paste0(naiveBayes.preds.pred, collapse = " + "),
    paste0("I(", naiveBayes.preds.pred, "^2)", collapse = " + "),
    paste0("sqrt(", naiveBayes.preds.pred, ")", collapse = " + "), 
    paste0("log(", naiveBayes.preds.pred, ")", collapse = " + "),
    sep = " + "),
    paste0(" + ObsForPred* NumberOfFeatures")))

glm.naiveBayes.pred.int = glm(naiveBayes.glm.pred.fmla.int, data = naiveBayes.df.mean,
  family = Gamma(link = "log"))
step.glm.naiveBayes.pred.int = step(glm.naiveBayes.pred.int)
step.glm.naiveBayes.pred.int$anova
n.sel.glm.pred.int = c("log(NumberOfFeatures)", "log(NumberOfClasses)")
coeffs.glm.naiveBayes.pred.int = summary(glm.naiveBayes.pred.int)$coefficients[,c(1,4)]
coeffs.glm.naiveBayes.pred.int[n.sel.glm.pred.int, ] = rep(NA, length(n.sel.glm.pred.int))
coeffs.glm.naiveBayes.pred.int[complete.cases(coeffs.glm.naiveBayes.pred.int), ] = summary(step.glm.naiveBayes.pred.int)$coefficients[,c(1,4)]

coefs.pred.int = cbind(coefs.naiveBayes.glmboost.pred.int, coeffs.glm.naiveBayes.pred.int)

#### randomForest ####
# use ugly hack again
naiveBayes.df.mean3 = naiveBayes.df.mean
naiveBayes.df.mean3$NumberOfInstances = naiveBayes.df.mean3$ObsForPred

naiveBayes.rF.pred = randomForest(timepredict.test.mean ~ MajorityClassSize +
    + NumberOfClasses + NumberOfFeatures + NumberOfInstances, data = naiveBayes.df.mean3,
  importance = TRUE)
plot(naiveBayes.rF.pred)

varUsed(naiveBayes.rF.pred, by.tree=FALSE, count=TRUE)

# Variable importance
imp.test.pred = importance(naiveBayes.rF.pred)
varImpPlot(naiveBayes.rF.pred, main = NULL)

# Partial dependence
impvar.pred <- rownames(imp.test.pred)[order(imp.test.pred[, 1], decreasing=TRUE)]
op <- par(mfrow=c(2, 2))
for (i in seq_along(impvar.pred)) {
  partialPlot(naiveBayes.rF.pred, naiveBayes.df.mean, impvar.pred[i],
    xlab=impvar.pred[i],ylim=c(0, 5), main = "")
}
par(op)


#### Comparison of models ####

# Create dataframe with estimated coefficients for glmboost and glm on training
# and prediction
coefs.both = cbind(coefs.test, coefs.pred)
coefs.both.int = cbind(coeffs.test.int, coefs.pred.int)

# RMSE and RAE
errors.test = calculateErrorsForComparison(time = "training", glmboost = naiveBayes.glmboost.test,
  glmboost.data = naiveBayes.df.mean, gamboost = naiveBayes.gamboost.test, 
  gamboost.data = naiveBayes.df.gam.test, glm = step.glm.naiveBayes.test,
  glm.data = naiveBayes.df.mean, rF = naiveBayes.rF, naiveBayes.df.mean2)
errors.test.int = calculateErrorsForComparison(time = "training", glmboost = naiveBayes.glmboost.test.int,
  glmboost.data = naiveBayes.df.mean, gamboost = naiveBayes.gamboost.test.int, 
  gamboost.data = naiveBayes.df.gam.test, glm = step.glm.naiveBayes.test.int,
  glm.data = naiveBayes.df.mean, rF = naiveBayes.rF, naiveBayes.df.mean2)

errors.pred = calculateErrorsForComparison(time = "prediction", glmboost = naiveBayes.glmboost.pred,
  glmboost.data = naiveBayes.df.mean, gamboost = naiveBayes.gamboost.pred, 
  gamboost.data = naiveBayes.df.gam.pred, glm = step.glm.naiveBayes.pred,
  glm.data = naiveBayes.df.mean, rF = naiveBayes.rF.pred, naiveBayes.df.mean3)
errors.pred.int = calculateErrorsForComparison(time = "prediction", glmboost = naiveBayes.glmboost.pred.int,
  glmboost.data = naiveBayes.df.mean, gamboost = naiveBayes.gamboost.pred.int, 
  gamboost.data = naiveBayes.df.gam.pred, glm = step.glm.naiveBayes.pred.int,
  glm.data = naiveBayes.df.mean, rF = naiveBayes.rF.pred, naiveBayes.df.mean3)

# Obs vs fitted values
plot(naiveBayes.df.mean$timetrain.test.mean, predict(naiveBayes.glmboost.test,
  type = "response", newdata = naiveBayes.df.mean), col = "red",
  xlab = "observed training time", ylab = "fitted values")
abline(a = 0, b = 1)
points(naiveBayes.df.mean$timetrain.test.mean, 
  predict(step.glm.naiveBayes.test, newdata = naiveBayes.df.mean,type = "response"), col = "black")
points(naiveBayes.df.mean$timetrain.test.mean,
  predict(naiveBayes.gamboost.test, newdata = naiveBayes.df.gam.test, type = "response"), col = "blue")
points(naiveBayes.df.mean$timetrain.test.mean,
  predict(naiveBayes.rF, newdata = naiveBayes.df.mean2, type = "response"), col= "green")  

plot(naiveBayes.df.mean$timepredict.test.mean, predict(naiveBayes.glmboost.pred,
  type = "response", newdata = naiveBayes.df.mean), col = "red",
  xlab = "observed training time", ylab = "fitted values")
abline(a = 0, b = 1)
points(naiveBayes.df.mean$timepredict.test.mean, 
  predict(step.glm.naiveBayes.pred, newdata = naiveBayes.df.mean,type = "response"), col = "black")
points(naiveBayes.df.mean$timepredict.test.mean,
  predict(naiveBayes.gamboost.pred, newdata = naiveBayes.df.gam.test, type = "response"), col = "blue")
points(naiveBayes.df.mean$timepredict.test.mean,
  predict(naiveBayes.rF.pred, newdata = naiveBayes.df.mean3, type = "response"), col= "green")  


# with ggplot
df = data.frame(traintime = naiveBayes.df.mean$timetrain.test.mean,
  glmboost = as.numeric(predict(naiveBayes.glmboost.test, type = "response", newdata = naiveBayes.df.mean)),
  gamboost = as.numeric(predict(naiveBayes.gamboost.test, newdata = naiveBayes.df.gam.test, type = "response")),
  glm = as.numeric(predict(step.glm.naiveBayes.test, newdata = naiveBayes.df.mean,type = "response")),
  randomForest = as.numeric(predict(naiveBayes.rF, newdata = naiveBayes.df.mean2, type = "response"))
)
df2 = melt(df, id.var = 1)
colnames(df2)[2] = "model"

df.pred = data.frame(timepredict = naiveBayes.df.mean$timepredict.test.mean,
  glmboost = as.numeric(predict(naiveBayes.glmboost.pred,type = "response", newdata = naiveBayes.df.mean)),
  gamboost = as.numeric(predict(naiveBayes.gamboost.pred, newdata = naiveBayes.df.gam.pred, type = "response")),
  glm = as.numeric(predict(step.glm.naiveBayes.pred, newdata = naiveBayes.df.mean,type = "response")),
  randomForest = as.numeric(predict(naiveBayes.rF.pred, newdata = naiveBayes.df.mean3, type = "response")))
df.pred2 = melt(df.pred, id.var = 1)
colnames(df.pred2)[2] = "model"

p1 =ggplot(data = df2, aes(traintime,value,colour=model))+
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

naiveBayes.df.mean[which.max(predict(naiveBayes.glmboost.pred,type = "response", newdata = naiveBayes.df.mean)),]
