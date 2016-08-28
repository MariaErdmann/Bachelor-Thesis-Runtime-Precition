# Glmboost()
set.seed(334)
library(mboost)
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/nnetdfmean.RData")
source("~/Bachelor-Thesis-Runtime-Prediction/Results/formulas.R")

plot(density(nnet.df.mean$timetrain.test.mean), "Kernel density on training time on nnet")
polygon(density(nnet.df.mean$timetrain.test.mean), col = "green")

# Model fit
nnet.df.mean$skip = as.factor(nnet.df.mean$skip)

nnet.glmboost.test = glmboost(nnet.glm.test.fmla, data = nnet.df.mean,
  control = boost_control(nu = 0.3), center = TRUE, offset = 0, family = GammaReg())

# Find optimal number of boosting iterations (mstop)
wght.m = cv(model.weights(nnet.glmboost.test), type = "kfold")
cvm.nnet.glmboost.test = cvrisk(nnet.glmboost.test, folds = wght.m, grid = 1:100000)

# Create model with appropriate mstop
mstop(cvm.nnet.glmboost.test)
nnet.glmboost.test[mstop(cvm.nnet.glmboost.test)]

# Save model with appropriate mstop
save(cvm.nnet.glmboost.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_nnet_glmboost_test.rda")
save(nnet.glmboost.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/nnet_glmboost_test.rda")
