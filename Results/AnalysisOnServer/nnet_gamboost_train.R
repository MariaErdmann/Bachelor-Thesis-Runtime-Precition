# gamboost()
set.seed(334)
library(mboost)
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/nnetdfmean.RData")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/nnetdfgam_test.RData")
source("~/Bachelor-Thesis-Runtime-Prediction/Results/formulas.R")

# Model fit
nnet.df.gam.test$skip = as.factor(nnet.df.gam.test$skip)
nnet.gamboost.test = gamboost(nnet.gam.test.fmla, data = nnet.df.gam.test,
  control = boost_control(nu = 0.3), offset = 0, family = GammaReg())

# Find optimal number of boosting iterations (mstop)
wght.m = cv(model.weights(nnet.gamboost.test), type = "kfold")
cvm.nnet.gamboost.test = cvrisk(nnet.gamboost.test, folds = wght.m, grid = 1:100000)

# Create model with appropriate mstop
mstop(cvm.nnet.gamboost.test)
nnet.gamboost.test[mstop(cvm.nnet.gamboost.test)]

# Save model with appropriate mstop
save(cvm.nnet.gamboost.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_nnet_gamboost_test.rda")
save(nnet.gamboost.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/nnet_gamboost_test.rda")
