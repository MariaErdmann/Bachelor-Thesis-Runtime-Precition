# gamboost()
set.seed(334)
library(mboost)
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/glmnetdfmean.RData")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/glmnetdfgam_test.RData")
source("~/Bachelor-Thesis-Runtime-Prediction/Results/formulas.R")

# Model fit
glmnet.gamboost.test = gamboost(glmnet.gam.test.fmla, data = glmnet.df.gam.test,
  control = boost_control(nu = 0.3), offset = 0, family = GammaReg())

# Find optimal number of boosting iterations (mstop)
wght.m = cv(model.weights(glmnet.gamboost.test), type = "kfold")
cvm.glmnet.gamboost.test = cvrisk(glmnet.gamboost.test, folds = wght.m, grid = 1:1000)

# Create model with appropriate mstop
mstop(cvm.glmnet.gamboost.test)
glmnet.gamboost.test[mstop(cvm.glmnet.gamboost.test)]

# Save model with appropriate mstop
save(cvm.glmnet.gamboost.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_glmnet_gamboost_test.rda")
save(glmnet.gamboost.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/glmnet_gamboost_test.rda")