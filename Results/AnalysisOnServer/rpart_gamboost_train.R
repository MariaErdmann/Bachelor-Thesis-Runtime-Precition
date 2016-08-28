# gamboost()
set.seed(334)
library(mboost)
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rpartdfmean.RData")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rpartdfgam_test.RData")
source("~/Bachelor-Thesis-Runtime-Prediction/Results/formulas.R")

# Model fit
rpart.gamboost.test = gamboost(rpart.gam.test.fmla, data = rpart.df.gam.test,
  control = boost_control(nu = 0.3), offset = 0, family = GammaReg())

# Find optimal number of boosting iterations (mstop)
wght.m = cv(model.weights(rpart.gamboost.test), type = "kfold")
cvm.rpart.gamboost.test = cvrisk(rpart.gamboost.test, folds = wght.m, grid = 1:100000)

# Create model with appropriate mstop
mstop(cvm.rpart.gamboost.test)
rpart.gamboost.test[mstop(cvm.rpart.gamboost.test)]

# Save model with appropriate mstop
save(cvm.rpart.gamboost.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_rpart_gamboost_test.rda")
save(rpart.gamboost.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/rpart_gamboost_test.rda")
