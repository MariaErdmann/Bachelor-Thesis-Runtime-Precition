# gamboost()
set.seed(334)
library(mboost)
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rpartdfmean.RData")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rpartdfgam_pred.RData")
source("~/Bachelor-Thesis-Runtime-Prediction/Results/formulas.R")

plot(density(rpart.df.gam.pred$timepredict.test.mean), main = "Kernel Density of prediction 
  time on rpart")

# Model fit
rpart.gamboost.pred = gamboost(rpart.gam.pred.fmla, data = rpart.df.gam.pred,
  control = boost_control(nu = 0.3), offset = 0, family = GammaReg())

# Find optimal number of boosting iterations (mstop)
wght.m = cv(model.weights(rpart.gamboost.pred), type = "kfold")
cvm.rpart.gamboost.pred = cvrisk(rpart.gamboost.pred, folds = wght.m, grid = 1:100000)

# Create model with appropriate mstop
mstop(cvm.rpart.gamboost.pred)
rpart.gamboost.pred[mstop(cvm.rpart.gamboost.pred)]

# Save model with appropriate mstop
save(cvm.rpart.gamboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_rpart_gamboost_pred.rda")
save(rpart.gamboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/rpart_gamboost_pred.rda")