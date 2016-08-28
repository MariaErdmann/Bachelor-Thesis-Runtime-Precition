# gamboost()
set.seed(334)
library(mboost)
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/gbmdfmean.RData")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/gbmdfgam_pred.RData")
source("~/Bachelor-Thesis-Runtime-Prediction/Results/formulas.R")

# Model fit
gbm.gamboost.pred = gamboost(gbm.gam.pred.fmla, data = gbm.df.gam.pred,
  control = boost_control(nu = 0.3), offset = 0, family = GammaReg())

# Find optimal number of boosting iterations (mstop)
wght.m = cv(model.weights(gbm.gamboost.pred), type = "kfold")
cvm.gbm.gamboost.pred = cvrisk(gbm.gamboost.pred, folds = wght.m, grid = 1:100000)

# Create model with appropriate mstop
mstop(cvm.gbm.gamboost.pred)
gbm.gamboost.pred[mstop(cvm.gbm.gamboost.pred)]

# Save model with appropriate mstop
save(cvm.gbm.gamboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_gbm_gamboost_pred.rda")
save(gbm.gamboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/gbm_gamboost_pred.rda")