# gamboost()
set.seed(334)
library(mboost)
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/glmnetdfmean.RData")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/glmnetdfgam_pred.RData")
source("~/Bachelor-Thesis-Runtime-Prediction/Results/formulas.R")

# Model fit
glmnet.gamboost.pred = gamboost(glmnet.gam.pred.fmla, data = glmnet.df.gam.pred,
  control = boost_control(nu = 0.3), offset = 0, family = GammaReg())

# Find optimal number of boosting iterations (mstop)
wght.m = cv(model.weights(glmnet.gamboost.pred), type = "kfold")
cvm.glmnet.gamboost.pred = cvrisk(glmnet.gamboost.pred, folds = wght.m, grid = 1:100000)

# Create model with appropriate mstop
mstop(cvm.glmnet.gamboost.pred)
glmnet.gamboost.pred[mstop(cvm.glmnet.gamboost.pred)]

# Save model with appropriate mstop
save(cvm.glmnet.gamboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_glmnet_gamboost_pred.rda")
save(glmnet.gamboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/glmnet_gamboost_pred.rda")
