# gamboost()
set.seed(334)
library(mboost)
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/nnetdfmean.RData")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/nnetdfgam_pred.RData")
source("~/Bachelor-Thesis-Runtime-Prediction/Results/formulas.R")

plot(density(nnet.df.gam.pred$timepredict.test.mean), main = "Kerndel density of
  prediction time on nnet")
polygon(density(nnet.df.gam.pred$timepredict.test.mean), col = "green")

# Model fit
nnet.df.gam.pred$skip = as.factor(nnet.df.gam.pred$skip)
nnet.gamboost.pred = gamboost(nnet.gam.pred.fmla, data = nnet.df.gam.pred,
  control = boost_control(nu = 0.3), offset = 0, family = GammaReg())

# Find optimal number of boosting iterations (mstop)
wght.m = cv(model.weights(nnet.gamboost.pred), type = "kfold")
cvm.nnet.gamboost.pred = cvrisk(nnet.gamboost.pred, folds = wght.m, grid = 1:100000)

# Create model with appropriate mstop
mstop(cvm.nnet.gamboost.pred)
nnet.gamboost.pred[mstop(cvm.nnet.gamboost.pred)]

# Save model with appropriate mstop
save(cvm.nnet.gamboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_nnet_gamboost_pred.rda")
save(nnet.gamboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/nnet_gamboost_pred.rda")
