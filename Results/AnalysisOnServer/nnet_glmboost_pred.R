# Glmboost()
set.seed(334)
library(mboost)
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/nnetdfmean.RData")
source("~/Bachelor-Thesis-Runtime-Prediction/Results/formulas.R")

# Model fit
nnet.df.mean$skip = as.factor(nnet.df.mean$skip)
nnet.glmboost.pred = glmboost(nnet.glm.pred.fmla, data = nnet.df.mean,
  control = boost_control(nu = 0.3), center = TRUE, offset = 0, family = GammaReg())

# Find optimal number of boosting iterations (mstop)
wght.m = cv(model.weights(nnet.glmboost.pred), type = "kfold")
cvm.nnet.glmboost.pred = cvrisk(nnet.glmboost.pred, folds = wght.m, grid = 1:100000)

# Create model with appropriate mstop
mstop(cvm.nnet.glmboost.pred)
nnet.glmboost.pred[mstop(cvm.nnet.glmboost.pred)]

# Save model with appropriate mstop
save(cvm.nnet.glmboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_nnet_glmboost_pred.rda")
save(nnet.glmboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/nnet_glmboost_pred.rda")
