# Glmboost()
set.seed(334)
library(mboost)
library(xlsx)
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/gbmdfmean.RData")
source("~/Bachelor-Thesis-Runtime-Prediction/Results/formulas.R")

# Look at the kernel density to check which family matches best
plot(density(gbm.df.mean$timepredict.test.mean), main = "Kernel density of training time on gbm")
polygon(density(gbm.df.mean$timepredict.test.mean), col = "green")

# Model fit
gbm.glmboost.pred = glmboost(gbm.glm.pred.fmla, data = gbm.df.mean,
  control = boost_control(nu = 0.3), center = TRUE, offset = 0, family = GammaReg())

# Find optimal number of boosting iterations (mstop)
wght.m = cv(model.weights(gbm.glmboost.pred), type = "kfold")
cvm.gbm.glmboost.pred = cvrisk(gbm.glmboost.pred, folds = wght.m, grid = 1:100000)

# Create model with appropriate mstop
mstop(cvm.gbm.glmboost.pred)
gbm.glmboost.pred[mstop(cvm.gbm.glmboost.pred)]

# Save model with appropriate mstop
save(cvm.gbm.glmboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_gbm_glmboost_pred.rda")
save(gbm.glmboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/gbm_glmboost_pred.rda")

