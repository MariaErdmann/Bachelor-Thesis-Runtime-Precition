# Glmboost()
set.seed(334)
library(mboost)
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/glmnetdfmean.RData")
source("~/Bachelor-Thesis-Runtime-Prediction/Results/formulas.R")

plot(density(glmnet.df.mean$timepredict.test.mean))

# Model fit
glmnet.glmboost.pred = glmboost(glmnet.glm.pred.fmla, data = glmnet.df.mean,
  control = boost_control(nu = 0.3), center = TRUE, offset = 0, family = GammaReg())

# Find optimal number of boosting iterations (mstop)
wght.m = cv(model.weights(glmnet.glmboost.pred), type = "kfold")
cvm.glmnet.glmboost.pred = cvrisk(glmnet.glmboost.pred, folds = wght.m, grid = 1:100000)


# Create model with appropriate mstop
mstop(cvm.glmnet.glmboost.pred)
glmnet.glmboost.pred[mstop(cvm.glmnet.glmboost.pred)]

# Save model with appropriate mstop
save(cvm.glmnet.glmboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_glmnet_glmboost_pred.rda")
save(glmnet.glmboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/glmnet_glmboost_pred.rda")
