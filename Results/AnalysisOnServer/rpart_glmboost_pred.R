# Glmboost()
set.seed(334)
library(mboost)
library(xlsx)
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rpartdfmean.RData")
source("~/Bachelor-Thesis-Runtime-Prediction/Results/formulas.R")

# Model fit
rpart.glmboost.pred = glmboost(rpart.glm.pred.fmla, data = rpart.df.mean,
  control = boost_control(nu = 0.3), center = TRUE, offset = 0, family = GammaReg())

# Find optimal number of boosting iterations (mstop)
wght.m = cv(model.weights(rpart.glmboost.pred), type = "kfold")
cvm.rpart.glmboost.pred = cvrisk(rpart.glmboost.pred, folds = wght.m, grid = 1:100000)


# Create model with appropriate mstop
mstop(cvm.rpart.glmboost.pred)
rpart.glmboost.pred[mstop(cvm.rpart.glmboost.pred)]

# Save model with appropriate mstop
save(cvm.rpart.glmboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_rpart_glmboost_pred.rda")
save(rpart.glmboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/rpart_glmboost_pred.rda")
