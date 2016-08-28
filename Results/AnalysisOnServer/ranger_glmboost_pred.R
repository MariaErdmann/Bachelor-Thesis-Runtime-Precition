#### Glmboost() with prediction time as response ####

library(mboost)
set.seed(334)
#load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rangerdfquad.RData")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rangerdfmean.RData")
source("~/Bachelor-Thesis-Runtime-Prediction/Results/formulas.R")

#plot(density(ranger.df.mean$timepredict.test.mean), main = "Kernel density of training time on ranger")
#polygon(density(ranger.df.mean$timepredict.test.mean), col = "green")

# Model fit
ranger.glmboost.pred = glmboost(ranger.glm.pred.fmla, data = ranger.df.mean,
  control = boost_control(nu = 0.1), center = TRUE, offset = 0, family = GammaReg())

# Find optimal number of boosting iterations (mstop)
wght.m = cv(model.weights(ranger.glmboost.pred), type = "kfold")
cvm.ranger.glmboost.pred = cvrisk(ranger.glmboost.pred, folds = wght.m, grid = 1:100000)

# Create model with optimal number of boosting iterations
mstop(cvm.ranger.glmboost.pred)
ranger.glmboost.pred[mstop(cvm.ranger.glmboost.pred)]
save(ranger.glmboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/ranger_glmboost_pred.rda")
save(cvm.ranger.glmboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_ranger_glmboost_pred.rda")
