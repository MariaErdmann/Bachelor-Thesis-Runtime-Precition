#### Glmboost() with traintime as response ####

library(mboost)
library(xlsx)
set.seed(334)
#load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rangerdfquad.RData")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rangerdfmean.RData")
source("~/Bachelor-Thesis-Runtime-Prediction/Results/formulas.R")

#plot(density(ranger.df.mean$timetrain.test.mean), main = "Kernel density of training time on ranger")
#polygon(density(ranger.df.mean$timetrain.test.mean), col = "green")

# Model fit
ranger.glmboost.test = glmboost(ranger.glm.test.fmla, data = ranger.df.mean,
  control = boost_control(nu = 0.1), center = TRUE, offset = 0, family = GammaReg())

# Find optimal number of boosting iterations (mstop)
wght.m = cv(model.weights(ranger.glmboost.test), type = "kfold")
cvm.ranger.glmboost.test = cvrisk(ranger.glmboost.test, folds = wght.m, grid = 1:100000)

# Create model with optimal number of boosting iterations
mstop(cvm.ranger.glmboost.test)
ranger.glmboost.test[mstop(cvm.ranger.glmboost.test)]
save(ranger.glmboost.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/ranger_glmboost_test.rda")
save(cvm.ranger.glmboost.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_ranger_glmboost_test.rda")
