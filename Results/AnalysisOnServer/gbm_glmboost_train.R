# Glmboost()
set.seed(334)
library(mboost)
library(xlsx)
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/gbmdfmean.RData")
#load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/gbmdfquad.RData")
source("~/Bachelor-Thesis-Runtime-Prediction/Results/formulas.R")

# Look at the kernel density to check which family matches best
plot(density(gbm.df.mean$timetrain.test.mean), main = "Kernel density of training time on gbm")
polygon(density(gbm.df.mean$timetrain.test.mean), col = "green")

# Model fit
gbm.glmboost.test = glmboost(gbm.glm.test.fmla, data = gbm.df.mean,
  control = boost_control(nu = 0.3), center = TRUE, offset = 0, family = GammaReg())

# Find optimal number of boosting iterations (mstop)
wght.m = cv(model.weights(gbm.glmboost.test), type = "kfold")
cvm.gbm.glmboost.test = cvrisk(gbm.glmboost.test, folds = wght.m, grid = 1:1000)

# Create model with appropriate mstop
mstop(cvm.gbm.glmboost.test) # 40
gbm.glmboost.test[mstop(cvm.gbm.glmboost.test)]

# Save model with appropriate mstop
save(cvm.gbm.glmboost.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_gbm_glmboost_test.rda")
save(gbm.glmboost.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/gbm_glmboost_test.rda")
