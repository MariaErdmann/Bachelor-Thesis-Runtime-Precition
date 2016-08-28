# Glmboost()
set.seed(334)
library(mboost)
library(xlsx)
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rpartdfmean.RData")
source("~/Bachelor-Thesis-Runtime-Prediction/Results/formulas.R")

plot(density(rpart.df.mean$timetrain.test.mean), main = "Kernel density of training time on rpart")
polygon(density(rpart.df.mean$timetrain.test.mean), col = "green")

# Model fit
rpart.glmboost.test = glmboost(rpart.glm.test.fmla, data = rpart.df.mean,
  control = boost_control(nu = 0.3), center = TRUE, offset = 0, family = GammaReg())

# Find optimal number of boosting iterations (mstop)
wght.m = cv(model.weights(rpart.glmboost.test), type = "kfold")
cvm.rpart.glmboost.test = cvrisk(rpart.glmboost.test, folds = wght.m, grid = 1:100000)

# Create model with appropriate mstop
mstop(cvm.rpart.glmboost.test)
rpart.glmboost.test[mstop(cvm.rpart.glmboost.test)]

# Save model with appropriate mstop
save(cvm.rpart.glmboost.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_rpart_glmboost_test.rda")
save(rpart.glmboost.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/rpart_glmboost_test.rda")