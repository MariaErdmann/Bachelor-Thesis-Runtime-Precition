# Glmboost()
set.seed(334)
library(mboost)
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/glmnetdfmean.RData")
source("~/Bachelor-Thesis-Runtime-Prediction/Results/formulas.R")

# Look at the kernel density to check which family matches best
#plot(density(glmnet.df.mean$timetrain.test.mean), main = "Kernel density of training time on glmnet")
#polygon(density(glmnet.df.mean$timetrain.test.mean), col = "green")

# Model fit
glmnet.glmboost.test = glmboost(glmnet.glm.test.fmla, data = glmnet.df.mean,
  control = boost_control(nu = 0.3), center = TRUE, offset = 0, family = GammaReg())

# Find optimal number of boosting iterations (mstop)
wght.m = cv(model.weights(glmnet.glmboost.test), type = "kfold")
cvm.glmnet.glmboost.test = cvrisk(glmnet.glmboost.test, folds = wght.m, grid = 1:100000)

# Create model with appropriate mstop
mstop(cvm.glmnet.glmboost.test)
glmnet.glmboost.test[mstop(cvm.glmnet.glmboost.test)]

# Save model with appropriate mstop
save(cvm.glmnet.glmboost.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_glmnet_glmboost_test.rda")
save(glmnet.glmboost.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/glmnet_glmboost_test.rda")
