# gamboost()
set.seed(334)
library(mboost)
library(xlsx)
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/gbmdfmean.RData")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/gbmdfgam_test.RData")
source("~/Bachelor-Thesis-Runtime-Prediction/Results/formulas.R")

# Model fit
gbm.gamboost.test = gamboost(gbm.gam.test.fmla, data = gbm.df.gam.test,
  control = boost_control(nu = 0.3), offset = 0, family = GammaReg())

# Find optimal number of boosting iterations (mstop)
wght.m = cv(model.weights(gbm.gamboost.test), type = "kfold")
cvm.gbm.gamboost.test = cvrisk(gbm.gamboost.test, folds = wght.m, grid = 1:100000)

# Create model with appropriate mstop
mstop(cvm.gbm.gamboost.test)
gbm.gamboost.test[mstop(cvm.gbm.gamboost.test)]

# Plot error
#jpeg("~/Bachelor-Thesis-Runtime-Prediction/Results/Plots and Coefficients/gbm/error_gbm_gam_test.jpeg")
#plot(cvm.gbm.gamboost.test)
#dev.off()

# Save model with appropriate mstop
save(cvm.gbm.gamboost.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_gbm_gamboost_test.rda")
save(gbm.gamboost.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/gbm_gamboost_test.rda")
