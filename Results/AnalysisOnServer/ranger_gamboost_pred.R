#### Gamboost() with traintime as response ####

library(mboost)
set.seed(334)
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rangerdfmean.RData")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rangerdfgam_pred.RData")
source("~/Bachelor-Thesis-Runtime-Prediction/Results/formulas.R")


# Model fit
ranger.df.gam.pred$replace = as.factor(ranger.df.gam.pred$replace)
ranger.df.gam.pred$respect.unordered.factors = as.factor(ranger.df.gam.pred$respect.unordered.factors)
ranger.gamboost.pred = gamboost(ranger.gam.pred.fmla, data = ranger.df.gam.pred,
  control = boost_control(nu = 0.1), offset = 0, family = GammaReg())

# Error in .local(x, ...) : 
#   internal_chm_factor: Cholesky-Faktorisierung fehlgeschlagen
# In addition: Warning message:
#   In .local(x, ...) :
#   Cholmod-Warnung 'not positive definite' bei Datei ../Cholesky/t_cholmod_rowfac.c, Zeile 431

# Find optimal number of boosting iterations (mstop)
#wght.m = cv(model.weights(ranger.gamboost.pred), type = "kfold")
#cvm.ranger.gamboost.pred = cvrisk(ranger.gamboost.pred, folds = wght.m, grid = 1:100000)

# Create model with optimal number of boosting iterations
# mstop(cvm.ranger.gamboost.pred)
# ranger.gamboost.pred[mstop(cvm.ranger.gamboost.pred)]
# save(ranger.gamboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/ranger_gamboost_pred.rda")



