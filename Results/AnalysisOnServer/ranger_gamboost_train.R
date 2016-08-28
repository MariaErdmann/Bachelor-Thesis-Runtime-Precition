#### Gamboost() with traintime as response ####

library(mboost)
set.seed(334)
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rangerdfmean.RData")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rangerdfgam_test.RData")
source("~/Bachelor-Thesis-Runtime-Prediction/Results/formulas.R")

#plot(density(ranger.df.mean$timetrain.test.mean), main = "Kernel density of training time on ranger")
#polygon(density(ranger.df.mean$timetrain.test.mean), col = "green")

# Model fit
ranger.df.gam.test$replace = as.factor(ranger.df.gam.test$replace*1)
ranger.df.gam.test$respect.unordered.factors = as.factor(ranger.df.gam.test$respect.unordered.factors*1)
ranger.gamboost.test = gamboost(ranger.gam.test.fmla, data = ranger.df.gam.test,
  control = boost_control(nu = 0.1), offset = 0, family = Gaussian())

# Error in .local(x, ...) : 
#   internal_chm_factor: Cholesky-Faktorisierung fehlgeschlagen
# In addition: Warning message:
#   In .local(x, ...) :
#   Cholmod-Warnung 'not positive definite' bei Datei ../Cholesky/t_cholmod_rowfac.c, Zeile 431

# Find optimal number of boosting iterations (mstop)
#wght.m = cv(model.weights(ranger.gamboost.test), type = "kfold")
#cvm.ranger.gamboost.test = cvrisk(ranger.gamboost.test, folds = wght.m, grid = 1:100000)

# Create model with optimal number of boosting iterations
# mstop(cvm.ranger.gamboost.test)
# ranger.gamboost.test[mstop(cvm.ranger.gamboost.test)]
# save(ranger.gamboost.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/ranger_gamboost_test.rda")