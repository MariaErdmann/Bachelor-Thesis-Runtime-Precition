# gamboost()
set.seed(334)
library(mboost)
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/naiveBayesdfmean.RData")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/naiveBayesdfgam_test.RData")
source("~/Bachelor-Thesis-Runtime-Prediction/Results/formulas.R")

# # Model fit
naiveBayes.gamboost.test = gamboost(naiveBayes.gam.test.fmla, data = naiveBayes.df.gam.test,
   control = boost_control(nu = 0.3), offset = 0, family = GammaReg())

# Find optimal number of boosting iterations (mstop)
wght.m = cv(model.weights(naiveBayes.gamboost.test), type = "kfold")
cvm.naiveBayes.gamboost.test = cvrisk(naiveBayes.gamboost.test, folds = wght.m, grid = 1:100000)

# Create model with appropriate mstop
mstop(cvm.naiveBayes.gamboost.test)
naiveBayes.gamboost.test[mstop(cvm.naiveBayes.gamboost.test)]

# Save model with appropriate mstop
save(cvm.naiveBayes.gamboost.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_naiveBayes_gamboost_test.rda")
save(naiveBayes.gamboost.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/naiveBayes_gamboost_test.rda")


# Model with interaction
int.fmla.gam.test = as.formula(paste("timetrain.test.mean ~ ",
  paste(
  paste(
    paste0("bols(intercept, intercept = FALSE)"),
    paste0("bols(", naiveBayes.preds.test, ", intercept = FALSE)", collapse = " + "),
    paste0("bbs(", naiveBayes.preds.test, ", center = TRUE, df = 1, knots = 20)", collapse =" + "),
    collapse = " + ", sep = " + "), " + bols(ObsForTrain, by = NumberOfFeatures, intercept = FALSE)")))

naiveBayes.gamboost.test.int = gamboost(int.fmla.gam.test, data = naiveBayes.df.gam.test,
  control = boost_control(nu = 0.3), offset = 0, family = GammaReg())

# Find optimal number of boosting iterations (mstop)
wght.m = cv(model.weights(naiveBayes.gamboost.test.int), type = "kfold")
cvm.naiveBayes.gamboost.test.int = cvrisk(naiveBayes.gamboost.test.int, folds = wght.m, grid = 1:100000)

# Create model with appropriate mstop
mstop(cvm.naiveBayes.gamboost.test.int)
naiveBayes.gamboost.test.int[mstop(cvm.naiveBayes.gamboost.test.int)]

# Save model with appropriate mstop
save(cvm.naiveBayes.gamboost.test.int, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_naiveBayes_gamboost_test_int.rda")
save(naiveBayes.gamboost.test.int, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/naiveBayes_gamboost_test_int.rda")
