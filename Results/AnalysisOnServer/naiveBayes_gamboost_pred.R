# gamboost()
set.seed(334)
library(mboost)
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/naiveBayesdfmean.RData")
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/naiveBayesdfgam_pred.RData")
source("~/Bachelor-Thesis-Runtime-Prediction/Results/formulas.R")

# Model fit
naiveBayes.gamboost.pred = gamboost(naiveBayes.gam.pred.fmla, data = naiveBayes.df.gam.pred,
  control = boost_control(nu = 0.3), offset = 0, family = GammaReg())

# Find optimal number of boosting iterations (mstop)
wght.m = cv(model.weights(naiveBayes.gamboost.pred), type = "kfold")
cvm.naiveBayes.gamboost.pred = cvrisk(naiveBayes.gamboost.pred, folds = wght.m, grid = 1:100000)

# Create model with appropriate mstop
mstop(cvm.naiveBayes.gamboost.pred)
naiveBayes.gamboost.pred[mstop(cvm.naiveBayes.gamboost.pred)]

# Save model with appropriate mstop
save(cvm.naiveBayes.gamboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_naiveBayes_gamboost_pred.rda")
save(naiveBayes.gamboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/naiveBayes_gamboost_pred.rda")

# Interation effect between ObsForPred and NumberOfFeatures considered
naiveBayes.gam.pred.fmla.int = as.formula(paste("timepredict.test.mean", "~ ",
  paste(
    paste0("bols(intercept, intercept = FALSE)"),
    paste0("bols(", naiveBayes.preds.pred, ", intercept = FALSE)", collapse = " + "),
    paste0("bbs(", naiveBayes.preds.pred, ", center = TRUE, df = 1, knots = 20)", collapse =" + "),
    collapse = " + ", sep = " + "), paste0("+ bols(ObsForPred, intercept = FALSE, by = NumberOfFeatures)"),
  paste0(" + bbs(ObsForPred, center = TRUE, df = 1, knots = 20, by = NumberOfFeatures)")))

naiveBayes.gamboost.pred.int = gamboost(naiveBayes.gam.pred.fmla.int, data = naiveBayes.df.gam.pred,
  control = boost_control(nu = 0.3), offset = 0, family = GammaReg())
# does not work
