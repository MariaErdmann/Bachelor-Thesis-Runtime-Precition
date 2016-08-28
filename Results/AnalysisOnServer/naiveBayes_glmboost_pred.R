# Glmboost()
set.seed(334)
library(mboost)
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/naiveBayesdfmean.RData")
source("~/Bachelor-Thesis-Runtime-Prediction/Results/formulas.R")

#plot(density(naiveBayes.df.mean$timepredict.test.mean), main = "Kernel density plot of prediction time on naive Bayes")
#polygon(density(naiveBayes.df.mean$timepredict.test.mean), col = "green")

# Model fit
naiveBayes.glmboost.pred = glmboost(naiveBayes.glm.pred.fmla, data = naiveBayes.df.mean,
  control = boost_control(nu = 0.3), center = TRUE, offset = 0, family = GammaReg())

# Find optimal number of boosting iterations (mstop)
wght.m = cv(model.weights(naiveBayes.glmboost.pred), type = "kfold")
cvm.naiveBayes.glmboost.pred = cvrisk(naiveBayes.glmboost.pred, folds = wght.m, grid = 1:100000)

# Create model with appropriate mstop
mstop(cvm.naiveBayes.glmboost.pred)
naiveBayes.glmboost.pred[mstop(cvm.naiveBayes.glmboost.pred)]

# Save model with appropriate mstop
save(cvm.naiveBayes.glmboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_naiveBayes_glmboost_pred.rda")
save(naiveBayes.glmboost.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/naiveBayes_glmboost_pred.rda")


# Model with interaction term
naiveBayes.glm.pred.fmla.int = 
  as.formula(paste("timepredict.test.mean ~ ", paste(
    paste0(naiveBayes.preds.pred, collapse = " + "),
    paste0("I(", naiveBayes.preds.pred, "^2)", collapse = " + "),
    paste0("sqrt(", naiveBayes.preds.pred, ")", collapse = " + "), 
    paste0("log(", naiveBayes.preds.pred, ")", collapse = " + "),
    sep = " + "),
    paste0(" + ObsForPred* NumberOfFeatures")))

naiveBayes.glmboost.pred.int = glmboost(naiveBayes.glm.pred.fmla.int, data = naiveBayes.df.mean,
  control = boost_control(nu = 0.3), center = TRUE, offset = 0, family = GammaReg())

# Find optimal number of boosting iterations (mstop)
wght.m = cv(model.weights(naiveBayes.glmboost.pred.int), type = "kfold")
cvm.naiveBayes.glmboost.pred.int = cvrisk(naiveBayes.glmboost.pred.int, folds = wght.m, grid = 1:100000)

mstop(cvm.naiveBayes.glmboost.pred.int) # 24
naiveBayes.glmboost.pred.int[mstop(cvm.naiveBayes.glmboost.pred.int)]

save(cvm.naiveBayes.glmboost.pred.int, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_naiveBayes_glmboost_pred_int.rda")
save(naiveBayes.glmboost.pred.int, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/naiveBayes_glmboost_pred_int.rda")

