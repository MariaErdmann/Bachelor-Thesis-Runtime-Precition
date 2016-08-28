# Glmboost()
set.seed(334)
library(mboost)
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/naiveBayesdfmean.RData")
source("~/Bachelor-Thesis-Runtime-Prediction/Results/formulas.R")

# plot(density(naiveBayes.df.mean$timetrain.test.mean), main = "Kernel density of training time on naive Bayes")
# polygon(density(naiveBayes.df.mean$timetrain.test.mean), col = "green")
# 
# # Model fit
naiveBayes.glmboost.test = glmboost(naiveBayes.glm.test.fmla, data = naiveBayes.df.mean,
   control = boost_control(nu = 0.3), center = TRUE, offset = 0, family = GammaReg())

# Find optimal number of boosting iterations (mstop)
wght.m = cv(model.weights(naiveBayes.glmboost.test), type = "kfold")
cvm.naiveBayes.glmboost.test = cvrisk(naiveBayes.glmboost.test, folds = wght.m, grid = 1:100000)

# Create model with appropriate mstop
mstop(cvm.naiveBayes.glmboost.test)
naiveBayes.glmboost.test[mstop(cvm.naiveBayes.glmboost.test)]
 
# # Save model with appropriate mstop
save(cvm.naiveBayes.glmboost.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_naiveBayes_glmboost_test.rda")
save(naiveBayes.glmboost.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/naiveBayes_glmboost_test.rda")


# Model with interactions

int.fmla.test = as.formula(paste("timetrain.test.mean ~ ", paste(
  paste0(naiveBayes.preds.test, collapse = " + "),
  paste0("I(", naiveBayes.preds.test, "^2)", collapse = " + "),
  paste0("sqrt(", naiveBayes.preds.test, ")", collapse = " + "), 
  paste0("log(", naiveBayes.preds.test, ")", collapse = " + "),
  sep = " + ",
  paste0("ObsForTrain*NumberOfFeatures"))))

# Model fit
naiveBayes.glmboost.test.int = glmboost(int.fmla.test, data = naiveBayes.df.mean,
  control = boost_control(nu = 0.3), center = TRUE, offset = 0, family = GammaReg())

# Find optimal number of boosting iterations (mstop)
wght.m = cv(model.weights(naiveBayes.glmboost.test.int), type = "kfold")
cvm.naiveBayes.glmboost.test.int = cvrisk(naiveBayes.glmboost.test.int, folds = wght.m, grid = 1:100000)

# Create model with appropriate mstop
mstop(cvm.naiveBayes.glmboost.test.int)
naiveBayes.glmboost.test[mstop(cvm.naiveBayes.glmboost.test.int)]

# Save model with appropriate mstop
save(cvm.naiveBayes.glmboost.test.int, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/cvm_naiveBayes_glmboost_test.int.rda")
save(naiveBayes.glmboost.test.int, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/ServerResults/naiveBayes_glmboost_test.int.rda")