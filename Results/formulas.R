# Formulas
source("~/Bachelor-Thesis-Runtime-Prediction/Results/helperFunctions.R")

#### Ranger ####
# glmboost()
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rangerdfmean.RData")
ranger.preds.test = names(ranger.df.mean[, !(names(ranger.df.mean) %in% c("job.id2", "did","name",
  "target.feature", "algorithm", "NumberOfSymbolicFeatures", "NumberOfNumericFeatures",
  "MinorityClassSize", "lrn.id", "sample.fraction", "mtry", "min.node.size",
  "timetrain.test.mean","timepredict.test.mean", "timeboth.test.mean","mmce.test.mean",
  "NumberOfInstances", "sub.sample.frac", "ObsForPred"))])

ranger.preds.pred = names(ranger.df.mean[, !(names(ranger.df.mean) %in% c("job.id2", "did","name",
  "target.feature", "algorithm", "NumberOfSymbolicFeatures", "NumberOfNumericFeatures",
  "MinorityClassSize", "lrn.id", "sample.fraction", "mtry", "min.node.size",
  "timetrain.test.mean","timepredict.test.mean", "timeboth.test.mean","mmce.test.mean",
  "NumberOfInstances", "sub.sample.frac", "ObsForTrain"))])

ranger.numeric.preds.test = setdiff(ranger.preds.test, c("replace", "respect.unordered.factors"))
ranger.numeric.preds.pred = setdiff(ranger.preds.pred, c("replace", "respect.unordered.factors"))

# can't use createGlmboostFormula here because of the factors
ranger.glm.test.fmla = as.formula(paste("timetrain.test.mean", "~ ", paste(
  paste0(ranger.numeric.preds.test, collapse = " + "),
  paste0("I(", ranger.numeric.preds.test, "^2)", collapse = " + "),
  paste0("sqrt(", ranger.numeric.preds.test, ")", collapse = " + "),
  paste0("log(", ranger.numeric.preds.test, ")", collapse = " + "), 
  "replace",  "respect.unordered.factors",
  sep = " + ")))


ranger.glm.pred.fmla = as.formula(paste("timepredict.test.mean", "~ ", paste(
  paste0(ranger.numeric.preds.pred, collapse = " + "),
  paste0("I(", ranger.numeric.preds.pred, "^2)", collapse = " + "),
  paste0("sqrt(", ranger.numeric.preds.pred, ")", collapse = " + "),
  paste0("log(", ranger.numeric.preds.pred, ")", collapse = " + "), 
  "replace",  "respect.unordered.factors",
  sep = " + ")))


# gamboost()
# Use ridge penalized linear base learners to overcome variable selection bias
# and unpenalized parametric baselearners and the smooth effects
ranger.gam.test.fmla = as.formula(
  paste("timetrain.test.mean ~", 
  paste(paste0("bols(intercept, intercept = FALSE)"),
  paste0("bols(", ranger.preds.test, ", intercept = FALSE, df = 1)", collapse = " + "),
  paste0("bols(", ranger.preds.test, ", intercept = FALSE)", collapse = " + "),
  paste0("bbs(", ranger.numeric.preds.test, ", center = TRUE, df = 1, knots = 20)", collapse = " + "),
  collapse = " + ", sep = " + ")))
ranger.gam.pred.fmla = as.formula(
  paste("timepredict.test.mean ~", 
    paste(paste0("bols(intercept, intercept = FALSE)"),
      paste0("bols(", ranger.preds.pred, ", intercept = FALSE, df = 1)", collapse = " + "),
      paste0("bols(", ranger.preds.pred, ", intercept = FALSE)", collapse = " + "),
      paste0("bbs(", ranger.numeric.preds.pred, ", center = TRUE, df = 1, knots = 20)", collapse = " + "),
      collapse = " + ", sep = " + ")))


#### Rpart ####
# glmboost()
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rpartdfmean.RData")
rpart.preds.test =  names(rpart.df.mean[, !(names(rpart.df.mean) %in% c("job.id2", "did","name",
  "target.feature", "algorithm", "NumberOfSymbolicFeatures", "NumberOfNumericFeatures",
  "MinorityClassSize", "lrn.id", "minsplit", "minbucket", "cp", "timetrain.test.mean",
  "timepredict.test.mean", "timeboth.test.mean", "mmce.test.mean",
  "NumberOfInstances", "ObsForPred", "sub.sample.frac"))])

rpart.preds.pred =  names(rpart.df.mean[, !(names(rpart.df.mean) %in% c("job.id2", "did","name",
  "target.feature", "algorithm", "NumberOfSymbolicFeatures", "NumberOfNumericFeatures",
  "MinorityClassSize", "lrn.id", "minsplit", "minbucket", "cp", "timetrain.test.mean",
  "timepredict.test.mean", "timeboth.test.mean", "mmce.test.mean",
  "NumberOfInstances", "ObsForTrain", "sub.sample.frac"))])

rpart.glm.test.fmla = createGlmboostFormula(rpart.preds.test, "timetrain.test.mean")
rpart.glm.pred.fmla = createGlmboostFormula(rpart.preds.pred, "timepredict.test.mean")

# gamboost()
rpart.gam.test.fmla = createGamboostFormula(rpart.preds.test, "timetrain.test.mean")
rpart.gam.pred.fmla = createGamboostFormula(rpart.preds.pred, "timepredict.test.mean")


#### Gbm ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/gbmdfmean.RData")
gbm.preds.test =  names(gbm.df.mean[, !(names(gbm.df.mean) %in% c("job.id2", "did","name",
  "target.feature", "algorithm", "NumberOfSymbolicFeatures", "NumberOfNumericFeatures",
  "MinorityClassSize", "lrn.id", "shrinkage", "timetrain.test.mean",
  "NumberOfInstances", "ObsForPred", "sub.sample.frac","timepredict.test.mean", "timeboth.test.mean", "mmce.test.mean"))])

gbm.preds.pred =  names(gbm.df.mean[, !(names(gbm.df.mean) %in% c("job.id2", "did","name",
  "target.feature", "algorithm", "NumberOfSymbolicFeatures", "NumberOfNumericFeatures",
  "MinorityClassSize", "lrn.id", "shrinkage", "timetrain.test.mean",
  "NumberOfInstances", "ObsForTrain", "sub.sample.frac","timepredict.test.mean", "timeboth.test.mean", "mmce.test.mean"))])

# glmboost
gbm.glm.test.fmla = createGlmboostFormula(gbm.preds.test, "timetrain.test.mean")
gbm.glm.pred.fmla = createGlmboostFormula(gbm.preds.pred, "timepredict.test.mean")

# gamboost
gbm.gam.test.fmla = createGamboostFormula(gbm.preds.test, "timetrain.test.mean")
gbm.gam.pred.fmla = createGamboostFormula(gbm.preds.pred, "timepredict.test.mean")

#### glmnet ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/glmnetdfmean.RData")
glmnet.preds.test =  names(glmnet.df.mean[, !(names(glmnet.df.mean) %in% c("job.id2", "did","name",
  "target.feature", "algorithm", "NumberOfSymbolicFeatures", "NumberOfNumericFeatures",
  "MinorityClassSize", "lrn.id", "lambda.min.ratio", "timetrain.test.mean",
  "timepredict.test.mean", "timeboth.test.mean", "mmce.test.mean",
  "NumberOfInstances", "ObsForPred", "sub.sample.frac"))])

glmnet.preds.pred =  names(glmnet.df.mean[, !(names(glmnet.df.mean) %in% c("job.id2", "did","name",
  "target.feature", "algorithm", "NumberOfSymbolicFeatures", "NumberOfNumericFeatures",
  "MinorityClassSize", "lrn.id", "lambda.min.ratio", "timetrain.test.mean",
  "timepredict.test.mean", "timeboth.test.mean", "mmce.test.mean",
  "NumberOfInstances", "ObsForTrain", "sub.sample.frac"))])

# glmboost
glmnet.glm.test.fmla = createGlmboostFormula(glmnet.preds.test, "timetrain.test.mean")
glmnet.glm.pred.fmla = createGlmboostFormula(glmnet.preds.pred, "timepredict.test.mean")

# gamboost
glmnet.gam.test.fmla = createGamboostFormula(glmnet.preds.test, "timetrain.test.mean")
glmnet.gam.pred.fmla = createGamboostFormula(glmnet.preds.pred, "timepredict.test.mean")


#### naiveBayes ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/naiveBayesdfmean.RData")
naiveBayes.preds.test =  names(naiveBayes.df.mean[, !(names(naiveBayes.df.mean) %in% c("job.id2", "did","name",
  "target.feature", "algorithm", "NumberOfSymbolicFeatures", "NumberOfNumericFeatures",
  "MinorityClassSize", "lrn.id", "laplace", "timetrain.test.mean",
  "timepredict.test.mean", "timeboth.test.mean", "mmce.test.mean",
  "NumberOfInstances", "ObsForPred", "sub.sample.frac"))])

naiveBayes.preds.pred =  names(naiveBayes.df.mean[, !(names(naiveBayes.df.mean) %in% c("job.id2", "did","name",
  "target.feature", "algorithm", "NumberOfSymbolicFeatures", "NumberOfNumericFeatures",
  "MinorityClassSize", "lrn.id", "laplace", "timetrain.test.mean",
  "timepredict.test.mean", "timeboth.test.mean", "mmce.test.mean",
  "NumberOfInstances", "ObsForTrain", "sub.sample.frac"))])

# glmboost
naiveBayes.glm.test.fmla = createGlmboostFormula(naiveBayes.preds.test, "timetrain.test.mean")
naiveBayes.glm.pred.fmla = createGlmboostFormula(naiveBayes.preds.pred, "timepredict.test.mean")

# gamboost
naiveBayes.gam.test.fmla = createGamboostFormula(naiveBayes.preds.test, "timetrain.test.mean")
naiveBayes.gam.pred.fmla = createGamboostFormula(naiveBayes.preds.pred, "timepredict.test.mean")


#### nnet ####
load("~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/nnetdfmean.RData")
nnet.preds.test =  names(nnet.df.mean[, !(names(nnet.df.mean) %in% c("job.id2", "did","name",
  "target.feature", "algorithm", "NumberOfSymbolicFeatures", "NumberOfNumericFeatures",
  "MinorityClassSize", "lrn.id", "MaxNWts", "timetrain.test.mean",
  "timepredict.test.mean", "timeboth.test.mean", "mmce.test.mean",
  "NumberOfInstances", "ObsForPred", "sub.sample.frac"))])
nnet.preds.test.numeric = setdiff(nnet.preds.test, "skip")

nnet.preds.pred =  names(nnet.df.mean[, !(names(nnet.df.mean) %in% c("job.id2", "did","name",
  "target.feature", "algorithm", "NumberOfSymbolicFeatures", "NumberOfNumericFeatures",
  "MinorityClassSize", "lrn.id", "MaxNWts", "timetrain.test.mean",
  "timepredict.test.mean", "timeboth.test.mean", "mmce.test.mean",
  "NumberOfInstances", "ObsForTrain", "sub.sample.frac"))])
nnet.preds.pred.numeric = setdiff(nnet.preds.pred, "skip")

# glmboost without helper function because of factor
nnet.glm.test.fmla = as.formula(paste("timetrain.test.mean", "~ ", paste(
  paste0(nnet.preds.test.numeric, collapse = " + "),
  paste0("I(", nnet.preds.test.numeric, "^2)", collapse = " + "),
  paste0("sqrt(", nnet.preds.test.numeric, ")", collapse = " + "), 
  paste0("log(", nnet.preds.test.numeric[-length(nnet.preds.test.numeric)], ")", collapse = " + "),
  "skip",
  sep = " + ")))
  
  
nnet.glm.pred.fmla = as.formula(paste("timepredict.test.mean", "~ ", paste(
  paste0(nnet.preds.pred.numeric, collapse = " + "),
  paste0("I(", nnet.preds.pred.numeric, "^2)", collapse = " + "),
  paste0("sqrt(", nnet.preds.pred.numeric, ")", collapse = " + "), 
  paste0("log(", nnet.preds.pred.numeric[-length(nnet.preds.test.numeric)], ")", collapse = " + "),
  "skip",
  sep = " + ")))


# gamboost
# Use ridge penalized linear base learners to overcome variable selection bias
# and unpenalized parametric baselearners and the smooth effects
nnet.gam.test.fmla = as.formula(paste("timetrain.test.mean ~ ",
  paste(
    paste0("bols(intercept, intercept = FALSE)"),
    paste0("bols(", nnet.preds.test, ", intercept = FALSE, df = 1)", collapse = " + "),
    paste0("bols(", nnet.preds.test, ", intercept = FALSE)", collapse = " + "),
    paste0("bbs(", nnet.preds.test.numeric, ", center = TRUE, df = 1, knots = 20)", collapse =" + "),
    collapse = " + ", sep = " + ")))

nnet.gam.pred.fmla = as.formula(paste("timepredict.test.mean ~ ",
  paste(
    paste0("bols(intercept, intercept = FALSE)"),
    paste0("bols(", nnet.preds.pred, ", intercept = FALSE, df = 1)", collapse = " + "),
    paste0("bols(", nnet.preds.pred, ", intercept = FALSE)", collapse = " + "),
    paste0("bbs(", nnet.preds.pred.numeric, ", center = TRUE, df = 1, knots = 20)", collapse =" + "),
    collapse = " + ", sep = " + ")))
