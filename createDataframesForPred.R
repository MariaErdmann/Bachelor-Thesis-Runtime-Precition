# Creates and prepares the datasets for analysis and predicition
source("~/Bachelor-Thesis-Runtime-Prediction/Results/helperFunctions.R")
source("~/Bachelor-Thesis-Runtime-Prediction/Results/formulas.R")

load("~/Bachelor-Thesis-Runtime-Prediction/Results/finalresult.RData")
load("~/Bachelor-Thesis-Runtime-Prediction/class.dsets.RData")
tasks = class.dsets
OMLDATASETS =  tasks[!(tasks$did %in% c(1476, 1459, 1486)),]

# only keep columns of interest
OMLDATASETS = OMLDATASETS[,c("did", "name", "target.feature", "MajorityClassSize",
  "MinorityClassSize", "NumberOfClasses", "NumberOfFeatures", "NumberOfInstances",
  "NumberOfNumericFeatures", "NumberOfSymbolicFeatures")]

# change column name from "problem" to "did", so that data.frames can be merged by did
colnames(big.res)[13] = "did"

# merge dataset description df with result df
res.ddesc = merge(OMLDATASETS, big.res, by = "did")

# create a variable that multiplies sub.sample.frac with NumberOfInstances
InstanceTrafoTrain = function(algorithm, sub.sample.frac, NumberOfInstances) { 
  if (algorithm == "eval") ObsForTrain = ceiling(sub.sample.frac * NumberOfInstances)
  else if (algorithm == "default") ObsForTrain = NumberOfInstances
}
res.ddesc$ObsForTrain = mapply(InstanceTrafoTrain, res.ddesc$algorithm,
  res.ddesc$sub.sample.frac, res.ddesc$NumberOfInstances)

InstanceTrafoPred = function(algorithm, sub.sample.frac, NumberOfInstances) { 
  if (algorithm == "eval") ObsForPred = floor((1-sub.sample.frac) * NumberOfInstances)
  else if (algorithm == "default") ObsForPred = NumberOfInstances
}
res.ddesc$ObsForPred = mapply(InstanceTrafoPred, res.ddesc$algorithm,
  res.ddesc$sub.sample.frac, res.ddesc$NumberOfInstances)

save(res.ddesc, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/ResultsDDesc.RData")

# Delete all columns that are not needed for modelling
drops = c("submitted", "started", "done", "error", "memory", "batch.id",
  "job.hash", "time.queued", "time.running", "pars.hash", "i.memory", "walltime")
res.ddesc = res.ddesc[, !(names(res.ddesc) %in% drops)]



# To create the subsets for each algo specification for columns to be kept will be needed
# Since I will probably change code above I do not want to call columns by numbers
# but by their direct name
desc.cols = c("did", "name", "target.feature", "MajorityClassSize", "MinorityClassSize",
    "NumberOfClasses", "NumberOfFeatures", "ObsForTrain", "ObsForPred", "NumberOfInstances", "NumberOfNumericFeatures",
    "NumberOfSymbolicFeatures", "job.id", "repl", "algorithm", "lrn.id") 
measure.cols = c("timetrain.test.mean", "timepredict.test.mean", "timeboth.test.mean","mmce.test.mean")


# Create dataframes for each algo

#### Ranger ####
ranger.df = subset(res.ddesc, lrn.id == "ranger") #39037 obs
ranger.cols = c("num.trees", "replace", "sample.fraction", "mtry",
  "respect.unordered.factors", "min.node.size", "sub.sample.frac")
ranger.df = ranger.df[,c(desc.cols, ranger.cols, measure.cols)]

# Add values for default settings
ranger.df[ranger.df$algorithm == "default", "num.trees"] = 500
ranger.df[ranger.df$algorithm == "default", "replace"] = TRUE
ranger.df[ranger.df$algorithm == "default", "respect.unordered.factors"] = FALSE
ranger.df[ranger.df$algorithm == "default", "sub.sample.frac"] = 1
save(ranger.df, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rangerdf.RData")

# Backtransformation of variables
ranger.df.trafo = ranger.df
mtryTrafo = function(algorithm, mtry, NumberOfFeatures) { 
  if (algorithm == "eval") max(1, ceiling(mtry*NumberOfFeatures))
  else if (algorithm == "default") max(1, floor(sqrt(NumberOfFeatures)))
}
ranger.df.trafo$mtry.trafo = mapply(mtryTrafo, ranger.df.trafo$algorithm, ranger.df.trafo$mtry, ranger.df.trafo$NumberOfFeatures)
sampleFractionTrafo = function(algorithm, sample.fraction, NumberOfInstances) {
  if (algorithm == "eval") {
    fraction = 1/NumberOfInstances
    max(sample.fraction, fraction)
  } else if(algorithm == "default") { 
    sample.fraction = sample.fraction
  }
}
ranger.df.trafo$sample.fraction.trafo = mapply(sampleFractionTrafo, ranger.df.trafo$algorithm, ranger.df.trafo$sample.fraction, ranger.df.trafo$NumberOfInstances)

# Ranger.df: dataset with all replications and observations
ranger.df.trafo = ranger.df.trafo[,c(desc.cols, ranger.cols, "sample.fraction.trafo", "mtry.trafo", measure.cols)]


# Create data frame for prediction (name for df for pred: <algo.name>.df.mean)
ranger.df.new = createNewJobIdForReplications(data = ranger.df.trafo) #39037 obs
save(ranger.df.new, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/ranger_jobid2.RData")
ranger.df.mean = CreateDataFrameWithMeans(data = ranger.df.new) #3842

# ranger.df.mean: dataset where the replications are summarisied  to one obs
save(ranger.df.mean, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rangerdfmean.RData")

# ranger.df.gam.test: dataset with centered numeric predictors and an intercept column
ranger.df.gam.test = ranger.df.mean
ranger.df.gam.test$intercept = rep(1, dim(ranger.df.gam.test)[1])
ranger.df.gam.test[ranger.numeric.preds.test] = lapply(ranger.df.gam.test[ranger.numeric.preds.test], function(x) (x - mean(x)))
save(ranger.df.gam.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rangerdfgam_test.RData")

# ranger.df.gam.pred
ranger.df.gam.pred = ranger.df.mean
ranger.df.gam.pred$intercept = rep(1, dim(ranger.df.gam.pred)[1])
ranger.df.gam.pred[ranger.numeric.preds.pred] = lapply(ranger.df.gam.pred[ranger.numeric.preds.pred], function(x) (x - mean(x)))
save(ranger.df.gam.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rangerdfgam_pred.RData")


# Split dataset, so that an explorative analysis can be made on only the default settings
s.df = split(ranger.df.mean, f = ranger.df.mean$algorithm)
ranger.df.default = s.df[[2]] # 65 obs.
save(ranger.df.default, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rangerdfdefault.RData")


#### Rpart ####
rpart.df = subset(res.ddesc, lrn.id == "rpart") #33150 obs
rpart.cols = c("minsplit", "minbucket", "cp", "maxdepth", "sub.sample.frac")
rpart.df = rpart.df[,c(desc.cols, rpart.cols, measure.cols)]
rpart.df[rpart.df$algorithm == "default", "minbucket"] = 20/3
rpart.df[rpart.df$algorithm == "default", "cp"] = 0.01
rpart.df[rpart.df$algorithm == "default", "maxdepth"] = 30
rpart.df[rpart.df$algorithm == "default", "sub.sample.frac"] = 1
save(rpart.df, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rpartdf.RData")

# Backtransformation of variables
rpart.df.trafo = rpart.df
minsplitTrafo = function(algorithm, minsplit, sub.sample.frac, NumberOfInstances) { 
  if (algorithm == "eval") max(1, ceiling(minsplit * sub.sample.frac * NumberOfInstances))
  else if (algorithm == "default") minsplit = minsplit
}
rpart.df.trafo$minsplit.trafo = mapply(minsplitTrafo, rpart.df.trafo$algorithm,
  rpart.df.trafo$minsplit, rpart.df.trafo$sub.sample.frac, rpart.df.trafo$NumberOfInstances)

minbucketTrafo = function(algorithm, minbucket, sub.sample.frac, NumberOfInstances){
  if (algorithm == "eval") max(1, ceiling(minbucket * sub.sample.frac * NumberOfInstances))
  else if (algorithm == "default") minbucket = minbucket
}
rpart.df.trafo$minbucket.trafo = mapply(minbucketTrafo, rpart.df.trafo$algorithm,
  rpart.df.trafo$minbucket, rpart.df.trafo$sub.sample.frac, rpart.df.trafo$NumberOfInstances)

cpTrafo = function(algorithm, cp){
  if (algorithm == "eval") 10^cp
  else if (algorithm == "default") cp = cp
}
rpart.df.trafo$cp.trafo = mapply(cpTrafo, rpart.df.trafo$algorithm, rpart.df.trafo$cp)
rpart.df.trafo = rpart.df.trafo[, c(desc.cols, rpart.cols, "minsplit.trafo",
  "minbucket.trafo", "cp.trafo", measure.cols)]

# Create data frame for prediction (name for df for pred: <algo.name>.df.mean)
rpart.df.new = createNewJobIdForReplications(data = rpart.df.trafo) #33150 obs
save(rpart.df.new, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rpart_jobid2.RData")
rpart.df.mean = CreateDataFrameWithMeans(data = rpart.df.new) #3315 obs

# rpart.df.mean: dataset where the replications are summarisied  to one obs
save(rpart.df.mean, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rpartdfmean.RData")

# ranger.df.gam.test: dataset with centered numeric predictors and an intercept column
rpart.df.gam.test = rpart.df.mean
rpart.df.gam.test$intercept = rep(1, dim(rpart.df.gam.test)[1])
rpart.df.gam.test[rpart.preds.test] = lapply(rpart.df.gam.test[rpart.preds.test], function(x) (x - mean(x)))
save(rpart.df.gam.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rpartdfgam_test.RData")

# ranger.df.gam.pred: dataset with centered numeric predictors and an intercept column
rpart.df.gam.pred = rpart.df.mean
rpart.df.gam.pred$intercept = rep(1, dim(rpart.df.gam.pred)[1])
rpart.df.gam.pred[rpart.preds.pred] = lapply(rpart.df.gam.pred[rpart.preds.pred], function(x) (x - mean(x)))
save(rpart.df.gam.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rpartdfgam_pred.RData")

# Split dataset, so that an explorative analysis can be made on only the default settings
s.df = split(rpart.df.mean, f = rpart.df.mean$algorithm)
rpart.df.default = s.df[[2]] # 65 obs.
save(rpart.df.default, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/rpartdfdefault.RData")


#### gbm ####
gbm.df = subset(res.ddesc, lrn.id == "gbm") #31449 obs
gbm.cols = c("n.trees", "interaction.depth", "shrinkage", "bag.fraction", "sub.sample.frac")
gbm.df = gbm.df[,c(desc.cols, gbm.cols, measure.cols)]
# Add values for default settings
gbm.df[gbm.df$algorithm == "default", "interaction.depth"] = 1
gbm.df[gbm.df$algorithm == "default", "shrinkage"] = 0.001
gbm.df[gbm.df$algorithm == "default", "bag.fraction"] = 0.5
gbm.df[gbm.df$algorithm == "default", "sub.sample.frac"] = 1
# gbm.df: dataframe with all replications
save(gbm.df, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/gbmdf.RData")

# Backtransformation of variables
gbm.df.trafo = gbm.df
shrinkageTrafo = function(algorithm, shrinkage) {
  if (algorithm == "eval") 10^shrinkage
  else if (algorithm == "default") shrinkage = shrinkage
}
gbm.df.trafo$shrinkage.trafo = mapply(shrinkageTrafo, gbm.df.trafo$algorithm, gbm.df.trafo$shrinkage)

# Create data frame for prediction (name for df for pred: <algo.name>.df.mean)
gbm.df.new = createNewJobIdForReplications(data = gbm.df.trafo) # 31449 obs
save(gbm.df.new, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/gbm_jobid2.RData")
gbm.df.mean = CreateDataFrameWithMeans(data = gbm.df.new) # 3150 obs

# rpart.df.mean: dataset where the replications are summarisied  to one obs
save(gbm.df.mean, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/gbmdfmean.RData")

# gbm.df.gam.test: dataset with centered numeric predictors and an intercept column
gbm.df.gam.test = gbm.df.mean
gbm.df.gam.test$intercept = rep(1, dim(gbm.df.gam.test)[1])
gbm.df.gam.test[gbm.preds.test] = lapply(gbm.df.gam.test[gbm.preds.test], function(x) (x - mean(x)))
save(gbm.df.gam.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/gbmdfgam_test.RData")

# gbm.df.gam.pred: dataset with centered numeric predictors and an intercept column
gbm.df.gam.pred = gbm.df.mean
gbm.df.gam.pred$intercept = rep(1, dim(gbm.df.gam.pred)[1])
gbm.df.gam.pred[gbm.preds.pred] = lapply(gbm.df.gam.pred[gbm.preds.pred], function(x) (x - mean(x)))
save(gbm.df.gam.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/gbmdfgam_pred.RData")


# Split dataset, so that an explorative analysis can be made on only the default settings
s.df = split(gbm.df.mean, f = gbm.df.mean$algorithm)
gbm.df.default = s.df[[2]] # 64 obs.
save(gbm.df.default, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/gbmdfdefault.RData")


#### glmnet ####
glmnet.df = subset(res.ddesc, lrn.id == "glmnet") #13440 obs
glmnet.cols = c("alpha", "sub.sample.frac", "lambda.min.ratio")
glmnet.df = glmnet.df[,c(desc.cols, glmnet.cols, measure.cols)]
glmnet.df[glmnet.df$algorithm == "default", "alpha"] = 1
glmnet.df[glmnet.df$algorithm == "default", "sub.sample.frac"] = 1

# glmnet.df: dataset with all replications
save(glmnet.df, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/glmnetdf.RData")

# Create data frame for prediction (name for df for pred: <algo.name>.df.mean)
glmnet.df.new = createNewJobIdForReplications(data = glmnet.df) # 13440 obs
save(glmnet.df.new, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/glmnet_jobid2.RData")
glmnet.df.mean = CreateDataFrameWithMeans(data = glmnet.df.new) # 1344 obs

# glmnet.df.mean: dataset where the replications are summarisied  to one obs
save(glmnet.df.mean, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/glmnetdfmean.RData")

# glmnet.df.gam.test: dataset with centered numeric predictors and an intercept column
glmnet.df.gam.test = glmnet.df.mean
glmnet.df.gam.test$intercept = rep(1, dim(glmnet.df.gam.test)[1])
glmnet.df.gam.test[glmnet.preds.test] = lapply(glmnet.df.gam.test[glmnet.preds.test], function(x) (x - mean(x)))
save(glmnet.df.gam.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/glmnetdfgam_test.RData")

# glmnet.df.gam.pred: dataset with centered numeric predictors and an intercept column
glmnet.df.gam.pred = glmnet.df.mean
glmnet.df.gam.pred$intercept = rep(1, dim(glmnet.df.gam.pred)[1])
glmnet.df.gam.pred[glmnet.preds.pred] = lapply(glmnet.df.gam.pred[glmnet.preds.pred], function(x) (x - mean(x)))
save(glmnet.df.gam.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/glmnetdfgam_pred.RData")

# Split dataset, so that an explorative analysis can be made on only the default settings
s.df = split(glmnet.df.mean, f = glmnet.df.mean$algorithm)
glmnet.df.default = s.df[[2]] # 64 obs.
save(glmnet.df.default, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/glmnetdfdefault.RData")


#### naiveBayes ####
naiveBayes.df = subset(res.ddesc, lrn.id == "naiveBayes") #7150 obs
naive.Bayes.cols = c("laplace", "sub.sample.frac")
naiveBayes.df = naiveBayes.df[,c(desc.cols, naive.Bayes.cols, measure.cols)]
naiveBayes.df[naiveBayes.df$algorithm == "default", "sub.sample.frac"] = 1
# naiveBayes.df: dataset with all replications
save(naiveBayes.df, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/naiveBayesdf.RData")

# Create data frame for prediction (name for df for pred: <algo.name>.df.mean)
naiveBayes.df.new = createNewJobIdForReplications(data = naiveBayes.df) # 7150 obs
save(naiveBayes.df.new, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/naiveBayes_jobid2.RData")
naiveBayes.df.mean = CreateDataFrameWithMeans(data = naiveBayes.df.new) # 715 obs

# naiveBayes.df.mean: dataset where the replications are summarisied  to one obs
save(naiveBayes.df.mean, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/naiveBayesdfmean.RData")

# naiveBayes.df.gam.test: dataset with centered numeric predictors and an intercept column
naiveBayes.df.gam.test = naiveBayes.df.mean
naiveBayes.df.gam.test$intercept = rep(1, dim(naiveBayes.df.gam.test)[1])
naiveBayes.df.gam.test[naiveBayes.preds.test] = lapply(naiveBayes.df.gam.test[naiveBayes.preds.test], function(x) (x - mean(x)))
save(naiveBayes.df.gam.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/naiveBayesdfgam_test.RData")

# naiveBayes.df.gam.pred: dataset with centered numeric predictors and an intercept column
naiveBayes.df.gam.pred = naiveBayes.df.mean
naiveBayes.df.gam.pred$intercept = rep(1, dim(naiveBayes.df.gam.pred)[1])
naiveBayes.df.gam.pred[naiveBayes.preds.pred] = lapply(naiveBayes.df.gam.pred[naiveBayes.preds.pred], function(x) (x - mean(x)))
save(naiveBayes.df.gam.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/naiveBayesdfgam_pred.RData")

# Split dataset, so that an explorative analysis can be made on only the default settings
s.df = split(naiveBayes.df.mean, f = naiveBayes.df.mean$algorithm)
naiveBayes.df.default = s.df[[2]] # 65 obs.
save(naiveBayes.df.default, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/naiveBayesdefault.RData")


#### nnet ####
nnet.df = subset(res.ddesc, lrn.id == "nnet") #32385 obs
nnet.cols = c("size", "MaxNWts", "maxit", "skip", "decay", "sub.sample.frac")
nnet.df = nnet.df[,c(desc.cols, nnet.cols, measure.cols)]
nnet.df[nnet.df$algorithm == "default", "MaxNWts"] = 1000
nnet.df[nnet.df$algorithm == "default", "maxit"] = 100
nnet.df[nnet.df$algorithm == "default", "skip"] = FALSE
nnet.df[nnet.df$algorithm == "default", "decay"] = 0
nnet.df[nnet.df$algorithm == "default", "sub.sample.frac"] = 1
# nnet.df: dataset with all replications
save(nnet.df, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/nnetdf.RData")

# Create data frame for prediction (name for df for pred: <algo.name>.df.mean)
nnet.df.new = createNewJobIdForReplications(data = nnet.df) # 32385 obs
save(nnet.df.new, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/nnet_jobid2.RData")
nnet.df.mean = CreateDataFrameWithMeans(data = nnet.df.new) # 3240 obs

# nnet.df.mean: dataset where the replications are summarisied  to one obs
save(nnet.df.mean, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/nnetdfmean.RData")

# nnet.df.gam.test: dataset with centered numeric predictors and an intercept column
nnet.df.gam.test = nnet.df.mean
nnet.df.gam.test$intercept = rep(1, dim(nnet.df.gam.test)[1])
nnet.df.gam.test[nnet.preds.test.numeric] = lapply(nnet.df.gam.test[nnet.preds.test.numeric], function(x) (x - mean(x)))
save(nnet.df.gam.test, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/nnetdfgam_test.RData")

# nnet.df.gam.pred: dataset with centered numeric predictors and an intercept column
nnet.df.gam.pred = nnet.df.mean
nnet.df.gam.pred$intercept = rep(1, dim(nnet.df.gam.pred)[1])
nnet.df.gam.pred[nnet.preds.pred.numeric] = lapply(nnet.df.gam.pred[nnet.preds.pred.numeric], function(x) (x - mean(x)))
save(nnet.df.gam.pred, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/nnetdfgam_pred.RData")

# Split dataset, so that an explorative analysis can be made on only the default settings
s.df = split(nnet.df.mean, f = nnet.df.mean$algorithm)
nnet.df.default = s.df[[2]] # 63 obs.
save(nnet.df.default, file = "~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/nnetdefault.RData")


# create a summary-table
obs.summary = matrix(NA, nrow = 6, ncol = 3)
colnames(obs.summary) = c("All replications", "After summarising replications", "Default")
rownames(obs.summary) = c("ranger", "rpart", "gbm", "glmnet", "naiveBayes", "nnet")
obs.summary[1,1] = dim(ranger.df)[1]
obs.summary[1,2] = dim(ranger.df.mean)[1]
obs.summary[1,3] = dim(ranger.df.default)[1]
obs.summary[2,1] = dim(rpart.df)[1]
obs.summary[2,2] = dim(rpart.df.mean)[1]
obs.summary[2,3] = dim(rpart.df.default)[1]
obs.summary[3,1] = dim(gbm.df)[1]
obs.summary[3,2] = dim(gbm.df.mean)[1]
obs.summary[3,3] = dim(gbm.df.default)[1]
obs.summary[4,1] = dim(glmnet.df)[1]
obs.summary[4,2] = dim(glmnet.df.mean)[1]
obs.summary[4,3] = dim(glmnet.df.default)[1]
obs.summary[5,1] = dim(naiveBayes.df)[1]
obs.summary[5,2] = dim(naiveBayes.df.mean)[1]
obs.summary[5,3] = dim(naiveBayes.df.default)[1]
obs.summary[6,1] = dim(nnet.df)[1]
obs.summary[6,2] = dim(nnet.df.mean)[1]
obs.summary[6,3] = dim(nnet.df.default)[1]
  

