# Set working directory
setwd("C:\\Users\\Maria\\Documents\\Studium\\Statistik\\Bachelorarbeit")


# delete Files/Directories
unlink("runtime_prediction_test1-files", recursive = TRUE)

# Load packages
library("BatchExperiments")
getConfig()

# load functions needed
if(!exists("didsForOMLDataSets", mode="function")) source("didsForOMLDataSets.R")
if(!exists("getTaskForHPS", mode="function")) source("getTaskForHPS.R")

# Create a regestriy
myreg = makeExperimentRegistry(id = "runtime_prediction_test1",
  packages = c("mlr","OpenML"))


t.type = "Supervised Regression"
regression.dids = didsForOMLDataSets(t.type)
data.did = regression.dids[92]
boston.task = getTaskForHPS(data.did = data.did, t.type = t.type)

#for (i in seq_along(tasks)) {
#  tk = tasks[[i]]
tk = boston.task
static = list(task = tk)
addProblem(myreg, id = getTaskId(tk), 
  static = static, 
  dynamic = function(static, ratio) {
    rdesc = makeResampleDesc("Subsample", iters = 1, split = ratio)
    rin = makeResampleInstance(desc = rdesc, task = static$task)
    list(rin = rin)
  }, 
  seed = 123, overwrite = TRUE)
#}


# Add regr.randomForest as algo

lrn = makeLearner("regr.randomForest")
lrn$id

addAlgorithm(myreg, 
  id = lrn$id, 
  fun = function(static, dynamic, ...) {
    configureMlr(on.learner.error = "warn")
    lrn = setHyperPars(lrn, ...)
    print(getHyperPars(lrn))
    #print(length(dynamic$rin$train.inds[[1]]))
    r = resample(learner = lrn, task = static$task, resampling = dynamic$rin,
      measures = list(timetrain, timepredict, timeboth), models = TRUE)
    print(r)
    r
  },
  overwrite = TRUE
)


ratio = 0.67
maxFeatures = sum(boston.task$task.desc$n.feat)
maxObs = boston.task$task.desc$size
maxObsTraining = boston.task$task.desc$size*ratio

ps1 = makeParamSet(
  makeIntegerParam("ntree", lower = 1, upper = 100),
  makeIntegerParam("mtry", lower = 1, upper = maxFeatures),
  makeIntegerParam("sampsize", lower = 1, upper = maxObsTraining),
  makeIntegerParam("nodesize", lower = 1, upper = maxObsTraining)
)

des = generateRandomDesign(50, par.set = ps1)
replace = rep(TRUE,50)
des = cbind(des, replace)
boston.algo.design = makeDesign("regr.randomForest", design = des)


ratio = c(0.67)
pars = list(ratio = ratio)
task.design = makeDesign(getTaskId(tk), exhaustive = pars)


addExperiments(myreg, rep = 1, algo.designs = boston.algo.design,
  prob.design = task.design)


# Summarize experiments
summarizeExperiments(myreg)


# Submit some jobs and check the log

submitJobs(myreg, 1:10)
showLog(myreg, 1)


# Get the results: refer to getResults



