# Source programs to get tasks
#setwd("C:\\Users\\Maria\\Documents\\Studium\\Statistik\\Bachelorarbeit")


if(!exists("didsForOMLDataSets", mode="function")) source("didsForOMLDataSets.R")
if(!exists("getTaskForHPS", mode="function")) source("getTaskForHPS.R")


# Get a task
t.type = "Supervised Regression"
regression.dids = didsForOMLDataSets(t.type)
data.did = regression.dids[92]
tk = getTaskForHPS(data.did = data.did, t.type = t.type)

# Create regr.randomForest with upper and lower limits geared to the task's characteristics
lrn = makeLearner ("regr.randomForest")
implrn = makeImputeWrapper(lrn, classes = list(numeric = imputeMean(),
  factor = imputeMode(), integer = imputeMedian()))

ratios = seq(0.1, 0.9, by = 0.1)

learner.list = list()
for (ratio in ratios){

maxFeatures = sum(tk$task.desc$n.feat)
maxObs = tk$task.desc$size
maxObsTraining = tk$task.desc$size*ratio*0.67

ps = makeParamSet(
  makeIntegerParam("ntree", lower = 1, upper = 100),
  makeIntegerParam("mtry", lower = 1, upper = maxFeatures),
  makeIntegerParam("sampsize", lower = 1, upper = maxObsTraining),
  makeIntegerParam("nodesize", lower = 1, upper = maxObsTraining)
)

des = generateRandomDesign(50, par.set = ps)
pars = dfRowsToList(des,ps)
#pars = pars[c(1:10)]

learner.ps.list = list()
for (i in seq_along(pars)) {   
  # check if features have missing values, if yes impute
  if (tk$task.desc$has.missings) {
    lrn_i = setHyperPars(implrn, par.vals = pars[[i]])
    lrn_i2 = makeDownsampleWrapper(lrn_i, dw.perc = ratio, dw.stratify = FALSE)
    lrn_i2$id = paste(lrn_i$id, i, ".r.", ratio, sep="")
    learner.ps.list[[i]] = lrn_i2
  } else {
    lrn_i = setHyperPars(lrn, par.vals = pars[[i]])
    lrn_i2 = makeDownsampleWrapper(lrn_i, dw.perc = ratio, dw.stratify = FALSE)
    lrn_i2$id = paste(lrn_i$id, i, ".r.", ratio, sep="")
    learner.ps.list[[i]] = lrn_i2
  }
  #names(learner.list) <- paste(lrn_i2$id, seq_along(learner.list), "r", ratio, sep = "")
}
learner.list = c(learner.list, learner.ps.list)
}


