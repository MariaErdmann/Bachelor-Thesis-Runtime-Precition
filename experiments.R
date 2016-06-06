library(mlr)
library(batchtools)
library(plyr)
library(OpenML)

setOMLConfig(apikey = "6f5535ee9d1e819c0f85447006bca0c3", arff.reader = "farff")

dir = "~/code/"
#dir = "C:/Users/Maria/Documents/Studium/Statistik/Bachelorarbeit/Code_neu"
setwd(paste0(dir,"/Results"))
source(paste0(dir,"/definitions.R"))

unlink("runtime-try", recursive = TRUE)
regis = makeExperimentRegistry("runtime-try", 
  packages = c("mlr", "OpenML", "ranger", "methods"),
  source = "../definitions.R",
  work.dir = ".",
  conf.file = "../"
)

# add our selected OML dsets as problems
for (did in OMLDATASETS) {
  data = list(did = did)
  addProblem(name = as.character(did), data = data)
}

# add one generic 'algo' that evals the algos in hyperpar space
addAlgorithm("eval", fun = function(job, data, instance, lrn.id, ...) {
  par.vals = list(...)
  oml.dset = getOMLDataSet(data$did)             
  task = convertOMLDataSetToMlr(oml.dset)
  type = getTaskType(task)
  bin.or.multi = length(task$task.desc$class.levels)
  par.vals = par.vals[!(is.na(par.vals))]
  par.vals = CONVERTPARVAL(par.vals, task, lrn.id)
  sub.sample.frac = par.vals[["sub.sample.frac"]]
  par.vals = par.vals[names(par.vals) != "sub.sample.frac"]
  lrn.id = paste0(type, ".", lrn.id)
  lrn = makeLearner(lrn.id)
  lrn = setHyperPars(lrn, par.vals = par.vals)
  measures = MEASURES
  #mod = train(lrn, task)
  #p = predict(mod, task)
  #list(performance(pred = p, measures = measures, model = mod), lrn)
  # instead of predict() und performance()
  subsample(lrn, task, iters = 1L, split = sub.sample.frac, stratify = FALSE, models = TRUE, keep.pred = FALSE, measures = measures)
})

# Random design
set.seed(124)
ades = data.frame()
for (lid in LEARNERIDS) {
  ps = makeMyParamSet(lid, task = NULL)
  des.size = DESSIZE(ps)
  d = generateRandomDesign(des.size, ps)
  d = cbind(lrn.id = lid, d, stringsAsFactors = FALSE)
  ades = rbind.fill(ades, d)
}
addExperiments(algo.designs = list(eval = ades))

# FIXME: If  I need to add the defaults of each learner
# defaults
# ades_def = data.frame()
# for (lid in "ranger") {
#   ps = makeMyDefaultParamSet(lid)
#   d = generateGridDesign(ps, resolution = 1)
#   d = cbind(lrn.id = lid, d, stringsAsFactors = FALSE)
#   ades_def = rbind.fill(ades_def, d)
# }

summarizeExperiments()
ids = getJobTable()$job.id
ids = c(1:5,20:25,45:50)


# Auskoemmentieren für Cluseter
submitJobs(ids)
getStatus()
getErrorMessages()
# Error messages produces when using ranger: 
# Error in setHyperPars2.Learner(learner, insert(par.vals, args)) : \n  numeric(0) is not feasible for parameter 'nu'!
getJobPars(getErrorMessages()$job.id)
# Looking at the parameters nu should be NA since type = "C-classification"

# Results
# res = reduceResultsList(ids, fun = function (r) r, reg = regis)
# Does not produce a data table any more because of the class of subsample, I guess
# res = reduceResultsDataTable(1, fun = function(r) as.data.frame(as.list(r)), reg = regis)
# res
# hyper_pars = getJobPars(ids)
# hyper_pars

# zu Debugzwecken
#lrn.id = "ranger"
#par.vals = as.list(ades[1,-1])
#data$did = 457
