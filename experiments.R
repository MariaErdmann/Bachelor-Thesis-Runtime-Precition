library(mlr)
library(batchtools)
library(plyr)
library(OpenML)

setOMLConfig(apikey = "1f964041d13dac76fbb6b2867d4e1f51", arff.reader = "farff")

# For testing
# dir = "C:/Users/Maria/Documents/Studium/Statistik/Bachelorarbeit/Bachelor-Thesis-Runtime-Prediction"
# setwd(paste0(dir,"/Results"))
# source(paste0(dir,"/definitions.R"))


# Cluster settings
dir = "~/code/"
source(paste0(dir,"definitions.R"))
setwd("/naslx/projects/ua341/di49sib")


# registry name depends on dataset used
regname = paste0("reg", OMLDATASET)

#unlink(regname, recursive = TRUE)
regname = makeExperimentRegistry(regname, 
  packages = c("mlr", "OpenML", "ranger", "methods"),
  source = "~/code/definitions.R",
  conf.file = "~/.batchtools.conf.R",
  seed = 334
)

# add selected OML dataset as problem
data = list(did = OMLDATASET)
addProblem(name = as.character(OMLDATASET), data = data)


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
  if (lrn.id %in% c("svm.linear", "svm.polynomial", "svm.radial", "svm.sigmoid")) lrn.id.mlr = "svm" else lrn.id.mlr = lrn.id
  lrn.id.mlr = paste0(type, ".", lrn.id.mlr)
  lrn = makeLearner(lrn.id.mlr)
  lrn = setHyperPars(lrn, par.vals = par.vals)
  measures = MEASURES
  #mod = train(lrn, task)
  #p = predict(mod, task)
  #r= performance(pred = p, measures = measures, model = mod)
  # instead of predict() und performance()
  subsample(lrn, task, iters = 1L, split = sub.sample.frac, stratify = TRUE, models = FALSE, keep.pred = FALSE, measures = measures)
})

# Random design
ades = data.frame()
for (lid in LEARNERIDS) {
  ps = makeMyParamSet(lid, task = NULL)
  des.size = DESSIZE(ps)
  d = generateRandomDesign(des.size, ps)
  d = cbind(lrn.id = lid, d, stringsAsFactors = FALSE)
  ades = rbind.fill(ades, d)
}

# add default algos
addAlgorithm("default", fun = function(job, data, instance, lrn.id, ...) {
  par.vals = list(...)
  oml.dset = getOMLDataSet(data$did)             
  task = convertOMLDataSetToMlr(oml.dset)
  type = getTaskType(task)
  par.vals = par.vals[!(is.na(par.vals))]
  par.vals = CONVERTPARVAL.DEF(par.vals, task, lrn.id)
  if (lrn.id %in% c("svm.linear", "svm.polynomial", "svm.radial", "svm.sigmoid")) lrn.id.mlr = "svm" else lrn.id.mlr = lrn.id
  lrn.id.mlr = paste0(type, ".", lrn.id.mlr)
  lrn = makeLearner(lrn.id.mlr)
  lrn = setHyperPars(lrn, par.vals = par.vals)
  measures = MEASURES
  mod = train(lrn, task)
  #p = predict(mod, task)
  #r= performance(pred = p, measures = measures, model = mod)
  # instead of predict() und performance()
  subsample(lrn, task, iters = 1L, split = 0.67, stratify = TRUE, models = FALSE, keep.pred = FALSE, measures = measures)
})

ades_def = data.frame()
for (lid in LEARNERIDS) {
  ps = makeMyDefaultParamSet(lid)
  d = generateGridDesign(ps, resolution = 1)
  d = cbind(lrn.id = lid, d, stringsAsFactors = FALSE)
  ades_def = rbind.fill(ades_def, d)
}
addExperiments(algo.designs = list(eval = ades, default = ades_def), repls = 10)
#addExperiments(algo.designs = list(eval = ades, default = ades_def), repls = 1)

#summarizeExperiments()
#ids = getJobTable()$job.id
#ids = c(1:3, 509)

# Comment for cluster (just for testing purposes)
# submitJobs()
# getStatus()
# getErrorMessages()


# Results
#res = reduceResultsList(ids, fun = function (r) r, reg = regis)
#res2 = reduceResultsDataTable(fun = function(r) data.frame(as.list(r$aggr)), reg = regis)
#res
#hyper_pars2 = getJobTable()
#hyper_pars
#hyper.pars.time2 = merge(hyper_pars2, res2, by = "job.id")
