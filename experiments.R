library(mlr)
library(batchtools)
library(plyr)

dir = "C:/Users/Maria/Documents/Studium/Statistik/Bachelorarbeit"
setwd(paste0(dir,"/Results"))
source(paste0(dir,"/Code/definitions.R"))

unlink("runtime-try", recursive = TRUE)
regis = makeExperimentRegistry("runtime-try", 
  packages = c("mlr", "OpenML", "randomForest", "methods"),
  source = "C:/Users/Maria/Documents/Studium/Statistik/Bachelorarbeit/Code/definitions.R",
  work.dir = "C:/Users/Maria/Documents/Studium/Statistik/Bachelorarbeit/results",
  conf.file = "C:/Users/Maria/Documents/Studium/Statistik/Bachelorarbeit.R"
)

#regis$cluster.functions = makeClusterFunctionsMulticore() 

# add our selected OML dsets as problems
for (did in OMLDATASETS) {
  data = list(did = did)
  addProblem(name = as.character(did), data = data)
}

# add one generic 'algo' that evals the RF in hyperpar space
addAlgorithm("eval", fun = function(job, data, instance, lrn.id, ...) {
  par.vals = list(...)
  oml.dset = getOMLDataSet(data$did)             
  task = convertOMLDataSetToMlr(oml.dset)
  type = getTaskType(task)
  par.vals = par.vals[!(is.na(par.vals))]
  par.vals = CONVERTPARVAL(par.vals, task, lrn.id)
  lrn.id = paste0(type, ".", lrn.id)
  lrn = switch(type, "classif" = makeLearner(lrn.id), "regr" = makeLearner(lrn.id))
  lrn = setHyperPars(lrn, par.vals = par.vals)
  measures = MEASURES
  mod = train(lrn, task)
  p = predict(mod, task)
  performance(pred = p, measures = measures, model = mod)
  # statt predict und performance
  #subsample(lrn, task, split = 2/3, models = TRUE, keep.pred = FALSE, meausres = measures)
})

# Random maximin design
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

# FIXME: we need to add the defaults of each learner and defaults that we could invent.
# defaults
ades_def = data.frame()
for (lid in "ranger") {
  ps = makeMyDefaultParamSet(lid)
  d = generateGridDesign(ps, resolution = 1)
  d = cbind(lrn.id = lid, d, stringsAsFactors = FALSE)
  ades_def = rbind.fill(ades_def, d)
}

summarizeExperiments()
ids = getJobTable()$job.id
ids = c(1:3,11:13,33:36,49:51,60:63,82:84,96:98,107:108)



# Paritions jobs into chunks of size 100, these will be executed together
#ids = chunkIds(findNotDone(ids), chunk.size = 100)

submitJobs(ids)

# checken was das ist:
# submitJobs(ids, resources = list(chunk.ncpus = 9))

getStatus()
getErrorMessages()

# Results
res = reduceResultsDataTable(ids, fun = function(r) as.data.frame(as.list(r)), reg = regis)
res
hyper_pars = getJobPars(ids)
hyper_pars

hyper.pars.time = merge(hyper_pars, res, by = "job.id")
save(hyper.pars.time, file = "C:/Users/Maria/Documents/Studium/Statistik/Bachelorarbeit/results/hyper.pars.time.RData")

# zu Debugzwecken
#lrn.id = "ranger"
#par.vals = as.list(ades[1,-1])
#data$did = 457