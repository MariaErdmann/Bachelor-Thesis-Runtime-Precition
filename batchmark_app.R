library(mlr)
library(batchtools)
library(checkmate)
source("C:\\Users\\Maria\\Documents\\Studium\\Statistik\\Bachelorarbeit\\lrz_examples-master\\lrz_examples-master\\batchmark\\batchmark.r")

learners = learner.list
tasks = list(tk)
resamplings = list(mlr::makeResampleDesc("Subsample", iters = 2L))
# here the split needs to be fixed. Otherwise sampsize may bee too large and mod
# cannot be fitted

unlink("registry", recursive = TRUE)
reg = makeExperimentRegistry()

ids = batchmark(learners, tasks, resamplings, reg = reg,
  measures = list(timepredict, timeboth, timeboth, mse), save.models = TRUE)
summarizeExperiments()

submitJobs(1:10)
showLog(1)
loadResult(1)

