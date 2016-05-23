# Get results for BatchExperiments
res1 = reduceResultsExperiments(myreg, ids = findDone(myreg), fun = function(job, res) res$aggr)
res2 = reduceResultsExperiments(myreg, ids = findDone(myreg), fun = function(job, res) res$measures.test)



# Get Result for batchtools::batchmark
res3 = reduceResults(ids = 1, fun = function(job,res) res$model)

