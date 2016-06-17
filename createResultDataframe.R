library(mlr)
library(batchtools)
library(plyr)
library(OpenML)


# checks single results
dir = "~/code/"
#dir = "C:/Users/Maria/Documents/Studium/Statistik/Bachelorarbeit/Bachelor-Thesis-Runtime-Prediction"
setwd(paste0(dir,"/Results"))
source(paste0(dir,"/definitions.R"))


regname.list = c("reg1554", "reg478") # for testing purposes
#regname.list = paste0("reg",OMLDATASETS)


big.res = data.frame()
error.frame = data.frame()
for (regname in regname.list) {
  regname = loadRegistry(regname)
  done = getStatus(reg = regname)$done + getStatus(reg = regname)$error + getStatus(reg = regname)$expired
  if (done == getStatus(reg = regname)$submitted) {
  
    # create a table with errors
    error.jobs = data.frame()
    if (getStatus()$error > 0) {
      error.jobs = as.data.frame(subset(getJobTable(), subset = !is.na(error),
        select = -c(submitted, memory, batch.id, time.queued,
          pars.hash, resources)))
    }
  
    # get the ids which do not have an error to create results
    error.ids = getErrorMessages()$job.id
    if (length(error.ids) != 0) {
      all.ids = getJobStatus()$job.id
      ids.ok = all.ids[-error.ids]
    } else {
      ids.ok = all.ids
    }
  
    # Create Results
    time = reduceResultsDataTable(ids.ok, fun = function(r) data.frame(as.list(r$aggr)),
      reg = regname)
    hyper.pars = getJobTable(ids.ok, reg = regname)
    res = merge(hyper.pars, time, by = "job.id")
  } else {
    print("Not yet finished.")
  }
  # Create a data.frame which contains all results and a data.frame that contains
  # all errors
  big.res = rbind(big.res, res)
  error.frame = rbind(error.frame, error.jobs)
}


