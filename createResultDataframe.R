library(mlr)
library(batchtools)
library(plyr)
library(OpenML)

# for cluster
source("~/code/definitions.R")
dir = "/naslx/projects/ua341/di49sib/"
setwd(dir)

# for testing
# dir = "~/Bachelor-Thesis-Runtime-Prediction/"
# setwd(paste0(dir,"Results"))
# source(paste0(dir,"/definitions.R"))


#regname.list = c("reg1554", "reg478") # for testing purposes
regname.list = paste0("reg",OMLDATASETS)


big.res = data.frame()
error.frame = data.frame()
for (regname in regname.list) {
  reg = loadRegistry(regname)
  done = getStatus()$done + getStatus()$error + getStatus()$expired
  if (done == getStatus(reg = reg)$submitted) {
  
    # create a table with errors
    error.jobs = data.frame()
    if (getStatus()$error > 0) {
      error.jobs = as.data.frame(subset(getJobTable(), subset = !is.na(error),
        select = -c(submitted, memory, batch.id, time.queued,
          pars.hash)))
    }
  
    # get the ids which do not have an error to create results
    error.ids = as.integer(getErrorMessages()$job.id)
    expired.ids = as.integer(findExpired()$job.id)
    all.ids = getJobStatus()$job.id
    if (length(error.ids) != 0 | length(expired.ids) != 0) {
      ids.not.ok = c(error.ids, expired.ids)
      ids.not.ok = sort(ids.not.ok)
      ids.ok = all.ids[-ids.not.ok]
    } else {
      ids.ok = all.ids
    }
  
    # Create Results
    if (length(error.ids) != length(all.ids)) {
      time = reduceResultsDataTable(ids.ok, fun = function(r) data.frame(as.list(r$aggr)))
      hyper.pars = getJobTable(ids.ok)
      res = merge(hyper.pars, time, by = "job.id")
    } else {
      print("No results for output.")
    }
    
  } else {
    print("Not finished yet.")
  }
  
  # Create a data.frame which contains all results and a data.frame that contains
  # all errors
  big.res = rbind(big.res, res)
  resfile = paste0(dir, "finalresult.RData")
  save(big.res, file = resfile)
  error.frame = rbind(error.frame, error.jobs)
  errorfile = paste0(dir, "allerrors.RData")
  save(error.frame, file = errorfile)
}


