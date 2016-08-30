library(mlr)
library(batchtools)
library(plyr)
library(OpenML)

# for testing
# dir = "~/Bachelor-Thesis-Runtime-Prediction/Results/"
# setwd(paste0(dir,"Results"))
# source("~/Bachelor-Thesis-Runtime-Prediction/definitions.R")


# for cluster
source("~/code/definitions.R")
dir = "/naslx/projects/ua341/di49sib/"
setwd(dir)


#regNAME = paste0("reg", OMLDATASET)
regNAME = "reg4135"
reg = loadRegistry(regNAME)

# check if work is done
done = getStatus()$done + getStatus()$error + getStatus()$expired
if (done == getStatus()$submitted) {
  
  # create a table with errors
  if (getStatus()$error > 0) {
    error.jobs = as.data.frame(subset(getJobTable(), subset = !is.na(error),
      select = -c(submitted, memory, batch.id, time.queued,
        pars.hash)))
    filename = paste0(dir, regNAME, "/errorframe.RData")
    save(error.jobs, file = filename)
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
    hyper.pars = getJobTable()
    res = merge(hyper.pars, time, by= "job.id")
    filename = paste0(dir, regNAME, "/singleres.RData")
    save(res, file = filename)
  } else {
    print("No results for output.")
  }
} else {
  print("Not yet finished.")
}



