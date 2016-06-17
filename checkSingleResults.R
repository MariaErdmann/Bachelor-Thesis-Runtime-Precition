library(mlr)
library(batchtools)
library(plyr)
library(OpenML)


# checks single results
dir = "~/code/"
#dir = "C:/Users/Maria/Documents/Studium/Statistik/Bachelorarbeit/Bachelor-Thesis-Runtime-Prediction"
setwd(paste0(dir,"/Results"))
source(paste0(dir,"/definitions.R"))

regname = paste0("reg", OMLDATASET)
regname = loadRegistry(regname)

# check if work is done
done = getStatus(reg = regname)$done + getStatus(reg = regname)$error + getStatus(reg = regname)$expired
if (done == getStatus(reg = regname)$submitted) {
  
  # create a table with errors
  if (getStatus()$error > 0) {
    error.jobs = as.data.frame(subset(getJobTable(), subset = !is.na(error),
      select = -c(submitted, memory, batch.id, time.queued,
        pars.hash, resources)))
  }
  
  # get the ids which do not have an error to create results
  error.ids = getErrorMessages()$job.id
  all.ids = getJobStatus()$job.id
  if (length(error.ids) != 0) {
    ids.ok = all.ids[-error.ids]
  } else {
    ids.ok = all.ids
  }
  
  # Create Results
  time = reduceResultsDataTable(ids.ok, fun = function(r) data.frame(as.list(r$aggr)),
    reg = regname)
  hyper.pars = getJobTable(reg = regname)
  res = merge(hyper.pars, time, by= "job.id")
} else {
  print("Not yet finished.")
}

