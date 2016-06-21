library(mlr)
library(batchtools)
library(plyr)
library(OpenML)

# for testing
# dir = "C:/Users/Maria/Documents/Studium/Statistik/Bachelorarbeit/Bachelor-Thesis-Runtime-Prediction/"
# setwd(paste0(dir,"Results"))
# source(paste0(dir,"/definitions.R"))


# for cluster
source("~/code/definitions.R")
dir = "/naslx/projects/ua341/di49sib/"
setwd(dir)


regNAME = paste0("reg", OMLDATASET)
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
  error.ids = getErrorMessages()$job.id
  all.ids = getJobStatus()$job.id
  if (length(error.ids) != 0) {
    ids.ok = all.ids[-error.ids]
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



