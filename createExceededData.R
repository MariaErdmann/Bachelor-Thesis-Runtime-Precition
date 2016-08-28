library(mlr)
library(batchtools)
library(plyr)
library(OpenML)
library(xtable)

# for testing
# dir = "~/Bachelor-Thesis-Runtime-Prediction/"
# setwd(paste0(dir,"Results"))
# source(paste0(dir,"/definitions.R"))

#### RUN ON CLUSTER
# for cluster
source("~/code/definitions.R")
dir = "/naslx/projects/ua341/di49sib/"
setwd(dir)

regname.list = paste0("reg",OMLDATASETS)
expired.jobs = data.frame()

for (regname in regname.list) {
  reg = loadRegistry(regname)
  df = getJobTable()
  df$expired = rep(0, nrow(df))
  df$exc.memory = rep(0, nrow(df))
  df$time.limit = rep(0, nrow(df))
  exp.ids = findExpired()$job.id
  rownum.exp.ids = which(df$job.id %in% exp.ids)

  for (id in rownum.exp.ids) {
    df[id, "expired"] = 1
    if (length(grepLogs(id = id, pattern = "Exceeded step memory limit", reg = reg)$matches) > 0) {
      df[id, "exc.memory"] = 1
      df[id, "time.limit"] = 3
    }
  }
  expired.jobs = rbind(expired.jobs, df)
  filename = paste0(dir, "/expired.RData")
  save(expired.jobs, file = filename)
}


#### AFTER CLUSTER
load("~/Bachelor-Thesis-Runtime-Prediction/expired.RData")
# time limit column needs to be 'corrected' a little bit
# if jobs are expired (=1), not due to memory (=0) but due to time limit, then
# time limit needs to set to 1
expired.jobs[expired.jobs$expired == 1 & expired.jobs$exc.memory == 0 & expired.jobs$time.limit == 0, "time.limit"] = 1
# if jobs are expired (= 1), due to memory ( = 1), then time limit shall be 1 (now it is 3)
expired.jobs[expired.jobs$expired == 1 & expired.jobs$exc.memory == 1 & expired.jobs$time.limit == 3, "time.limit"] = 0

# save expired jobs
only.expired.jobs = expired.jobs[expired.jobs$expired == TRUE, ]
sum.exp.jobs.mem = ddply(only.expired.jobs, .(lrn.id), summarise, sum = sum(exc.memory))
sum.exp.jobs.tim = ddply(only.expired.jobs, .(lrn.id), summarise, sum = sum(time.limit))
sum.exp.jobs = merge(sum.exp.jobs.mem, sum.exp.jobs.tim, by = "lrn.id")
colnames(sum.exp1027.jobs) = c("lrn.id", "memory", "time")
save(only.expired.jobs, file = "~/Bachelor-Thesis-Runtime-Prediction/only_expired_jobs.RData")
