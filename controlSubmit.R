#load(file = "C:/Users/Maria/Documents/Studium/Statistik/Bachelorarbeit/Bachelor-Thesis-Runtime-Prediction/seqend.RData")
#load(file = "C:/Users/Maria/Documents/Studium/Statistik/Bachelorarbeit/Bachelor-Thesis-Runtime-Prediction/count.RData")
#load(file = "C:/Users/Maria/Documents/Studium/Statistik/Bachelorarbeit/Bachelor-Thesis-Runtime-Prediction/datasetcount.RData")

load(file = "~/code/seqend.RData")
load(file = "~/code/count.RData")
load(file = "~/code/datasetcount.RData")

error.jobs = data.frame()
  seqstart = seqend
  seqend = seqstart + 5000
  if (seqstart == 0) ids = seq(1,seqend)
  if (seqstart > 0) ids = seq((seqstart+1), seqend)
  
  submitJobs(ids)
  
  done = getStatus()$done + getStatus()$error + getStatus()$expired
  
  if (done == seqend) {
    if (getStatus()$error > 0) {
      error.jobs = subset(getJobTable(), subset = !is.na(error),
        select = -c(submitted, memory, batch.id, time.queued,
          pars.hash, resources))
    }
    count = count + 1
    data.set.count = append(data.set.count, count)
    error.jobs
  }
  
#save(seqend, file = "C:/Users/Maria/Documents/Studium/Statistik/Bachelorarbeit/Bachelor-Thesis-Runtime-Prediction/seqend.RData")
#save(error.jobs, file = "C:/Users/Maria/Documents/Studium/Statistik/Bachelorarbeit/Bachelor-Thesis-Runtime-Prediction/errorframe.RData")
#save(count, file = "C:/Users/Maria/Documents/Studium/Statistik/Bachelorarbeit/Bachelor-Thesis-Runtime-Prediction/count.RData")
#save(data.set.count, file = "C:/Users/Maria/Documents/Studium/Statistik/Bachelorarbeit/Bachelor-Thesis-Runtime-Prediction/datasetcount.RData")

save(seqend, file = "~/code/seqend.RData")
save(error.jobs, file = "~/code/errorframe.RData")
save(count, file = "~/code/count.RData")
save(data.set.count, file = "~/code/datasetcount.RData")  
  