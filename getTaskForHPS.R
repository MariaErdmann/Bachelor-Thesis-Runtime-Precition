# Get the task
# No task will be returned if there are missing values in the target variable of the data
# since most algorithms can't handle this case


getTaskForHPS = function(data.did, t.type){
  
  library(mlr)
  
  tasks = listOMLTasks()
  extract.tasks = subset(tasks, tasks$did == data.did & tasks$task.type == t.type)
  task.id = extract.tasks$task.id[1]
  task = getOMLTask(task.id = task.id)
  MlrTask = convertOMLTaskToMlr(task)
  MlrTask = MlrTask$mlr.task
  
  target = MlrTask$task.desc$target
  MlrData = getTaskData(MlrTask)
  
  if (anyNA(MlrData[, target])){
    stopf("Target of OML Task '%s' has missing values.", task.id)
  } else {
    return(MlrTask)
  }
}


# example
#data.did = regression.dids[92]
#t.type = "Supervised Regression"
#boston.task = getTaskForHPS(data.did = data.did, t.type = t.type)


