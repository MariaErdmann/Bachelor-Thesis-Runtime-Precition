# Function to get datasets dids (generates oml.task.id)

didsForOMLDataSets = function(t.type) {
  
  library(OpenML)

  # Get all availbale tasks
  tasks = listOMLTasks()

  # Include only tasks of a specific type and exclude tasks with instances > 100 and if data set name is QSAR or fri_*
  sub.tasks = subset(tasks, NumberOfInstances > 100 & task.type == t.type)
  sub.tasks.unique.names = unique(sub.tasks$name)

  not.used = sort(c(grep(pattern = "QSAR", x = sub.tasks.unique.names),
    grep(pattern = "fri_c", x = sub.tasks.unique.names)))
  used = setdiff(1:length(sub.tasks.unique.names), not.used)
  sub.tasks.names.used = sub.tasks.unique.names[used]

  # for some datasets there are more versions available. We only take the first one
  dids = c()
  for (i in 1:length(sub.tasks.names.used)){
    dids = c(dids, unique(sub.tasks[sub.tasks$name == sub.tasks.names.used[i],]$did)[1])
  }
  dids
}


#t.type = "Supervised Regression"
#regression.dids = didsForOMLDataSets(t.type)

#t.type = "Supervised Classification"
#classification.dids = didsForOMLDataSets(t.type)





