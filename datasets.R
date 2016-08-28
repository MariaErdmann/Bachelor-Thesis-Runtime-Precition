library(OpenML)


benchmark_tasks = listOMLDataSets(tag = "study_14") # 127
benchmark_tasks = benchmark_tasks[benchmark_tasks$NumberOfMissingValues == 0,] # 111
benchmark_dids = benchmark_tasks$did

tasks = listOMLTasks()
sub.tasks = subset(tasks, tasks$did %in% benchmark_dids & task.type == "Supervised Classification")
sub.tasks = sub.tasks[order(sub.tasks$did),]
logic = logical(nrow(sub.tasks))
logic = rep(TRUE, nrow(sub.tasks))
for (i in 2:nrow(sub.tasks))
  if(sub.tasks$did[i] == sub.tasks$did[i-1]) logic[i] = FALSE
sub.tasks = sub.tasks[logic, ]

sub.sub.tasks = subset(sub.tasks, sub.tasks$NumberOfFeatures <= 500 & sub.tasks$NumberOfClasses < 20)
# 75

# Exclude did == 292, since "File seems to be of sparse format. farff does not
# support this yet!" Concerns dataset "Australia" and "webdata_wXa"
sub.sub.tasks = subset(sub.sub.tasks, sub.sub.tasks$did != 292 & sub.sub.tasks$did != 350)
# 73


# Target should be a factor
# options(java.parameters = "- Xmx1024m")
# dsets.with.factors = character(nrow(sub.sub.tasks))
#  for (i in 1:nrow(sub.sub.tasks)) {
#    task = getOMLTask(task.id = sub.sub.tasks$task.id[i])
#    dsets.with.factors[i] = try(class(task$input$data.set$data[,task$input$data.set$target.features]))
#    save(dsets.with.factors, file ="~/Bachelor-Thesis-Runtime-Prediction/dsets.with.factors.RData")
#  }

load("~/Bachelor-Thesis-Runtime-Prediction/dsets.with.factors.RData")
sub.sub.tasks = sub.sub.tasks[which(dsets.with.factors == "factor"), ] # 68

class.dsets = sub.sub.tasks
class.dsets = class.dsets[order(class.dsets$NumberOfInstances, class.dsets$NumberOfFeatures),]
save(class.dsets, file = "~/Bachelor-Thesis-Runtime-Prediction/class.dsets.RData")
