# Function to get datasets dids (generates oml.task.id)

didsForOMLDataSets = function() {
  
  library(OpenML)
  options(java.parameters = "- Xmx1024m")  # avoid java overhead

  # Get all availbale tasks
  #tasks = listOMLTasks()
  #save(tasks, file = "C:\\Users\\Maria\\Documents\\Studium\\Statistik\\Bachelorarbeit\\tasks.RData")
  load("C:\\Users\\Maria\\Documents\\Studium\\Statistik\\Bachelorarbeit\\tasks.RData")

  # Include only classifcation datasets
  # and exclude datasets with instances > 1000 and < 50000, with missing values
  # exclude artificial datasets: QSAR, fri_*, volcanoes?, BNG? 
  sub.tasks = subset(tasks, NumberOfInstances > 1000 & NumberOfInstances < 50000
    & task.type == "Supervised Classification" & NumberOfMissingValues == 0)
  
  # only use every dataset once
  sub.tasks = sub.tasks[order(sub.tasks$did),]
  logic = logical(nrow(sub.tasks))
  logic = rep(TRUE, nrow(sub.tasks))
  for (i in 2:nrow(sub.tasks))
    if(sub.tasks$did[i] == sub.tasks$did[i-1]) logic[i] = FALSE
  sub.tasks = sub.tasks[logic, ]
  
  # Exclude QSAR, fri_*, volcanoes?, BNG? *.wc
  # Exclude did = 350 since it is in sparse.format
  sub.tasks = sub.tasks[!(grepl(pattern  = "volcanoes", x = sub.tasks$name)
    | grepl(pattern = "BNG", x = sub.tasks$name) | grepl(pattern = "QSAR",
      x = sub.tasks$name) | grepl(pattern = "fri_c", x = sub.tasks$name)
    | sub.tasks$did == 1146), ]
  # 163
  
  # Datasets with a categorial target only (datasets with data.frame as target are being excluded)
  # I will try these datasets seperately since I don't know yet what the problem with factors are
  # dsets.with.factors = character(nrow(sub.tasks))
  # for (i in 1:nrow(sub.tasks)) {
  #   task = getOMLTask(task.id = sub.tasks$task.id[i])
  #   dsets.with.factors[i] = try(class(task$input$data.set$data[,task$input$data.set$target.features]))
  #   save(dsets.with.factors, file ="C:\\Users\\Maria\\Documents\\Studium\\Statistik\\Bachelorarbeit\\dsets.with.factors.RData")
  # }
  
  load("C:\\Users\\Maria\\Documents\\Studium\\Statistik\\Bachelorarbeit\\dsets.with.factors.RData")
  sub.tasks = sub.tasks[which(dsets.with.factors == "factor"), ] # 151
  
  # Check why target needs to be a factor with these 12 datasets
  no.factors = sub.tasks[which(dsets.with.factors != "factor"), ] # 12
  no.factor.dids = sub.tasks[which(dsets.with.factors != "factor"), ]$did
  
  # There are datasets appearing multiple times with the same name, but different dids
    logic2 = duplicated(sub.tasks$name)
    multiple = sub.tasks[logic2, ]
    
    not.multiple = setdiff(sub.tasks$name, multiple$name)
    sub.tasks.not.multiple = data.frame()
    for (ind in not.multiple){
      sub.tasks.not.multiple = rbind(sub.tasks.not.multiple, sub.tasks[sub.tasks$name == ind,])
    }
    # sub.tasks.not.multiple contains all datasets without a duplicated name in sub.tasks
    
    
    # Choose one of the datasets where the name is duplicated. Datasets differ 
    # in NumberOfClasses and/or NumberOfFeatures.
    # Choose the did where NumberOfClasses or NumberOfFeatures is greatest 
    # 'quake' does not differ in characteristics of interest --> take the first entry
    single.dsets = data.frame()
    for (nme in unique(multiple$name)) {
      same.name = sub.tasks[sub.tasks$name == nme,]
      if (length(unique(same.name$NumberOfClasses)) > 1)
        single.dsets = rbind(single.dsets, same.name[same.name$NumberOfClasses == max(same.name$NumberOfClasses), ])
      else if (length(unique(same.name$NumberOfFeatures)) > 1)
        single.dsets = rbind(single.dsets, same.name[same.name$NumberOfFeatures == max(same.name$NumberOfFeatures), ])
      else if (length(unique(same.name$NumberOfInstances)) > 1)
        single.dsets = rbind(single.dsets, same.name[same.name$NumberOfInstances == max(same.name$NumberOfInstances), ])
      else if (nme == "quake") # needs hardcoding since selected features are the same
        single.dsets = rbind(single.dsets, same.name[1,])
      }
    sub.tasks.mod = rbind(sub.tasks.not.multiple, single.dsets) # need to be 127
    # logic3 = duplicated(sub.tasks.mod$name)
    
    # order sub.tasks.mod with NumberOfInstances descending
    sub.tasks.mod = sub.tasks.mod[order(sub.tasks.mod$NumberOfInstances, sub.tasks.mod$NumberOfFeatures),]
    # FIXME: only one of datasets with similar characteristics should be used
    
    return(sub.tasks.mod)
    
}

class.dsets = didsForOMLDataSets()
save(class.dsets, file = "C:\\Users\\Maria\\Documents\\Studium\\Statistik\\Bachelorarbeit\\class.dsets.RData")




