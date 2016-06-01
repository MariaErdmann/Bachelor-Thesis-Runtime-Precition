load("C:\\Users\\Maria\\Documents\\Studium\\Statistik\\Bachelorarbeit\\class.dsets.RData")
#load("/home/probst/Random_Forest/RFParset/results/reg.RData")
tasks = class.dsets

OMLDATASETS = tasks$did
#OMLDATASETS = tasks$did[!(tasks$did %in% c(1054, 1071, 1065))] # Cannot guess task.type from data! for these 3
OMLDATASETS = tasks$did[!(tasks$did %in% c(373))] # cannot be converted because of unsupported feature type
OMLDATASETS = OMLDATASETS[c(5,7,10)] # datasets where target is of type class FIXME: Probably other targets do not work
                                       # 2 does not work in prediction, cannot produce error

MEASURES = list(timetrain, timepredict, timeboth, mmce)

LEARNERIDS = c("ranger", "glmnet", "gbm")
#LEARNERIDS = c("ranger", "glmnet", "naiveBayes", "gbm", "rpart")

DESSIZE = function(ps) {
  2 * sum(getParamLengths(ps))
  # 10 * sum(getParamLengths(ps)) actually growth sould be exponential
}

makeMyParamSet = function(lrn.id, task = NULL) {
  switch(lrn.id,
    ranger = makeParamSet(
      makeIntegerParam("num.trees", lower = 50, upper = 10000),
      makeLogicalParam("replace"),
      makeNumericParam("sample.fraction", lower = 0, upper = 1),
      makeNumericParam("mtry", lower = 0, upper = 1),
      makeNumericParam("min.node.size", lower = 0, upper = 0.5)
    ),
    glmnet = makeParamSet(
      makeNumericParam(id = "alpha", lower = 0, upper = 1),
      makeNumericParam(id = "s", lower = 0, upper = 1),
      makeLogicalParam(id = "exact"),
      makeIntegerParam(id = "nlambda", lower = 1, upper = 2^12),
      makeNumericParam(id = "lambda.min.ratio", lower = 0, upper = 1),
      makeLogicalParam(id = "standardize"),
      makeLogicalParam(id = "intercept"),
      #makeNumericParam(id = "thresh", lower = 0, upper =),
      #makeIntegerParam(id = "dfmax", lower = 0L, upper =),
      #makeIntegerParam(id = "pmax", lower = 0L, upper =), 
      makeIntegerParam(id = "maxit", lower = 1L, upper = 1000000L), # check upper limit
      makeDiscreteParam(id = "type.logistic", values = c("Newton", "modified.Newton")),
      makeDiscreteParam(id = "type.multinomial", values = c("ungrouped", "grouped")),
      makeNumericParam(id = "fdev", lower = 0, upper = 1), #?
      makeNumericParam(id = "devmax", lower = 0, upper = 1), # ?
      makeNumericParam(id = "eps", lower = 0, upper = 0.05) #?
      # makeNumericParam(id = "big", default = 9.9e35),
      #makeIntegerParam(id = "mnlam", default = 5, lower = 1),
      #makeNumericParam(id = "pmin", lower = 0.001, upper = 1)
      #makeNumericParam(id = "exmx", lower =, upper =),
      #makeNumericParam(id = "prec", default = 1e-10),
      #makeIntegerParam(id = "mxit", lower = 1L, upper =),
    ),
    naiveBayes = makeParamSet(
      makeNumericParam(id = "laplace", lower = 0, upper = 1) #? Probably better to fix it to one, so that it can run on all dsets
    ),
    gbm = makeParamSet(
      
    )
  )
}

makeMyDefaultParamSet = function (lrn.id, task = NULL) {
  switch(lrn.id,
    ranger = makeParamSet(
      makeIntegerParam("num.trees", lower = 10000, upper = 10000),
      makeLogicalParam("replace"),
      makeDiscreteParam("sample.fraction", values = c("1", "0.632")),
      makeDiscreteParam("mtry", values = c("log", "sqrt", "1", "2")),
      makeDiscreteParam("min.node.size", values = c("log", "sqrt", "5", "1"))
    )
  ) 
}


CONVERTPARVAL = function(par.vals, task, lrn.id) {
  typ = getTaskType(task)
  n = getTaskSize(task)
  p = getTaskNFeats(task)
  if (is.null(par.vals$default)) {
    if (lrn.id == "ranger") {
      par.vals$sample.fraction = max(par.vals$sample.fraction, 1/n) # sollte nicht kleiner als "1" Beobachtung sein
      par.vals$mtry = ceiling(par.vals$mtry * p)
      par.vals$min.node.size =  ceiling(par.vals$min.node.size * ceiling(par.vals$sample.fraction * n)) # nodesize darf nicht größer sein als sampsize!
    }
    if (lrn.id == "glmnet") {
      par.vals$type.logistic = switch(as.character(par.vals$type.logistic), Newton = "Newton", modified.Newton = "modified.Newton")
      par.vals$type.multinomial = switch(as.character(par.vals$type.multinomial), grouped = "grouped", ungrouped = "ungrouped")
    }
    
  } else {
    
  }
  
  return(par.vals)
}