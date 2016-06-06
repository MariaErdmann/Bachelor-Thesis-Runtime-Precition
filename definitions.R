load("~/code/class.dsets.RData")
#load("C:/Users/Maria/Documents/Studium/Statistik/Bachelorarbeit/Code_neu/class.dsets.RData")
tasks = class.dsets

OMLDATASETS = tasks$did
OMLDATASETS = tasks$did[!(tasks$did %in% c(373))] # cannot be converted because of unsupported feature type
OMLDATASETS = OMLDATASETS[c(5,7,10)] # datasets where target is of type class 

MEASURES = list(timetrain, timepredict, timeboth, mmce)

LEARNERIDS = c("svm") # for testing
#LEARNERIDS = c("ranger", "rpart", "svm", "gbm", "naiveBayes", "nnet", "glmnet") # for final experiment

DESSIZE = function(ps) {
  2 * sum(getParamLengths(ps)) # for testing
  # 10 * sum(getParamLengths(ps)) # final dessize; actually growth would be exponential
}

makeMyParamSet = function(lrn.id, task = NULL) {
  switch(lrn.id,
    ranger = makeParamSet(
      makeIntegerParam(id = "num.trees", lower = 50, upper = 10000),
      makeLogicalParam(id = "replace"),
      makeNumericParam(id = "sample.fraction", lower = 0, upper = 1),
      makeNumericParam(id = "mtry", lower = 0, upper = 1),
      makeNumericParam(id = "min.node.size", lower = 0, upper = 0.5),
      makeNumericParam(id = "sub.sample.frac", lower = 0.5, upper = 0.9) # Split parameter for subsample
    ),
    rpart = makeParamSet(
      makeNumericParam(id = "minsplit", lower = 0, upper = 0.5),
      makeNumericParam(id = "minbucket", lower = 0, upper = 0.5),
      makeNumericParam(id = "cp", lower = 0, upper = 1),
      #makeNumericParam(id = "maxcompete", lower = 0, upper = 0.5), ? dependent on number of values in features, how to implement?
      makeNumericParam(id = "maxsurrogate", lower = 0, upper = 1), # not sure about the range
      makeDiscreteParam(id = "usesurrogate", values = 0:2),
      makeDiscreteParam(id = "surrogatestyle", values = 0:1),
      makeIntegerParam(id = "maxdepth", lower = 1L, upper = 30L), # ?
      makeNumericParam(id = "sub.sample.frac", lower = 0.5, upper = 0.9) 
    ),
    svm = makeParamSet(
      makeDiscreteParam(id = "type", values = c("C-classification", "nu-classification")),
      makeNumericParam(id = "cost",  lower = 0, upper =  2^12,requires = quote(type=="C-classification")), # svm suggests upper limit of 1000
      makeNumericParam(id = "nu", lower = 0, upper = 1, requires = quote(type=="nu-classification")),# vielleicht hier auch 0.5 als upper?
      makeDiscreteParam(id = "kernel", values = c("linear", "polynomial", "radial", "sigmoid")),
      makeIntegerParam(id = "degree", lower = 1L, upper = 5L, requires = quote(kernel=="polynomial")),
      makeNumericParam(id = "coef0", lower = 0, upper = 8,requires = quote(kernel=="polynomial" || kernel=="sigmoid")), # everything above 8 throws error
      makeNumericParam(id = "gamma", lower = 0, upper = 100, requires = quote(kernel!="linear")),
      #makeNumericParam(id = "cachesize", default = 40L), ? Reasonable ranges?
      makeNumericParam(id = "tolerance", lower = 0, upper = 0.5),
      makeLogicalParam(id = "shrinking"),
      # why are scale and epsilon missing as parameters?
      makeNumericParam(id = "sub.sample.frac", lower = 0.5, upper = 0.9)
    ),
    gbm = makeParamSet(
      makeDiscreteParam(id = "distribution", values = c("bernoulli", "adaboost", "huberized")),
      makeIntegerParam(id = "n.trees", lower = 500L, upper = 10000L),
      #makeIntegerParam(id = "cv.folds", lower = 0L, upper = 10L), 
      # Problem: cv.fold = 1 throws error "Object 'p' not found."
      makeIntegerParam(id = "interaction.depth", lower = 1L, upper = 5L),
      makeNumericParam(id = "n.minobsinnode", lower = 0, upper = 0.5),
      makeNumericParam(id = "shrinkage", lower = 10^-4, upper = 10^-1),
      makeNumericParam(id = "bag.fraction", lower = 0, upper = 1),
      makeNumericParam(id = "train.fraction", lower = 0, upper = 1),
      makeNumericParam(id = "sub.sample.frac", lower = 0.5, upper = 0.9)
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
      makeNumericParam(id = "eps", lower = 0, upper = 0.05),
      makeNumericParam("sub.sample.frac", lower = 0.5, upper = 0.9) #?
      # makeNumericParam(id = "big", default = 9.9e35),
      #makeIntegerParam(id = "mnlam", default = 5, lower = 1),
      #makeNumericParam(id = "pmin", lower = 0.001, upper = 1)
      #makeNumericParam(id = "exmx", lower =, upper =),
      #makeNumericParam(id = "prec", default = 1e-10),
      #makeIntegerParam(id = "mxit", lower = 1L, upper =),
    ),
    naiveBayes = makeParamSet(
      makeNumericParam(id = "laplace", lower = 0, upper = 1),
      #? Probably better to fix it to one, so that it can run on all dsets
      makeNumericParam("sub.sample.frac", lower = 0.5, upper = 0.9)
      
    )
  )
}

# FIXME: Shall Default Param Sets be implemented?
# makeMyDefaultParamSet = function (lrn.id, task = NULL) {
#   switch(lrn.id,
#     ranger = makeParamSet(
#       makeIntegerParam("num.trees", lower = 10000, upper = 10000),
#       makeLogicalParam("replace"),
#       makeDiscreteParam("sub.sample.fraction", values = c("1", "0.632")),
#       makeDiscreteParam("mtry", values = c("log", "sqrt", "1", "2")),
#       makeDiscreteParam("min.node.size", values = c("log", "sqrt", "5", "1"))
#     )
#   ) 
# }


CONVERTPARVAL = function(par.vals, task, lrn.id) {
  typ = getTaskType(task)
  n = getTaskSize(task)
  p = getTaskNFeats(task)
  n.class = length(getTaskClassLevels(task))
  if (is.null(par.vals$default)) {
    if (lrn.id == "ranger") {
      par.vals$sample.fraction = max(par.vals$sample.fraction, 1/n) # sollte nicht kleiner als "1" Beobachtung sein
      par.vals$mtry = ceiling(par.vals$mtry * p)
      par.vals$min.node.size = ceiling(par.vals$min.node.size * ceiling(par.vals$sample.fraction * par.vals$sub.sample.frac * n)) # nodesize darf nicht größer sein als sampsize!
    }
    if (lrn.id == "rpart") {
      par.vals$minsplit = max(1, ceiling(par.vals$minsplit * ceiling(par.vals$sub.sample.frac * n))) 
      # check with Bernd: two 'ceilings' necessary?
      par.vals$minbucket = ceiling(par.vals$minbucket * par.vals$sub.sample.frac * n)
      #par.vals$maxcompete = Ausprägungen_in_Variablen * p
      par.vals$maxsurrogate = max(1,ceiling(par.vals$maxsurrogate * par.vals$sub.sample.frac * n))
      par.vals$usesurrogate = as.numeric(as.character(par.vals$usesurrogate))
      par.vals$surrogatestyle = as.numeric(as.character(par.vals$surrogatestyle))
    }
    if (lrn.id == "svm") {
      par.vals$type = as.character(par.vals$type)
      par.vals$kernel = as.character(par.vals$kernel)
      par.vals$nu = par.vals$nu * (par.vals$sub.sample.frac - 0.1) # nu bw. [0,1], and is relative to trainingset size
    }
    if (lrn.id == "gbm") {
      if (n.class == 2) par.vals$distribution = as.character(par.vals$distribution)
      if (n.class > 2) par.vals$distribution = switch(as.character(par.vals$distribution), bernoulli = "multinomial", adaboost = "multinomial", huberized = "multinomial")
      par.vals$n.minobsinnode = max(1, (ceiling(par.vals$n.minobsinnode * (par.vals$sub.sample.frac * par.vals$train.fraction *par.vals$bag.fraction)/2 * n)-1))
    }
    if (lrn.id == "glmnet") {
      par.vals$type.logistic = as.character(par.vals$type.logistic)
      par.vals$type.multinomial = as.character(par.vals$type.multinomial)
    }
  } else {
  }
  return(par.vals)
}
