#load("~/code/class.dsets.RData")
load("C:\Users\Maria\Documents\Studium\Statistik\Bachelorarbeit\Bachelor-Thesis-Runtime-Precition")
tasks = class.dsets

OMLDATASETS = tasks$did
OMLDATASETS = OMLDATASETS[c(2,4,7)]

MEASURES = list(timetrain, timepredict, timeboth, mmce)

LEARNERIDS = c("nnet") # for testing (change to test other algorithms)
#LEARNERIDS = c("ranger", "rpart", "svm.linear", "svm.polynomial", "svm.radial", "svm.sigmoid", "gbm", "glmnet", "naiveBayes", "nnet") # for final experiment

DESSIZE = function(ps) {
  2 * sum(getParamLengths(ps)) # for testing
  # 10 * sum(getParamLengths(ps)) # final dessize; actually growth would be exponential
}

makeMyParamSet = function(lrn.id, task = NULL) {
  switch(lrn.id,
    ranger = makeParamSet(
      makeIntegerParam(id = "num.trees", lower = 1, upper = 1000),
      makeLogicalParam(id = "replace"),
      makeNumericParam(id = "sample.fraction", lower = 0, upper = 1),
      makeNumericParam(id = "mtry", lower = 0, upper = 1),
      makeLogicalParam(id = "respect.unordered.factors"),
      makeNumericParam(id = "sub.sample.frac", lower = 0.5, upper = 0.9) # Split parameter for subsample
    ),
    rpart = makeParamSet(
      makeNumericParam(id = "minsplit", lower = 0, upper = 0.5),
      makeNumericParam(id = "minbucket", lower = 0, upper = 0.5),
      makeNumericParam(id = "cp", lower = -4, upper = -1),
      makeIntegerParam(id = "maxdepth", lower = 1L, upper = 30L),
      makeNumericParam(id = "sub.sample.frac", lower = 0.5, upper = 0.9) 
    ),
    svm.linear = makeParamSet(
      makeNumericParam(id = "cost",  lower = -12, upper =  12),
      makeDiscreteParam(id = "kernel", values = c("linear")),
      makeIntegerParam(id = "cachesize", lower = 5, upper = 200),
      makeLogicalParam(id = "shrinking"),
      makeNumericParam(id = "sub.sample.frac", lower = 0.5, upper = 0.9)
    ),
    svm.polynomial = makeParamSet(
      makeNumericParam(id = "cost",  lower = -12, upper =  12),
      makeDiscreteParam(id = "kernel", values = c("polynomial")),
      makeIntegerParam(id = "degree", lower = 1L, upper = 5L),
      makeNumericParam(id = "gamma", lower = -12, upper = 12),
      makeIntegerParam(id = "cachesize", lower = 5, upper = 200),
      makeLogicalParam(id = "shrinking"),
      makeNumericParam(id = "sub.sample.frac", lower = 0.5, upper = 0.9)
    ),
    svm.radial = makeParamSet(
      makeNumericParam(id = "cost",  lower = -12, upper =  12),
      makeDiscreteParam(id = "kernel", values = c("radial")),
      makeNumericParam(id = "gamma", lower = -12, upper = 12),
      makeIntegerParam(id = "cachesize", lower = 5, upper = 200),
      makeLogicalParam(id = "shrinking"),
      makeNumericParam(id = "sub.sample.frac", lower = 0.5, upper = 0.9)
    ),
    svm.sigmoid = makeParamSet(
      makeNumericParam(id = "cost",  lower = -12, upper =  12), 
      makeDiscreteParam(id = "kernel", values = c("sigmoid")),
      makeNumericParam(id = "gamma", lower = -12, upper = 12),
      makeIntegerParam(id = "cachesize", lower = 5, upper = 200),
      makeLogicalParam(id = "shrinking"),
      makeNumericParam(id = "sub.sample.frac", lower = 0.5, upper = 0.9)
    ),
    gbm = makeParamSet(
      makeIntegerParam(id = "n.trees", lower = 500L, upper = 10000L),
      makeIntegerParam(id = "interaction.depth", lower = 1L, upper = 5L),
      makeNumericParam(id = "shrinkage", lower = -4, upper = -1),
      makeNumericParam(id = "bag.fraction", lower = 0, upper = 1),
      makeNumericParam(id = "sub.sample.frac", lower = 0.5, upper = 0.9)
    ),
    glmnet = makeParamSet(
      makeNumericParam(id = "alpha", lower = 0, upper = 1),
      makeNumericParam(id = "sub.sample.frac", lower = 0.5, upper = 0.9)
    ),
    naiveBayes = makeParamSet(
      makeDiscreteParam(id = "laplace", values = c("1")),
      makeNumericParam(id = "sub.sample.frac", lower = 0.5, upper = 0.9)
    ),
    nnet = makeParamSet(
      #makeIntegerParam(id = "size", lower = 3L, upper = 45L), # optional: mean(p, n.class) put in convertparval
      makeIntegerParam(id = "maxit", lower = 2L, upper = 1000L),
      makeLogicalParam(id = "skip"),
      makeNumericParam(id = "rang", lower = 0, upper = 1), # not sure if this should be used
      makeNumericParam(id = "decay", lower = 0.00001, upper = 1.0),
      #makeIntegerParam(id = "MaxNWts", lower = 1L, upper = 1L),
      makeNumericParam(id = "sub.sample.frac", lower = 0.5, upper = 0.9)
    )
  )
}


makeMyDefaultParamSet = function (lrn.id, task = NULL) {
  switch(lrn.id,
    ranger = makeParamSet(
      makeIntegerParam(id = "mtry", lower = 1L, upper = 1L),
      makeIntegerParam(id = "min.node.size", lower = 1L, upper = 1L),
      makeNumericParam(id = "sample.fraction", lower = 1L, upper = 1L)
    ),
    rpart = makeParamSet(
      makeIntegerParam(id = "minsplit", lower = 20L, upper = 20L) # at least one Param needs to be defined.
    ),
    svm.linear = makeParamSet(
      makeDiscreteParam(id = "kernel", values = c("linear"))
    ),
    svm.polynomial = makeParamSet(
      makeDiscreteParam(id = "kernel", values = c("polynomial")),
      makeNumericParam(id = "gamma", lower = 1, upper = 1)
    ),
    svm.radial = makeParamSet(
      makeDiscreteParam(id = "kernel", values = c("radial")),
      makeNumericParam(id = "gamma", lower = 1, upper = 1)
    ),
    svm.sigmoid = makeParamSet(
      makeDiscreteParam(id = "kernel", values = c("sigmoid")),
      makeNumericParam(id = "gamma", lower = 1, upper = 1)
    ),
    gbm = makeParamSet(
      makeIntegerParam(id = "n.trees", lower = 100L, upper = 100L) # need at least one param
    ),
    glmnet = makeParamSet(
      makeNumericParam(id = "lambda.min.ratio", lower = 1L, upper = 1L),
      makeIntegerParam(id = "dfmax", lower = 1L, upper = 1L),
      makeIntegerParam(id = "pmax", lower = 1L, upper = 1L)
    ),
    naiveBayes = makeParamSet(
      makeNumericParam(id = "laplace", lower = 1, upper = 1) # not set do default in order to prevent algo from breaking
    ),
    nnet = makeParamSet(
      makeIntegerParam(id = "size", lower = 3L, upper = 3L)
    )
  )
}


CONVERTPARVAL = function(par.vals, task, lrn.id) {
  typ = getTaskType(task)
  n = getTaskSize(task)
  p = getTaskNFeats(task)
  n.class = length(getTaskClassLevels(task))
  if (is.null(par.vals$default)) {
    if (lrn.id == "ranger") {
      par.vals$sample.fraction = max(par.vals$sample.fraction, 1/n) # sollte nicht kleiner als "1" Beobachtung sein
      par.vals$mtry = max(1, ceiling(par.vals$mtry * p))
    }
    if (lrn.id == "rpart") {
      par.vals$minsplit = max(1, ceiling(par.vals$minsplit * par.vals$sub.sample.frac * n)) # Adapt?
      par.vals$minbucket = ceiling(par.vals$minbucket * par.vals$sub.sample.frac * n) # Adapt?
      par.vals$cp = 10^par.vals$cp
    }
    if (lrn.id == "svm.linear") {
      par.vals$kernel = as.character(par.vals$kernel)
      par.vals$cost = 2^par.vals$cost
    }
    if (lrn.id == "svm.polynomial") {
      par.vals$kernel = as.character(par.vals$kernel)
      par.vals$cost = 2^par.vals$cost
      par.vals$gamma = 2^par.vals$gamma
    }
    if (lrn.id == "svm.radial") {
      par.vals$kernel = as.character(par.vals$kernel)
      par.vals$cost = 2^par.vals$cost
      par.vals$gamma = 2^par.vals$gamma
    }
    if (lrn.id == "svm.sigmoid") {
      par.vals$kernel = as.character(par.vals$kernel)
      par.vals$cost = 2^par.vals$cost
      par.vals$gamma = 2^par.vals$gamma
    }
    if (lrn.id == "gbm") {
      par.vals$shrinkage = 10^par.vals$shrinkage
    }
    if (lrn.id == "naiveBayes") {
      par.vals$laplace = as.numeric(par.vals$laplace)
    }
    # if (lrn.id == "nnet") {
    #  par.vals$MaxNWts = 21 * par.vals$size
    # }
  } else {
  }
  return(par.vals)
}




CONVERTPARVAL.DEF = function(par.vals, task, lrn.id) {
  typ = getTaskType(task)
  n = getTaskSize(task)
  p = getTaskNFeats(task)
  n.class = length(getTaskClassLevels(task))
  if (is.null(par.vals$default)) {
    if (lrn.id == "ranger") {
      par.vals$mtry = max(1, ceiling(sqrt(par.vals$mtry * p)))
    }
    if (lrn.id == "svm.linear") {  
      par.vals$kernel = as.character(par.vals$kernel)
    }
    if (lrn.id == "svm.polynomial") {
      par.vals$kernel = as.character(par.vals$kernel)
      par.vals$gamma = par.vals$gamma/n
    }
    if (lrn.id == "svm.radial") {
      par.vals$kernel = as.character(par.vals$kernel)
      par.vals$gamma = par.vals$gamma/n
    }
    if (lrn.id == "svm.sigmoid") {
      par.vals$kernel = as.character(par.vals$kernel)
      par.vals$gamma = par.vals$gamma/n
    }
    if (lrn.id == "glmnet") {
      if (n < p) {
        par.vals$lambda.min.ratio = par.vals$lambda.min.ratio*0.01
      } else {
        par.vals$lambda.min.ratio = par.vals$lambda.min.ratio*0.0001
      }
      par.vals$dfmax = par.vals$dfmax*n +1
      par.vals$pmax = min(par.vals$dfmax * 2+20, n)
    }
  } else {
  }
  return(par.vals)
}