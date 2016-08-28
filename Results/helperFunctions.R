# Create a new variable that is the same for all replications
# New variable job.id2 indicating which jobs have the same parameter setting
# since we have 10 replications for each setting
# job.id = 1, 2, ..., 10 are job.id2 = 1, job.id 11, 12, ...., 20, are job.id2 =2

createNewJobIdForReplications = function(data) {
  
  # Initialisieren
  if (data$job.id[1] %in% seq(1:10)){
    j = 1
  } else {
    j = 0
  }
  
  job.id2 = rep(NA, nrow(data)) 
  data = cbind(data, job.id2)
  data.new = data.frame()
  
  OMLDATASETS = unique(data$did)
  for (OMLDATASET in OMLDATASETS) {
    data.loop = data[data$did == OMLDATASET,]
    i = 1
    for (id in data.loop$job.id) {
      
      if (id %in% rep(i:(i+9))) {
        data.loop[data.loop$job.id == id, "job.id2"] = j
      } else {
        i = i + 10
        while (!(id %in% rep(i:(i+9)))) {i = i + 10}
        j = j + 1
        data.loop[data.loop$job.id == id, "job.id2"] = j
      }
    }
    data.new = rbind(data.new, data.loop)
  }
  data.new
}



# Creates a dataframe where the replicates are summarisied to one row and
# the measures (time, mmce) of the replicates are the mean

CreateDataFrameWithMeans = function(data) {
  
  # Calculate the mean of the replications (done via the self-created variable job.id2)
  means.data = aggregate(data[, c("timetrain.test.mean", "timepredict.test.mean", "timeboth.test.mean", "mmce.test.mean")], by = list(data$job.id2), FUN = mean)
  colnames(means.data)[1] = "job.id2"
  
  # Gets the first row of the replicates dataset description and parameter settings
  drops = c("job.id", "repl", "timetrain.test.mean", "timepredict.test.mean", "timeboth.test.mean", "mmce.test.mean")
  df.to.reduce = data[, !(names(data) %in% drops)]
  reduced.data = aggregate(df.to.reduce, list(df.to.reduce$job.id2), FUN=head, 1)
  
  if (all.equal(reduced.data$Group.1, reduced.data$job.id2)) {
    
    # Merge the calculated means of measures and dataset descr + param setting
    merged.data = merge(reduced.data, means.data, by = "job.id2")
    drop = c("Group.1")
    merged.data = merged.data[,!(names(merged.data) %in% drop)]
  } else {
    print("Something went wrong!")
  }
}


# Create dataframe where the replications are summarised by standard deviation
createDataFrameWithCv = function(data) {
  
  library(raster)
  # Calculate the mean of the replications (done via the self-created variable job.id2)
  sd.data = aggregate(data[, c("timetrain.test.mean", "timepredict.test.mean", "timeboth.test.mean", "mmce.test.mean")], by = list(data$job.id2), FUN = cv)
  colnames(sd.data)[1] = "job.id2"
  colnames(sd.data)[colnames(sd.data)=="timetrain.test.mean"] = "timetrain.sd"
  colnames(sd.data)[colnames(sd.data)=="timepredict.test.mean"] = "timepredict.sd"
  colnames(sd.data)[colnames(sd.data)=="timeboth.test.mean"] = "timeboth.sd"
  colnames(sd.data)[colnames(sd.data)=="mmce.test.mean"] = "mmce.sd"
  
  # Gets the first row of the replicates dataset description and parameter settings
  drops = c("job.id", "repl", "timetrain.test.mean", "timepredict.test.mean", "timeboth.test.mean", "mmce.test.mean")
  df.to.reduce = data[, !(names(data) %in% drops)]
  reduced.data = aggregate(df.to.reduce, list(df.to.reduce$job.id2), FUN=head, 1)
  
  if (all.equal(reduced.data$Group.1, reduced.data$job.id2)) {
    
    # Merge the calculated means of measures and dataset descr + param setting
    merged.data = merge(reduced.data, sd.data, by = "job.id2")
    drop = c("Group.1")
    merged.data = merged.data[,!(names(merged.data) %in% drop)]
  } else {
    print("Something went wrong!")
  }
}


# Create a summary table
getTimeSummary = function(data) {
  library(fBasics)
  times = t(round(basicStats(data[,c("timetrain.test.mean", "timepredict.test.mean",
    "timeboth.test.mean", "mmce.test.mean")])[c("Minimum", "Maximum",
    "1. Quartile", "3. Quartile", "Mean", "Median", "Stdev", "SE Mean",
    "NAs"),], 3))
  
  par(mfrow = c(1,2))
  bp.train.pred = boxplot(data$timetrain.test.mean, data$timetrain.test.mean,
  ylab = "Time (seconds)", names = c("Training", "Prediction"))
  bp.both = boxplot(data$timeboth.test.mean, names = "Both")
  return(times)
}

getFeautureSummary = function(data, preds) {
  summary(data[preds])
}

getFeautureSummaryTable = function(data, preds) {
  library(fBasics)
  t(round(basicStats(data[preds])[c("Minimum", "Maximum",
      "1. Quartile", "3. Quartile", "Mean", "Median", "Stdev", "SE Mean",
      "NAs"),], 3))
}


# Creates Formulas
createGlmboostFormula = function(preds, response) {
  as.formula(paste(response, "~ ", paste(
    paste0(preds, collapse = " + "),
    paste0("I(", preds, "^2)", collapse = " + "),
    paste0("sqrt(", preds, ")", collapse = " + "), 
    paste0("log(", preds, ")", collapse = " + "),
    sep = " + ")))
}

createGamboostFormula = function(preds, response) {
  as.formula(paste(response, "~ ",
    paste(
      paste0("bols(intercept, intercept = FALSE)"),
      paste0("bols(", preds, ", intercept = FALSE)", collapse = " + "),
      paste0("bbs(", preds, ", center = TRUE, df = 1, knots = 20)", collapse =" + "),
      collapse = " + ", sep = " + ")))
}


# Function taken from http://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
grid_arrange_shared_legend <- function(..., nrow = 1, ncol = length(list(...)), position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  gl <- c(gl, nrow = nrow, ncol = ncol)
  
  combined <- switch(position,
    "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
      legend,
      ncol = 1,
      heights = unit.c(unit(1, "npc") - lheight, lheight)),
    "right" = arrangeGrob(do.call(arrangeGrob, gl),
      legend,
      ncol = 2,
      widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  
}


# Function taken from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
      ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
        layout.pos.col = matchidx$col))
    }
  }
}

# creates partial effect plots for the linear or the smooth base-learner of 
# gamboost with ggplot2.
createSinglePartialEffectPlots = function(input, model, which, xlab){
  df = data.frame(covariate = input,
    part.effect = predict(model, which = which))
  colnames(df) = c("covariate", "part.effect")
  ggplot(df, aes(x = covariate, y = part.effect)) + 
    geom_point(size = 0.8) + 
    geom_line(linetype = "dotted") +
    labs (x = xlab, y = "Parital effect")
}

#creates combined partial effect plots with ggplot2, where functional estimates 
# of the linear and the smooth base-learner are summed
createCombinedPartialEffectPlots = function(input, model, which, xlab){
  df = data.frame(covariate = input,
  part.effect = rowSums(predict(model, which = which)))
  ggplot(df, aes(x = covariate, y = part.effect)) + 
  geom_point(size = 0.8) + 
  geom_line(linetype = "dotted") +
  labs (x = xlab, y = "Parital effect")
}

# reates effect plots for the sum of the functional estimates for each covariate
createPartialEffectsForGlm = function(input, model, linear.effect = NULL, quadratic.effect = NULL, sqrt.effect = NULL, log.effect = NULL, xlab) {
  x1 = sort(input)
  col1 = coef(model)[linear.effect]*(x1)
  col2 = coef(model)[quadratic.effect]*(x1^2)
  col3 =coef(model)[sqrt.effect]*sqrt(x1)
  col4 = coef(model)[log.effect]*log(x1)
  y1 = apply(cbind(col1,col2,col3,col4),1,sum,na.rm = TRUE) 
 
  df1 = data.frame(x1,y1)
  ggplot(df1, aes(x=x1, y = y1)) + 
    geom_point(size = 0.8) +
    geom_line(linetype = "dotted")+
    labs(x = xlab, y = "Partial effect")
}


# Calculate RMSE and RAE. Input: all regression models and the datasets the regression models
# were trained on
calculateErrorsForComparison = function(time, glmboost, glmboost.data, gamboost,
  gamboost.data, glm, glm.data, rF, rF.data){
  
  if (time == "training") {
  
  mse.test.1 = mean((glmboost.data$timetrain.test.mean - predict(glmboost, newdata = glmboost.data, type = "response"))^2)
  mse.test.2 = mean((gamboost.data$timetrain.test.mean - predict(gamboost, newdata = gamboost.data, type = "response"))^2)
  mse.test.3 = mean((glm.data$timetrain.test.mean - predict(glm, newdata = glm.data, type = "response"))^2)
  mse.test.4 = mean((rF.data$timetrain.test.mean - predict(rF, newdata = rF.data, type = "response"))^2)
  
  rae.test.1 = sum(abs(glmboost.data$timetrain.test.mean - predict(glmboost, newdata = glmboost.data, type = "response")))/
      sum(abs(glmboost.data$timetrain.test.mean - mean(glmboost.data$timetrain.test.mean)))
  rae.test.2 = sum(abs(gamboost.data$timetrain.test.mean - predict(gamboost, newdata = gamboost.data, type = "response")))/
      sum(abs(gamboost.data$timetrain.test.mean - mean(gamboost.data$timetrain.test.mean)))
  rae.test.3 = sum(abs(glm.data$timetrain.test.mean - predict(glm, newdata = glm.data, type = "response")))/
      sum(abs(glm.data$timetrain.test.mean - mean(glm.data$timetrain.test.mean)))
  rae.test.4 = sum(abs(rF.data$timetrain.test.mean - predict(rF, newdata = rF.data, type = "response")))/
      sum(abs(rF.data$timetrain.test.mean - mean(rF.data$timetrain.test.mean)))
  
  errors = rbind(glmboost = c(mse.test.1, sqrt(mse.test.1), rae.test.1),
    gamboost = c(mse.test.2, sqrt(mse.test.2), rae.test.2),
    glm = c(mse.test.3, sqrt(mse.test.3), rae.test.3),
    randomForest = c(mse.test.4, sqrt(mse.test.4), rae.test.4))
  
  } else if (time == "prediction") {
    mse.pred.1 = mean((glmboost.data$timepredict.test.mean - predict(glmboost, newdata = glmboost.data, type = "response"))^2)
    mse.pred.2 = mean((gamboost.data$timepredict.test.mean - predict(gamboost, newdata = gamboost.data, type = "response"))^2)
    mse.pred.3 = mean((glm.data$timepredict.test.mean - predict(glm, newdata = glm.data, type = "response"))^2)
    mse.pred.4 = mean((rF.data$timepredict.test.mean - predict(rF, newdata = rF.data, type = "response"))^2)
    
    rae.pred.1 = sum(abs(glmboost.data$timepredict.test.mean - predict(glmboost, newdata = glmboost.data, type = "response")))/
        sum(abs(glmboost.data$timepredict.test.mean - mean(glmboost.data$timepredict.test.mean)))
    rae.pred.2 = sum(abs(gamboost.data$timepredict.test.mean - predict(gamboost, newdata = gamboost.data, type = "response")))/
        sum(abs(gamboost.data$timepredict.test.mean - mean(gamboost.data$timepredict.test.mean)))
    rae.pred.3 = sum(abs(glm.data$timepredict.test.mean - predict(glm, newdata = glm.data, type = "response")))/
        sum(abs(glm.data$timepredict.test.mean - mean(glm.data$timepredict.test.mean)))
    rae.pred.4 = sum(abs(rF.data$timepredict.test.mean - predict(rF, newdata = rF.data, type = "response")))/
        sum(abs(rF.data$timepredict.test.mean - mean(rF.data$timepredict.test.mean)))
    
    errors = rbind(glmboost = c(mse.pred.1, sqrt(mse.pred.1), rae.pred.1),
      gamboost = c(mse.pred.2, sqrt(mse.pred.2), rae.pred.2),
      glm= c(mse.pred.3, sqrt(mse.pred.3), rae.pred.3),
      randomForest = c(mse.pred.4, sqrt(mse.pred.4), rae.pred.4))
  }
   colnames(errors) = c("mse", "rmse", "rae")
  errors
}

# Calculate RMSE and RAE. Input: all prediction vectors made by the regression models
# and the datasets the regression models
# were trained on
calculateErrorsForComparison2 = function(time, glmboost.pred, glmboost.data, gamboost.pred,
  gamboost.data, glm.pred, glm.data, rF.pred, rF.data){
  
  if (time == "training") {
    
    mse.test.1 = mean((glmboost.data$timetrain.test.mean - glmboost.pred)^2)
    mse.test.2 = mean((gamboost.data$timetrain.test.mean - gamboost.pred)^2)
    mse.test.3 = mean((glm.data$timetrain.test.mean - glm.pred)^2)
    mse.test.4 = mean((rF.data$timetrain.test.mean - rF.pred)^2)
    
    rae.test.1 = sum(abs(glmboost.data$timetrain.test.mean - glmboost.pred))/
        sum(abs(glmboost.data$timetrain.test.mean - mean(glmboost.data$timetrain.test.mean)))
    rae.test.2 = sum(abs(gamboost.data$timetrain.test.mean - gamboost.pred))/
        sum(abs(gamboost.data$timetrain.test.mean - mean(gamboost.data$timetrain.test.mean)))
    rae.test.3 = sum(abs(glm.data$timetrain.test.mean - glm.pred))/
        sum(abs(glm.data$timetrain.test.mean - mean(glm.data$timetrain.test.mean)))
    rae.test.4 = sum(abs(rF.data$timetrain.test.mean - rF.pred))/
        sum(abs(rF.data$timetrain.test.mean - mean(rF.data$timetrain.test.mean)))
    
    errors = rbind(glmboost = c(mse.test.1, sqrt(mse.test.1), rae.test.1),
      gamboost = c(mse.test.2, sqrt(mse.test.2), rae.test.2),
      glm = c(mse.test.3, sqrt(mse.test.3), rae.test.3),
      randomForest = c(mse.test.4, sqrt(mse.test.4), rae.test.4))
    
  } else if (time == "prediction") {
    mse.pred.1 = mean((glmboost.data$timepredict.test.mean - glmboost.pred)^2)
    mse.pred.2 = mean((gamboost.data$timepredict.test.mean - gamboost.pred)^2)
    mse.pred.3 = mean((glm.data$timepredict.test.mean - glm.pred)^2)
    mse.pred.4 = mean((rF.data$timepredict.test.mean - rF.pred)^2)
    
    rae.pred.1 = sum(abs(glmboost.data$timepredict.test.mean - glmboost.pred))/
        sum(abs(glmboost.data$timepredict.test.mean - mean(glmboost.data$timepredict.test.mean)))
    rae.pred.2 = sum(abs(gamboost.data$timepredict.test.mean - gamboost.pred))/
        sum(abs(gamboost.data$timepredict.test.mean - mean(gamboost.data$timepredict.test.mean)))
    rae.pred.3 = sum(abs(glm.data$timepredict.test.mean - glm.pred))/
        sum(abs(glm.data$timepredict.test.mean - mean(glm.data$timepredict.test.mean)))
    rae.pred.4 = sum(abs(rF.data$timepredict.test.mean - rF.pred))/
        sum(abs(rF.data$timepredict.test.mean - mean(rF.data$timepredict.test.mean)))
    
    errors = rbind(glmboost = c(mse.pred.1, sqrt(mse.pred.1), rae.pred.1),
      gamboost = c(mse.pred.2, sqrt(mse.pred.2), rae.pred.2),
      glm= c(mse.pred.3, sqrt(mse.pred.3), rae.pred.3),
      randomForest = c(mse.pred.4, sqrt(mse.pred.4), rae.pred.4))
  }
  colnames(errors) = c("mse", "rmse", "rae")
  errors
}