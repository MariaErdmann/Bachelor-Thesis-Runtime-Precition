# Bachelor Thesis: Hyperparameter dependent modelling of runtime behaviour of machine learning algorithms
## PART II: Scripts and folders for analyis of runtime.

1. Folder `SingleResults`:
   
   contains all results from each registry/dataset. Results were collected from the lrz cluster and stored here.

   These are the results obtained in step 6 of part I.

2. Folder `ErrorsForEachTask`: 
   
   contains all erroneous results from each registry/dataset. Results were collected from the lrz cluster and stored here.

   These are the results obtained in step 6 of part I.

3. Folder `DataSetForAnalysis`: 
   
   contains all dataframes used for analysing and predicting runtime

4. Folder `AnalysisOnServer`: 
   
   contains all scripts to model runtime with glmboost and gamboost.

   The scripts were not run locally since the grid search for an appropriate mstop exceeds performance of my local machine. For testing purposes the `grid` argument of the `cvrisk`function should be changed to `1:500`.
   
   The scripts were, therefore, run on the server of the statistical departement.
   
   `<classifier's name>_glmboost_train`: glmboost model on training time
   
   `<classifier's name>_glmboost_pred`: glmboost model on prediction time
   
   `<classifier's name>_gamboost_train`: gamboost model on training time
   
   `<classifier's name>_gamboost_pred`: gamboost model on prediction time
   
   Model results are stored in folder `ServerResults`
   
   Scripts load the dataframes from folder `DataSetForAnalysis`
   
   Scripts source formulas.R, which contains the formulas used in the glmboost/gamboost function.

5. Foler `ServerResults`: 
   
   model results of classifier naiveBayes for the glmboost and gamboost model. The remaining results could not be loaded into the repository, since most results are bigger than 25 MB.

   contains the result of the search for the optimal number of boosting iterations (result of function `cvrisk())` for all models (`cvm<classifier's name>_<model name>_<pred for prediction or test for training>`)

6. `formulas.R`: 
   
   Script which produces formulas, that are used for modelling runtime. 
   Script that is sourced by `createDataframesForPred.R`, since here the numeric predictors for the models for each target classifier are defined. The numeric predictors are needed in `reateDataframesForPred.R`, when centering the numeric predictors. 
   
   This script sources `helperFunctions.R`

7. `helperFunctions.R`: 
   
   contains several heplerFunctions

   `createNewJobIdForReplications`: creates a new job id that serves as a helper to summarise the ten replications of one experiment
   
   `CreateDataFrameWithMeans`: created the dataframe with aggregated runtime results. With the help of the new job id, the average runtime of the ten replications (or less in case of erroneous jobs) is calculated and merge to the respective learner and it's hyperparameters.
   
   `getTimeSummary`, `getFeautureSummary`, `getFeautureSummaryTable`: helper to get a quik look on the data. Somehow, depricated after discovering plyr.
   
   `createGlmboostFormula`, `createGamboostFormula`: creates the formulas used in the glmboost and gamboost models
   grid_arrange_shared_legend: function to generate two plots side by side with one shared legend. Function taken from 
   [http://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots]
   
   `multiplot`: function to generate multiple plots at once. Function taken from [http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/]
   
   `createSinglePartialEffectPlots`: creates partial effect plots for the linear or the smooth base-learner of gamboost with ggplot2.
   
   `createCombinedPartialEffectPlots`: creates combined partial effect plots with ggplot2, where functional estimates of the linear and the smooth base-learner are summed
   
   `createSinglePartialEffectPlots` and `createCombinedPartialEffectPlots` are the ggplot2 version of `plot(<gamboost-model-object>)`
   
   `createPartialEffectsForGlm`: creates effect plots for the sum of the functional estimates for each covariate
   
   `calculateErrorsForComparison`: calculates the RMSE and the RAE of all regression models for all target classifiers. 

8. `ExplorativeAnalysis.R`

   Sources `helperFunctions.R` and `formulas.R`
   
   Loads `finalresult.RData`, `ResultsDDesc.RData` and `class.dsets.RData`	
   
   Creates summaries on runtime and mean misclassification error
   
   Histograms with kernel density estimator for runtime and mmce
   
   Summaries on runtime and featueres of the datasets used for modelling runtime for each target classifier
   
   Analysis of variability with variance and the coefficient of variation

9. `gbmAnalysis.R`, `glmnetAnalysis.R`, `naiveBayesAnalysis.R`, `nnetAnalysis.R`, `rangerAnalysis.R`, `rpartAnalysis.R`

   If the regression model is big, results of the analysis need to be buffered. Otherwise, R crashes.
   
   Since each script sources the glmboost and gamboost results from folder `ServerResults`, only`naiveBayesAnalysis.R` can be run, since the model results for naiveBayes are uploaded. Running the other analysis files requires to run the scripts creating the corresponding glmboost and gamboost models in `AnalysisOnServer`.
   
   Each script contains the analysis of the regression models of the respective target classifier
   
   Structure is the same for all scripts:
   
   a) Results for models on training time
   
   b) Results for models on prediction time
   
   Models are glmboost, gamboost, glm and randomForest
   
   Model results for glmboost include: mstop, coefficient paths, estimated coefficients, excluded variables, predictions, partial effect plots with ggplot2
   
   Model results for gamboost include: mstop, estimated coefficients of the linear components, excluded variables, predictions, partial effect plots with ggplot2
   
   Model results for glm: Excluded varaibles, estimated coefficients, predictions
   
   Model results for randomForest: error plot, variable importance, partial dependence plot
   
   Additional results: Dataframe containing all estimated coefficients of glm and glmboost on training and prediction time, dataframes containing RMSE and RAE
   
   for all regression models, plot obs vs. fitted
  
   
