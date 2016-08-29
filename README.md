# Bachelor Thesis: Hyperparameter dependent modelling of runtime behaviour of machine learning algorithms
## PART I: 

* Scripts for creating dataframe that contains runtime measurements for all target classifiers with their different hyperparameter settings on all datasets.
* Script for creating dataframes that are used for modelling runtime.


1. `datasets.R`
   
   * creates a `class.dsets.RData` dataframe with the datasets that are analysed. Dataframe includes dataset characteristics and the dataset id (did)
   * creates `dset.with.factors.RData`, a vector, which indicates whether the target of the dataset is a factor or not. For analysis, only datasets where the target is a factor are used

2. `definitions.R`

  Definition of learner's parameter settings, respectively the ranges of the parameters used to create a parameter set.
  It contains following learners:
  * classif.ranger
  * classif.rpart
  * classif.gbm
  * classif.naiveBayes
  * classif.glmnet
  * classif.nnet

  This script is sourced by `experiments.R` and requires `class.dsets.RData`


3. `experiments.R`

  Batchtools set up. Creates a job for every learner with a random param set on each dataset.
  Creates a registry for each dataset. Therfore, the dataset needs to be passed/changed manually in `definitions.R`.
  
4. `.batchtools_conf.R`

  only needed when working on  cluster
  

5. `lmu_lrz_new.tmpl`

   only needed when working on cluster

6. `checkSingleResults.R`

   * After running all learners on a particular dataset, results where checked, running this script on the cluster.
   * Creates `singleres.RData`, a dataframe containing all results of one registry (= dataset), that were not erroneous 
   or were expired. Contains information on the learner, it's hyperparamters, the runtime, and some more information 
   about the computational execution of the job.
   * Creates `errorframe.RData`, a dataframe containing all jobs, that were erroneous. Contains information on the learner and
   it's hyperparameters, the error message and some more information about the computational execution of the job.
   * Needs to be run/ was run on the cluster.
   * The respective results (dataframes) are available here: `~\Bachelor-Thesis-Runtime-Prediction\Results\SingleResults` and here
   `~\Bachelor-Thesis-Runtime-Prediction\Results\ErrorsForEachTask`

7. `createResultDataframe.R`

   * After running all learners and checking each registry individually, this script is run to assemble all results from all registries in  `~/Results/finalresult.RData`. `finalresult.RData` contains information on the learner, it's hyperparamters, the runtime, and some more information about the computational execution of the job. Furthermore, it assembles all erroroneous jobs from all registries in `~/Results/allerrors.RData`. `allerrors.RData` contains information on the learner and
   it's hyperparameters, the error message and some more information about the computational execution of the job.
   * This script needs to be run/ was run on the cluster, and the results were collected for further work on the local machine.

8. `createExceededData.R`

   * Creates `expired.RData`, a dataframe containing information about the jobs, that were expired. Contains information on the
   learner and it's hyperparameter, information about, whether the job expired due to exceeding the time limit or the
   memory limit, and some more information about the computational execution of the job.
   * This script first needs to be run/ was run on the cluster, and the results were collected for further work on the local machine.
   Afterwards, the last part of the script is run locally, which takes `expired.RData` and modifies it a little bit in order to get
   0/1 coding for the varaibles containing information about whether the job expired due to exceeding the time limit or the
   memory limit. It finally creates `only_expired_jobs.RData`

9. `plausibilityCheck.R`

   Script to check whether the final result dataframe, `finalresult.RData` (object called "big.res"), has no missing values in runtimes
   and whether values are reasonable.

10. `createDataframesForPred.R`

   * Creates new variable ObsForTrain, which is NumberOfInstances multiplied with sub.sample.frac. ObsForTrain then corresponds
   to the actual number of instances the learner has been trained on.
   * Creates new variable ObsForPred, which is NumberOfInstances multiplied with (1 - sub.sample.frac). ObsForPred then corresponds to the actual number of instances, for which the learner made predictions.
   * Splits the big.res dataframe into six dataframes, whereat each corresponds to one target classifier.  
   * Complements the individual datasets for each classifier with values for default settings.
   * Transforms some hyperparameters, which values are dependent on dataset properties and which were therefore transformed by some function in `definitions.R`. Transfromation is needed, since the "*raw-values*" of the hyperparameters were stored (= values before functions for transforming the values were applied).
   
   * Creates all dataframes that are used for analysis and runtime prediction:
   * All dataframes are stored in the following subfolder: `~/Bachelor-Thesis-Runtime-Prediction/Results/DatasetsForAnalysis/`
   * `ResultsDDesc.RData`: contains all observations, only columns of interest are kept.
   * `<classifier's name>df.RData`: contains all runtime results and hyperparameters for the classifier
   * `<classifier's name>_jobid2.RData`: same as rangerdf.RData, just with an additional jobid, which is used to summaries the 10 replications of each experiment.
   * `<classifier's name>dfmean.RData`: contains aggregated runtime results. Thus, it contains the runtime results after averaging across the ten replications of one experiments. This dataframe is used for runtime prediction with glmboost, glm and randomForest.
   * `<classifier's name>dfgam_test.RData`: contains aggreagated runtime results. In this dataframe the numeric variables are centered. This dataframe is used for prediction of training time with gamboost.
   * `<classifier's name>dfgam_pred.RData`: contains aggreagated runtime results. In this dataframe the numeric variables are centered. This dataframe is used for prediction of prediction time with gamboost.
   * `<classifier's name>dfdefault.RData`: contains runtime results on the default hyperparameter settings.

   
   


