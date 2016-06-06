# Bachelor Thesis: Runtime Prediction
Modelling and prediction of runtime behaviour of machine learning algorithms with respect to dataset size, number of features and algorithm's hyperparameter setting



1. definitions.R

Learner parameter settings, respectively the ranges of the parameters used to create a parameter set, are defined.
It contains following learners:
classif.ranger
classif.rpart
classif.gbm
classif.svm (this is not yet working probabaly. Throws error it shouldn't throw)
classif.naiveBayes
classif.glmnet
Not yet implented: classif.nnet

this script is sourced by experiments.R and requires class.dsets.RData


2. experiments

Batchtools set up. Creates a job for every learner with a random param set on each task.

3. .batchtools_conf.R

only needed when working on  cluster

4. lmu_lrz_new.tmpl

only needed when working on cluster



didsForOMLDataSets2. R creates class.dsets.RData and is still in progress but for testing it works
getDataSetList.R creates a list with characteristics of the datasets used in experiments.R. I will use this as a table in my thesis.
