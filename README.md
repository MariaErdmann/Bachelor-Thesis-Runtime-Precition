# Bachelor Thesis: Runtime Prediction
Modelling and prediction of runtime behaviour of machine learning algorithms with respect to dataset size, number of features and algorithm's hyperparameter setting



1. didsForOMLDataSets(t.type)

  Returns a vector with the dataset ids (called "dids" in OpenML) of all regression or classification datasets considered for the analysis of running time/ for this bachelor thesis.


2. GetDataSetList(dids)

  Returns an object of type data.frame with information about the qualified regression or classification datasets. The data.frame contains information about the dataset id  (=did), the name of the dataset (=name), the number of instances, features in total, numeric features, symbolic features, and missing values.


3. getTaskForHPS (data.did, t.type)

  Returns a task object  from the package "mlr".

  Parameters
  Data.did: an integer value containing the dataset id (= did)
  t.type: a string. Possible values are either "Supervised Regression" or "Supervised Classification". Note: Make sure to commit a did from the vector of regression dids if you choose "Supervised Regression" and a did of the vector of classification dids for "Supervised Classification"


4. Depending on what we decide on

  a) BatchExperiments_Runtime_test3.R
    uses BatchExperiments as basis
    Advantages: nice output (exactly the table I want to have)
    Disadvantage: each algorithm needs to be run on each task seperately, which means having 500 classification tasks and ? (number of classification algos to be analysed) classification algorithms 500 * ? for classification and 200 * ? (number of regression algos to be analysed)
  
  b) Create_Learner_List_For_One_Task.R
    uses batchtools as basis
    Advantages: when param.set shall be dependend on the task, all algorithms could be run on one task at once --> 500 experiments for classification and 200 experiments for regression. If param.set shall not be dependend on the task, only one experiment for classification and one for regression needed
    Disadvantages: Currently it is only possible to get the time measures and the model as a list. Result as table absolutely necessary

5. getResults()
  Currently contains both results calls for BatchExperiment program and batchtools program

