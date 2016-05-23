# Produces an overview about the datasets used for analysis
source("C:\\Users\\Maria\\Documents\\Studium\\Statistik\\Bachelorarbeit\\didsForOMLDataSets.R")
regression.dids = didsForOMLDataSets(t.type = "Supervised Regression")
classification.dids = didsForOMLDataSets(t.type = "Supervised Classification")

getDataSetList = function(dids) {

  dats = listOMLDataSets()
  sub.dats = data.frame()
  for (i in 1:length(dids)){
    sub.dats = rbind(sub.dats, subset(dats, did == dids[i]))
  }
  # Reduce dataset to columns needed
  sub.dats = sub.dats[,c("did", "name", "NumberOfInstances", "NumberOfFeatures",
  "NumberOfNumericFeatures", "NumberOfSymbolicFeatures")]
}

regression.dsets = getDataSetList(regression.dids)
classification.dsets = getDataSetList(classification.dids)


# print(xtable(classification.dsets),
#   type = "latex",
#   file = "",
#   append = FALSE,
#   floating = FALSE,
#   caption.placement = "top",
#   tabular.environment = "longtable",
#   include.rownames = FALSE,
#   only.contents = FALSE,
#   add.to.row = NULL,
#   print.results = TRUE,
#   booktabs = TRUE
#   )