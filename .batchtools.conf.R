cluster.functions = batchtools::makeClusterFunctionsSLURM("~/lmu_lrz_new.tmpl", clusters = 
    "serial")
default.resources = list(walltime = 1000L, memory = 2200L)

debug = TRUE
