# check if there are any time measurements missing and if meausurements are reasonable
anyNA(res$timetrain.test.mean)
anyNA(res$timepredict.test.mean)
anyNA(res$timeboth.test.mean)
summary(res$timetrain.test.mean)
summary(res$timepredict.test.mean)
summary(res$timeboth.test.mean)

