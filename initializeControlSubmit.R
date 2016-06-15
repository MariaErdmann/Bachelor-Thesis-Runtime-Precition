# Create a single value vector, which value is the end of the ids submitted in
# one call of controlSubmit
# Initializing it with zero
seqend = c()
seqend = append(seqend, 0)
save(seqend, file = "~/code/seqend.RData")
# save(seqend, file = "C:/Users/Maria/Documents/Studium/Statistik/Bachelorarbeit/Bachelor-Thesis-Runtime-Prediction/seqend.RData")

# Creating a vector, that helps to keep track of how many times I submitted Jobs
# controlSubit is created in such a way, that when running it, all job ids for
# one task are submitted. So if data.set.count's last element (which corresponds
# to 'count') is i.e. 4, then jobs for the 4th element of OMLDATASETS are
# submitted, hence the jobs for the fourth data set.
data.set.count = c()
count = 0
save(count, file = "~/code/count.RData")
#save(count, file = "C:/Users/Maria/Documents/Studium/Statistik/Bachelorarbeit/Bachelor-Thesis-Runtime-Prediction/count.RData")
data.set.count = append(data.set.count, count)
save(data.set.count, file = "~/code/datasetcount.RData")
#save(data.set.count, file = "C:/Users/Maria/Documents/Studium/Statistik/Bachelorarbeit/Bachelor-Thesis-Runtime-Prediction/datasetcount.RData")
