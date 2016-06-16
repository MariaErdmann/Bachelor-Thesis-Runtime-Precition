test = 1 # for testing purposes

# Create a single value vector, which value is the end of the ids submitted in
# one call of controlSubmit
# Initializing it with zero
seqend = c()
seqend = append(seqend, 0)

# Creating a vector, that helps to keep track of how many times I submitted Jobs
# controlSubit is created in such a way, that when running it, all job ids for
# one task are submitted. So if data.set.count's last element (which corresponds
# to 'count') is i.e. 4, then jobs for the 4th element of OMLDATASETS are
# submitted, hence the jobs for the fourth data set.
data.set.count = c()
count = 0
data.set.count = append(data.set.count, count)

if (test == 1) {
  save(seqend, file = "C:/Users/Maria/Documents/Studium/Statistik/Bachelorarbeit/Bachelor-Thesis-Runtime-Prediction/seqend.RData")
  save(count, file = "C:/Users/Maria/Documents/Studium/Statistik/Bachelorarbeit/Bachelor-Thesis-Runtime-Prediction/count.RData")
  save(data.set.count, file = "C:/Users/Maria/Documents/Studium/Statistik/Bachelorarbeit/Bachelor-Thesis-Runtime-Prediction/datasetcount.RData")
} else {
 save(seqend, file = "~/code/seqend.RData")  
 save(count, file = "~/code/count.RData")
 save(data.set.count, file = "~/code/datasetcount.RData")
}