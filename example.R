source("simulate.R")
example <- simulate("example.pbm", storedata, n_agents = 5, plot=TRUE)
plot_mat(example[[1]])
print(example[[2]])
