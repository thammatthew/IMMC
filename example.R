source("simulate.R")
example <- simulate("example.pbm", storedata, n_agents = 5, plot=TRUE)
plot_mat(example[[1]])
print(example[[2]])

store_layout <- read_img_map("example.pbm")
plot_mat(store_layout$walls_mat)
