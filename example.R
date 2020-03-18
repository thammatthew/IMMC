source("simulate.R", local=TRUE)
example <- simulate("example.pbm", storedata=storedata, n_agents = 100, plot=TRUE)
plot_mat(example[[1]])
plot_mat(example[[2]])
print(example[[3]])

store_layout <- read_img_map("example.pbm")
plot_mat(store_layout$walls_mat)
plot_df(store_layout$target_df)
