source("simulate.R", local=TRUE)
source("shelfer.R", local=TRUE)
# Organised Layout Experimentation
n_reps=5
n_agents=500
# Derek, Layout 1, Random Item Placement
derek_layout_1 <- read_img_map("store_layout/ideas/derek_1.pbm")
derek_layout_1 <- place_items(derek_layout_1, storedata, select_positions_random, order_positions_ascending_y, order_items_random)
derek_layout_1_results = list()
for(i in 1:n_reps) {
  derek_layout_1_results[[i]] <- simulate(store_layout = derek_layout_1, storedata=storedata, n_agents = n_agents, plot=FALSE, from_bitmap = FALSE)
}
# Save those results in an .rds file for later use
saveRDS(derek_layout_1_results, "derek_layout_1_results")
derek_layout_1_results <- readRDS("derek_layout_1_results")


for(i in derek_layout_1_results) {
  i[[2]] <-get_loss_mat(storedata, i[[1]], derek_layout_1$target_df, n_agents)
}

plot_df(derek_layout_1$target_df)
plot_mat(derek_layout_1_results[[1]][[1]])
test <- get_loss_mat(storedata, derek_layout_1_results[[1]][[1]], derek_layout_1$target_df, n_agents)
plot_mat(test)
