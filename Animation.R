source("simulate.R", local=TRUE)
source("shelfer.R", local=TRUE)
n_agents = 100

# Default Layout, Euclidean cashier loss
default_store_layout <- read_img_map("store_layout/bitmap/original_layout_blocked.pbm")
store_layout_1 <- place_items(default_store_layout, storedata, select_positions_random, order_positions_euclidean, order_items_ascending_ghi, threshold=0.9, x=15, y=44)
result_1 <- simulate(store_layout = store_layout_1, storedata=storedata, n_agents = n_agents, plot=TRUE, pixmap=TRUE, from_bitmap = FALSE, name = "default_euclidean_cashier_loss")
save_mat(store_layout_1$walls_mat, name="walls_default_euclidean_cashier_loss")
save_mat(store_layout_1$cashier_mat, name="cashier_default_euclidean_cashier_loss")

# Arranged Layout, Euclidean cashier loss
store_layout_2 <- read_img_map("store_layout/ideas/derek_3.pbm")
plot_mat(store_layout_2["walls_mat"])
store_layout_2 <- place_items(store_layout_2, storedata, select_positions_random, order_positions_euclidean, order_items_ascending_ghi, threshold=0.9, x=46, y=44)
result_2 <- simulate(store_layout = store_layout_2, storedata=storedata, n_agents = n_agents, plot=TRUE, pixmap=TRUE, from_bitmap = FALSE, name = "arranged_euclidean_cashier_loss")
save_mat(store_layout_2$walls_mat, name="walls_arranged_euclidean_cashier_loss")
save_mat(store_layout_2$cashier_mat, name="cashier_arranged_euclidean_cashier_loss")

# Optimised Layout, Euclidean cashier loss
store_layout_3 <- read_img_map("store_layout/ideas/tham_3.pbm")
plot_mat(store_layout_3["walls_mat"])
store_layout_3 <- place_items(store_layout_3, storedata, select_positions_random, order_positions_euclidean, order_items_ascending_ghi, threshold=0.9, x=41, y=44)
result_3 <- simulate(store_layout = store_layout_3, storedata=storedata, n_agents = n_agents, plot=TRUE, pixmap=TRUE, from_bitmap = FALSE, name = "optimised_euclidean_cashier_loss")
save_mat(store_layout_3$walls_mat, name="walls_arranged_optimised_cashier_loss")
save_mat(store_layout_3$cashier_mat, name="cashier_arranged_optimised_cashier_loss")

