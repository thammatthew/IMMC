source("simulate.R", local=TRUE)
source("shelfer.R", local=TRUE)

n_reps = 10
n_agents = 100

# Pull a store_layout from an image
store_layout <- read_img_map("store_layout/bitmap/original_layout_blocked.pbm")

# Baseline: completely randomised arrangement of items
# Place items into the layout
store_layout_random <- place_items(store_layout, storedata, select_positions_static, order_positions_euclidean, order_items_ascending_ghi,threshold=0.9,x=23,y=38)

# Create a blank list to store results
results = list()
# Run simulation for 5 reps
for(i in 1:n_reps) {
  results[[i]] <- simulate(store_layout = store_layout_random, storedata=storedata, n_agents = n_agents, plot=FALSE, from_bitmap = FALSE)
}


#save
output <- list(store_layout_random, results)
saveRDS(output, "default_euc_cashier_0.75.rds")
plot_output(output, "default_euc_cashier_0.75")



saveRDS(store_layout,"storelayout.rds")
plot_mat(store_layout$blocked_mat)
