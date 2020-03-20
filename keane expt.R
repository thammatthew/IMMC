source("simulate.R", local=TRUE)
source("shelfer.R", local=TRUE)

n_reps = 10
n_agents = 100

# Pull a store_layout from an image
store_layout <- read_img_map("store_layout/ideas/min_1.pbm")

# Baseline: completely randomised arrangement of items
# Place items into the layout
store_layout_random <- place_items(store_layout, storedata, select_positions_random, order_positions_euclidean, order_items_ascending_ghi,threshold=0.9,x=4,y=35)

# Create a blank list to store results
results = list()
# Run simulation for 5 reps
for(i in 1:n_reps) {
  results[[i]] <- simulate(store_layout = store_layout_random, storedata=storedata, n_agents = n_agents, plot=FALSE, from_bitmap = FALSE)
}


#save
output <- list(store_layout_random, results)
saveRDS(output, "default_euc_cashier_0.9_static.rds")
plot_output(output, "default_euc_cashier_0.0_static")



saveRDS(store_layout,"storelayout.rds")
plot_df(store_layout$target_df)
