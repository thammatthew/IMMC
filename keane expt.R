source("simulate.R", local=TRUE)
source("shelfer.R", local=TRUE)

n_reps = 10
n_agents = 100

# Pull a store_layout from an image
store_layout <- read_img_map("store_layout/bitmap/original_layout_blocked.pbm")

# Baseline: completely randomised arrangement of items
# Place items into the layout
store_layout_random <- place_items(store_layout, storedata, select_positions_random, order_positions_euclidean, order_items_ascending_ghi,threshold=0,x=35,y=48)

# Create a blank list to store results
results = list()
# Run simulation for 5 reps
for(i in 1:n_reps) {
  results[[i]] <- simulate(store_layout = store_layout_random, storedata=storedata, n_agents = n_agents, plot=FALSE, from_bitmap = FALSE)
}
# Save those results in an .rds file for later use
saveRDS(results, "default_euc_entrance.rds")


# Ascending (i.e. more desirable items farther away)
store_layout_ascending <- place_items(store_layout, storedata, select_positions_random, order_positions_ascending_y, order_items_ascending_ghi)
results_ascending = list()
for(i in 1:n_reps) {
  results_ascending[[i]] <- simulate(store_layout = store_layout_ascending, storedata=storedata, n_agents = n_agents, plot=FALSE, from_bitmap = FALSE)
}
saveRDS(results_ascending, "default_ascending.rds")

# Descending
store_layout_descending <- place_items(store_layout, storedata, select_positions_random, order_positions_descending_y, order_items_ascending_ghi)
results_descending = list()
for(i in 1:n_reps) {
  results_descending[[i]] <- simulate(store_layout = store_layout_descending, storedata=storedata, n_agents = n_agents, plot=FALSE, from_bitmap = FALSE)
}
saveRDS(results_descending, file="default_descending.rds")
output <- list(store_layout_random, results)
plot_output(output, "default_descending")
plot_mat(results[1][1])


