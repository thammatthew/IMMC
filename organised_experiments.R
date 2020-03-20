source("simulate.R", local=TRUE)
source("shelfer.R", local=TRUE)

n_reps = 10
n_agents = 100

store_layout <- read_img_map("store_layout/ideas/derek_3.pbm")
store_layout_ascending <- place_items(store_layout, storedata, select_positions_random, order_positions_ascending_y, order_items_ascending_ghi)

results = list()
# Run simulation for 5 reps
for(i in 1:n_reps) {
  print(paste("Simulating replicate",i))
  results[[i]] <- simulate(store_layout = store_layout_ascending, storedata=storedata, n_agents = n_agents, plot=FALSE, from_bitmap = FALSE)
}

output <- list(store_layout_ascending, results)
saveRDS(output, "derek_layout_3_ascending.rds")
