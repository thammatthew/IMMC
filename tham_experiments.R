source("simulate.R", local=TRUE)
n_reps = 5

# Baseline: completely randomised arrangement of items
results = list()

for(i in 1:n_reps) {
  results[[i]] <- simulate("example.pbm", storedata=storedata, n_agents = 100, plot=FALSE)
}

# Get the store_layout for editing
store_layout <- read_img_map("example.pbm")
store_layout_ascending <- store_layout
shelf_mat <- store_layout_ascending$walls_mat
# Zero out the entrance wall
shelf_mat[nrow(shelf_mat),] <- 0
# Convert the available spots into df format for sampling
shelf_df <- make_df(shelf_mat, 1)
shelf_positions <- shelf_df[sample(nrow(shelf_df), 134),]
shelf_positions<-shelf_positions[order(shelf_positions$y),]
#Sort Item catalog by happiness
storedata_sorted<-storedata[order(storedata$ghi),]
shelf_positions$item_id <- storedata_sorted$item_id
shelf_positions$ghi <- storedata_sorted$ghi
shelf_positions <- shelf_positions[order(shelf_positions$item_id),]
store_layout_ascending$target_df<-shelf_positions

results_ascending = list()
for(i in 1:n_reps) {
  results_ascending[i] <- simulate(store_layout=store_layout, storedata=storedata, n_agents = 100, plot=FALSE, from_bitmap = FALSE)
}