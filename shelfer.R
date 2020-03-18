#ffff00 = Target objects (for now, the first target for all agents)
## Can be placed in any shelf position (so every wall tile minus the entrance wall)
shelf_mat <- store_layout$walls_mat
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
store_layout$target_df<-shelf_positions
