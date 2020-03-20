source('simulate.R',local=T)
# Get the store_layout for editing
store_layout <- read_img_map("example.pbm")

# Core place_items function; no need to edit this code
place_items <- function(store_layout, storedata, select_positions, order_positions, order_items) {
  walls_mat <- store_layout$walls_mat
  blocked_mat <- store_layout$blocked_mat
  shelf_mat <- walls_mat-blocked_mat
  # Zero out the entrance wall
  # Convert the available spots into df format for sampling
  shelf_df <- make_df(shelf_mat, 1)
  # Select 134 shelf positions from possible positions
  shelf_positions <- select_positions(shelf_df)
  # Insert an item into each shelf position
  ## Sort positions by increasing y position
  shelf_positions<-order_positions(shelf_positions)
  ## Sort items by increasing GHI
  storedata_sorted<-order_items(storedata)
  ## Bind positions to items
  shelf_positions$value <- storedata_sorted$item_id
  shelf_positions$ghi <- storedata_sorted$ghi
  shelf_positions <- shelf_positions[order(shelf_positions$value),]
  store_layout$target_df<-shelf_positions
  return(store_layout)
}

# Some example selection/ordering functions (you'll have to write more of these yourself)

# A select_position function takes in a shelf_df, and selects 134 positions from it somehow
select_positions_random <- function(shelf_df) {
  shelf_positions <- shelf_df[sample(nrow(shelf_df), 134),]
  return(shelf_positions)
}

select_positions_adaptive <- function(shelf_df){
  decay<-5
  items<-134
  tally<-0
  shelf_positions<-NULL
  for (i in 1:48){
    n<-48-i
    s<-shelf_df[which(shelfdf$y==n),]
    shelf_temp<-s$x%%decay==0
    tally<-tally+nrow(shelf_temp)
    if ((items-tally<items/10)&(decay>1)){
      decay=decay-1
      tally=0
    }
    items=items-nrow(shelf_temp)
    shelf_positions<-cbind(shelf_positions,shelf_temp)
  }
  return(shelf_positions)
}
# An order_position function takes in a shelf_positions df with 134 rows, and orders them somehow
order_positions_ascending_y <- function(shelf_positions) {
  shelf_positions<-shelf_positions[order(shelf_positions$y),]
  return(shelf_positions)
}

order_positions_descending_y <- function(shelf_positions) {
  shelf_positions<-shelf_positions[order(-shelf_positions$y),]
  return(shelf_positions)
}

order_positions_euclidean<-function(shelf_positions,x,y){
  shelf_positions$euclidean<-(shelf_positions$x-x)^2+(shelf_positions$y-y)^2
  shelf_positions<-shelf_positions[order(shelf_positions$euclidean,)]
  shelf_positions$euclidean<-NULL
  return(shelf_positions)
}
# An order_items function sorts store_data based on some attribute (e.g. ghi, fragility, wtvr else)
order_items_ascending_ghi <- function(storedata) {
  storedata_sorted<-storedata[order(storedata$ghi),]
  storedata+sorted<-storedata[order(storedata$dpmt),]
  return(storedata_sorted)
}

order_items_random <- function(storedata) {
  storedata_sorted <- storedata[sample(storedata$item_id),]
  storedata+sorted<-storedata[order(storedata$dpmt),]
}
## An example call to place_items(): 
## Concept is that you tell place_items() how to select shelves from the walls (select_positions()), how to order those positions according to coordinates (order_positions()), and how to insert item_ids into the sorted coordinates (order_items())

# test_store_layout <- place_items(store_layout, storedata, select_positions_random, order_positions_descending_y, order_items_ascending_ghi)

## After creating a new store layout, pass it to the simulate function to get your loss. 

# n_reps = 5
# results_descending = list()
# for(i in 1:n_reps) {
#   results_descending[[i]] <- simulate(store_layout = store_layout_descending, storedata=storedata, n_agents = 100, plot=FALSE, from_bitmap = FALSE)
# }

## NOTE: PLEASE RUN YOUR EXPERIMENTS IN A SEPARATE FILE TO AVOID DELETING CORE FUNCTIONS. LOOK INTO tham_experiments.R TO SEE HOW YOU CAN ORGANISE EXPERIMENTS

# Either add new selection/ordering functions in this file, and push to the git, or write them in your own experiments file
