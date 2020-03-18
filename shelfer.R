library(dplyr)
library(grid)
library(reshape2)
library(pixmap)
library(ggplot2)
source("astar.R")
source("searchmaze.R")

make_df <- function(mat, val) {
  df <- which(mat == val, arr.ind = TRUE) %>%
    as.data.frame() %>%
    transmute(y = row, x = col)
  return(df)
}

make_df_full <- function(mat) {
  df <- setNames(melt(mat), c('y', 'x', 'value'))
}


img <- read.pnm("example.pbm")
img_w = img@size[1]
img_h = img@size[2]

#ffffff = Walls (all non-traversable tiles, including shelves and walls) 
walls_mat <- img@red + img@green + img@blue
walls_mat[which(walls_mat != 3)] <- 0
walls_mat <- walls_mat/3

# Two ways of plotting our matrices

# walls_df_1 <- make_df(walls_mat, 1)

# ggplot(walls_df_1, aes(x=x,y=y)) +
#   geom_tile() +
#   scale_y_continuous(breaks = seq(0, 48, 1), limits = c(0, 48.5), minor_breaks = NULL) +
#   scale_x_continuous(breaks = seq(0, 48, 1), limits = c(0, 48.5), minor_breaks = NULL) +
#   coord_equal() +
#   theme_minimal() +
#   theme(axis.title = element_blank(),
#         axis.text = element_blank())
# 

# walls_df_2 <- make_df_full(walls_mat)

# ggplot(walls_df_2, aes(x=x,y=y,fill=value)) +
#   geom_tile() +
#   scale_y_continuous(breaks = seq(0, 48, 1), limits = c(0, 48.5), minor_breaks = NULL) +
#   scale_x_continuous(breaks = seq(0, 48, 1), limits = c(0, 48.5), minor_breaks = NULL) +
#   coord_equal() +
#   theme_minimal() +
#   theme(axis.title = element_blank(),
#         axis.text = element_blank())

#00ffff = Cashiers (for now, the second target for all agents)
cashier_mat <- img@green + img@blue + img@red
cashier_mat[which(cashier_mat != 2)] <- 0
cashier_mat <- cashier_mat/2

#ff0000 = Entrances (possible source points)
entrance_mat <- img@red - walls_mat

#0000ff = Exits (final target for all agents)
exit_mat <- img@blue - walls_mat - cashier_mat

#################################################################################
#ffff00 = Target objects (for now, the first target for all agents)
## Can be placed in any shelf position (so every wall tile minus the entrance wall)
shelf_mat <- walls_mat
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
rownames(shelf_positions) <- shelf_positions$item_id
# List format is y, x, item_id, GHI
shelf_positions_list <- as.list(as.data.frame(t(shelf_positions)))
####################################################################

# Convert randomised item positions back into a matrix
shelf_mat <- matrix(0, img_w, img_h)
for(i in shelf_positions_list) {
  shelf_mat[i[1], i[2]] <- i[3]
}