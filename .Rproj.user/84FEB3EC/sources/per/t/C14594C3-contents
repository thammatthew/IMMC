library(dplyr)
library(grid)
library(reshape2)
library(pixmap)
library(ggplot2)
source("/Users/matthewtham/Desktop/IMMC/IMMC/astar.R")
source("/Users/matthewtham/Desktop/IMMC/IMMC/examples.R")

# Instantiate search environment from a bitmap (cheap way of dealing with a matrix)
img <- read.pnm("test.pbm")
img_w = img@size[1]
img_h = img@size[2]

# Void = #000000, Walls = #FFFFFF, Source = #00FF00 (Green), Target = #0000FF (Red)
walls_mat <- matrix(NA, img_w, img_h)
walls_mat <- img@blue
source_mat <- matrix(NA, img_w, img_h)
source_mat <- img@green - img@blue
target_mat <- matrix(NA, img_w, img_h)
target_mat <- img@red - img@blue

make_df <- function(mat) {
  df <- which(mat == 1, arr.ind = TRUE) %>%
    as.data.frame() %>%
    transmute(y = row, x = col)
  return(df)
}

source = make_df(source_mat)
target = make_df(target_mat)

# Base density matrix has a density of 0 at each point
density_mat = matrix(0, img_w, img_h)
coeff = 0.001

start_time <- Sys.time()
for(i in 1:100) {
  print(i)
  mg <- SearchMaze2D$new(walls_mat, density_mat, coeff)
  goal_path <- mg$run(as.numeric(source), as.numeric(target))
  for(j in goal_path) {
    density_mat[j[1], j[2]] <- density_mat[j[1], j[2]] + 1
  }
}
end_time <- Sys.time()
print(end_time-start_time)
# For coeff = 0.001, Time difference of 23.10066 secs
# For coeff = 0.1, Time difference of 1.213553 mins
# For coeff = 1, Time difference of 1.50853 mins

keypoint_mat <- (source_mat + target_mat)*max(density_mat)
density_mat_cleared <- density_mat - keypoint_mat

density_df = setNames(melt(density_mat_cleared), c('y', 'x', 'density'))
ggplot(density_df, aes(x=x,y=y,fill=density)) +
  geom_tile() +
  scale_y_continuous(breaks = seq(0, 48, 1), limits = c(0, 48.5), minor_breaks = NULL) +
  scale_x_continuous(breaks = seq(0, 48, 1), limits = c(0, 48.5), minor_breaks = NULL) +
  coord_equal() +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank())


