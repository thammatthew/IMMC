library(gganimate)
library(dplyr)
library(png)
library(grid)
library(gganimate)
library(gifski)
library(pixmap)
library(reshape2)
source("astar.R")
source("searchmaze.R")
source("storedata.R")

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

# Base density matrix has a density of 1 at each point
density_mat = matrix(1, img_w, img_h) 

# Helper function to convert a matrix into a dataframe where each row refers to the coordinates of a tile to be plotted
make_df <- function(mat) {
  df <- which(mat == 1, arr.ind = TRUE) %>%
    as.data.frame() %>%
    transmute(y = row, x = col)
  return(df)
}

# Apply make_df to our walls, source and target in advance
walls = make_df(walls_mat)
source = make_df(source_mat)
target = make_df(target_mat)

# Run astar
mg <- SearchMaze2D$new(walls_mat, density_mat)
goal_path <- mg$run(as.numeric(source), as.numeric(target))

# Plot the route
path_df <- as.data.frame(t(as.matrix(as.data.frame(goal_path))))
colnames(path_df) = c("y", "x")
ggplot(walls, aes(x,y)) +
  geom_tile(width = 1, height = 1, fill = "#9E9E9E") +
  scale_y_continuous(breaks = seq(0, 48, 1), limits = c(0, 48.5), minor_breaks = NULL) +
  scale_x_continuous(breaks = seq(0, 48, 1), limits = c(0, 48.5), minor_breaks = NULL) +
  coord_equal() +
  geom_tile(data=target, fill="#4CAF50") +
  geom_tile(data=source, fill="#F44336") +
  geom_point(data=path_df) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank())

# Update the density map based on the most recently plotted route
for(i in goal_path) {
  density_mat[i[1], i[2]] <- density_mat[i[1], i[2]] + 1
}


test = setNames(melt(density_mat), c('y', 'x', 'density'))

ggplot(test, aes(x=x,y=y,fill=density)) +
  geom_tile() +
  scale_y_continuous(breaks = seq(0, 48, 1), limits = c(0, 48.5), minor_breaks = NULL) +
  scale_x_continuous(breaks = seq(0, 48, 1), limits = c(0, 48.5), minor_breaks = NULL) +
  coord_equal() +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank())



