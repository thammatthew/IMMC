library(dplyr)
library(grid)
library(reshape2)
library(pixmap)
library(ggplot2)
source("astar.R")
source("examples.R")

make_df <- function(mat, val) {
  df <- which(mat == val, arr.ind = TRUE) %>%
    as.data.frame() %>%
    transmute(y = row, x = col)
  return(df)
}

make_df_full <- function(mat) {
  df <- setNames(melt(mat), c('y', 'x', 'value'))
}

read_img_map <- function(img_path) {
  img <- read.pnm(img_path)
  img_w = img@size[1]
  img_h = img@size[2]
  
  #ffffff = Walls (all non-traversable tiles, including shelves and walls) 
  walls_mat <- img@red + img@green + img@blue
  walls_mat[which(walls_mat != 3)] <- 0
  walls_mat <- walls_mat/3
  
  #00ffff = Cashiers (for now, the second target for all agents)
  cashier_mat <- img@green + img@blue + img@red
  cashier_mat[which(cashier_mat != 2)] <- 0
  cashier_mat <- cashier_mat/2
  
  #ff0000 = Entrances (possible source points)
  entrance_mat <- img@red - walls_mat
  
  #0000ff = Exits (final target for all agents)
  exit_mat <- img@blue - walls_mat - cashier_mat
  
  #00xx00 = Target objects (for now, the first target for all agents)
  # @Keane: Encode target objects by converting their numerical item_id to hex, then setting that to be the colour of the green channel
  
  
}