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
  # convert back to 0-255 encoding
  target_mat <- img@green*255
  target_mat[which(target_mat == 255)] <- 0
  target_df <- make_df_full(target_mat)
  target_df <- target_df[which(target_df$value != 0),]
  target_df <- target_df[order(target_df$item_id),]
  target_df$ghi <- storedata$ghi
  
  store_layout <- list("walls_mat" = walls_mat, "cashier_mat" = cashier_mat, "entrance_mat" = entrance_mat, "exit_mat" = exit_mat, "target_df" = target_df)
  
  return(store_layout)
}

create_agent_list <- function(store_layout, n_agents) {
  walls_df <- make_df(store_layout[["walls_mat"]], 1)
  cashier_df <- make_df(store_layout[["cashier_mat"]], 1)
  entrance_df <- make_df(store_layout[["entrance_mat"]], 1)
  exit_df <- make_df(store_layout[["exit_mat"]], 1)
  target_df <- store_layout[["target_df"]]
  
  # we sample with replacement for each step to generate dfs of targets for all agents
  entrance_tiles <- entrance_df[sample(nrow(entrance_df), size=n_agents, replace = TRUE),]
  target_tiles <- target_df[sample(target_df$item_id, prob=target_df$ghi, replace=TRUE, size=n_agents),]
  cashier_tiles <- cashier_df[sample(nrow(cashier_df), size=n_agents, replace = TRUE),]
  exit_tiles <- exit_df[sample(nrow(exit_df), size=n_agents, replace = TRUE),]
  
  agent_list = list()
  
  for(i in 1:n_agents) {
    entrance_coords = as.numeric(entrance_tiles[i,])
    target_coords = as.numeric(target_tiles[i,1:2])
    cashier_coords = as.numeric(cashier_tiles[i,])
    exit_coords = as.numeric(exit_tiles[i,])
    agent_list[[i]] = list(entrance_coords, target_coords, cashier_coords, exit_coords)
  }
  return(agent_list)
}

simulate_density <- function(store_layout, agent_list, coeff=0.1, plot=FALSE, name="density_plot") {
  density_mat = matrix(0, img_w, img_h)
  walls_mat = store_layout[["walls_mat"]]
  
  for(i in 1:length(agent_list)) {
    current_agent <- agent_list[[i]]
    # Route btw first two targets (entrance to target 1)
    n_routes <- length(current_agent) - 1
    for(j in 1:n_routes) {
      source = current_agent[[j]]
      target = current_agent[[j+1]]
      mg <- SearchMaze2D$new(walls_mat, density_mat, coeff)
      current_path <- mg$run(source, target)
      if (is.null(current_path)) {
        current_path <- mg$run(target, source)
      }
      for(k in current_path) {
        density_mat[k[1], k[2]] <- density_mat[k[1], k[2]] + 1
      }
      if(plot==TRUE) {
        file_name = paste(name,"_", i, "_", j, ".png", sep="")
        density_df = make_df_full(density_mat)
        p <- ggplot(density_df, aes(x=x,y=y,fill=value)) +
          geom_tile() +
          geom_tile(data=walls_df_1, fill="#FFFFFF") +
          scale_y_continuous(breaks = seq(0, 48, 1), limits = c(0, 48.5), minor_breaks = NULL) +
          scale_x_continuous(breaks = seq(0, 48, 1), limits = c(0, 48.5), minor_breaks = NULL) +
          coord_equal() +
          scale_fill_viridis_c() +
          theme_minimal() +
          theme(axis.title = element_blank(),
                axis.text = element_blank(),
                legend.position = "none")
        ggsave(plot=p, filename=file_name, width=1, height=1, units="in", dpi=150, device="png")
      }
    }
  }
  return(density_mat)
}

test <- simulate_density(store_layout, agent_list, plot=TRUE)

