library(dplyr)
library(grid)
library(reshape2)
library(pixmap)
library(ggplot2)
source("astar.R")
source("searchmaze.R")
source("storedata.R")

reverse_encode <- function(store_layout, img_w=48, img_h=48) {
  walls_mat <- store_layout$walls_mat
  entrance_mat <- store_layout$entrance_mat
  exit_mat <- store_layout$exit_mat
  target_df <- store_layout$target_df
  target_mat <- matrix(0, img_w, img_h)
  for(row in 1:nrow(target_df)) {
    coord = as.numeric(target_df[row, 1:3])
    target_mat[coord[1], coord[2]] <- coord[3]
  }
  img_red <- walls_mat + entrance_mat
  img_green <- ((walls_mat + cashier_mat)*255 - target_mat)/255
  img_blue <- walls_mat + cashier_mat + exit_mat
  
  img_array <- array(c(img_red, img_green, img_blue), dim=c(48,48,3))
  img <- pixmapRGB(img_array)
  return(img)
}

plot_mat <- function(mat) {
  df = make_df_full(mat)
  ggplot(df, aes(x=x,y=y,fill=value)) +
    geom_tile() +
    scale_y_continuous(breaks = seq(0, 48, 1), limits = c(0, 48.5), minor_breaks = NULL) +
    scale_x_continuous(breaks = seq(0, 48, 1), limits = c(0, 48.5), minor_breaks = NULL) +
    coord_equal() +
    scale_fill_viridis_c() +
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          legend.position = "none")
}

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

  #ffffff = Walls (all non-traversable tiles, including shelves and walls)
  green_minus_target = img@green
  green_minus_target[which(green_minus_target != 0)] <- 1
  walls_mat <- img@red + green_minus_target + img@blue
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
  target_df$value = 255 - target_df$value
  target_df <- target_df[order(target_df$value),]
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
  target_tiles <- target_df[sample(target_df$value, prob=target_df$ghi, replace=TRUE, size=n_agents),]
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

simulate_density <- function(store_layout, agent_list, coeff=0.1, plot=FALSE, name="density_plot", img_w=48, img_h=48) {
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
        walls_df = make_df(walls_mat, 1)
        p <- ggplot(density_df, aes(x=x,y=y,fill=value)) +
          geom_tile() +
          geom_tile(data=walls_df, fill="#FFFFFF") +
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

# Some loss functions
get_loss <- function(densities) {
  loss = sum(densities)
  return(loss)
}

get_loss_sqrt <- function(densities) {
  loss = sum(sqrt(densities))
  return(loss)
}

get_loss_log <- function(densities) {
  loss = sum(log(densities+1))
  return(loss)
}

norm_loss <- function(loss, get_loss, worst_case) {
  loss_max = get_loss(worst_case)
  return(loss/loss_max)
}

simulate <- function(pbm_path, storedata, n_agents=100, loss_fn=get_loss, max_routes=3, coeff=0.1, reps=5, plot=FALSE, name="density_plot") {
  # This value is technically not necessarily the same for all agents, but we're assuming it is

  store_layout <- read_img_map(pbm_path)
  img_w = dim(store_layout$walls_mat)[1]
  print(img_w)
  img_h = dim(store_layout$walls_mat)[2]
  print(img_h)
  
  agent_list <- create_agent_list(store_layout, n_agents)
  density_mat <- simulate_density(store_layout, agent_list, coeff, plot, name, img_w, img_h)
  density_df = make_df_full(density_mat)
  
  worst_case = rep(n_agents*max_routes, img_w*img_h/2)
  
  loss <- norm_loss(loss_fn(density_df$value), loss_fn, worst_case)
  
  output = list(density_mat, loss)
  return(output)
}
