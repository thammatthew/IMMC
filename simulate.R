library(dplyr)
library(grid)
library(reshape2)
library(pixmap)
library(ggplot2)
library(plotly)
source("astar.R", local=TRUE)
source("searchmaze.R", local=TRUE)
source("storedata.R", local=TRUE)

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
  p<-ggplot(df, aes(x=x,y=y,fill=value)) +
    geom_tile() +
    scale_y_continuous(breaks = seq(0, 48, 1), limits = c(0, 48.5), minor_breaks = NULL) +
    scale_x_continuous(breaks = seq(0, 48, 1), limits = c(0, 48.5), minor_breaks = NULL) +
    coord_equal() +
    scale_fill_viridis_c() +
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          legend.position = "none")
  p<-ggplotly(p)
  return(p)
}

plot_df <- function(df) {
  p<-ggplot(df, aes(x=x,y=y,fill=value)) +
    geom_tile() +
    scale_y_continuous(breaks = seq(0, 48, 1), limits = c(0, 48.5), minor_breaks = NULL) +
    scale_x_continuous(breaks = seq(0, 48, 1), limits = c(0, 48.5), minor_breaks = NULL) +
    coord_equal() +
    scale_fill_viridis_c() +
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          legend.position = "none")
  p<-ggplotly(p)
  return(p)
}

plot_df_ghi <- function(df) {
  p<-ggplot(df, aes(x=x,y=y,fill=ghi)) +
    geom_tile() +
    scale_y_continuous(breaks = seq(0, 48, 1), limits = c(0, 48.5), minor_breaks = NULL) +
    scale_x_continuous(breaks = seq(0, 48, 1), limits = c(0, 48.5), minor_breaks = NULL) +
    coord_equal() +
    scale_fill_viridis_c() +
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          legend.position = "none")
  p<-ggplotly(p)
  return(p)
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

make_mat <- function(df, val_col, img_w=48, img_h=48) {
  mat <- matrix(0, img_w, img_h)
  for(i in 1:nrow(df)) {
    row = df[i,]
    coord = as.numeric(row[1:2])
    value = row[[val_col]]
    mat[coord[1], coord[2]] <- value
  }
  return(mat)
}

read_img_map <- function(img_path, targets_encoded=FALSE) {
  img <- read.pnm(img_path)

  #ffffff = Walls (all non-traversable tiles, including shelves and walls)
  green_minus_target = img@green
  green_minus_target[which(green_minus_target != 0)] <- 1
  walls_mat <- img@red + green_minus_target + img@blue
  walls_mat[which(walls_mat != 3)] <- 0
  walls_mat <- walls_mat/3
  
  #cc00cc = Blocked walls (walls where items cannot be placed)
  blocked_mat <- img@red + img@blue + img@green
  blocked_mat[which(blocked_mat!=1.6)]<-0
  blocked_mat <- blocked_mat / 1.6
  
  walls_mat <- blocked_mat+walls_mat
  
  #00ffff = Cashiers (for now, the second target for all agents)
  cashier_mat <- img@green + img@blue + img@red
  cashier_mat[which(cashier_mat != 2)] <- 0
  cashier_mat <- cashier_mat/2
  
  #330000 = Cashier inlet (for supporting queueing behaviour)
  cashier_in_mat <- img@red
  cashier_in_mat[which(cashier_in_mat!=0.2)] <- 0
  cashier_in_mat <- cashier_in_mat * 5
  
  #000033 = Cashier outlet (for supporting queueing behaviour)
  cashier_out_mat <- img@blue
  cashier_out_mat[which(cashier_out_mat!=0.2)] <- 0
  cashier_out_mat <- cashier_out_mat * 5
  
  #ff0000 = Entrances (possible source points)
  entrance_mat <- img@red
  entrance_mat[which(entrance_mat!=1)] <- 0
  entrance_mat = entrance_mat - walls_mat + blocked_mat
  
  #0000ff = Exits (final target for all agents)
  exit_mat <- img@blue
  exit_mat[which(exit_mat!=1)] <- 0
  exit_mat = exit_mat - walls_mat - cashier_mat + blocked_mat
  
  #00xx00 = Target objects (for now, the first target for all agents)
  # @Keane: Encode target objects by converting their numerical item_id to hex, then setting that to be the colour of the green channel
  # convert back to 0-255 encoding
  if(targets_encoded == TRUE) {
    target_mat <- img@green*255
    target_mat[which(target_mat == 255)] <- 0
    target_df <- make_df_full(target_mat)
    target_df <- target_df[which(target_df$value != 0),]
    target_df$value = 255 - target_df$value
    target_df <- target_df[order(target_df$value),]
    target_df$ghi <- storedata$ghi
  } else {
    target_df <- NULL
  }
  
  store_layout <- list("walls_mat" = walls_mat, "blocked_mat" = blocked_mat, "cashier_mat" = cashier_mat, "cashier_in_mat" = cashier_in_mat, "cashier_out_mat" = cashier_out_mat, "entrance_mat" = entrance_mat, "exit_mat" = exit_mat, "target_df" = target_df)
  
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
  cashier_in_mat = store_layout[["cashier_in_mat"]]
  cashier_out_mat = store_layout[["cashier_out_mat"]]
  
  walls_cashier_in = walls_mat + cashier_out_mat
  walls_cashier_out = walls_mat + cashier_in_mat
  
  for(i in 1:length(agent_list)) {
    current_agent <- agent_list[[i]]
    # Route btw first two targets (entrance to target 1)
    n_routes <- length(current_agent) - 1
    for(j in 1:n_routes) {
      source = current_agent[[j]]
      target = current_agent[[j+1]]
      if(j == 2) {mg <- SearchMaze2D$new(walls_cashier_in, density_mat, coeff)} else if(j == 3) {mg <- SearchMaze2D$new(walls_cashier_out, density_mat, coeff)} else {mg <- SearchMaze2D$new(walls_mat, density_mat, coeff)}
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
    print(paste("Agent",i,"simulated..."))
  }
  return(density_mat)
}

get_monetary_loss <- function(row, item_df, total_ghi, n_agents) {
  coord = row[1:2]
  density = row[3]
  value_lost<-0
  if(density == 0) {return(value_lost)}
  adj<-c()
  for(i in 1:-1)
    for(j in 1:-1)
      if(i!=0 || j !=0)
        adj<-rbind(adj,coord+c(i,j)) 
  for(i in 1:nrow(adj)) {
    if(adj[i,1]==0 | adj[i,2]==0 | adj[i,1]>48 | adj[i,2]>48) {next}
    if(item_df[which(item_df$y==adj[i,1] & item_df$x==adj[i,2]),3]==0) {next} else {
      item <- item_df[which(item_df$y==adj[i,1] & item_df$x==adj[i,2]),3:7]
    }
    e_sold = n_agents * (item$ghi / total_ghi)
    e_left = item$qty - e_sold
    if (e_left >= 0) {
      avg_remaining = (item$qty + e_left)/2
      integrated_qty = 1/2 * (item$qty-e_left) + e_left
    } else {
      time_sold_out = item$qty/e_sold
      integrated_qty = 1/2 * item$qty * time_sold_out
    }
    value_lost = value_lost + (density^2/100000) * item$discounted_price * item$frag * integrated_qty
  }
  return(as.numeric(value_lost))
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

get_loss_mat <- function(storedata, density_mat, target_df, n_agents) {
  density_df <- make_df_full(density_mat)
  
  item_mat <- make_mat(target_df, "value")
  item_df <- make_df_full(item_mat)
  item_df <- item_df[order(-item_df$value),]
  
  item_df$frag <- c(rev(storedata$frag), rep(0, nrow(item_df)-length(storedata$frag)))
  item_df$ghi <- c(rev(storedata$ghi), rep(0, nrow(item_df)-length(storedata$ghi)))
  item_df$discounted_price <- c(rev(storedata$discounted_price), rep(0, nrow(item_df)-length(storedata$discounted_price)))
  item_df$qty <- c(rev(storedata$qty), rep(0, nrow(item_df)-length(storedata$qty)))
  
  total_ghi = sum(storedata$ghi)
  
  loss_mat <- matrix(0, nrow=48, ncol=48)
  loss_mat[] <- apply(density_df,1,get_monetary_loss, item_df=item_df, total_ghi=total_ghi, n_agents=n_agents)
  return(loss_mat)
}

simulate <- function(pbm_path, store_layout = NULL, storedata, n_agents=100, loss_fn=get_loss, max_routes=3, coeff=0.1, reps=5, plot=FALSE, name="density_plot", from_bitmap=TRUE) {
  # This value is technically not necessarily the same for all agents, but we're assuming it is
  print("Reading store layout from bitmap...")
  if(from_bitmap == TRUE) {
    store_layout <- read_img_map(pbm_path)
  } else {
    store_layout = store_layout
  }
  img_w = dim(store_layout$walls_mat)[1]
  img_h = dim(store_layout$walls_mat)[2]
  print("Store layout loaded.")
  
  print("Randomly generating agents...")
  agent_list <- create_agent_list(store_layout, n_agents)
  print("Running density simulation...")
  density_mat <- simulate_density(store_layout, agent_list, coeff, plot, name, img_w, img_h)
  print("Density simulation complete.")
  
  print("Computing estimated loss...")
  
  loss_mat <- get_loss_mat(storedata, density_mat, store_layout$target_df, n_agents)
  
  loss_df <- make_df_full(loss_mat)
  
  loss <- loss_fn(loss_df$value)
  
  print("Simulation completed successfully.")

  output = list(density_mat, loss_mat, loss)
  return(output)
}

plot_output <- function(output, filename) {
  results <- output[[2]]
  store_layout <- output[[1]]
  density_mat_avg <- matrix(0, 48, 48)
  loss_mat_avg <- matrix(0,48,48)
  loss <- c()
  for(i in 1:length(results)) {
    density_mat_avg <- density_mat_avg + results[[i]][[1]]
    loss_mat_avg <- loss_mat_avg + results[[i]][[2]]
    loss <- c(loss, results[[i]][[3]])
  }
  loss_avg <- mean(loss)
  loss_sd <- sd(loss)
  density_mat_avg <- density_mat_avg/length(results)
  loss_mat_avg <- loss_mat_avg/length(results)

  density_df_avg <- make_df_full(density_mat_avg)
  loss_df_avg <- make_df_full(loss_mat_avg)
  
  walls_df <- make_df(store_layout$walls_mat, 1)
  entrance_df <- make_df(store_layout$entrance_mat, 1)
  exit_df <- make_df(store_layout$exit_mat, 1)
  target_df <- store_layout$target_df
  
  p1<-ggplot(walls_df, aes(x=x, y=y)) +
    geom_rect(xmin=0.5,xmax=48.5,ymin=0.5,ymax=48.5, fill="#000000") +
    geom_tile(data=walls_df, fill="#FFFFFF") +
    geom_tile(data=entrance_df, fill="#FF0000") +
    geom_tile(data=exit_df, fill="#0000FF") +
    geom_tile(data=target_df, aes(fill=ghi)) +
    scale_y_continuous(breaks = seq(0, 48, 4), limits = c(0, 48.5), minor_breaks = NULL) +
    scale_x_continuous(breaks = seq(0, 48, 4), limits = c(0, 48.5), minor_breaks = NULL) +
    coord_equal() +
    theme_minimal() +
    scale_fill_distiller(palette="Spectral") +
    theme(axis.title = element_blank(),
          axis.text = element_blank()) +
    labs(fill="GHI", title = "Store Layout")
  ggsave(plot=p1, filename=paste(filename,"_layout.svg",sep=""), width=5, height=5, units="in", dpi=300, device="svg")
  
  p2<-ggplot(density_df_avg, aes(x=x, y=y, fill=value)) +
    geom_tile() +
    geom_tile(data=walls_df, fill="#FFFFFF") +
    geom_tile(data=entrance_df, fill="#FF0000") +
    geom_tile(data=exit_df, fill="#0000FF") +
    scale_y_continuous(breaks = seq(0, 48, 4), limits = c(0, 48.5), minor_breaks = NULL) +
    scale_x_continuous(breaks = seq(0, 48, 4), limits = c(0, 48.5), minor_breaks = NULL) +
    coord_equal() +
    theme_minimal() +
    scale_fill_viridis_c() +
    theme(axis.title = element_blank(),
          axis.text = element_blank()) +
    labs(fill="Density", title = "Pedestrian Density")
  ggsave(plot=p2, filename=paste(filename,"_density.svg",sep=""), width=5, height=5, units="in", dpi=300, device="svg")
  
  p3<-ggplot(loss_df_avg, aes(x=x, y=y, fill=value)) +
    geom_tile() +
    geom_tile(data=walls_df, fill="#FFFFFF") +
    geom_tile(data=entrance_df, fill="#FF0000") +
    geom_tile(data=exit_df, fill="#0000FF") +
    scale_y_continuous(breaks = seq(0, 48, 4), limits = c(0, 48.5), minor_breaks = NULL) +
    scale_x_continuous(breaks = seq(0, 48, 4), limits = c(0, 48.5), minor_breaks = NULL) +
    coord_equal() +
    theme_minimal() +
    scale_fill_viridis_c() +
    theme(axis.title = element_blank(),
          axis.text = element_blank()) +
    labs(fill="Loss", title = paste("Mean Loss =",round(loss_avg, 2),"SD =",round(loss_sd,2)))
  ggsave(plot=p3, filename=paste(filename,"_loss.svg",sep=""), width=5, height=5, units="in", dpi=300, device="svg")
  
  plots <- list(p1, p2, p3)
  return(plots)
}
