library(dplyr)
library(grid)
library(reshape2)
library(pixmap)
library(ggplot2)
source("/Users/matthewtham/Desktop/IMMC/IMMC/astar.R")
source("/Users/matthewtham/Desktop/IMMC/IMMC/examples.R")

make_df <- function(mat, val) {
  df <- which(mat == val, arr.ind = TRUE) %>%
    as.data.frame() %>%
    transmute(y = row, x = col)
  return(df)
}

make_df_full <- function(mat) {
  df <- setNames(melt(mat), c('y', 'x', 'value'))
}


img <- read.pnm("original_layout_cleaned.pbm")
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

#ffff00 = Target objects (for now, the first target for all agents)
## Can be placed in any shelf position (so every wall tile minus the entrance wall)
shelf_mat <- walls_mat
# Zero out the entrance wall
shelf_mat[nrow(shelf_mat),] <- 0
# Convert the available spots into df format for sampling
shelf_df <- make_df(shelf_mat, 1)
shelf_positions <- shelf_df[sample(nrow(shelf_df), 134),]
shelf_positions$item_id <- sample(1:134)
shelf_positions$ghi <- storedata[shelf_positions$item_id,"happiness"]
shelf_positions <- shelf_positions[order(shelf_positions$item_id),]
rownames(shelf_positions) <- shelf_positions$item_id
# List format is y, x, item_id, GHI
shelf_positions_list <- as.list(as.data.frame(t(shelf_positions)))

# Convert randomised item positions back into a matrix
shelf_mat <- matrix(0, img_w, img_h)
for(i in shelf_positions_list) {
  shelf_mat[i[1], i[2]] <- i[3]
}

# Plot of items atop walls to check out how our randomisation is going
shelf_df_2 <- make_df_full(shelf_mat)
walls_df_1 <- make_df(walls_mat, 1)

ggplot(shelf_df_2, aes(x=x,y=y,fill=value)) +
  geom_tile() +
  geom_tile(data=walls_df_1, fill="#FFFFFF", alpha=0.2) +
  scale_y_continuous(breaks = seq(0, 48, 1), limits = c(0, 48.5), minor_breaks = NULL) +
  scale_x_continuous(breaks = seq(0, 48, 1), limits = c(0, 48.5), minor_breaks = NULL) +
  coord_equal() +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank())

# Create dfs for agent target generation
entrance_df_1 <- make_df(entrance_mat, 1)
target_df_1 <- shelf_positions

cashier_df_1 <- make_df(cashier_mat, 1)
exit_df_1 <- make_df(exit_mat, 1)
walls_df_1 <- make_df(walls_mat, 1)

# Plot them because why not
ggplot(target_df_1, aes(x=x, y=y)) +
  # Plotting cashiers is broken for some reason, but rest assured that the underlying data is correct
  geom_tile(data=cashier_df_1, fill="#00FFFF") +
  geom_tile(data=entrance_df_1, fill="#FF0000") +
  geom_tile(data=walls_df_1, fill="#000000") +
  geom_tile(data=exit_df_1, fill="#0000FF") +
  geom_tile(aes(fill=ghi)) +
  scale_fill_viridis_c() +
  coord_equal() +
  theme_minimal()

create_agent_list <- function(n_agents, entrance_df, target_df, cashier_df, exit_df) {
  # we sample with replacement for each step
  entrance_tiles <- entrance_df[sample(nrow(entrance_df), n_agents, replace = TRUE),]
  target_tiles <- target_df[sample(target_df$item_id, prob=target_df$ghi, replace=TRUE, size=n_agents),]
  cashier_tiles <- cashier_df[sample(nrow(cashier_df), n_agents, replace = TRUE),]
  exit_tiles <- exit_df[sample(nrow(exit_df), n_agents, replace = TRUE),]
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
# 
# 
# # Quick test to see if sampling with probabilities is working correctly
# plot(x=shelf_positions$item_id, y=shelf_positions$ghi)
# 
# target_ids <- sample(target_df_1$item_id, prob=target_df_1$ghi, replace=TRUE, size=10000)
# barplot(table(target_ids))
# 
# target_tiles <- target_df_1[sample(target_df_1$item_id, prob=target_df_1$ghi, replace=TRUE, size=10000),]
# target_ids_2 <- target_tiles$item_id
# barplot(table(target_ids_2))
# 
# agent_list = create_agent_list(10000, entrance_df_1, target_df_1, cashier_df_1, exit_df_1)
# sample = c()
# original_id = c()
# for(i in 1:10000) {
#   sample = agent_list[[i]][[2]]
#   original_id[i] = shelf_positions[which(shelf_positions$y==sample[1] & shelf_positions$x==sample[2]), "item_id"]
# }
# barplot(table(original_id))
# ## After a great deal of annoyance, all the samples match the target probabilities

# Let's write our actual pathing loop

# Start with an easy sample of 100
n_agents = 100
agent_list = create_agent_list(n_agents, entrance_df_1, target_df_1, cashier_df_1, exit_df_1)

# Initialise with an empty density matrix
density_mat = matrix(0, img_w, img_h)
# Starting crowd avoidance coefficient of 0.001
coeff = 0.001

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
    # Optional code to save a density plot after every path
    # file_name = paste("density_plot_", i, "_", j, ".png", sep="")
    # density_df = make_df_full(density_mat)
    # p <- ggplot(density_df, aes(x=x,y=y,fill=value)) +
    #   geom_tile() +
    #   geom_tile(data=walls_df_1, fill="#FFFFFF") +
    #   scale_y_continuous(breaks = seq(0, 48, 1), limits = c(0, 48.5), minor_breaks = NULL) +
    #   scale_x_continuous(breaks = seq(0, 48, 1), limits = c(0, 48.5), minor_breaks = NULL) +
    #   coord_equal() +
    #   scale_fill_viridis_c() +
    #   theme_minimal() +
    #   theme(axis.title = element_blank(),
    #         axis.text = element_blank(),
    #         legend.position = "none")
    # ggsave(plot=p, filename=file_name, width=1, height=1, units="in", dpi=150, device="png")
  }
}
density_df = make_df_full(density_mat)
ggplot(density_df, aes(x=x,y=y,fill=value)) +
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

# Shaping a loss function
n_agents = 100
n_routes = 3
# Represents the density in the worst case scenario
## Value is divided by 2 because even in the worst case (a linear maze of shelves) where all agents seek the last item, the average path length is about equal to half the total tile count

# Helper functions for normalising and plotting loss function shapes
## Ideally, we want a loss function where the gradient becomes steeper toward better values, providing high rewards for beneficial changes while mildly penalising for harmful changes
worst_case = rep(n_agents*n_routes, img_h*img_w/2)
norm_loss <- function(loss, get_loss) {
  loss_max = get_loss(worst_case)
  return(loss/loss_max)
}
plot_loss <- function(get_loss) {
  loss_norm_y <- c()
  for(i in 0:(n_agents*n_routes)){
    densities = rep(i, img_h*img_w/2)
    loss = get_loss(densities)
    loss_norm_y[i+1] = norm_loss(loss, get_loss)
  }
  loss_norm_df <- data.frame(x=0:(n_agents*n_routes), y=loss_norm_y)
  ggplot(loss_norm_df, aes(x=x, y=y)) +
    geom_line() +
    theme_minimal()
}

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

# Im a fan of log because it has the desired shape, and because of the nature of our simulation outputs (where densities peaking at ~100 are expected for a naive, random arrangement of items), as it skews heavily toward lower values
plot_loss(get_loss_log)

# Loss for our current arrangement (complete randomisation)
current_loss <- norm_loss(get_loss_log(density_df$value), get_loss_log)
print(current_loss)
