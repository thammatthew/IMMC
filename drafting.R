test <- simulate("example.pbm", storedata=storedata, n_agents = 5)
test_store_layout <- read_img_map("example.pbm")
density_mat <- test[[1]]
item_df <- test_store_layout$target_df
p<-plot_mat(density_mat)
p
p<-plot_df(item_df)
p

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

item_mat <- make_mat(item_df, "value")
density_df <- make_df_full(density_mat)
item_df <- make_df_full(item_mat)
item_df <- item_df[order(-item_df$value),]


item_df$frag <- c(rev(storedata$frag), rep(0, nrow(item_df)-length(storedata$frag)))
item_df$ghi <- c(rev(storedata$ghi), rep(0, nrow(item_df)-length(storedata$ghi)))
item_df$discounted_price <- c(rev(storedata$discounted_price), rep(0, nrow(item_df)-length(storedata$discounted_price)))
item_df$qty <- c(rev(storedata$qty), rep(0, nrow(item_df)-length(storedata$qty)))

get_monetary_loss <- function(row, item_df, total_ghi, n_agents) {
  coord = row[1:2]
  density = row[3]
  if(density == 0) {return}
  adj<-c()
  for(i in 1:-1)
    for(j in 1:-1)
      if(i!=0 || j !=0)
        adj<-rbind(adj,coord+c(i,j)) 
  value_lost<-0
  for(i in 1:nrow(adj)) {
    if(adj[i,][1]==0 | adj[i,][2]==0 | adj[i,][1]>48 | adj[i,][2]>48) {next}
    if(item_df[which(item_df$y==adj[i,1] & item_df$x==adj[i,2]),3]==0) {next} else {
      item <- item_df[which(item_df$y==adj[i,1] & item_df$x==adj[i,2]),3:7]
    }
    value_lost = value_lost + (density^1.5/3000) * item$discounted_price * item$frag * (item$qty + (item$qty - (n_agents * (item$ghi / total_ghi))))/2
  }
  return(value_lost)
}

loss_mat <- matrix(0, nrow=48, ncol=48)
loss_mat[] <- apply(density_df,1,get_monetary_loss, item_df=item_df_1, total_ghi=total_ghi, n_agents=n_agents)




  