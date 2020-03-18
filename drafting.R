density_mat <- simulate_density(store_layout, agent_list, coeff, plot, name, img_w, img_h)
density_df = make_df_full(density_mat)

worst_case = rep(n_agents*max_routes, img_w*img_h/2)

loss <- norm_loss(loss_fn(density_df$value), loss_fn, worst_case)

output = list(density_mat, loss)