param0<-readRDS("experimental_results/default_euclidean_cashier.rds")
param25<-readRDS("experimental_results/default_euclidean_cashier_25.rds")
param50<-readRDS("experimental_results/default_euclidean_cashier_50.rds")
param75<-readRDS("experimental_results/default_euclidean_cashier_75.rds")
param90<-readRDS("experimental_results/default_euclidean_cashier_90.rds")
params <- list(param0, param25, param50, param75, param90)

losslist<-list()

for(j in 1:length(params)) {
  param=params[[j]]
  lossvector<-c()
  for(i in 1:length(param[[2]])) {
    lossvector <- c(lossvector, param[[2]][[i]][[3]])
  }
  losslist[[j]] <- lossvector
}

get_mean_sd <- function(dataset, treatment, observation, avg_column, sd_column) {
  for (i in unique(dataset[,treatment])) {
    dataset[which(dataset[,treatment]==i),avg_column] <- mean(dataset[which(dataset[,treatment]==i),observation])
    dataset[which(dataset[,treatment]==i),sd_column] <- sd(dataset[which(dataset[,treatment]==i),observation])
  }
  return(dataset)
}

x<-c(rep(0,10),rep(25,10),rep(50,10),rep(75,10),rep(90,10))
y<-c(losslist[[1]],losslist[[2]],losslist[[3]],losslist[[4]],losslist[[5]])

lossdf<-data.frame(prominence=x, loss=y)
lossdf<-get_mean_sd(lossdf, "prominence", "loss", "loss_avg", "sd_avg")

library(ggpubr)

ggplot(lossdf, aes(x=prominence, y=loss)) +
  geom_point(aes(y=loss_avg)) +
  geom_errorbar(aes(ymin=loss_avg-sd_avg, ymax=loss_avg+sd_avg)) +
  geom_smooth(method="lm", formula="y~poly(x, degree=2)", se=F) +
  theme_pubr() +
  labs(x="Prominence threshold", y="Loss", caption="Error bars represent ± 1 standard deviation for 10 replicates")



