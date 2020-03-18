# Dataset
library(readxl)
library(ggplot2)
# storedata <- read_excel("StoreData_IMMC.xlsx")
# storedata <- as.data.frame(storedata)
# colnames(storedata) <- storedata[1,]
# storedata <- storedata[-1,]
# colnames(storedata) <- c("dpmt", "cat", "type", "brand", "item", "price", "discounted_price", "qty", "rating")
# storedata <- storedata[-(ncol(storedata):(ncol(storedata)-1))]
# rownames(storedata)<-1:nrow(storedata)
# storedata <- storedata[-(135:152),]
# temp <- storedata[c("price", "discounted_price", "qty", "rating")]
# temp <- lapply(temp, as.numeric)
# temp <- as.data.frame(temp)
# storedata[c("price", "discounted_price", "qty", "rating")] <- temp
# rm(temp)
# 
# storedata$discount_amount <- storedata$price - storedata$discounted_price
# storedata$discount_percentage <- storedata$discount_amount / storedata$price
# 
# storedata$ghi <- storedata$discount_amount * storedata$discount_percentage * storedata$rating / storedata$qty
# 
# # Assign a unique item_id at this point to maintain consistency throughout code
# storedata$item_id <- 1:nrow(storedata)

storedata <- read_excel("StoreData_IMMC_Filled.xlsx")
storedata <- as.data.frame(storedata)
colnames(storedata) <- storedata[1,]
storedata <- storedata[-1,1:11]
rownames(storedata)<-1:nrow(storedata)
storedata <- storedata[-(135:152),]
colnames(storedata) <- c("dpmt", "cat", "type", "brand", "item", "price", "discounted_price", "qty", "rating", "ghi", "frag")
temp <- storedata[c("price", "discounted_price", "qty", "rating","ghi","frag")]
temp <- lapply(temp, as.numeric)
temp <- as.data.frame(temp)
storedata[c("price", "discounted_price", "qty", "rating","ghi","frag")] <- temp
rm(temp)
storedata$frag <- storedata$frag/100
storedata$item_id <- 1:nrow(storedata)
