rm(list=ls())
full202052 <- read.csv("~/Downloads/full202052.dat", header=TRUE)
full202052 <- na.omit(full202052)

head(full202052)
# After googling a bit I have chosen a small subset of the product types to do an analysis on. 
# I believe that this makes more sense, as a subset of similar products are more comparable. 
# I have chosen to use the BEC categories. To see which product is which, download excel sheet: https://tulli.fi/en/statistics/bec-goods-classification

df <- full202052[,c(1:5,11,13:16,18:20)]

# Transforming variable to factor.
df$STAT_REGIME <- as.factor(df$STAT_REGIME)
df$FLOW <- as.factor(df$FLOW)

# Creating a functon to reveal insight for specific product types, using only observations with for a specific product type.
# Uses the product BEC to specificy certain products types.

f_insight <- function(product_type, df, reg) {
  
  product_rows <- which(df[,"PRODUCT_BEC"] == product_type)
  
  df <- df[product_rows,]
  
  # Single trades
  origin_quant_max <- max(df[,"QUANTITY_IN_KG"])
  country_origin_quant_max <- as.character(df[which(df[,"QUANTITY_IN_KG"] == max(df[,"QUANTITY_IN_KG"])),"DECLARANT_ISO"])
  avr_trade_quant <- mean(df[,"QUANTITY_IN_KG"])
  
  origin_value_max <- max(df[,"VALUE_IN_EUROS"])
  country_origin_value_max <- as.character(df[which(df[,"VALUE_IN_EUROS"] == max(df[,"VALUE_IN_EUROS"])),"DECLARANT_ISO"])
  avr_trade_value <- mean(df[,"VALUE_IN_EUROS"])
  
  
  destination_quant_max <- max(df[,"QUANTITY_IN_KG"])
  country_destination_quant_max <- as.character(df[which(df[,"QUANTITY_IN_KG"] == max(df[,"QUANTITY_IN_KG"])),"PARTNER_ISO"])
  
  destination_value_max <- max(df[,"VALUE_IN_EUROS"])
  country_destination_value_max <- as.character(df[which(df[,"VALUE_IN_EUROS"] == max(df[,"VALUE_IN_EUROS"])),"PARTNER_ISO"])
  
  fill_matrix <- c(country_origin_quant_max,origin_quant_max,avr_trade_quant,country_origin_value_max,origin_value_max,avr_trade_value,
                   country_destination_quant_max,destination_quant_max,avr_trade_quant,country_destination_value_max,destination_value_max,avr_trade_value)
  
  single_trades_summary <- as.data.frame(matrix(fill_matrix, nrow = 6, ncol = 2, byrow = FALSE))
  colnames(single_trades_summary) <- c("Country of Origin", "Destination Country")
  rownames(single_trades_summary) <- c("Max. Quantity Trade Country", "Max. Quantity Trade in KG", "Mean Quantity Trade in KG",
                                       "Max. Value Trade Country", "Max. Value Trade in EUR", "Mean Value Trade in EUR")
  
  
  # Sum of all trades
  sum_per_country <- as.data.frame(matrix(0,nrow = length(unique(df[,"DECLARANT_ISO"])), ncol = 3))
  rownames(sum_per_country) <- as.character(unique(df[,"DECLARANT_ISO"]))
  colnames(sum_per_country) <- c("Quantity", "Value", "Country")
  sum_per_country[,3] <- as.character(unique(df[,"DECLARANT_ISO"]))
  
  for (i in 1:nrow(sum_per_country)) {
    
    rows <- which(df[,"DECLARANT_ISO"] == unique(df[,"DECLARANT_ISO"])[i])
    sum_per_country[i,1] <- sum(df[rows,"QUANTITY_IN_KG"])
    sum_per_country[i,2] <- sum(df[rows,"VALUE_IN_EUROS"])
    
  }
  
  sum_per_country_dest <- as.data.frame(matrix(0,nrow = length(unique(df[,"PARTNER_ISO"])), ncol = 3))
  rownames(sum_per_country_dest) <- as.character(unique(df[,"PARTNER_ISO"]))
  colnames(sum_per_country_dest) <- c("Quantity", "Value", "Country")
  sum_per_country_dest[,3] <- as.character(unique(df[,"PARTNER_ISO"]))
  
  for (i in 1:nrow(sum_per_country_dest)) {
    
    rows <- which(df[,"PARTNER_ISO"] == unique(df[,"PARTNER_ISO"])[i])
    sum_per_country_dest[i,1] <- sum(df[rows,"QUANTITY_IN_KG"])
    sum_per_country_dest[i,2] <- sum(df[rows,"VALUE_IN_EUROS"])
    
  }
  
  
  origin_quant_max <- max(sum_per_country[,"Quantity"])
  country_origin_quant_max <- as.character(sum_per_country[which(sum_per_country[,"Quantity"] == max(sum_per_country[,"Quantity"])),"Country"])
  avr_trade_quant <- mean(sum_per_country[,"Quantity"])
  
  origin_value_max <- max(sum_per_country[,"Value"])
  country_origin_value_max <- as.character(sum_per_country[which(sum_per_country[,"Value"] == max(sum_per_country[,"Value"])),"Country"])
  avr_trade_value <- mean(sum_per_country[,"Value"])
  
  
  destination_quant_max <- max(sum_per_country_dest[,"Quantity"])
  country_destination_quant_max <- as.character(sum_per_country_dest[which(sum_per_country_dest[,"Quantity"] == max(sum_per_country_dest[,"Quantity"])),"Country"])
  
  destination_value_max <- max(sum_per_country_dest[,"Value"])
  country_destination_value_max <- as.character(sum_per_country_dest[which(sum_per_country_dest[,"Value"] == max(sum_per_country_dest[,"Value"])),"Country"])
  
  fill_matrix <- c(country_origin_quant_max,origin_quant_max,avr_trade_quant,country_origin_value_max,origin_value_max,avr_trade_value,
                   country_destination_quant_max,destination_quant_max,avr_trade_quant,country_destination_value_max,destination_value_max,avr_trade_value)
  
  sum_trades_summary <- as.data.frame(matrix(fill_matrix, nrow = 6, ncol = 2, byrow = FALSE))
  colnames(sum_trades_summary) <- c("Country of Origin", "Destination Country")
  rownames(sum_trades_summary) <- c("Max. Quantity Trade Country", "Max. Quantity Trade in KG", "Mean Quantity Trade in KG",
                                       "Max. Value Trade Country", "Max. Value Trade in EUR", "Mean Value Trade in EUR")
  
  print("Single Trade Statistics:")
  print(single_trades_summary)
  
  print("Sum of Trades Statistics:")
  print(sum_trades_summary)
  
  pp <- plot(sum_per_country[,2], type = "h", main = "Sum of Exports of Product per Country",
       xlab = "Country", ylab = "EUR", xaxt = "n", col = "blue")
  axis(1, at=1:length(unique(df[,"DECLARANT_ISO"])), labels=unique(df[,"DECLARANT_ISO"]), cex.axis=0.5)
  print(pp)
  
  if (reg == TRUE) {
    # Fitting regression to gain additional insight.
    df_lasso <- model.matrix(VALUE_IN_EUROS ~ DECLARANT_ISO + TRADE_TYPE + FLOW + STAT_REGIME + QUANTITY_IN_KG + SUP_QUANTITY,df)[,-1]
    df_lasso<- as.data.frame(df_lasso)
    fit  <- lm(df[,"VALUE_IN_EUROS"] ~ .,df_lasso)
    print(summary(fit))
  } else {}
  
}

# Product types. See linked spradsheet for explanaition of type of goods. Ideally I would have imported the sheet into R so you could
# see the types directly here, but you told us to keep it simple.
unique(df$PRODUCT_BEC)
f_insight(122, df, TRUE)
# "Single Trade Statistics" is simply some summary statistics for the product type for a single trade. I assume that the dataset 
