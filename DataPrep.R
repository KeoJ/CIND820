#### Data Collection, Cleaning, and Exploration ####

# install.packages("rvest")
# install.packages(c("httr", "jsonlite"))
library(PerformanceAnalytics)
library(reshape2)
library(stringr)
library(rvest)
library(ggplot2)
library(tidyverse)



btc_price <- read_csv("C:/Users/Kevin/Desktop/Languages/R/CIND820/coin_Bitcoin.csv")
bitinfo <- read_html("https://bitinfocharts.com/comparison/tweets-btc.html")


### Bitinfocharts Web Scraping ###
twt_vl <- bitinfo %>%
  html_elements("script")

script_info <- as.character(twt_vl[5])

# trimming script_info using regex
x <- gsub('.*\\(\\"container\\"\\)\\,','',script_info)
y <- gsub('\\, \\{labels\\:.*','',x)
z <- str_replace_all(y, c('\\[' = '', '\\]' = '', 'new Date' = '', '\\(' = '', '\\)' = '', '\\\"' = ''))
zx <- strsplit(z, ',')

# creating dataframe
tweet_vol = c()
date = c()

for(i in 1:length(zx[[1]])) {
  if(i %% 2 == 0){
    tweet_vol <- append(tweet_vol, zx[[1]][i])
  }
  else {
    date <- append(date, zx[[1]][i])
  }
}

btc_twitter <- data.frame(date, tweet_vol)



### General Analysis & Cleaning ###
## btc_twitter analysis
# btc_twitter$tweet_vol <- as.numeric(btc_twitter$tweet_vol)
# btc_twitter$date <- as.Date(btc_twitter$date)
sapply(btc_twitter, class)
summary(btc_twitter)
sd(btc_twitter$tweet_vol, na.rm = T)

which(is.na(btc_twitter$tweet_vol))

boxplot(btc_twitter$tweet_vol, na.rm = T)
hist(btc_twitter$tweet_vol)

# btc_price analysis 
sapply(btc_price, class)
summary(btc_price)
sd(btc_price$Close)

boxplot(btc_price$Close, na.rm = T)
hist(btc_price$Close)

# Combine data frames through innerjoin
btc_price$Date <- as.Date(btc_price$Date)
btc_total <- merge(btc_price, btc_twitter, by.x = "Date", by.y = "date")

btc_total <- btc_total %>%
  select(Date, High, Low, Open, Close, Volume, Marketcap, tweet_vol)
btc_total$ID <- c(1:2517)

# Filling missing values by averaging the neighbours
btc_total$tweet_vol[2293] <- round(mean(c(btc_total$tweet_vol[2292], btc_total$tweet_vol[2294])), 0)
btc_total$tweet_vol[270] <- round(mean(c(btc_total$tweet_vol[269], btc_total$tweet_vol[271])), 0)
btc_total$tweet_vol[1703] <- round(mean(c(btc_total$tweet_vol[1702], btc_total$tweet_vol[1704])), 0)

# Filling missing values through linear regression using the surrounding rows with values
for(vol in 1:35) {
  pos = 1424
  pos = pos + vol
  value <- round((-8841/36)*vol + 61317, 0)
  btc_total$tweet_vol[pos] <- value
}

for(vol in 1:18) {
  pos = 1172
  pos = pos + vol
  value <- round((-1701/19)*vol + 42760, 0)
  btc_total$tweet_vol[pos] <- value
}



### Exploratory Analysis ###
## Price Plot
ggplot(data=btc_total, aes(x=Date, y=Close, group=1)) +
  geom_line()+
  labs(title = "BTC Price VS Date", y = "BTC Price ($)")

## Correlation Analysis
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

reorder_cormat <- function(cormat){
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder & melt the correlation matrix
cormat <- round(cor(btc_total[,c(2:8)]),2)
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Plot Heatmap
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

## Linear Regresssion
# p < 2e-16, tweet_vol statistically significant.
# R^2 = 0.3618
lm_price <- lm(Close ~ tweet_vol, data = btc_total)
summary(lm_price)

plot(y = btc_total$Close, x = btc_total$tweet_vol, pch = 16, col = "blue",
     xlab = "Tweet Volume", ylab = "BTC Price ($)")
abline(lm_price)

plot(lm_price$residuals, pch = 16, col = "red")




