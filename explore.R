setwd('~/my_Git_repos/SF_Crime/')
library(data.table)
library(ggplot2)
library(scales)
library(plyr)
library(doParallel)

grid_plot_ggplot2 <- function(ggplotList, ncol, nrow){
  library(gridExtra)
  library(methods)
  plotList <- c(ggplotList, nrow = nrow, ncol = ncol)
  do.call(grid.arrange, plotList)
}


sf_train <- fread('~/Documents/Kaggle/SF_crime/train.csv', sep = ',')
sapply(sf_train, class)
str(sf_train)
sf_train$Category  <- factor(sf_train$Category, 
                             levels = names(sort(table(sf_train$Category), decreasing = TRUE)))
sf_train$PdDistrict  <- factor(sf_train$PdDistrict, 
                             levels = names(sort(table(sf_train$PdDistrict), decreasing = TRUE)))
sf_train$Resolution <- factor(sf_train$Resolution, 
                              levels = names(sort(table(sf_train$Resolution), decreasing = TRUE)))
sf_train$DayOfWeek <- factor(sf_train$DayOfWeek)

sf_train$Day <- as.Date(sapply(sf_train$Dates, function(x)strsplit(x, ' ')[[1]][1]))

sf_train$Year <- format(sf_train$Day, '%Y')
sf_train$Month <- format(sf_train$Day, '%m')
sf_train$Date <- format(sf_train$Day, '%d')

sf_train$Time <- sapply(sf_train$Dates, function(x)strsplit(x, ' ')[[1]][2])
sf_train$Hour <- sapply(sf_train$Time, function(x)strsplit(x, ':')[[1]][1])
sf_train$Dates1 <- as.POSIXct(sf_train$Dates, format="%Y-%m-%d %H:%M:%S")
sf_train$Dates1[1:10]

sf_train_by_category <- dlply(sf_train, .variables = 'Category')

plotByYear <- llply(1:length(sf_train_by_category), function(x){
  ggplot(sf_train_by_category[[x]], aes(x = Year)) + 
    geom_bar() +
    labs(x = 'Year', y = 'count', title = names(sf_train_by_category)[[x]]) +
    theme(title = element_text(size = 15), axis.title = element_text(size = 15), axis.text = element_text(size = 15),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_line(size = 1))
})
png('Category_byYear.png', 7*400, 6*300)
grid_plot_ggplot2(plotByYear, nrow = 6, ncol = 7)
dev.off()

plotByMonth <- llply(1:length(sf_train_by_category), function(x){
  ggplot(sf_train_by_category[[x]], aes(x = Month)) + 
    geom_bar() +
    labs(x = 'Month', y = 'count', title = names(sf_train_by_category)[[x]]) +
    theme(title = element_text(size = 15), axis.title = element_text(size = 15), axis.text = element_text(size = 15),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_line(size = 1))
})
png('Category_byMonth.png', 7*400, 6*300)
grid_plot_ggplot2(plotByMonth, nrow = 6, ncol = 7)
dev.off()

plotByDate <- llply(1:length(sf_train_by_category), function(x){
  ggplot(sf_train_by_category[[x]], aes(x = Date)) + 
    geom_bar() +
    labs(x = 'Date', y = 'count', title = names(sf_train_by_category)[[x]]) +
    theme(title = element_text(size = 15), axis.title = element_text(size = 15), axis.text = element_text(size = 15),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_line(size = 1))
})
png('Category_byDate.png', 7*400, 6*300)
grid_plot_ggplot2(plotByDate, nrow = 6, ncol = 7)
dev.off()

plotByHour <- llply(1:length(sf_train_by_category), function(x){
  ggplot(sf_train_by_category[[x]], aes(x = Hour)) + 
    geom_bar() +
    labs(x = 'Hour', y = 'count', title = names(sf_train_by_category)[[x]]) +
    theme(title = element_text(size = 15), axis.title = element_text(size = 15), axis.text = element_text(size = 15),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_line(size = 1))
})
png('Category_byHour.png', 7*400, 6*300)
grid_plot_ggplot2(plotByHour, nrow = 6, ncol = 7)
dev.off()


plotByPdDisrict <- llply(1:length(sf_train_by_category), function(x){
  ggplot(sf_train_by_category[[x]], aes(x = PdDistrict)) + 
    geom_bar() +
    labs(x = 'Pd', y = 'count', title = names(sf_train_by_category)[[x]]) +
    theme(title = element_text(size = 15), axis.title = element_text(size = 15), axis.text = element_text(size = 15),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_line(size = 1))
})
png('Category_byPdDistrict.png', 7*400, 6*300)
grid_plot_ggplot2(plotByPdDisrict, nrow = 6, ncol = 7)
dev.off()

x_scale = c(quantile(sf_train$X, 0.01), quantile(sf_train$X, 0.99))
y_scale = c(quantile(sf_train$Y, 0.01), quantile(sf_train$Y, 0.99))
geoPlotByPdDisrict <- llply(1:length(sf_train_by_category), function(x){
  ggplot(sf_train_by_category[[x]], aes(x = X, y = Y, color = PdDistrict)) + 
    geom_point() +
    coord_cartesian(xlim = x_scale, ylim = y_scale) + 
    labs(x = 'longitude', y = 'latitute', title = names(sf_train_by_category)[[x]]) +
    theme(title = element_text(size = 30), axis.title = element_text(size = 30), axis.text = element_text(size = 30),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_line(size = 1))
})
png('coords_ByPdDisrict.png', 7*1000, 6*800)
grid_plot_ggplot2(geoPlotByPdDisrict, nrow = 6, ncol = 7)
dev.off()

png('coords_ByPdDisrict_1.png', 1000, 800, res = 200)
ggplot(sf_train_by_category[[1]], aes(x = X, y = Y, color = PdDistrict)) + 
  geom_point() +
  coord_cartesian(xlim = x_scale, ylim = y_scale) + 
  labs(x = 'longitude', y = 'latitute', title = names(sf_train_by_category)[[1]]) +
  theme(title = element_text(size = 10), axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_line(size = 1))
dev.off()




ggplot(sf_train, aes(x = Category, fill = PdDistrict)) + 
  geom_bar(position = 'fill') + 
  # scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(sf_train, aes(x = PdDistrict, fill = Category)) + 
  # geom_bar(position = 'fill') + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(sf_train, aes(x = Category, fill = Resolution)) + 
  geom_bar(position = 'fill') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(sf_train, aes(x = Category, fill = DayOfWeek)) + 
  geom_bar(position = 'fill') + 
  # scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


png('coords_byPdDistrict.png', 2000, 2000, res = 200)
plot(Y~X, data = sf_train[sample(878094, 10000, replace = F), ], pch = 16, cex = 0.3, col = factor(PdDistrict),
     xlim = c(quantile(sf_train$X, 0.01), quantile(sf_train$X, 0.99)),
     ylim = c(quantile(sf_train$Y, 0.01), quantile(sf_train$Y, 0.99)))
dev.off()

png('pd_cat.png', 2000, 2000, res = 200)
plot(PdDistrict~Category, data = sf_train[sample(878094, 10000, replace = F), ], col = factor(Category))
dev.off()

sf_train$PdDistrict
sf_train$Descript

install.packages('tm')
install.packages('wordcloud')
library(tm)
library(wordcloud)
Descript_source <- VectorSource(sf_train$Descript)
# Descript_corpus <- Corpus(Descript_source)

test_source <- VectorSource(sf_train$Descript[1:100])
test_corpus <- Corpus(Descript_test)
length(test_corpus)
test_corpus <- tm_map(test_corpus, content_transformer(tolower))
test_corpus <- tm_map(test_corpus, removePunctuation)
test_corpus <- tm_map(test_corpus, removeWords, stopwords('english'))
test_term_matrix <- DocumentTermMatrix(test_corpus)
inspect(test_term_matrix[1:10, 1:10])

str(sf_train)
sf_train$Address[sample(800000,100)]

Address1 <- llply(sf_train$Address, function(x){
  strsplit(x, ' / | of ')[[1]]
})
hist(sapply(Address1, length))

sf_train$Address[1:20]



strsplit(sf_train$Address[15], ' / | of ')
