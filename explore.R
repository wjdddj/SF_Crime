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
sf_train$DayOfWeek <- factor(sf_train$DayOfWeek,
                             levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

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

plotByDayOfWeek <- llply(1:length(sf_train_by_category), function(x){
  ggplot(sf_train_by_category[[x]], aes(x = DayOfWeek)) + 
    geom_bar() +
    labs(x = 'Weekdays', y = 'count', title = names(sf_train_by_category)[[x]]) +
    theme(title = element_text(size = 15), axis.title = element_text(size = 15), axis.text = element_text(size = 15),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_line(size = 1))
})
png('Category_byDayOfWeek.png', 7*400, 6*300)
grid_plot_ggplot2(plotByDayOfWeek, nrow = 6, ncol = 7)
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

plotByResolution <- llply(1:length(sf_train_by_category), function(x){
  ggplot(sf_train_by_category[[x]], aes(x = Resolution)) + 
    geom_bar() +
    labs(x = 'Resolution', y = 'count', title = names(sf_train_by_category)[[x]]) +
    theme(title = element_text(size = 15), axis.title = element_text(size = 15), axis.text = element_text(size = 15),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_line(size = 1))
})
png('Category_byResolution.png', 7*400, 6*600)
grid_plot_ggplot2(plotByResolution, nrow = 6, ncol = 7)
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
png('coords_ByPdDisrict.png', 7*1200, 6*1000)
grid_plot_ggplot2(geoPlotByPdDisrict, nrow = 6, ncol = 7)
dev.off()


## some customized text mining
words <- unlist(strsplit(sf_train$Descrip, ' '))
unique_word <- unique(unlist(strsplit(sf_train$Descrip, ' ')))
list_unique_word <- as.list(unique_word)

### compute document frequency
cl <- makeCluster(4)
registerDoParallel(cl)
word_freq <- laply(list_unique_word, .parallel = T, .paropts = list(.export = c('words')), function(x){
  sum(words == x)
})
stopCluster(cl)
system.time(sum(words == list_unique_word[[1]]))
system.time(grepl(list_unique_word[[1]], words))
doc_freq <- data.frame(unique_word = unique_word,
                        word_freq = word_freq)

head(doc_freq[order(doc_freq$word_freq, decreasing = F)[1:100], ])
hist(word_freq, breaks = 1000)

# manually compute tf-dif





## some text mining
install.packages('tm')
install.packages('wordcloud')
library(tm)
library(wordcloud)
#Descript_source <- VectorSource(sf_train$Descript)
#Descript_corpus <- Corpus(Descript_source)
#Descript_corpus <- tm_map(Descript_corpus, content_transformer(tolower))
#Descript_corpus <- tm_map(Descript_corpus, removePunctuation)
#Descript_corpus <- tm_map(Descript_corpus, removeWords, stopwords('english'))
#dtm <- DocumentTermMatrix(Descript_corpus)
#inspect(test_term_matrix[1:10, 1:10])


test_source <- VectorSource(sf_train$Descript[1:100])
test_corpus <- Corpus(test_source)
length(test_corpus)
inspect(test_corpus[1:2])
as.character(test_corpus[[1]])
lapply(test_corpus[1:10], as.character)

test_corpus <- tm_map(test_corpus, content_transformer(tolower))
test_corpus <- tm_map(test_corpus, removePunctuation)
test_corpus <- tm_map(test_corpus, removeWords, stopwords('english'))
test_term_matrix <- DocumentTermMatrix(test_corpus)
inspect(test_term_matrix[1:10, 1:10])

df_test_term_matrix <- data.frame(inspect(test_term_matrix))
class(df_test_term_matrix)

str(sf_train)
sf_train$Address[sample(800000,100)]

Address1 <- llply(sf_train$Address, function(x){
  strsplit(x, ' / | of ')[[1]]
})
hist(sapply(Address1, length))

sf_train$Address[1:20]



strsplit(sf_train$Address[15], ' / | of ')
